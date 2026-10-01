.cdrgam_cli_select_projects <- function(projects=NULL, checkout=NULL) {
    configuration <- .cdrgam_cli_site(checkout, create_root=TRUE)
    directory <- file.path(configuration$cdrgam_root, 'projects')
    available <- if (dir.exists(directory)) {
        basename(list.dirs(directory, recursive=FALSE, full.names=TRUE))
    } else character()
    available <- sort(available[vapply(available, function(name) {
        file.exists(file.path(directory, name, .cdrgam_cli_marker))
    }, logical(1))])
    if (is.null(projects) || !length(projects)) {
        inferred <- tryCatch(.cdrgam_cli_infer_project(checkout), error=function(error) NULL)
        if (!is.null(inferred)) return(inferred)
        return(available)
    }
    .cdrgam_cli_match_names(projects, available, 'project')
}

#' List project definitions
#'
#' @param projects Project selectors. The default is the current project when
#'   inside one, otherwise every configured project.
#' @param cdrgam_root Configured CDR-GAM root.
#' @return Definitions grouped by project, invisibly.
#' @export
cdrgam_cli_list <- function(projects=NULL, cdrgam_root=NULL) {
    selected <- .cdrgam_cli_select_projects(projects, cdrgam_root)
    output <- lapply(stats::setNames(selected, selected), function(project) {
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=FALSE, checkout=cdrgam_root
        )
        list(
            datasets=names(definitions$datasets), models=names(definitions$models),
            visualizations=names(definitions$visualizations),
            comparisons=names(definitions$comparisons)
        )
    })
    for (project in names(output)) {
        cat(project, ':\n', sep='')
        for (kind in names(output[[project]])) cat(
            '  ', kind, ': ',
            if (length(output[[project]][[kind]])) {
                paste(output[[project]][[kind]], collapse=', ')
            } else '(none)', '\n', sep=''
        )
    }
    invisible(output)
}

.cdrgam_cli_combined_graph <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, checkout=NULL, progress=NULL
) {
    .cdrgam_cli_progress(progress, 'selecting projects')
    selected <- .cdrgam_cli_select_projects(projects, checkout)
    if (!length(selected)) .cdrgam_cli_abort('No configured projects matched')
    combined <- list(items=list(), targets=character(), definitions=list())
    fingerprints <- NULL
    for (project in selected) {
        .cdrgam_cli_progress(progress, paste0('reading definitions for ', project))
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=TRUE, checkout=checkout
        )
        if (is.null(fingerprints)) {
            fingerprints <- .cdrgam_cli_fingerprint_context(
                definitions$site, progress
            )
            on.exit(.cdrgam_cli_flush_fingerprint_context(fingerprints), add=TRUE)
        }
        .cdrgam_cli_progress(
            progress, paste0('resolving data identities for ', project)
        )
        graph <- .cdrgam_cli_resolve_graph(
            definitions, models, predictions, visualizations, comparisons,
            fingerprints=fingerprints
        )
        combined$items <- c(combined$items, graph$items)
        combined$targets <- c(combined$targets, graph$targets)
        combined$definitions[[project]] <- definitions
    }
    combined$targets <- unique(combined$targets)
    .cdrgam_cli_progress(
        progress,
        paste0(
            'resolved ', length(combined$items), ' work item',
            if (length(combined$items) == 1L) '' else 's'
        )
    )
    combined
}

.cdrgam_cli_graph_order <- function(graph) {
    remaining <- names(graph$items)
    ordered <- character()
    while (length(remaining)) {
        ready <- remaining[vapply(remaining, function(key) {
            all(graph$items[[key]]$dependencies %in% ordered)
        }, logical(1))]
        if (!length(ready)) .cdrgam_cli_abort('Resolved work graph contains a cycle')
        ordered <- c(ordered, ready)
        remaining <- setdiff(remaining, ready)
    }
    ordered
}

.cdrgam_cli_plan_table <- function(graph) {
    order <- .cdrgam_cli_graph_order(graph)
    do.call(rbind, lapply(order, function(key) {
        item <- graph$items[[key]]
        data.frame(
            project=item$project, kind=item$kind, name=item$name,
            identity=item$identity,
            state=if (.cdrgam_cli_complete_artifact(item$output, item$identity)) {
                'complete'
            } else if (file.exists(item$output)) 'stale' else 'missing',
            target=key %in% graph$targets, path=item$output,
            stringsAsFactors=FALSE
        )
    }))
}

#' Plan root work without executing it
#'
#' @param projects,models,predictions,visualizations,comparisons Conjunctive
#'   selectors. Repeated prediction selectors expand model partitions.
#' @param cdrgam_root Configured CDR-GAM root.
#' @return A data frame describing the dependency-closed work plan.
#' @export
cdrgam_cli_plan <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, cdrgam_root=NULL
) {
    graph <- .cdrgam_cli_combined_graph(
        projects, models, predictions, visualizations, comparisons, cdrgam_root
    )
    output <- .cdrgam_cli_plan_table(graph)
    print(output, row.names=FALSE)
    invisible(output)
}

.cdrgam_cli_run_local <- function(graph) {
    results <- list()
    failed <- character()
    for (key in .cdrgam_cli_graph_order(graph)) {
        item <- graph$items[[key]]
        definitions <- graph$definitions[[item$project]]
        if (any(item$dependencies %in% failed)) {
            .cdrgam_cli_registry_state(definitions$site, item, 'blocked')
            failed <- c(failed, key)
            results[[key]] <- list(
                status='blocked', path=item$output, item=item$key,
                message='An upstream work item did not converge'
            )
            next
        }
        if (.cdrgam_cli_complete_artifact(item$output, item$identity)) {
            .cdrgam_cli_registry_state(definitions$site, item, 'complete')
            results[[key]] <- list(status='reused', path=item$output, item=item$key)
            next
        }
        attempt <- .cdrgam_cli_attempt_directory(definitions, item)
        .cdrgam_cli_registry_attempt(
            definitions$site, item,
            list(
                status='running', job_id=NULL, path=attempt$path,
                definitions=definitions
            )
        )
        .cdrgam_cli_registry_state(definitions$site, item, 'running')
        result <- tryCatch(
            callr::r(
                function(definitions, item, attempt_path) {
                    utils::getFromNamespace('.cdrgam_cli_run_item', 'cdrgam.cli')(
                        definitions, item, attempt_path=attempt_path
                    )
                },
                args=list(
                    definitions=definitions, item=item,
                    attempt_path=attempt$path
                ),
                libpath=.libPaths(), stdout='', stderr='',
                user_profile=FALSE, system_profile=FALSE
            ),
            error=function(error) {
                .cdrgam_cli_registry_state(definitions$site, item, 'failed')
                .cdrgam_cli_registry_attempt_state(
                    definitions$site, item, 'failed', conditionMessage(error)
                )
                stop(error)
            }
        )
        completed <- !identical(result$status, 'nonconverged')
        state <- if (completed) 'complete' else 'failed'
        .cdrgam_cli_registry_state(definitions$site, item, state)
        .cdrgam_cli_registry_attempt_state(
            definitions$site, item, state,
            if (completed) NULL else .cdrgam_cli_null(
                result$message, 'Fit did not converge'
            )
        )
        results[[key]] <- result
        if (!completed) failed <- c(failed, key)
    }
    results
}

#' Run root work
#'
#' @inheritParams cdrgam_cli_plan
#' @param dry_run Print the resolved graph without executing it.
#' @param cpus,memory,time,qos Optional Slurm resource overrides.
#' @details Local execution is serial and isolates each work item in its own R
#'   process. Slurm execution uses the root-wide controller and concurrency
#'   limit.
#' @return Work results, invisibly.
#' @export
cdrgam_cli_run <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, dry_run=FALSE, cpus=NULL, memory=NULL, time=NULL,
        qos=NULL, cdrgam_root=NULL
) {
    progress <- .cdrgam_cli_progress_context()
    on.exit(.cdrgam_cli_progress_finish(progress), add=TRUE)
    graph <- .cdrgam_cli_combined_graph(
        projects, models, predictions, visualizations, comparisons, cdrgam_root,
        progress
    )
    if (isTRUE(dry_run)) {
        output <- .cdrgam_cli_plan_table(graph)
        print(output, row.names=FALSE)
        return(invisible(output))
    }
    configuration <- graph$definitions[[1L]]$site
    selector <- paste(c(
        paste0('project=', projects), paste0('model=', models),
        paste0('prediction=', predictions), paste0('visualization=', visualizations),
        paste0('comparison=', comparisons)
    ), collapse=';')
    resources <- list(cpus=cpus, memory=memory, time=time, qos=qos)
    resources <- resources[!vapply(resources, is.null, logical(1))]
    .cdrgam_cli_progress(
        progress,
        if (identical(configuration$scheduler, 'slurm')) {
            'submitting request to the root scheduler'
        } else 'starting local execution'
    )
    results <- if (identical(configuration$scheduler, 'slurm')) {
        request <- .cdrgam_cli_random_id('submission')
        .cdrgam_cli_controller_submit(graph, request, resources)
    } else {
        if (length(resources)) {
            .cdrgam_cli_abort('Slurm resource overrides require a Slurm-configured root')
        }
        .cdrgam_cli_registry_record_graph(configuration, graph, selector)
        .cdrgam_cli_run_local(graph)
    }
    .cdrgam_cli_progress_finish(
        progress,
        if (identical(configuration$scheduler, 'slurm')) {
            'request accepted'
        } else 'local execution complete'
    )
    invisible(results)
}
