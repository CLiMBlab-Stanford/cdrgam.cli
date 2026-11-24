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
    available <- list(
        models=character(), visualizations=character(), comparisons=character()
    )
    fingerprints <- NULL
    for (project in selected) {
        .cdrgam_cli_progress(progress, paste0('reading definitions for ', project))
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=TRUE, checkout=checkout
        )
        available$models <- c(available$models, names(definitions$models))
        available$visualizations <- c(
            available$visualizations, names(definitions$visualizations)
        )
        available$comparisons <- c(
            available$comparisons, names(definitions$comparisons)
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
            fingerprints=fingerprints, strict_selectors=FALSE
        )
        combined$items <- c(combined$items, graph$items)
        combined$targets <- c(combined$targets, graph$targets)
        combined$definitions[[project]] <- definitions
    }
    requested <- list(
        models=models, visualizations=visualizations, comparisons=comparisons
    )
    for (field in names(requested)) if (length(requested[[field]])) {
        .cdrgam_cli_match_names(
            requested[[field]], unique(available[[field]]),
            sub('s$', '', field)
        )
    }
    combined$targets <- unique(combined$targets)
    if (!length(combined$targets)) {
        .cdrgam_cli_abort('No workloads matched the combined selectors')
    }
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
#' @param projects,models,predictions,visualizations,comparisons Selector
#'   dimensions are conjunctive; repeated values within a dimension are
#'   combined. With multiple projects, definition selectors match their union
#'   rather than being required to match inside every project. Repeated
#'   prediction selectors expand model partitions.
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
        attempt <- .cdrgam_cli_await_attempt_directory(definitions, item)
        if (is.null(attempt)) {
            .cdrgam_cli_registry_state(definitions$site, item, 'complete')
            results[[key]] <- list(status='reused', path=item$output, item=item$key)
            next
        }
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
        completed <- !(result$status %in% c('nonconverged', 'unresolved'))
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

.cdrgam_cli_stop_keys <- function(
        configuration, projects, models, predictions, visualizations, comparisons
) {
    selected <- .cdrgam_cli_select_projects(projects, configuration$cdrgam_root)
    project_ids <- vapply(selected, function(project) {
        .cdrgam_cli_read_project_definition(
            project, checkout=configuration$cdrgam_root
        )$definition$project$id
    }, character(1))
    rows <- .cdrgam_cli_registry_status_records(
        configuration, selected, project_ids
    )
    if (!nrow(rows)) return(character())
    keys <- character()
    for (project in selected) {
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=FALSE, checkout=configuration$cdrgam_root
        )
        candidates <- rows[rows$project == project, , drop=FALSE]
        for (index in seq_len(nrow(candidates))) {
            row <- candidates[index, , drop=FALSE]
            kind <- row$kind[[1L]]
            name <- row$name[[1L]]
            model <- dataset <- NULL
            related_models <- character()
            if (identical(kind, 'fit')) {
                model <- name
                related_models <- model
            } else if (kind %in% c('prediction', 'effect')) {
                model <- sub('_.*$', '', name)
                dataset <- if (identical(kind, 'prediction')) {
                    sub(paste0('^', model, '_'), '', name)
                } else NULL
                related_models <- model
            } else if (identical(kind, 'visualization')) {
                model <- definitions$visualizations[[name]]$model
                related_models <- .cdrgam_cli_null(model, character())
            } else if (identical(kind, 'comparison')) {
                related_models <- .cdrgam_cli_null(
                    definitions$comparisons[[name]]$models, character()
                )
            }
            record <- list(
                kind=kind, name=name, model=model, models=related_models,
                dataset=dataset
            )
            if (.cdrgam_cli_log_selected(
                    record, definitions, models, predictions, visualizations, comparisons
            )) {
                keys <- c(keys, row$work_key[[1L]])
            }
        }
    }
    unique(keys)
}

.cdrgam_cli_stop_scheduler <- function(configuration, force=FALSE) {
    worker_job_ids <- if (isTRUE(force)) {
        .cdrgam_cli_registry_active_worker_job_ids(configuration)
    } else character()
    endpoint <- .cdrgam_cli_controller_read(configuration)
    stopped <- FALSE
    if (!is.null(endpoint)) {
        stopped <- isTRUE(suppressWarnings(tryCatch({
            .cdrgam_cli_controller_call(
                endpoint, list(type='shutdown', force=isTRUE(force)), retries=2L
            )
            TRUE
        }, error=function(error) FALSE)))
    }
    metadata_path <- file.path(configuration$cdrgam_root, '.cdrgam', 'scheduler.yml')
    metadata <- tryCatch(.cdrgam_cli_read_yaml(metadata_path), error=function(error) NULL)
    if (!stopped) {
        if (!isTRUE(force)) {
            .cdrgam_cli_abort(paste(
                'Could not confirm root scheduler shutdown. Use --force only',
                'after confirming that cancelling its Slurm job is appropriate.'
            ))
        }
        if (!is.null(metadata$job_id)) .cdrgam_cli_scancel(metadata$job_id)
    }
    if (!is.null(metadata)) {
        metadata$state <- 'stopped'
        metadata$stopped_at <- .cdrgam_cli_timestamp()
        .cdrgam_cli_write_yaml(metadata, metadata_path)
    }
    if (isTRUE(force)) .cdrgam_cli_scancel(worker_job_ids)
    invisible(TRUE)
}

#' Run root work
#'
#' @inheritParams cdrgam_cli_plan
#' @param dry_run Print the resolved graph without executing it.
#' @param cpus,partition,memory,time,qos Optional Slurm worker resource
#'   overrides. `partition` also applies to a newly submitted root scheduler.
#' @param scheduler_time Optional root scheduler time-limit override. Worker time
#'   remains controlled by `time`.
#' @details Local execution is serial and isolates each work item in its own R
#'   process. Slurm execution uses the root-wide controller and concurrency
#'   limit.
#' @return Work results, invisibly.
#' @export
cdrgam_cli_run <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, dry_run=FALSE, cpus=NULL, partition=NULL, memory=NULL,
        time=NULL, scheduler_time=NULL, qos=NULL, cdrgam_root=NULL
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
    resources <- list(
        cpus=cpus, partition=partition, memory=memory, time=time, qos=qos
    )
    resources <- resources[!vapply(resources, is.null, logical(1))]
    scheduler_resources <- list(partition=partition, time=scheduler_time)
    scheduler_resources <- scheduler_resources[
        !vapply(scheduler_resources, is.null, logical(1))
    ]
    .cdrgam_cli_progress(
        progress,
        if (identical(configuration$scheduler, 'slurm')) {
            'submitting request to the root scheduler'
        } else 'starting local execution'
    )
    results <- if (identical(configuration$scheduler, 'slurm')) {
        request <- .cdrgam_cli_random_id('submission')
        .cdrgam_cli_controller_submit(
            graph, request, resources, scheduler_resources=scheduler_resources
        )
    } else {
        if (length(resources) || length(scheduler_resources)) {
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

#' Stop selected Slurm work
#'
#' @inheritParams cdrgam_cli_run
#' @param force Whether to record a work-item stop when the root scheduler
#'   cannot be contacted. In scheduler mode, force also cancels active worker
#'   jobs before the scheduler exits.
#' @param scheduler Whether to stop the idle root scheduler rather than work
#'   items. Scheduler mode cannot be combined with selectors.
#' @param cdrgam_root Configured CDR-GAM root.
#' @return Stopped work keys, invisibly.
#' @details Stops matching target work items without removing their definitions,
#' artifacts, logs, or checkpoints. A later `cdrgam run` may submit them again.
#' The root scheduler otherwise stays available until stopped or its Slurm time
#' limit expires.
#' @export
cdrgam_cli_stop <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, force=FALSE, scheduler=FALSE, cdrgam_root=NULL
) {
    configuration <- .cdrgam_cli_site(cdrgam_root, create_root=TRUE)
    if (!identical(configuration$scheduler, 'slurm')) {
        .cdrgam_cli_abort('cdrgam stop is available only for Slurm-managed work')
    }
    if (isTRUE(scheduler)) {
        if (any(c(
                length(projects), length(models), length(predictions),
                length(visualizations), length(comparisons)
        ) > 0L)) {
            .cdrgam_cli_abort('--scheduler cannot be combined with work selectors')
        }
        .cdrgam_cli_stop_scheduler(configuration, force=force)
        message('Stopped root scheduler')
        return(invisible(character()))
    }
    keys <- .cdrgam_cli_stop_keys(
        configuration, projects, models, predictions, visualizations, comparisons
    )
    if (!length(keys)) .cdrgam_cli_abort('No registered work items matched the selectors')
    job_ids <- .cdrgam_cli_registry_active_job_ids(configuration, keys)
    endpoint <- .cdrgam_cli_controller_read(configuration)
    acknowledged <- FALSE
    if (!is.null(endpoint)) {
        acknowledged <- isTRUE(suppressWarnings(tryCatch({
            .cdrgam_cli_controller_call(
                endpoint, list(type='stop', work_keys=keys), retries=2L
            )
            TRUE
        }, error=function(error) FALSE)))
    }
    if (!acknowledged && !is.null(endpoint) && !isTRUE(force)) {
        .cdrgam_cli_abort(paste(
            'Could not confirm the stop with the root scheduler. Stop that',
            'scheduler first, then repeat this command with --force.'
        ))
    }
    if (!acknowledged) {
        for (key in keys) {
            item <- list(key=key)
            .cdrgam_cli_registry_state(configuration, item, 'stopped')
            .cdrgam_cli_registry_attempt_state(
                configuration, item, 'stopped', 'Stopped by user'
            )
        }
    }
    .cdrgam_cli_scancel(job_ids)
    message(
        'Stopped ', length(keys), ' work item', if (length(keys) == 1L) '' else 's',
        if (length(job_ids)) paste0(' and cancelled ', length(job_ids),
            ' Slurm worker job', if (length(job_ids) == 1L) '' else 's') else ''
    )
    invisible(keys)
}
