.cdrgam_cli_select_projects <- function(projects=NULL, checkout=NULL) {
    configuration <- .cdrgam_cli_checkout(checkout, create_root=TRUE)
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
#' @param checkout Source checkout root.
#' @return Definitions grouped by project, invisibly.
#' @export
cdrgam_cli_list <- function(projects=NULL, checkout=NULL) {
    selected <- .cdrgam_cli_select_projects(projects, checkout)
    output <- lapply(stats::setNames(selected, selected), function(project) {
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=FALSE, checkout=checkout
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
        comparisons=NULL, checkout=NULL
) {
    selected <- .cdrgam_cli_select_projects(projects, checkout)
    if (!length(selected)) .cdrgam_cli_abort('No configured projects matched')
    combined <- list(items=list(), targets=character(), definitions=list())
    for (project in selected) {
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=TRUE, checkout=checkout
        )
        graph <- .cdrgam_cli_resolve_graph(
            definitions, models, predictions, visualizations, comparisons
        )
        combined$items <- c(combined$items, graph$items)
        combined$targets <- c(combined$targets, graph$targets)
        combined$definitions[[project]] <- definitions
    }
    combined$targets <- unique(combined$targets)
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

#' Plan checkout work without executing it
#'
#' @param projects,models,predictions,visualizations,comparisons Conjunctive
#'   selectors. Repeated prediction selectors expand model partitions.
#' @param checkout Source checkout root.
#' @return A data frame describing the dependency-closed work plan.
#' @export
cdrgam_cli_plan <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, checkout=NULL
) {
    graph <- .cdrgam_cli_combined_graph(
        projects, models, predictions, visualizations, comparisons, checkout
    )
    output <- .cdrgam_cli_plan_table(graph)
    print(output, row.names=FALSE)
    invisible(output)
}

.cdrgam_cli_run_local <- function(graph) {
    results <- list()
    for (key in .cdrgam_cli_graph_order(graph)) {
        item <- graph$items[[key]]
        definitions <- graph$definitions[[item$project]]
        if (.cdrgam_cli_complete_artifact(item$output, item$identity)) {
            .cdrgam_cli_registry_state(definitions$checkout, item, 'complete')
            results[[key]] <- list(status='reused', path=item$output, item=item$key)
            next
        }
        attempt <- .cdrgam_cli_attempt_directory(definitions, item)
        .cdrgam_cli_registry_attempt(
            definitions$checkout, item,
            list(
                status='running', job_id=NULL, path=attempt$path,
                definitions=definitions
            )
        )
        .cdrgam_cli_registry_state(definitions$checkout, item, 'running')
        result <- tryCatch(
            .cdrgam_cli_run_item(definitions, item, attempt_path=attempt$path),
            error=function(error) {
                .cdrgam_cli_registry_state(definitions$checkout, item, 'failed')
                .cdrgam_cli_registry_attempt_state(
                    definitions$checkout, item, 'failed', conditionMessage(error)
                )
                stop(error)
            }
        )
        .cdrgam_cli_registry_state(definitions$checkout, item, 'complete')
        .cdrgam_cli_registry_attempt_state(definitions$checkout, item, 'complete')
        results[[key]] <- result
    }
    results
}

#' Run checkout work
#'
#' @inheritParams cdrgam_cli_plan
#' @param dry_run Print the resolved graph without executing it.
#' @param cpus,memory,time,qos Optional Slurm resource overrides.
#' @return Work results, invisibly.
#' @export
cdrgam_cli_run <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, dry_run=FALSE, cpus=NULL, memory=NULL, time=NULL,
        qos=NULL, checkout=NULL
) {
    graph <- .cdrgam_cli_combined_graph(
        projects, models, predictions, visualizations, comparisons, checkout
    )
    if (isTRUE(dry_run)) {
        output <- .cdrgam_cli_plan_table(graph)
        print(output, row.names=FALSE)
        return(invisible(output))
    }
    configuration <- graph$definitions[[1L]]$checkout
    selector <- paste(c(
        paste0('project=', projects), paste0('model=', models),
        paste0('prediction=', predictions), paste0('visualization=', visualizations),
        paste0('comparison=', comparisons)
    ), collapse=';')
    resources <- list(cpus=cpus, memory=memory, time=time, qos=qos)
    resources <- resources[!vapply(resources, is.null, logical(1))]
    results <- if (identical(configuration$scheduler, 'slurm')) {
        request <- .cdrgam_cli_random_id('submission')
        .cdrgam_cli_controller_submit(graph, request, resources)
    } else {
        if (length(resources)) {
            .cdrgam_cli_abort('Slurm resource overrides require a Slurm-configured checkout')
        }
        .cdrgam_cli_registry_record_graph(configuration, graph, selector)
        .cdrgam_cli_run_local(graph)
    }
    invisible(results)
}

.cdrgam_cli_status_state <- function(state) {
    switch(
        state,
        complete='Success', running='Running', submitted='Queued',
        submitting='Queued', pending='Waiting', blocked='Blocked',
        failed='Error', stale='Stale', state
    )
}

.cdrgam_cli_paint <- function(text, style, color) {
    if (isTRUE(color)) paste0(style, text, '\033[0m') else text
}

.cdrgam_cli_status_report <- function(output, color=FALSE) {
    styles <- list(
        header='\033[1m\033[96m', Success='\033[92m', Running='\033[96m',
        Queued='\033[94m', Waiting='\033[94m', Blocked='\033[93m',
        Nonconverged='\033[91m\033[1m', Error='\033[91m\033[1m',
        Stale='\033[93m', detail='\033[2m', error='\033[91m'
    )
    columns <- list(
        PROJECT=if (nrow(output)) output$project else character(),
        TYPE=if (nrow(output)) output$kind else character(),
        WORKLOAD=if (nrow(output)) output$name else character(),
        STATUS=if (nrow(output)) output$display_state else character(),
        JOB=if (nrow(output)) ifelse(is.na(output$job), '-', output$job) else character(),
        UPDATED=if (nrow(output)) output$display_updated else character(),
        IDENTITY=if (nrow(output)) output$identity else character()
    )
    limits <- c(PROJECT=16L, TYPE=14L, WORKLOAD=32L, STATUS=12L,
        JOB=12L, UPDATED=16L, IDENTITY=16L)
    widths <- vapply(names(columns), function(name) {
        min(limits[[name]], max(nchar(name), nchar(columns[[name]]), 0L))
    }, integer(1))
    shorten <- function(value, width) {
        value <- as.character(value)
        long <- nchar(value) > width
        value[long] <- paste0(substr(value[long], 1L, width - 1L), '~')
        value
    }
    pad <- function(value, width) sprintf(paste0('%-', width, 's'),
        shorten(value, width))
    header <- paste(vapply(names(columns), function(name) {
        pad(name, widths[[name]])
    }, character(1)), collapse=' ')
    lines <- .cdrgam_cli_paint(header, styles$header, color)
    if (!nrow(output)) {
        lines <- c(lines, '', 'No registered work items matched the selection.')
    } else {
        for (i in seq_len(nrow(output))) {
            values <- vapply(names(columns), function(name) {
                pad(columns[[name]][[i]], widths[[name]])
            }, character(1))
            values[['STATUS']] <- .cdrgam_cli_paint(
                values[['STATUS']],
                .cdrgam_cli_null(styles[[output$display_state[[i]]]], '\033[95m'),
                color
            )
            lines <- c(lines, paste(values, collapse=' '))
        }
        counts <- sort(table(output$display_state), decreasing=TRUE)
        lines <- c(lines, '', paste0(
            'Summary: ', paste(paste(names(counts), as.integer(counts)), collapse=' | ')
        ))
    }
    failures <- output[output$display_state == 'Error', , drop=FALSE]
    if (nrow(failures)) {
        lines <- c(lines, '', .cdrgam_cli_paint('Errors', styles$error, color))
        for (i in seq_len(nrow(failures))) {
            failure <- failures[i, , drop=FALSE]
            lines <- c(lines, paste0(
                '- ', failure$project, ' ', failure$kind, '/', failure$name,
                ' [', failure$identity, ']'
            ))
            if (!is.na(failure$error) && nzchar(failure$error)) {
                lines <- c(lines, paste0(
                    '  ', .cdrgam_cli_paint('Error:', styles$error, color),
                    ' ', failure$error
                ))
            }
            if (!is.na(failure$log) && nzchar(failure$log)) {
                lines <- c(lines, paste0(
                    '  Log: ', .cdrgam_cli_paint(failure$log, styles$detail, color)
                ))
            }
        }
    }
    nonconverged <- output[output$display_state == 'Nonconverged', , drop=FALSE]
    if (nrow(nonconverged)) {
        lines <- c(lines, '', .cdrgam_cli_paint(
            'Nonconverged fits', styles$error, color
        ))
        for (i in seq_len(nrow(nonconverged))) {
            fit <- nonconverged[i, , drop=FALSE]
            lines <- c(lines, paste0(
                '- ', fit$project, ' fit/', fit$name,
                ' [', fit$identity, ']'
            ))
            if (!is.na(fit$diagnostic) && nzchar(fit$diagnostic)) {
                lines <- c(lines, paste0(
                    '  Diagnostic: ', fit$diagnostic
                ))
            }
            if (!is.na(fit$log) && nzchar(fit$log)) {
                lines <- c(lines, paste0(
                    '  Log: ', .cdrgam_cli_paint(fit$log, styles$detail, color)
                ))
            }
        }
    }
    paste0(paste(lines, collapse='\n'), '\n')
}

.cdrgam_cli_page_text <- function(
        text, use_pager=TRUE, pager=NULL, header_lines=0L
) {
    if (is.function(pager)) {
        pager(text)
        return(invisible(text))
    }
    if (!isTRUE(use_pager) || !isatty(stdout())) {
        cat(text)
        return(invisible(text))
    }
    pager <- .cdrgam_cli_null(pager, Sys.which('less'))
    if (!length(pager) || !nzchar(pager[[1L]])) {
        cat(text)
        return(invisible(text))
    }
    pager <- .cdrgam_cli_scalar_character(pager, 'pager')
    executable <- if (file.exists(pager)) pager else Sys.which(pager)
    if (!nzchar(executable)) .cdrgam_cli_abort(paste0('Pager was not found: ', pager))
    arguments <- '-R'
    if (header_lines > 0L) {
        help <- suppressWarnings(system2(
            executable, '--help', stdout=TRUE, stderr=TRUE
        ))
        if (any(grepl('--header', help, fixed=TRUE))) {
            arguments <- c(arguments, '--header', as.character(header_lines))
        }
    }
    input <- strsplit(sub('\n$', '', text), '\n', fixed=TRUE)[[1L]]
    tryCatch(
        system2(executable, arguments, input=input),
        interrupt=function(condition) invisible(130L)
    )
    invisible(text)
}

.cdrgam_cli_status_rows <- function(configuration, selected) {
    if (!file.exists(.cdrgam_cli_registry_path(configuration))) return(data.frame())
    project_ids <- if (length(selected)) vapply(selected, function(project) {
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=FALSE, checkout=configuration$checkout
        )
        definitions$project$project$id
    }, character(1)) else character()
    where <- if (length(selected)) {
        name_clause <- paste0(
            'w.project IN (',
            paste(vapply(selected, .cdrgam_cli_sql_quote, character(1)), collapse=','), ')'
        )
        id_clauses <- unlist(lapply(project_ids, function(id) vapply(
            c('fit', 'prediction', 'effect', 'visualization', 'comparison'),
            function(kind) paste0(
                'w.work_key LIKE ', .cdrgam_cli_sql_quote(paste0(kind, ':', id, ':%'))
            ), character(1)
        )), use.names=FALSE)
        paste0(' WHERE (', paste(c(name_clause, id_clauses), collapse=' OR '), ')')
    } else ''
    output <- .cdrgam_cli_registry_exec(configuration, paste0(
        'SELECT w.work_key,w.project,w.kind,w.name,w.identity,w.state,',
        'w.updated_at,w.artifact_path,a.scheduler_id AS job,a.path AS attempt_path,',
        'a.error FROM work_items w LEFT JOIN attempts a ON a.attempt_id=(',
        'SELECT a2.attempt_id FROM attempts a2 WHERE a2.work_key=w.work_key ',
        "ORDER BY COALESCE(a2.started_at,a2.completed_at,'') DESC,",
        'a2.attempt_id DESC LIMIT 1)', where,
        ' ORDER BY w.project,w.kind,w.name,w.updated_at DESC,w.identity DESC'
    ), query=TRUE)
    if (!nrow(output)) return(output)
    if (length(project_ids)) {
        for (project in names(project_ids)) {
            stable <- vapply(c('fit', 'prediction', 'effect', 'visualization', 'comparison'),
                function(kind) startsWith(
                    output$work_key, paste0(kind, ':', project_ids[[project]], ':')
                ), logical(nrow(output)))
            output$project[apply(stable, 1L, any)] <- project
        }
    }
    output$artifact_path <- vapply(output$artifact_path, function(path) {
        .cdrgam_cli_managed_resolve(
            configuration, path, field='registry artifact path', must_work=FALSE
        )
    }, character(1))
    present_attempts <- !is.na(output$attempt_path) & nzchar(output$attempt_path)
    output$attempt_path[present_attempts] <- vapply(
        output$attempt_path[present_attempts], function(path) {
            .cdrgam_cli_managed_resolve(
                configuration, path, field='registry attempt path', must_work=FALSE
            )
        }, character(1)
    )
    logical_key <- paste(output$project, output$kind, output$name, sep='\034')
    output <- output[!duplicated(logical_key), , drop=FALSE]
    output$display_state <- vapply(
        output$state, .cdrgam_cli_status_state, character(1)
    )
    output$diagnostic <- NA_character_
    completed_fits <- which(output$state == 'complete' & output$kind == 'fit')
    for (i in completed_fits) {
        manifest <- tryCatch(.cdrgam_cli_read_yaml(file.path(
            output$artifact_path[[i]], 'manifest.yml'
        )), error=function(error) NULL)
        if (!is.null(manifest) && identical(manifest$diagnostics$converged, FALSE)) {
            output$display_state[[i]] <- 'Nonconverged'
            output$diagnostic[[i]] <- .cdrgam_cli_null(
                manifest$diagnostics$message, 'Optimizer convergence was not reached'
            )
        }
    }
    output$display_updated <- sub(
        'T', ' ', substr(output$updated_at, 1L, 16L), fixed=TRUE
    )
    log_root <- ifelse(
        output$state == 'complete', output$artifact_path, output$attempt_path
    )
    output$log <- ifelse(is.na(log_root), NA_character_, file.path(log_root, 'run.log'))
    output
}

#' Report checkout work state
#'
#' @param projects Project selectors.
#' @param checkout Source checkout root.
#' @param pager Optional pager executable or function.
#' @param use_pager Whether interactive output may open the pager.
#' @return The current registry work-item state, invisibly. `display_state`
#' distinguishes a published fit whose optimizer did not converge.
#' @export
cdrgam_cli_status <- function(
        projects=NULL, checkout=NULL, pager=NULL, use_pager=TRUE
) {
    selected <- .cdrgam_cli_select_projects(projects, checkout)
    configuration <- .cdrgam_cli_checkout(checkout, create_root=TRUE)
    output <- .cdrgam_cli_status_rows(configuration, selected)
    color <- isatty(stdout()) && is.na(Sys.getenv(
        'NO_COLOR', unset=NA_character_
    ))
    report <- .cdrgam_cli_status_report(output, color=color)
    .cdrgam_cli_page_text(
        report, use_pager=use_pager, pager=pager, header_lines=1L
    )
    invisible(output)
}

.cdrgam_cli_log_record <- function(path, definitions) {
    directory <- dirname(path)
    manifest_path <- file.path(directory, 'manifest.yml')
    attempt_path <- file.path(directory, 'attempt.yml')
    metadata_path <- if (file.exists(manifest_path)) manifest_path else attempt_path
    if (!file.exists(metadata_path)) return(NULL)
    metadata <- tryCatch(.cdrgam_cli_read_yaml(metadata_path), error=function(error) NULL)
    if (is.null(metadata) || is.null(metadata$kind)) return(NULL)
    published <- identical(metadata_path, manifest_path)
    name <- if (published) metadata$definition else metadata$name
    if (is.null(name)) return(NULL)
    resolved <- if (published) metadata$resolved else NULL
    kind <- metadata$kind
    model <- NULL
    dataset <- NULL
    related_models <- character()
    if (identical(kind, 'fit')) {
        model <- .cdrgam_cli_null(resolved$definition$model, name)
        related_models <- model
    } else if (identical(kind, 'prediction')) {
        model <- resolved$model$definition$model
        dataset <- resolved$dataset$definition$dataset
        if (is.null(model)) model <- sub('_.*$', '', name)
        if (is.null(dataset)) dataset <- sub(paste0('^', model, '_'), '', name)
        related_models <- model
    } else if (identical(kind, 'visualization')) {
        model <- resolved$definition$model
        if (is.null(model) && !is.null(definitions$visualizations[[name]])) {
            model <- definitions$visualizations[[name]]$model
        }
        related_models <- .cdrgam_cli_null(model, character())
    } else if (identical(kind, 'effect')) {
        model <- resolved$inputs$model$definition$model
        related_models <- .cdrgam_cli_null(model, character())
    } else if (identical(kind, 'comparison')) {
        related_models <- resolved$definition$models
        if (is.null(related_models) && !is.null(definitions$comparisons[[name]])) {
            related_models <- definitions$comparisons[[name]]$models
        }
        related_models <- .cdrgam_cli_null(related_models, character())
    }
    list(
        path=path, project=definitions$project$project$name, kind=kind,
        name=name, model=model, models=related_models, dataset=dataset,
        identity=.cdrgam_cli_null(metadata$identity, ''),
        state=.cdrgam_cli_null(metadata$status, 'unknown'),
        modified=file.info(path)$mtime
    )
}

.cdrgam_cli_log_records <- function(definitions) {
    public_roots <- file.path(definitions$root, c('models', 'comparisons', 'analyses'))
    public <- unlist(lapply(public_roots[dir.exists(public_roots)], function(root) {
        list.files(
            root, pattern='^run\\.log$', full.names=TRUE, recursive=TRUE,
            include.dirs=FALSE
        )
    }), use.names=FALSE)
    work <- .cdrgam_cli_path(definitions, 'work')
    private <- if (dir.exists(work)) list.files(
        work, pattern='^run\\.log$', full.names=TRUE, recursive=TRUE,
        include.dirs=FALSE
    ) else character()
    records <- lapply(unique(c(public, private)), function(path) {
        .cdrgam_cli_log_record(path, definitions)
    })
    Filter(Negate(is.null), records)
}

.cdrgam_cli_log_selector_matches <- function(values, patterns) {
    if (!length(patterns)) return(TRUE)
    if (!length(values)) return(FALSE)
    any(vapply(patterns, function(pattern) {
        pattern <- .cdrgam_cli_scalar_character(pattern, 'log selector')
        if (identical(pattern, '*')) return(TRUE)
        expression <- paste0(
            '^', gsub('\\*', '.*', gsub('([][{}()+?.^$|\\\\])', '\\\\\\1', pattern)), '$'
        )
        any(grepl(expression, values))
    }, logical(1)))
}

.cdrgam_cli_log_prediction_names <- function(record, definitions) {
    values <- record$dataset
    if (!is.null(record$model) && !is.null(definitions$models[[record$model]])) {
        mapping <- definitions$models[[record$model]]$datasets
        values <- c(values, names(mapping)[unlist(mapping, use.names=FALSE) == record$dataset])
    }
    unique(values)
}

.cdrgam_cli_log_selected <- function(
        record, definitions, models, predictions, visualizations, comparisons
) {
    if (length(models) && !.cdrgam_cli_log_selector_matches(record$models, models)) {
        return(FALSE)
    }
    typed <- any(c(
        length(predictions), length(visualizations), length(comparisons)
    ) > 0L)
    if (!typed) return(TRUE)
    (identical(record$kind, 'prediction') && length(predictions) &&
        .cdrgam_cli_log_selector_matches(
            .cdrgam_cli_log_prediction_names(record, definitions), predictions
        )) ||
        (identical(record$kind, 'visualization') && length(visualizations) &&
            .cdrgam_cli_log_selector_matches(record$name, visualizations)) ||
        (identical(record$kind, 'comparison') && length(comparisons) &&
            .cdrgam_cli_log_selector_matches(record$name, comparisons))
}

.cdrgam_cli_display_logs <- function(records, lines=NULL, pager=NULL) {
    paths <- vapply(records, `[[`, character(1), 'path')
    labels <- vapply(records, function(record) paste0(
        record$project, '/', record$kind, '/', record$name,
        if (nzchar(record$identity)) paste0(' [', record$identity, ']') else '',
        ' (', record$state, ')'
    ), character(1))
    shown <- paths
    temporary <- NULL
    if (!is.null(lines)) {
        lines <- .cdrgam_cli_positive_integer(lines, 'lines')
        temporary <- tempfile('cdrgam-logs-')
        if (!dir.create(temporary)) .cdrgam_cli_abort('Could not create log view')
        on.exit(unlink(temporary, recursive=TRUE, force=TRUE), add=TRUE)
        shown <- vapply(seq_along(paths), function(index) {
            destination <- file.path(temporary, paste0(
                sprintf('%04d-', index), gsub('[^A-Za-z0-9._-]', '-', labels[[index]]),
                '.log'
            ))
            writeLines(utils::tail(readLines(paths[[index]], warn=FALSE), lines), destination)
            destination
        }, character(1))
    }
    if (is.null(pager) && isatty(stdout())) pager <- Sys.which('less')
    if (is.function(pager)) {
        pager(shown, labels)
    } else if (length(pager) && nzchar(pager[[1L]])) {
        pager <- .cdrgam_cli_scalar_character(pager, 'pager')
        executable <- if (file.exists(pager)) pager else Sys.which(pager)
        if (!nzchar(executable)) .cdrgam_cli_abort(paste0('Pager was not found: ', pager))
        message(
            'Opening ', length(paths), ' log(s); use :n and :p to switch files, q to quit.'
        )
        status <- system2(executable, shQuote(shown))
        if (!identical(status, 0L)) .cdrgam_cli_abort('Pager exited unsuccessfully')
    } else {
        for (index in seq_along(paths)) {
            cat('==> ', labels[[index]], ' <==\n', sep='')
            content <- readLines(paths[[index]], warn=FALSE)
            if (!is.null(lines)) content <- utils::tail(content, lines)
            if (length(content)) cat(content, sep='\n')
            cat('\n')
        }
    }
    invisible(paths)
}

#' View managed work logs
#'
#' @param projects,models,predictions,visualizations,comparisons Log selectors.
#'   Omitted selector dimensions match all logged workloads.
#' @param lines Optional maximum number of trailing lines from each log.
#' @param checkout Source checkout root.
#' @param pager Optional pager executable or function. Interactive terminals
#'   use `less` by default; non-interactive calls print labeled sections.
#' @return The selected log paths, newest first, invisibly.
#' @export
cdrgam_cli_log <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, lines=NULL, checkout=NULL, pager=NULL
) {
    selected_projects <- .cdrgam_cli_select_projects(projects, checkout)
    records <- list()
    for (project in selected_projects) {
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=FALSE, checkout=checkout
        )
        candidates <- .cdrgam_cli_log_records(definitions)
        candidates <- Filter(function(record) .cdrgam_cli_log_selected(
            record, definitions, models, predictions, visualizations, comparisons
        ), candidates)
        records <- c(records, candidates)
    }
    if (!length(records)) .cdrgam_cli_abort('No managed logs matched the selectors')
    modified <- vapply(records, function(record) as.numeric(record$modified), numeric(1))
    records <- records[order(modified, decreasing=TRUE)]
    logical_workload <- vapply(records, function(record) paste(
        record$project, record$kind, record$name, sep='\034'
    ), character(1))
    records <- records[!duplicated(logical_workload)]
    .cdrgam_cli_display_logs(records, lines=lines, pager=pager)
}

#' Preview or remove generated artifacts
#'
#' @param projects,models Project and model selectors.
#' @param work,logs Include private attempts or logs.
#' @param yes Remove selected paths. The default previews them.
#' @param checkout Source checkout root.
#' @return Selected generated paths, invisibly.
#' @export
cdrgam_cli_purge <- function(
        projects=NULL, models=NULL, work=FALSE, logs=FALSE, yes=FALSE,
        checkout=NULL
) {
    selected <- .cdrgam_cli_select_projects(projects, checkout)
    if (!length(models) && !isTRUE(work) && !isTRUE(logs)) {
        .cdrgam_cli_abort('Purge requires --model, --work, or --logs')
    }
    targets <- character()
    for (project in selected) {
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=FALSE, checkout=checkout
        )
        if (length(models)) {
            names <- .cdrgam_cli_match_names(models, names(definitions$models), 'model')
            targets <- c(targets, vapply(names, function(name) {
                .cdrgam_cli_path(definitions, 'model', name)
            }, character(1)))
        }
        for (kind in c(if (work) 'work', if (logs) 'log')) {
            directory <- .cdrgam_cli_path(definitions, kind)
            if (dir.exists(directory)) targets <- c(targets, directory)
        }
    }
    targets <- unique(targets[file.exists(targets)])
    if (!length(targets)) {
        message('No generated artifacts matched the selection')
        return(invisible(targets))
    }
    cat('Selected generated artifacts:\n', paste0('  ', targets, collapse='\n'), '\n')
    if (!isTRUE(yes)) {
        message('Preview only; pass --yes to remove these targets')
        return(invisible(targets))
    }
    for (target in targets) unlink(target, recursive=TRUE, force=FALSE)
    remaining <- targets[file.exists(targets)]
    if (length(remaining)) .cdrgam_cli_abort(paste0(
        'Could not remove: ', paste(remaining, collapse=', ')
    ))
    message('Removed ', length(targets), ' generated artifact target(s)')
    invisible(targets)
}
