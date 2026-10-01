.cdrgam_cli_status_state <- function(state) {
    switch(
        state,
        complete='Success', running='Running', submitted='Queued',
        submitting='Queued', pending='Waiting', blocked='Blocked',
        failed='Error', stale='Stale', state
    )
}

.cdrgam_cli_paint <- function(text, style, color) {
    if (!isTRUE(color)) return(text)
    switch(
        style,
        header=cli::style_bold(cli::col_cyan(text)),
        success=cli::col_green(text),
        active=cli::col_cyan(text),
        queued=cli::col_blue(text),
        warning=cli::col_yellow(text),
        error=cli::style_bold(cli::col_red(text)),
        detail=cli::style_dim(text),
        text
    )
}

.cdrgam_cli_status_report <- function(output, color=FALSE) {
    styles <- list(
        header='header', Success='success', Running='active', Queued='queued',
        Waiting='queued', Blocked='warning', Nonconverged='error', Error='error',
        Stale='warning', detail='detail', error='error'
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
    if (is.null(pager) && .cdrgam_cli_is_windows()) {
        temporary <- tempfile('cdrgam-status-', fileext='.txt')
        writeLines(text, temporary, useBytes=TRUE)
        base::file.show(
            temporary, title='cdrgam status', delete.file=TRUE,
            pager=getOption('pager')
        )
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
        help <- .cdrgam_cli_process_run(executable, '--help')
        if (!is.null(help) && grepl('--header', help$stdout, fixed=TRUE)) {
            arguments <- c(arguments, '--header', as.character(header_lines))
        }
    }
    temporary <- tempfile('cdrgam-status-', fileext='.txt')
    on.exit(unlink(temporary, force=TRUE), add=TRUE)
    writeLines(text, temporary, useBytes=TRUE)
    tryCatch(
        .cdrgam_cli_process_pager(executable, c(arguments, temporary)),
        interrupt=function(condition) NULL
    )
    invisible(text)
}

.cdrgam_cli_status_rows <- function(configuration, selected) {
    project_ids <- if (length(selected)) vapply(selected, function(project) {
        definition <- .cdrgam_cli_read_project_definition(
            project, checkout=configuration$cdrgam_root
        )
        definition$definition$project$id
    }, character(1)) else character()
    output <- .cdrgam_cli_registry_status_records(
        configuration, selected, project_ids
    )
    if (!nrow(output)) return(output)
    project_roots <- .cdrgam_cli_project_roots(configuration)
    if (length(project_ids)) {
        for (project in names(project_ids)) {
            stable <- Reduce(`|`, lapply(
                c('fit', 'prediction', 'effect', 'visualization', 'comparison'),
                function(kind) startsWith(
                    output$work_key,
                    paste0(kind, ':', project_ids[[project]], ':')
                )
            ))
            output$project[stable] <- project
        }
    }
    output$artifact_path <- .cdrgam_cli_registry_resolve(
        configuration, output$artifact_path, field='registry artifact path',
        project_roots=project_roots
    )
    present_attempts <- !is.na(output$attempt_path) & nzchar(output$attempt_path)
    output$attempt_path[present_attempts] <- .cdrgam_cli_registry_resolve(
        configuration, output$attempt_path[present_attempts],
        field='registry attempt path', project_roots=project_roots
    )
    logical_key <- paste(output$project, output$kind, output$name, sep='\034')
    output <- output[!duplicated(logical_key), , drop=FALSE]
    output$display_state <- vapply(
        output$state, .cdrgam_cli_status_state, character(1)
    )
    output$diagnostic <- NA_character_
    fit_rows <- which(output$kind == 'fit')
    for (i in fit_rows) {
        if (!(output$state[[i]] %in% c('complete', 'failed'))) next
        manifest <- tryCatch(suppressWarnings(.cdrgam_cli_read_yaml(file.path(
            output$artifact_path[[i]], 'manifest.yml'
        ))), error=function(error) NULL)
        diagnostics <- manifest$result$diagnostics
        if (!is.null(diagnostics) && identical(diagnostics$converged, FALSE)) {
            output$display_state[[i]] <- 'Nonconverged'
            output$diagnostic[[i]] <- .cdrgam_cli_null(
                diagnostics$message, 'Optimizer convergence was not reached'
            )
        }
    }
    output$display_updated <- sub(
        'T', ' ', substr(output$updated_at, 1L, 16L), fixed=TRUE
    )
    log_root <- ifelse(
        output$state == 'complete', output$artifact_path, output$attempt_path
    )
    legacy_log <- ifelse(
        is.na(log_root), NA_character_, file.path(log_root, 'run.log')
    )
    project_id <- sub('^[^:]+:([^:]+):.*$', '\\1', output$work_key)
    managed_log <- .cdrgam_cli_registry_work_log_paths(
        configuration, project_id, output$kind, output$name,
        project_roots=project_roots
    )
    output$log <- ifelse(file.exists(managed_log), managed_log, legacy_log)
    output
}

#' Report root work state
#'
#' @param projects Project selectors.
#' @param cdrgam_root Configured CDR-GAM root.
#' @param pager Optional pager executable or function.
#' @param use_pager Whether interactive output may open the pager.
#' @return The current registry work-item state, invisibly. `display_state`
#' distinguishes a published fit whose optimizer did not converge.
#' @export
cdrgam_cli_status <- function(
        projects=NULL, cdrgam_root=NULL, pager=NULL, use_pager=TRUE
) {
    selected <- .cdrgam_cli_select_projects(projects, cdrgam_root)
    configuration <- .cdrgam_cli_site(cdrgam_root, create_root=TRUE)
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

.cdrgam_cli_status_log_record <- function(row, definitions) {
    path <- row$log[[1L]]
    if (is.na(path) || !nzchar(path) || !file.exists(path)) return(NULL)
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
    list(
        path=path, project=row$project[[1L]], kind=kind, name=name,
        model=model, models=related_models, dataset=dataset,
        identity=row$identity[[1L]], state=row$state[[1L]],
        modified=file.info(path)$mtime
    )
}

.cdrgam_cli_worker_log_records <- function(configuration) {
    workers <- .cdrgam_cli_registry_worker_records(configuration)
    records <- lapply(seq_len(nrow(workers)), function(index) {
        worker <- workers[index, , drop=FALSE]
        path <- .cdrgam_cli_worker_log_path(
            configuration, worker$worker_id[[1L]]
        )
        if (!file.exists(path)) {
            directory <- .cdrgam_cli_managed_resolve(
                configuration, worker$path[[1L]],
                field='registry worker path', must_work=FALSE
            )
            scheduler_id <- worker$scheduler_id[[1L]]
            legacy <- if (is.na(scheduler_id) || !nzchar(scheduler_id)) {
                NA_character_
            } else file.path(directory, paste0(scheduler_id, '.log'))
            if (!is.na(legacy) && file.exists(legacy)) path <- legacy
        }
        if (!file.exists(path)) return(NULL)
        scheduler_id <- worker$scheduler_id[[1L]]
        list(
            path=path, project='', kind='worker',
            name=worker$worker_id[[1L]], model=NULL, models=character(),
            dataset=NULL,
            identity=if (is.na(scheduler_id)) '' else scheduler_id,
            state=worker$state[[1L]], modified=file.info(path)$mtime
        )
    })
    Filter(Negate(is.null), records)
}

.cdrgam_cli_log_selector_matches <- function(values, patterns, field='log') {
    if (!length(patterns)) return(TRUE)
    if (!length(values)) return(FALSE)
    any(vapply(patterns, function(pattern) {
        length(.cdrgam_cli_match_name_pattern(pattern, values, field)) > 0L
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
    if (length(models) && !.cdrgam_cli_log_selector_matches(
            record$models, models, 'model'
    )) {
        return(FALSE)
    }
    typed <- any(c(
        length(predictions), length(visualizations), length(comparisons)
    ) > 0L)
    if (!typed) return(TRUE)
    (identical(record$kind, 'prediction') && length(predictions) &&
        .cdrgam_cli_log_selector_matches(
            .cdrgam_cli_log_prediction_names(record, definitions), predictions,
            'prediction'
        )) ||
        (identical(record$kind, 'visualization') && length(visualizations) &&
            .cdrgam_cli_log_selector_matches(
                record$name, visualizations, 'visualization'
            )) ||
        (identical(record$kind, 'comparison') && length(comparisons) &&
            .cdrgam_cli_log_selector_matches(
                record$name, comparisons, 'comparison'
            ))
}

.cdrgam_cli_display_logs <- function(records, lines=NULL, pager=NULL) {
    paths <- vapply(records, `[[`, character(1), 'path')
    labels <- vapply(records, function(record) paste0(
        if (identical(record$kind, 'worker')) {
            paste0('worker/', record$name)
        } else paste0(record$project, '/', record$kind, '/', record$name),
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
    if (is.null(pager) && isatty(stdout()) && .cdrgam_cli_is_windows()) {
        base::file.show(shown, header=labels, pager=getOption('pager'))
        return(invisible(paths))
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
        result <- .cdrgam_cli_process_pager(executable, shown)
        if (is.null(result) || !identical(result$status, 0L)) {
            .cdrgam_cli_abort('Pager exited unsuccessfully')
        }
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
#' @param worker Whether to show generic worker lifecycle logs instead of
#'   work-item logs. Worker mode does not accept workload selectors.
#' @param cdrgam_root Configured CDR-GAM root.
#' @param pager Optional pager executable or function. Interactive terminals
#'   use `less` by default; non-interactive calls print labeled sections.
#' @return The selected log paths, newest first, invisibly.
#' @export
cdrgam_cli_log <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, lines=NULL, cdrgam_root=NULL, pager=NULL, worker=FALSE
) {
    configuration <- .cdrgam_cli_site(cdrgam_root, create_root=TRUE)
    if (isTRUE(worker)) {
        if (any(c(
                length(projects), length(models), length(predictions),
                length(visualizations), length(comparisons)
            ) > 0L)) {
            .cdrgam_cli_abort('Worker logs cannot be combined with workload selectors')
        }
        records <- .cdrgam_cli_worker_log_records(configuration)
        if (!length(records)) .cdrgam_cli_abort('No managed worker logs are available')
        modified <- vapply(
            records, function(record) as.numeric(record$modified), numeric(1)
        )
        return(.cdrgam_cli_display_logs(
            records[order(modified, decreasing=TRUE)],
            lines=lines, pager=pager
        ))
    }
    selected_projects <- .cdrgam_cli_select_projects(projects, cdrgam_root)
    records <- list()
    for (project in selected_projects) {
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=FALSE, checkout=cdrgam_root
        )
        rows <- .cdrgam_cli_status_rows(configuration, project)
        candidates <- lapply(seq_len(nrow(rows)), function(index) {
            .cdrgam_cli_status_log_record(rows[index, , drop=FALSE], definitions)
        })
        candidates <- Filter(Negate(is.null), candidates)
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
