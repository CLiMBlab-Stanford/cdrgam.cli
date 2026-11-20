#' Preview or remove generated artifacts
#'
#' @param projects,models,predictions,visualizations,comparisons,datasets
#'   Conjunctive selectors for generated results. Dataset selectors address
#'   dataset artifacts directly.
#' @param work,logs Include private attempts or logs.
#' @param force Remove selected paths without an interactive preview or prompt.
#' @param confirm Whether to confirm removal after showing the selection. By
#'   default, confirmation is offered only when standard input and output are
#'   terminals.
#' @param pager Optional pager executable or function for the selection.
#' @param use_pager Whether an interactive selection may open the pager.
#' @param cdrgam_root Configured CDR-GAM root.
#' @return Selected generated paths, invisibly.
#' @details Registry records and private attempt directories for selected work
#'   items are removed even when their generated artifacts are already absent.
#' @export
cdrgam_cli_purge <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, datasets=NULL, work=FALSE, logs=FALSE,
        force=FALSE, cdrgam_root=NULL, confirm=NULL, pager=NULL,
        use_pager=TRUE
) {
    workload_selected <- any(c(
        length(models), length(predictions), length(visualizations),
        length(comparisons), length(datasets)
    ) > 0L)
    if (!workload_selected && !isTRUE(work) && !isTRUE(logs) &&
            (is.null(projects) || !length(projects))) {
        inferred <- tryCatch(
            .cdrgam_cli_infer_project(cdrgam_root),
            error=function(error) NULL
        )
        if (is.null(inferred)) {
            .cdrgam_cli_abort(paste(
                'Purge requires a project or workload selector when run',
                'outside a configured project'
            ))
        }
        projects <- inferred
    }
    selected <- .cdrgam_cli_select_projects(projects, cdrgam_root)
    configuration <- .cdrgam_cli_site(cdrgam_root, create_root=TRUE)
    targets <- character()
    registry_keys <- character()
    for (project in selected) {
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=FALSE, checkout=cdrgam_root
        )
        model_names <- if (length(models)) {
            .cdrgam_cli_match_names(
                models, names(definitions$models), 'model'
            )
        } else names(definitions$models)
        typed <- any(c(
            length(predictions), length(visualizations), length(comparisons),
            length(datasets)
        ) > 0L)
        project_targets <- character()
        if (length(models) && !typed) {
            project_targets <- c(project_targets, vapply(
                model_names, function(name) {
                .cdrgam_cli_path(definitions, 'model', name)
            }, character(1)))
        }
        if (length(predictions)) {
            for (model_name in model_names) {
                model <- definitions$models[[model_name]]
                choices <- unique(c(
                    names(model$datasets), names(definitions$datasets)
                ))
                prediction_names <- .cdrgam_cli_match_names(
                    predictions, choices, 'prediction'
                )
                dataset_names <- unique(vapply(
                    prediction_names,
                    function(selector) .cdrgam_cli_resolve_prediction_dataset(
                        definitions, model, selector
                    ),
                    character(1)
                ))
                project_targets <- c(project_targets, vapply(
                    dataset_names, function(dataset_name) {
                        .cdrgam_cli_path(
                            definitions, 'prediction', model_name,
                            dataset=dataset_name
                        )
                    }, character(1)
                ))
            }
        }
        if (length(visualizations)) {
            visualization_names <- .cdrgam_cli_match_names(
                visualizations, names(definitions$visualizations),
                'visualization'
            )
            if (length(models)) {
                visualization_names <- visualization_names[vapply(
                    definitions$visualizations[visualization_names],
                    function(value) value$model %in% model_names,
                    logical(1)
                )]
                if (!length(visualization_names)) {
                    .cdrgam_cli_abort(paste(
                        'Combined model and visualization selectors',
                        'matched nothing'
                    ))
                }
            }
            project_targets <- c(project_targets, vapply(
                visualization_names, function(name) {
                    value <- definitions$visualizations[[name]]
                    .cdrgam_cli_path(
                        definitions, 'visualization', value$model,
                        dataset=name
                    )
                }, character(1)
            ))
        }
        if (length(comparisons)) {
            comparison_names <- .cdrgam_cli_match_names(
                comparisons, names(definitions$comparisons), 'comparison'
            )
            if (length(models)) {
                comparison_names <- comparison_names[vapply(
                    definitions$comparisons[comparison_names],
                    function(value) all(model_names %in% value$models),
                    logical(1)
                )]
                if (!length(comparison_names)) {
                    .cdrgam_cli_abort(paste(
                        'Combined model and comparison selectors matched',
                        'nothing'
                    ))
                }
            }
            project_targets <- c(project_targets, vapply(
                comparison_names, function(name) {
                    .cdrgam_cli_path(definitions, 'comparison', name)
                }, character(1)
            ))
        }
        if (length(datasets)) {
            dataset_names <- .cdrgam_cli_match_names(
                datasets, names(definitions$datasets), 'dataset'
            )
            project_targets <- c(project_targets, vapply(
                dataset_names, function(name) {
                    .cdrgam_cli_path(definitions, 'dataset', name)
                }, character(1)
            ))
        }
        if (!workload_selected) {
            roots <- file.path(definitions$root, 'results', c(
                'datasets', 'models', 'comparisons', 'analyses'
            ))
            project_targets <- c(project_targets, unlist(lapply(
                roots[dir.exists(roots)], function(root) {
                    list.files(root, full.names=TRUE, all.files=TRUE, no..=TRUE)
                }
            ), use.names=FALSE))
        }
        project_targets <- c(project_targets, .cdrgam_cli_purge_work_targets(
            definitions, project_targets
        ))
        project_registry_keys <- if (!workload_selected) {
            .cdrgam_cli_registry_keys_for_project(
                configuration, project, definitions$project$project$id
            )
        } else {
            .cdrgam_cli_registry_keys_for_artifacts(
                configuration, project_targets
            )
        }
        registry_keys <- c(registry_keys, project_registry_keys)
        targets <- c(targets, project_targets)
        for (kind in c(if (work) 'work', if (logs) 'log')) {
            directory <- .cdrgam_cli_path(definitions, kind)
            if (dir.exists(directory)) targets <- c(targets, directory)
        }
    }
    targets <- unique(targets[file.exists(targets)])
    registry_keys <- unique(registry_keys)
    if (!length(targets) && !length(registry_keys)) {
        message('No generated artifacts or registry workloads matched the selection')
        return(invisible(targets))
    }
    preview <- c(
        if (length(targets)) c(
            'Selected generated artifacts:', paste0('  ', targets)
        ) else character(),
        if (length(registry_keys)) c(
            'Selected registry workloads:', paste0('  ', registry_keys)
        ) else character()
    )
    preview <- paste0(paste(preview, collapse='\n'), '\n')
    proceed <- .cdrgam_cli_confirm_destructive(
        preview,
        prompt='Remove these selected paths and registry workloads? [y/N] ',
        force=force, confirm=confirm, pager=pager, use_pager=use_pager,
        cancel_message='Purge cancelled'
    )
    if (!proceed) {
        return(invisible(targets))
    }
    .cdrgam_cli_registry_assert_inactive(configuration, registry_keys)
    for (target in targets) unlink(target, recursive=TRUE, force=FALSE)
    remaining <- targets[file.exists(targets)]
    if (length(remaining)) .cdrgam_cli_abort(paste0(
        'Could not remove: ', paste(remaining, collapse=', ')
    ))
    .cdrgam_cli_registry_forget(configuration, registry_keys)
    message(
        'Removed ', length(targets), ' generated artifact target(s) and ',
        length(registry_keys), ' registry workload(s)'
    )
    invisible(targets)
}

.cdrgam_cli_confirm_destructive <- function(
        preview, prompt, force=FALSE, confirm=NULL, pager=NULL,
        use_pager=TRUE, cancel_message='Operation cancelled',
        reader=.cdrgam_cli_read_terminal_line
) {
    if (isTRUE(force)) return(TRUE)
    .cdrgam_cli_page_text(
        preview, use_pager=use_pager, pager=pager, header_lines=1L
    )
    if (is.null(confirm)) confirm <- isatty(stdin()) && isatty(stdout())
    if (!isTRUE(confirm)) {
        message('Preview only; rerun with --force to apply this removal')
        return(FALSE)
    }
    response <- reader(prompt)
    if (!(tolower(trimws(response)) %in% c('y', 'yes'))) {
        message(cancel_message)
        return(FALSE)
    }
    TRUE
}

.cdrgam_cli_read_terminal_line <- function(
        prompt, input=NULL, output=stderr()
) {
    cat(prompt, file=output)
    flush(output)
    if (!is.null(input)) {
        response <- readLines(input, n=1L, warn=FALSE)
    } else if (.cdrgam_cli_is_windows()) {
        response <- system2(
            Sys.getenv('COMSPEC', 'cmd.exe'),
            c(
                '/d', '/v:on', '/s', '/c',
                shQuote(
                    'set /p "cdrgam_response=" & echo(!cdrgam_response!'
                )
            ),
            stdout=TRUE
        )
    } else {
        response <- system2(
            '/bin/sh',
            c(
                '-c',
                shQuote(
                    "IFS= read -r response; printf '%s\\n' \"$response\""
                )
            ),
            stdout=TRUE
        )
    }
    if (length(response)) response[[1L]] else ''
}

# Private attempt directories are named `<kind>_<name>` beneath the work root.
# Return the existing directories that belong to work items whose generated
# artifacts are selected, so a purge cannot leave a failed attempt (and its
# optimizer checkpoints) to be resumed by the next run.
.cdrgam_cli_purge_work_targets <- function(definitions, targets) {
    root <- .cdrgam_cli_path(definitions, 'work')
    if (!length(targets) || !dir.exists(root)) return(character())
    existing <- basename(list.dirs(root, recursive=FALSE, full.names=TRUE))
    if (!length(existing)) return(character())
    # The central path builder returns normalized paths, so only the targets
    # need normalizing. Normalizing once keeps this linear in the number of
    # candidates.
    normalized_targets <- sub(
        '/+$', '', .cdrgam_cli_normalize_path(targets, must_work=FALSE)
    )
    selected <- function(path) {
        path <- sub('/+$', '', path)
        any(path == normalized_targets |
            startsWith(path, paste0(normalized_targets, '/')))
    }
    # Work items are named from their workload; list the expected names as
    # strings and build artifact paths only for work directories that exist.
    # Projects with many models and datasets otherwise build thousands of
    # paths, each of which touches the file system.
    models <- names(definitions$models)
    candidates <- list()
    add <- function(name, artifact) {
        candidates[[length(candidates) + 1L]] <<- list(name=name, artifact=artifact)
    }
    for (model_name in models) {
        add(paste0('fit_', model_name), list('model', model_name))
        model <- definitions$models[[model_name]]
        for (dataset_name in unique(c(
                names(model$datasets), names(definitions$datasets)))) {
            add(
                paste('prediction', model_name, dataset_name, sep='_'),
                list('prediction', model_name, dataset_name)
            )
        }
    }
    for (name in names(definitions$visualizations)) {
        add(
            paste0('visualization_', name),
            list('visualization', definitions$visualizations[[name]]$model, name)
        )
    }
    for (name in names(definitions$comparisons)) {
        add(paste0('comparison_', name), list('comparison', name))
    }
    # Effect items are named `<model>_<request hash>`; their outputs live
    # under the model, so they are selected exactly when the model is.
    for (model_name in models) {
        prefix <- paste0('effect_', model_name, '_')
        matches <- existing[startsWith(existing, prefix) & grepl(
            '^[0-9a-f]+$', substring(existing, nchar(prefix) + 1L)
        )]
        for (name in matches) add(name, list('model', model_name))
    }
    names_seen <- character()
    artifact_selected <- new.env(parent=emptyenv())
    output <- character()
    for (candidate in candidates) {
        if (!(candidate$name %in% existing) || candidate$name %in% names_seen) next
        names_seen <- c(names_seen, candidate$name)
        artifact <- candidate$artifact
        key <- paste(unlist(artifact), collapse='\034')
        if (!exists(key, envir=artifact_selected, inherits=FALSE)) {
            path <- switch(
                artifact[[1L]],
                model=.cdrgam_cli_path(definitions, 'model', artifact[[2L]]),
                prediction=.cdrgam_cli_path(
                    definitions, 'prediction', artifact[[2L]], dataset=artifact[[3L]]
                ),
                visualization=.cdrgam_cli_path(
                    definitions, 'visualization', artifact[[2L]], dataset=artifact[[3L]]
                ),
                comparison=.cdrgam_cli_path(definitions, 'comparison', artifact[[2L]])
            )
            assign(key, selected(path), envir=artifact_selected)
        }
        if (get(key, envir=artifact_selected, inherits=FALSE)) {
            output <- c(output, .cdrgam_cli_path(
                definitions, 'work', name=candidate$name
            ))
        }
    }
    output
}
