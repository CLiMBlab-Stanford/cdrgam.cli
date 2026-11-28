#' Preview or remove generated artifacts
#'
#' @param projects,models,predictions,visualizations,comparisons,datasets,tests
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
#'   Selectors match the names of current definitions and also the names left
#'   in results, work directories, logs, or the registry, so the outputs of a
#'   removed definition can still be purged.
#' @export
cdrgam_cli_purge <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, datasets=NULL, work=FALSE, logs=FALSE,
        force=FALSE, cdrgam_root=NULL, confirm=NULL, pager=NULL,
        use_pager=TRUE, tests=NULL
) {
    workload_selected <- any(c(
        length(models), length(predictions), length(visualizations),
        length(comparisons), length(datasets), length(tests)
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
        known <- .cdrgam_cli_purge_names(definitions, configuration)
        model_names <- if (length(models)) {
            .cdrgam_cli_match_names(models, known$models, 'model')
        } else known$models
        typed <- any(c(
            length(predictions), length(visualizations), length(comparisons),
            length(datasets), length(tests)
        ) > 0L)
        project_targets <- character()
        if (length(models) && !typed) {
            project_targets <- c(project_targets, vapply(
                model_names, function(name) {
                .cdrgam_cli_path(definitions, 'model', name)
            }, character(1)))
        }
        if (length(predictions)) {
            # Models match prediction selectors independently, so a selector
            # fails only when it matches no model's predictions.
            prediction_choices <- character()
            for (model_name in model_names) {
                model <- definitions$models[[model_name]]
                existing <- known$predictions(model_name)
                choices <- unique(c(
                    names(model$datasets), names(definitions$datasets), existing
                ))
                prediction_choices <- unique(c(prediction_choices, choices))
                prediction_names <- .cdrgam_cli_match_names(
                    predictions, choices, 'prediction', strict=FALSE
                )
                # A removed model or dataset leaves predictions named by
                # their dataset directory, which need no resolution.
                dataset_names <- unique(vapply(
                    prediction_names,
                    function(selector) if (is.null(model) || (selector %in% existing &&
                            !(selector %in% c(names(model$datasets),
                                names(definitions$datasets))))) {
                        selector
                    } else .cdrgam_cli_resolve_prediction_dataset(
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
            # Report a selector that matched nothing as a strict match would.
            .cdrgam_cli_match_names(
                setdiff(predictions, Filter(function(pattern) length(
                    .cdrgam_cli_match_name_pattern(pattern, prediction_choices, 'prediction')
                ), predictions)),
                prediction_choices, 'prediction'
            )
        }
        if (length(visualizations)) {
            visualization_names <- .cdrgam_cli_match_names(
                visualizations, known$visualizations, 'visualization'
            )
            if (length(models)) {
                # A visualization without a definition has no known model,
                # so a model selector leaves it out.
                visualization_names <- visualization_names[vapply(
                    visualization_names,
                    function(name) isTRUE(
                        definitions$visualizations[[name]]$model %in% model_names
                    ),
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
                    .cdrgam_cli_path(definitions, 'visualization', name)
                }, character(1)
            ))
        }
        if (length(comparisons)) {
            comparison_names <- .cdrgam_cli_match_names(
                comparisons, known$comparisons, 'comparison'
            )
            if (length(models)) {
                comparison_names <- comparison_names[vapply(
                    comparison_names,
                    function(name) {
                        value <- definitions$comparisons[[name]]
                        !is.null(value) && all(model_names %in% value$models)
                    },
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
        if (length(tests)) {
            test_names <- .cdrgam_cli_match_names(tests, known$tests, 'test')
            if (length(models)) {
                test_names <- test_names[vapply(
                    test_names,
                    function(name) isTRUE(definitions$tests[[name]]$model %in% model_names),
                    logical(1)
                )]
                if (!length(test_names)) {
                    .cdrgam_cli_abort('Combined model and test selectors matched nothing')
                }
            }
            project_targets <- c(project_targets, vapply(
                test_names, function(name) .cdrgam_cli_path(definitions, 'test', name),
                character(1)
            ))
        }
        if (length(datasets)) {
            dataset_names <- .cdrgam_cli_match_names(
                datasets, known$datasets, 'dataset'
            )
            project_targets <- c(project_targets, vapply(
                dataset_names, function(name) {
                    .cdrgam_cli_path(definitions, 'dataset', name)
                }, character(1)
            ))
        }
        if (!workload_selected) {
            roots <- file.path(definitions$root, 'results', c(
                'datasets', 'models', 'comparisons', 'analyses', 'tests'
            ))
            project_targets <- c(project_targets, unlist(lapply(
                roots[dir.exists(roots)], function(root) {
                    list.files(root, full.names=TRUE, all.files=TRUE, no..=TRUE)
                }
            ), use.names=FALSE))
        }
        project_targets <- c(project_targets, .cdrgam_cli_purge_work_targets(
            definitions, project_targets, known=known,
            archives=workload_selected && isTRUE(work),
            logs=workload_selected && isTRUE(logs)
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
        # With a workload selector, `--work` and `--logs` are limited to the
        # selected work items above. Only a project-wide purge removes the
        # whole work and log directories.
        if (!workload_selected) {
            for (kind in c(if (work) 'work', if (logs) 'log')) {
                directory <- .cdrgam_cli_path(definitions, kind)
                if (dir.exists(directory)) targets <- c(targets, directory)
            }
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

# Work item files are keyed by work-item kind and name. A live attempt
# directory is `work/<kind>_<name>`, an artifact moved aside on republication
# is `work/archive/<kind>_<name>_<identity>`, and the item log is
# `logs/<kind>/<name>.log`. Return the existing files that belong to work items
# whose generated artifacts are selected, so a purge cannot leave a failed
# attempt (and its optimizer checkpoints) to be resumed by the next run.
# Archived artifacts and logs are returned only when `archives` or `logs` is
# TRUE.
.cdrgam_cli_purge_work_targets <- function(
        definitions, targets, archives=FALSE, logs=FALSE,
        known=.cdrgam_cli_purge_names(definitions)
) {
    if (!length(targets)) return(character())
    work_root <- .cdrgam_cli_path(definitions, 'work')
    archive_root <- .cdrgam_cli_path(definitions, 'work', name='archive')
    log_root <- .cdrgam_cli_path(definitions, 'log')
    list_names <- function(directory, directories=TRUE) {
        if (!dir.exists(directory)) return(character())
        if (directories) {
            basename(list.dirs(directory, recursive=FALSE, full.names=TRUE))
        } else {
            list.files(directory, all.files=TRUE, no..=TRUE)
        }
    }
    existing <- list_names(work_root)
    archived <- if (archives) list_names(archive_root) else character()
    logged <- list()
    if (logs) {
        for (kind in list_names(log_root)) {
            files <- list_names(file.path(log_root, kind), directories=FALSE)
            files <- files[endsWith(files, '.log')]
            logged[[kind]] <- substring(files, 1L, nchar(files) - 4L)
        }
    }
    if (!length(existing) && !length(archived) && !length(logged)) {
        return(character())
    }
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
    hex <- function(value) grepl('^[0-9a-f]+$', value)
    # Work items are named from their workload; list the expected names as
    # strings and build artifact paths only for work items that left files.
    # Projects with many models and datasets otherwise build thousands of
    # paths, each of which touches the file system.
    models <- known$models
    candidates <- list()
    add <- function(kind, name, artifact) {
        candidates[[length(candidates) + 1L]] <<- list(
            kind=kind, name=name, artifact=artifact
        )
    }
    for (model_name in models) {
        add('fit', model_name, list('model', model_name))
        model <- definitions$models[[model_name]]
        for (dataset_name in unique(c(
                names(model$datasets), names(definitions$datasets),
                known$predictions(model_name)))) {
            add(
                'prediction', paste(model_name, dataset_name, sep='_'),
                list('prediction', model_name, dataset_name)
            )
        }
    }
    for (name in known$visualizations) {
        add(
            'visualization', name,
            list('visualization', name)
        )
    }
    for (name in known$comparisons) {
        add('comparison', name, list('comparison', name))
    }
    for (name in known$tests) {
        add('test', name, list('test', name))
    }
    # Effect items are named `<model>_<request hash>`; their outputs live
    # under the model, so they are selected exactly when the model is.
    effect_names <- unique(c(
        sub('^effect_', '', existing[startsWith(existing, 'effect_')]),
        sub('^effect_(.*)_[0-9a-f]+$', '\\1',
            archived[grepl('^effect_.*_[0-9a-f]+$', archived)]),
        logged[['effect']]
    ))
    for (model_name in models) {
        prefix <- paste0(model_name, '_')
        matches <- effect_names[startsWith(effect_names, prefix) &
            hex(substring(effect_names, nchar(prefix) + 1L))]
        for (name in matches) add('effect', name, list('model', model_name))
    }
    names_seen <- character()
    artifact_selected <- new.env(parent=emptyenv())
    output <- character()
    for (candidate in candidates) {
        item <- paste(candidate$kind, candidate$name, sep='_')
        if (item %in% names_seen) next
        names_seen <- c(names_seen, item)
        has_work <- item %in% existing
        archive_prefix <- paste0(item, '_')
        archive_matches <- archived[startsWith(archived, archive_prefix) &
            hex(substring(archived, nchar(archive_prefix) + 1L))]
        has_log <- candidate$name %in% logged[[candidate$kind]]
        if (!has_work && !length(archive_matches) && !has_log) next
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
                    definitions, 'visualization', artifact[[2L]]
                ),
                comparison=.cdrgam_cli_path(definitions, 'comparison', artifact[[2L]])
            )
            assign(key, selected(path), envir=artifact_selected)
        }
        if (!get(key, envir=artifact_selected, inherits=FALSE)) next
        if (has_work) {
            output <- c(output, .cdrgam_cli_path(definitions, 'work', name=item))
        }
        for (entry in archive_matches) {
            output <- c(output, .cdrgam_cli_path(
                definitions, 'work', name='archive', identity=entry
            ))
        }
        if (has_log) {
            output <- c(output, .cdrgam_cli_work_log_path(
                definitions, candidate$kind, candidate$name
            ))
        }
    }
    output
}

# The workload names purge can select in a project: those of its current
# definitions and those left by earlier ones in results, work directories,
# logs, and the registry, as lists of names by kind. `predictions` is a
# function of a model name giving its prediction dataset directories.
.cdrgam_cli_purge_names <- function(definitions, configuration=NULL) {
    children <- function(...) {
        directory <- file.path(...)
        if (!dir.exists(directory)) return(character())
        basename(list.dirs(directory, recursive=FALSE, full.names=TRUE))
    }
    results <- file.path(definitions$root, 'results')
    work <- children(.cdrgam_cli_path(definitions, 'work'))
    log_root <- .cdrgam_cli_path(definitions, 'log')
    logged <- function(kind) {
        directory <- file.path(log_root, kind)
        if (!dir.exists(directory)) return(character())
        files <- list.files(directory, pattern='[.]log$')
        sub('[.]log$', '', files)
    }
    prefixed <- function(kind) {
        prefix <- paste0(kind, '_')
        substring(work[startsWith(work, prefix)], nchar(prefix) + 1L)
    }
    registered <- if (is.null(configuration)) list() else {
        .cdrgam_cli_registry_project_names(
            configuration, definitions$project$project$name
        )
    }
    names_of <- function(defined, ...) {
        found <- unique(c(defined, ...))
        found[nzchar(found) & !(found %in% c('.', '..', 'archive'))]
    }
    list(
        models=names_of(
            names(definitions$models), children(results, 'models'),
            prefixed('fit'), logged('fit'), registered[['fit']]
        ),
        visualizations=names_of(
            names(definitions$visualizations), children(results, 'visualizations'),
            prefixed('visualization'), logged('visualization'),
            registered[['visualization']]
        ),
        comparisons=names_of(
            names(definitions$comparisons), children(results, 'comparisons'),
            prefixed('comparison'), logged('comparison'), registered[['comparison']]
        ),
        tests=names_of(
            names(definitions$tests), children(results, 'tests'),
            prefixed('test'), logged('test'), registered[['test']]
        ),
        datasets=names_of(names(definitions$datasets), children(results, 'datasets')),
        predictions=function(model_name) {
            children(results, 'models', model_name, 'predictions')
        }
    )
}
