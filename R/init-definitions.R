.cdrgam_cli_definition_target <- function(root, type, name) {
    directory <- switch(
        type,
        dataset='datasets',
        model='models',
        visualization='visualizations',
        comparison='comparisons',
        analysis='analyses'
    )
    file.path(root, 'definitions', directory, paste0(name, '.yml'))
}

.cdrgam_cli_starter_definition <- function(type, name, definitions) {
    if (identical(type, 'dataset')) {
        return(list(
            schema=1L,
            sources=list(
                impulses=list(
                    path=file.path('data', paste0(name, '-impulses.rds')), format='rds'
                ),
                responses=list(
                    path=file.path('data', paste0(name, '-responses.rds')), format='rds'
                )
            ),
            columns=list(
                series=list('series'), impulse_time='time', response_time='time'
            )
        ))
    }
    if (identical(type, 'model')) {
        datasets <- names(definitions$datasets)
        training <- if (length(datasets) == 1L) datasets[[1L]] else
            'choose-dataset'
        return(list(
            schema=1L,
            datasets=list(train=training),
            window=c(0, 2),
            k_l=10L,
            formula='response ~ irf(predictor)',
            fit=list(family='gaussian', method='REML', backend='sparse')
        ))
    }
    if (identical(type, 'visualization')) {
        models <- names(definitions$models)
        model <- if (length(models) == 1L) models[[1L]] else 'choose-model'
        return(list(
            schema=1L, model=model,
            query=list(
                terms=list(predictors='*', grouped=FALSE),
                composition='total', grouping='population',
                axes=list(
                    lag=list(grid='fitted', n=300L),
                    predictors=list(
                        '*'=list(at=list(summary='mean', 'offset-sd'=1))
                    )
                ),
                uncertainty=list(level=0.95, kind='pointwise')
            ),
            render=list(
                geometry='line',
                mappings=list(x='lag', y='estimate', color='term'),
                interval='ribbon', theme='paper', formats=c('pdf', 'png')
            )
        ))
    }
    if (identical(type, 'comparison')) {
        models <- names(definitions$models)
        selected <- if (length(models) == 2L) models else
            c('choose-model-one', 'choose-model-two')
        return(list(
            schema=1L,
            models=selected,
            methods='mse'
        ))
    }
    .cdrgam_cli_abort(paste0('No starter definition is available for ', type))
}

.cdrgam_cli_editor <- function(editor=NULL) {
    if (!is.null(editor)) return(editor)
    value <- Sys.getenv('VISUAL', '')
    if (!nzchar(value)) value <- Sys.getenv('EDITOR', '')
    if (!nzchar(value)) value <- getOption('editor', 'vi')
    value
}

.cdrgam_cli_run_editor <- function(path, editor=NULL) {
    editor <- .cdrgam_cli_editor(editor)
    status <- if (is.function(editor)) {
        editor(path)
        0L
    } else {
        editor <- .cdrgam_cli_scalar_character(editor, 'editor')
        if (all(utils::file.edit(path, editor=editor))) 0L else 1L
    }
    if (!identical(as.integer(status), 0L)) {
        .cdrgam_cli_abort(paste0('Editor exited with status ', status))
    }
    invisible(path)
}

.cdrgam_cli_edit_yaml <- function(
        target, initial=NULL, validate, editor=NULL, draft=NULL,
        initial_path=NULL
) {
    directory <- dirname(target)
    if (!dir.exists(directory) && !dir.create(directory, recursive=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not create ', sQuote(directory)))
    }
    temporary <- tempfile(
        paste0('.', basename(target), '-'), tmpdir=directory, fileext='.yml'
    )
    on.exit(unlink(temporary, force=TRUE), add=TRUE)
    recovering_draft <- !is.null(draft) && file.exists(draft)
    source <- if (recovering_draft) {
        draft
    } else if (file.exists(target)) {
        target
    } else {
        initial_path
    }
    if (!is.null(source)) {
        if (!file.exists(source)) {
            .cdrgam_cli_abort(paste0('Initial definition does not exist: ', source))
        }
        if (!file.copy(source, temporary, overwrite=TRUE)) {
            .cdrgam_cli_abort(paste0('Could not stage ', sQuote(source)))
        }
        if (recovering_draft) message('Recovered draft from ', draft)
    } else {
        yaml::write_yaml(initial, temporary)
    }
    baseline_time <- Sys.time() - 2
    Sys.setFileTime(temporary, baseline_time)
    baseline_hash <- .cdrgam_cli_source_hash(temporary)
    baseline_mtime <- file.info(temporary)$mtime
    editor_error <- tryCatch({
        .cdrgam_cli_run_editor(temporary, editor)
        NULL
    }, error=identity)
    saved <- !identical(.cdrgam_cli_source_hash(temporary), baseline_hash) ||
        !identical(file.info(temporary)$mtime, baseline_mtime)
    if (!saved) {
        if (inherits(editor_error, 'error')) stop(editor_error)
        return(invisible(FALSE))
    }
    tryCatch({
        if (inherits(editor_error, 'error')) stop(editor_error)
        validate(temporary)
    }, error=function(error) {
        if (is.null(draft)) stop(error)
        draft_directory <- dirname(draft)
        if (!dir.exists(draft_directory) &&
                !dir.create(draft_directory, recursive=TRUE)) {
            .cdrgam_cli_abort(paste0(
                conditionMessage(error), '\nCould not create draft directory ',
                sQuote(draft_directory)
            ))
        }
        .cdrgam_cli_atomic_write(draft, function(path) {
            if (!file.copy(temporary, path, overwrite=TRUE)) {
                .cdrgam_cli_abort(paste0('Could not save draft ', sQuote(draft)))
            }
        })
        .cdrgam_cli_abort(paste0(
            conditionMessage(error), '\nDraft saved to ', draft,
            '. Re-run the same def edit command to continue editing it.'
        ))
    })
    .cdrgam_cli_atomic_write(target, function(path) {
        if (!file.copy(temporary, path, overwrite=TRUE)) {
            .cdrgam_cli_abort(paste0('Could not stage edited ', sQuote(target)))
        }
    })
    if (!is.null(draft) && file.exists(draft)) unlink(draft, force=TRUE)
    invisible(TRUE)
}

.cdrgam_cli_validate_definition_candidate <- function(
        type, name, path, definitions
) {
    value <- .cdrgam_cli_read_yaml(path)
    validated <- switch(
        type,
        dataset=.cdrgam_cli_validate_dataset(
            value, path, definitions$root, check_exists=FALSE
        ),
        model=.cdrgam_cli_validate_model(value, path),
        visualization=.cdrgam_cli_validate_visualization(value, path),
        comparison=.cdrgam_cli_validate_comparison(value, path)
    )
    validated <- .cdrgam_cli_assign_definition_identity(
        validated, type, name, path
    )
    if (identical(type, 'model')) {
        missing <- setdiff(unlist(validated$datasets, use.names=FALSE),
            names(definitions$datasets))
        if (length(missing)) .cdrgam_cli_abort(paste0(
            path, ': model references missing datasets: ',
            paste(missing, collapse=', ')
        ))
    } else if (identical(type, 'visualization') &&
            !(validated$model %in% names(definitions$models))) {
        .cdrgam_cli_abort(paste0(
            path, ': visualization references missing model ', validated$model
        ))
    } else if (identical(type, 'comparison')) {
        missing <- setdiff(validated$models, names(definitions$models))
        if (length(missing)) .cdrgam_cli_abort(paste0(
            path, ': comparison references missing models: ',
            paste(missing, collapse=', ')
        ))
    }
    invisible(validated)
}

.cdrgam_cli_definition_edit_context <- function(root) {
    catalog <- function(directory) {
        files <- .cdrgam_cli_definition_files(root, directory)
        names <- sub('\\.ya?ml$', '', basename(files))
        stats::setNames(vector('list', length(names)), names)
    }
    list(
        root=root,
        datasets=catalog('datasets'),
        models=catalog('models')
    )
}

.cdrgam_cli_define_definition <- function(
        project, type, name, source=NULL, checkout=NULL, editor=NULL
) {
    name <- .cdrgam_cli_name(name, paste0(type, ' name'))
    root <- find_cdrgam_project(project, checkout)
    target <- .cdrgam_cli_definition_target(root, type, name)
    if (is.null(source) && file.exists(target)) {
        definitions <- .cdrgam_cli_definition_edit_context(root)
        saved <- .cdrgam_cli_edit_yaml(
            target,
            validate=function(path) .cdrgam_cli_validate_definition_candidate(
                type, name, path, definitions
            ),
            editor=editor,
            draft=.cdrgam_cli_draft_path(root, type, name)
        )
        message(if (saved) 'Updated ' else 'No changes to ', type,
            ' definition at ', target)
        return(invisible(target))
    }
    if (!is.null(source)) {
        source <- .cdrgam_cli_name(source, paste0('source ', type, ' name'))
        if (file.exists(target)) {
            .cdrgam_cli_abort(paste0(
                'Target ', type, ' definition already exists: ', target
            ))
        }
        source_path <- .cdrgam_cli_definition_target(
            root, type, source
        )
        if (!file.exists(source_path)) {
            .cdrgam_cli_abort(paste0(
                'Source ', type, ' definition does not exist: ', source
            ))
        }
        definitions <- .cdrgam_cli_definition_edit_context(root)
        saved <- .cdrgam_cli_edit_yaml(
            target,
            validate=function(path) .cdrgam_cli_validate_definition_candidate(
                type, name, path, definitions
            ),
            editor=editor,
            draft=.cdrgam_cli_draft_path(root, type, name),
            initial_path=source_path
        )
        if (saved) {
            message(
                'Initialized ', type, ' definition ', name, ' from ', source,
                ' at ', target
            )
        } else {
            message('No definition created for ', type, ' ', name)
        }
        return(invisible(target))
    }
    definitions <- .cdrgam_cli_read_definitions(
        project, check_sources=FALSE, checkout=checkout
    )
    target <- .cdrgam_cli_definition_target(definitions$root, type, name)
    value <- .cdrgam_cli_starter_definition(type, name, definitions)
    unresolved <- any(grepl(
        '^choose-', unlist(value, use.names=FALSE), perl=TRUE
    ))
    if (unresolved) {
        saved <- .cdrgam_cli_edit_yaml(
            target, value,
            validate=function(path) .cdrgam_cli_validate_definition_candidate(
                type, name, path, definitions
            ),
            editor=editor,
            draft=.cdrgam_cli_draft_path(
                definitions$root, type, name
            )
        )
        if (!saved) {
            message('No definition created for ', type, ' ', name)
            return(invisible(target))
        }
    } else {
        .cdrgam_cli_write_yaml(value, target)
    }
    complete <- FALSE
    on.exit({
        if (!complete) unlink(target, force=TRUE)
    }, add=TRUE)
    .cdrgam_cli_read_definitions(project, check_sources=FALSE, checkout=checkout)
    complete <- TRUE
    message('Initialized ', type, ' definition at ', target)
    invisible(target)
}

.cdrgam_cli_reference_definitions <- function(root, directory, target) {
    files <- .cdrgam_cli_definition_files(root, directory)
    values <- lapply(files, function(path) {
        value <- tryCatch(
            .cdrgam_cli_read_yaml(path),
            error=function(error) .cdrgam_cli_abort(paste0(
                'Cannot determine whether ', sQuote(target),
                ' can be removed because ', sQuote(path),
                ' could not be read: ', conditionMessage(error)
            ))
        )
        if (!is.list(value)) {
            .cdrgam_cli_abort(paste0(
                'Cannot determine whether ', sQuote(target),
                ' can be removed because ', sQuote(path),
                ' is not a definition mapping'
            ))
        }
        value
    })
    names(values) <- sub('\\.ya?ml$', '', basename(files))
    values
}

.cdrgam_cli_definition_references <- function(root, type, name) {
    if (identical(type, 'dataset')) {
        definitions <- .cdrgam_cli_reference_definitions(
            root, 'models', paste('dataset', name)
        )
        models <- names(Filter(function(model) {
            name %in% as.character(unlist(model$datasets, use.names=FALSE))
        }, definitions))
        return(if (length(models)) paste0('model ', models) else character())
    }
    if (identical(type, 'model')) {
        visualizations <- .cdrgam_cli_reference_definitions(
            root, 'visualizations', paste('model', name)
        )
        comparisons <- .cdrgam_cli_reference_definitions(
            root, 'comparisons', paste('model', name)
        )
        visualizations <- names(Filter(function(visualization) {
            name %in% as.character(unlist(
                visualization$model, use.names=FALSE
            ))
        }, visualizations))
        comparisons <- names(Filter(function(comparison) {
            name %in% as.character(unlist(comparison$models, use.names=FALSE))
        }, comparisons))
        return(c(
            if (length(visualizations)) {
                paste0('visualization ', visualizations)
            } else character(),
            if (length(comparisons)) {
                paste0('comparison ', comparisons)
            } else character()
        ))
    }
    character()
}

.cdrgam_cli_definition_artifacts <- function(definitions, type, name) {
    switch(
        type,
        dataset=.cdrgam_cli_path(definitions, 'dataset', name),
        model=.cdrgam_cli_path(definitions, 'model', name),
        visualization={
            models_root <- file.path(definitions$root, 'models')
            model_names <- if (dir.exists(models_root)) {
                basename(list.dirs(
                    models_root, recursive=FALSE, full.names=TRUE
                ))
            } else {
                character()
            }
            vapply(model_names, function(model) {
                .cdrgam_cli_path(
                    definitions, 'visualization', model, dataset=name
                )
            }, character(1))
        },
        comparison=.cdrgam_cli_path(definitions, 'comparison', name)
    )
}

.cdrgam_cli_definition_purge_command <- function(project, type, name) {
    selector <- switch(
        type,
        dataset=paste('--dataset', name),
        model=paste('-m', name),
        visualization=paste('-v', name),
        comparison=paste('-c', name)
    )
    paste('cdrgam purge -P', project, selector, '--yes')
}

.cdrgam_cli_project_has_active_work <- function(definitions) {
    configuration <- definitions$checkout
    if (!file.exists(.cdrgam_cli_registry_path(configuration))) return(FALSE)
    project <- definitions$project$project$name
    project_id <- definitions$project$project$id
    result <- .cdrgam_cli_registry_exec(configuration, paste0(
        'SELECT COUNT(*) AS n FROM attempts a JOIN work_items w ',
        'ON w.work_key=a.work_key WHERE ',
        "a.state IN ('submitting','submitted','running') AND (w.project=",
        .cdrgam_cli_sql_quote(project), ' OR w.work_key LIKE ',
        .cdrgam_cli_sql_quote(paste0('%:', project_id, ':%')), ')'
    ), query=TRUE)
    result$n[[1L]] > 0L
}

.cdrgam_cli_list_definitions <- function(
        project, selectors=list(), checkout=NULL
) {
    supported <- c('dataset', 'model', 'visualization', 'comparison')
    unexpected <- setdiff(names(selectors), supported)
    if (length(unexpected)) {
        .cdrgam_cli_abort(paste0(
            'Unsupported definition selectors: ',
            paste(unexpected, collapse=', ')
        ))
    }
    if (is.null(project)) {
        if (length(selectors)) {
            .cdrgam_cli_abort(
                'Definition selectors require one project name'
            )
        }
        configuration <- .cdrgam_cli_checkout(checkout, create_root=TRUE)
        projects_root <- file.path(configuration$cdrgam_root, 'projects')
        available <- if (dir.exists(projects_root)) {
            basename(list.dirs(
                projects_root, recursive=FALSE, full.names=TRUE
            ))
        } else {
            character()
        }
        available <- sort(available[vapply(available, function(name) {
            tryCatch({
                .cdrgam_cli_name(name, 'project')
                file.exists(file.path(projects_root, name, .cdrgam_cli_marker))
            }, error=function(error) FALSE)
        }, logical(1))])
        paths <- vapply(available, function(name) {
            marker <- file.path(
                .cdrgam_cli_project_root(name, checkout, must_work=FALSE),
                .cdrgam_cli_marker
            )
            as.character(fs::path_rel(
                marker, start=configuration$cdrgam_root
            ))
        }, character(1))
        output <- data.frame(
            type=rep('project', length(available)),
            name=available, path=paths, stringsAsFactors=FALSE
        )
        print(output, row.names=FALSE)
        return(invisible(output))
    }
    if (identical(project, 'site')) {
        if (length(selectors)) {
            .cdrgam_cli_abort('def ls site does not accept definition selectors')
        }
        checkout_root <- .cdrgam_cli_checkout_root(checkout)
        output <- data.frame(
            type='site', name='site',
            path=as.character(fs::path_rel(
                file.path(checkout_root, .cdrgam_cli_checkout_marker),
                start=checkout_root
            )),
            stringsAsFactors=FALSE
        )
        print(output, row.names=FALSE)
        return(invisible(output))
    }
    project <- .cdrgam_cli_name(project, 'project')
    root <- find_cdrgam_project(project, checkout)
    selected_types <- if (length(selectors)) names(selectors) else supported
    rows <- list()
    if (!length(selectors)) {
        rows[[length(rows) + 1L]] <- data.frame(
            type='project', name=project, path=.cdrgam_cli_marker,
            stringsAsFactors=FALSE
        )
    }
    directories <- c(
        dataset='datasets', model='models',
        visualization='visualizations', comparison='comparisons'
    )
    for (type in selected_types) {
        files <- .cdrgam_cli_definition_files(root, directories[[type]])
        available <- sub('\\.ya?ml$', '', basename(files))
        names <- if (length(selectors)) {
            .cdrgam_cli_match_names(selectors[[type]], available, type)
        } else {
            available
        }
        if (!length(names)) next
        targets <- vapply(names, function(name) {
            .cdrgam_cli_definition_target(root, type, name)
        }, character(1))
        rows[[length(rows) + 1L]] <- data.frame(
            type=type, name=names,
            path=vapply(targets, function(path) {
                as.character(fs::path_rel(path, start=root))
            }, character(1)),
            stringsAsFactors=FALSE
        )
    }
    output <- do.call(rbind, rows)
    rownames(output) <- NULL
    print(output, row.names=FALSE)
    invisible(output)
}

.cdrgam_cli_remove_definitions <- function(
        project, type, names, checkout=NULL
) {
    root <- find_cdrgam_project(project, checkout)
    directory <- switch(
        type,
        dataset='datasets', model='models',
        visualization='visualizations', comparison='comparisons'
    )
    files <- .cdrgam_cli_definition_files(root, directory)
    available <- sub('\\.ya?ml$', '', basename(files))
    names <- .cdrgam_cli_match_names(names, available, type)
    targets <- vapply(names, function(name) {
        .cdrgam_cli_definition_target(root, type, name)
    }, character(1))
    project_path <- file.path(root, .cdrgam_cli_marker)
    project_definition <- .cdrgam_cli_validate_project(
        .cdrgam_cli_read_yaml(project_path), project_path
    )
    project_definition$project$name <- basename(root)
    definitions <- list(
        root=root, project=project_definition,
        checkout=.cdrgam_cli_checkout(checkout)
    )
    for (name in names) {
        references <- .cdrgam_cli_definition_references(
            root, type, name
        )
        if (length(references)) {
            .cdrgam_cli_abort(paste0(
                'Cannot remove ', type, ' definition ', sQuote(name),
                '; it is referenced by: ', paste(references, collapse=', '),
                '. Remove or edit those definitions first.'
            ))
        }
        artifacts <- .cdrgam_cli_definition_artifacts(definitions, type, name)
        existing_artifacts <- artifacts[file.exists(artifacts)]
        if (length(existing_artifacts)) {
            command <- .cdrgam_cli_definition_purge_command(
                project, type, name
            )
            .cdrgam_cli_abort(paste0(
                'Cannot remove ', type, ' definition ', sQuote(name),
                ' while results exist at ',
                paste(existing_artifacts, collapse=', '), '. First run `',
                command, '`, then retry this command.'
            ))
        }
    }
    if (.cdrgam_cli_project_has_active_work(definitions)) {
        .cdrgam_cli_abort(
            'Cannot remove definitions while this project has active work'
        )
    }
    staged <- tempfile('.removed-definitions-', tmpdir=dirname(targets[[1L]]))
    if (!dir.create(staged)) {
        .cdrgam_cli_abort('Could not create a definition removal stage')
    }
    staged_targets <- file.path(staged, basename(targets))
    moved <- logical(length(targets))
    complete <- FALSE
    on.exit({
        if (!complete) {
            for (index in which(moved & file.exists(staged_targets))) {
                .cdrgam_cli_try_move_path(
                    staged_targets[[index]], targets[[index]]
                )
            }
        }
        unlink(staged, recursive=TRUE, force=TRUE)
    }, add=TRUE)
    for (index in seq_along(targets)) {
        if (!.cdrgam_cli_try_move_path(
                targets[[index]], staged_targets[[index]]
        )) {
            .cdrgam_cli_abort(paste0(
                'Could not stage definition removal: ', targets[[index]]
            ))
        }
        moved[[index]] <- TRUE
    }
    if (!unlink(staged, recursive=TRUE, force=FALSE) && dir.exists(staged)) {
        .cdrgam_cli_abort(paste0(
            'Could not remove staged definitions: ', staged
        ))
    }
    complete <- TRUE
    message(
        'Removed ', length(names), ' ', type,
        if (length(names) == 1L) ' definition: ' else ' definitions: ',
        paste(names, collapse=', ')
    )
    invisible(stats::setNames(targets, names))
}

.cdrgam_cli_define_site <- function(checkout=NULL, editor=NULL) {
    checkout <- tryCatch(
        .cdrgam_cli_checkout_root(checkout, must_work=FALSE),
        error=function(error) {
            if (!is.null(checkout) ||
                    !is.null(getOption('cdrgam.cli.checkout')) ||
                    nzchar(Sys.getenv('CDRGAM_CHECKOUT', ''))) {
                stop(error)
            }
            .cdrgam_cli_normalize_path(getwd(), must_work=TRUE)
        }
    )
    target <- file.path(checkout, .cdrgam_cli_checkout_marker)
    configured_root <- Sys.getenv('CDRGAM_ROOT', '')
    if (!nzchar(configured_root)) {
        configured_root <- file.path(checkout, 'cdrgam-root')
    }
    initial <- list(
        schema=1L,
        cdrgam_root=.cdrgam_cli_normalize_path(
            configured_root,
            must_work=FALSE
        ),
        concurrency=1L
    )
    saved <- .cdrgam_cli_edit_yaml(
        target, initial,
        validate=function(path) .cdrgam_cli_validate_checkout(
            .cdrgam_cli_read_yaml(path), path
        ),
        editor=editor,
        draft=.cdrgam_cli_draft_path(checkout, 'site', 'checkout')
    )
    if (!saved && !file.exists(target)) {
        message('No site definition created')
        return(invisible(target))
    }
    options(cdrgam.cli.checkout=checkout)
    .cdrgam_cli_checkout(checkout, create_root=TRUE)
    message(if (saved) 'Updated' else 'No changes to', ' site definition at ', target)
    invisible(target)
}

#' Create, edit, copy, or remove CDR-GAM definitions
#'
#' @param project Project name, or `"site"` for checkout configuration.
#' @param type Optional definition type.
#' @param name One or more definition names when `type` is supplied. Validation
#'   and removal accept `*` patterns; editing treats names literally.
#' @param source Optional source project or subordinate definition name. The
#'   source initializes the requested target without generated artifacts;
#'   subordinate definitions are opened in the editor before publication.
#' @param operation Either `"edit"` to create or edit a definition, or
#'   `"rm"` to remove a subordinate definition after safety checks, or
#'   `"val"` to validate definitions without changing them.
#' @param deep Whether `operation="val"` should read data and prepare model
#'   designs.
#' @param checkout Configured harness instance directory.
#' @param editor Editor command or callback. The default uses `VISUAL`, then
#'   `EDITOR`, then the R `editor` option.
#' @return The created, updated, or removed path, invisibly.
#' @export
cdrgam_cli_def <- function(
        project=NULL, type=NULL, name=NULL, source=NULL,
        checkout=NULL, editor=NULL, operation=c('edit', 'rm', 'val'),
        deep=FALSE
) {
    operation <- match.arg(operation)
    deep <- .cdrgam_cli_scalar_logical(deep, 'deep')
    if (!identical(operation, 'val') && isTRUE(deep)) {
        .cdrgam_cli_abort('deep applies only to def val')
    }
    project <- .cdrgam_cli_scalar_character(project, 'project')
    if (identical(operation, 'rm')) {
        if (identical(project, 'site')) {
            .cdrgam_cli_abort('def rm does not remove checkout configuration')
        }
        if (is.null(type) || is.null(name)) {
            .cdrgam_cli_abort(
                'def rm requires exactly one subordinate definition selector'
            )
        }
        if (!is.null(source) || !is.null(editor)) {
            .cdrgam_cli_abort('def rm does not accept source or editor')
        }
    }
    if (identical(project, 'site')) {
        if (identical(operation, 'val')) {
            if (!is.null(type) || !is.null(name) || !is.null(source) ||
                    !is.null(editor)) {
                .cdrgam_cli_abort(
                    'def val site does not accept definition selectors, source, or editor'
                )
            }
            configuration <- .cdrgam_cli_checkout(
                checkout, create_root=FALSE
            )
            message('Valid checkout definition at ', file.path(
                configuration$checkout, .cdrgam_cli_checkout_marker
            ))
            return(invisible(configuration))
        }
        if (!is.null(type) || !is.null(name) || !is.null(source)) {
            .cdrgam_cli_abort(
                'The site definition cannot be combined with project selectors or source'
            )
        }
        return(.cdrgam_cli_define_site(checkout, editor))
    }
    project <- .cdrgam_cli_name(project, 'project')
    if (!is.null(source)) {
        source <- .cdrgam_cli_name(source, 'source')
    }
    if (xor(is.null(type), is.null(name))) {
        .cdrgam_cli_abort('Definition type and name must be supplied together')
    }
    if (!is.null(name) && (!is.character(name) || !length(name) ||
            anyNA(name) || any(!nzchar(name)))) {
        .cdrgam_cli_abort('Definition names must be a nonempty string list')
    }
    supported <- c('dataset', 'model', 'visualization', 'comparison')
    if (!is.null(type) && !(type %in% supported)) {
        .cdrgam_cli_abort(paste0(
            'Unsupported definition type: ', type, '. Expected: ',
            paste(supported, collapse=', ')
        ))
    }
    if (identical(operation, 'val')) {
        if (!is.null(source) || !is.null(editor)) {
            .cdrgam_cli_abort('def val does not accept source or editor')
        }
        if (is.null(type)) {
            return(cdrgam_cli_validate(project, deep=deep, checkout=checkout))
        }
        return(.cdrgam_cli_validate_definitions(
            project, type, name, deep=deep, checkout=checkout
        ))
    }
    if (is.null(type)) {
        if (!is.null(source)) {
            if (!is.null(editor)) {
                .cdrgam_cli_abort('source cannot be combined with editor')
            }
            return(.cdrgam_cli_copy_project_definitions(
                source, project, checkout
            ))
        }
        .cdrgam_cli_checkout(checkout, create_root=TRUE)
        root <- .cdrgam_cli_project_root(
            project, checkout=checkout, must_work=FALSE
        )
        target <- file.path(root, .cdrgam_cli_marker)
        if (!file.exists(target)) {
            return(.cdrgam_cli_create_project(project, checkout))
        }
        original <- .cdrgam_cli_validate_project(
            .cdrgam_cli_read_yaml(target), target
        )
        saved <- .cdrgam_cli_edit_yaml(
            target,
            validate=function(path) {
                candidate <- .cdrgam_cli_validate_project(
                    .cdrgam_cli_read_yaml(path), path
                )
                if (!identical(candidate$project, original$project)) {
                    .cdrgam_cli_abort(
                        'Project name and ID are immutable after initialization'
                    )
                }
            },
            editor=editor,
            draft=.cdrgam_cli_draft_path(root, 'project', 'project')
        )
        message(if (saved) 'Updated' else 'No changes to',
            ' project definition at ', target)
        return(invisible(target))
    }
    if (identical(operation, 'rm')) {
        return(.cdrgam_cli_remove_definitions(
            project, type, name, checkout
        ))
    }
    names <- vapply(
        name, .cdrgam_cli_name, character(1), field=paste0(type, ' name')
    )
    paths <- vapply(names, function(name) {
        .cdrgam_cli_define_definition(
            project, type, name, source, checkout, editor
        )
    }, character(1))
    invisible(stats::setNames(paths, names))
}
