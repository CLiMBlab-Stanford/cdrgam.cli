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
            dataset=name,
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
            model=name,
            datasets=list(train=training),
            window=c(0, 2),
            formula='response ~ irf(predictor, k_l=10)',
            fit=list(family='gaussian', method='REML', backend='sparse')
        ))
    }
    if (identical(type, 'visualization')) {
        models <- names(definitions$models)
        model <- if (length(models) == 1L) models[[1L]] else 'choose-model'
        return(list(
            schema=1L, visualization=name, model=model,
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
            comparison=name,
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
        system(paste(editor, shQuote(path)))
    }
    if (!identical(as.integer(status), 0L)) {
        .cdrgam_cli_abort(paste0('Editor exited with status ', status))
    }
    invisible(path)
}

.cdrgam_cli_edit_yaml <- function(target, initial=NULL, validate, editor=NULL) {
    directory <- dirname(target)
    if (!dir.exists(directory) && !dir.create(directory, recursive=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not create ', sQuote(directory)))
    }
    temporary <- tempfile(
        paste0('.', basename(target), '-'), tmpdir=directory, fileext='.yml'
    )
    on.exit(unlink(temporary, force=TRUE), add=TRUE)
    if (file.exists(target)) {
        if (!file.copy(target, temporary, overwrite=TRUE)) {
            .cdrgam_cli_abort(paste0('Could not stage ', sQuote(target)))
        }
    } else {
        yaml::write_yaml(initial, temporary)
    }
    .cdrgam_cli_run_editor(temporary, editor)
    validate(temporary)
    .cdrgam_cli_atomic_write(target, function(path) {
        if (!file.copy(temporary, path, overwrite=TRUE)) {
            .cdrgam_cli_abort(paste0('Could not stage edited ', sQuote(target)))
        }
    })
    invisible(target)
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
    identity_field <- switch(
        type, dataset='dataset', model='model',
        visualization='visualization', comparison='comparison'
    )
    if (!identical(validated[[identity_field]], name)) {
        .cdrgam_cli_abort(paste0(
            path, ': ', identity_field, ' must remain ', sQuote(name)
        ))
    }
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

.cdrgam_cli_define_definition <- function(
        project, type, name, checkout=NULL, editor=NULL
) {
    name <- .cdrgam_cli_name(name, paste0(type, ' name'))
    definitions <- .cdrgam_cli_read_definitions(
        project, check_sources=FALSE, checkout=checkout
    )
    target <- .cdrgam_cli_definition_target(definitions$root, type, name)
    if (file.exists(target)) {
        .cdrgam_cli_edit_yaml(
            target,
            validate=function(path) .cdrgam_cli_validate_definition_candidate(
                type, name, path, definitions
            ),
            editor=editor
        )
        message('Updated ', type, ' definition at ', target)
        return(invisible(target))
    }
    value <- .cdrgam_cli_starter_definition(type, name, definitions)
    unresolved <- any(grepl(
        '^choose-', unlist(value, use.names=FALSE), perl=TRUE
    ))
    if (unresolved) {
        .cdrgam_cli_edit_yaml(
            target, value,
            validate=function(path) .cdrgam_cli_validate_definition_candidate(
                type, name, path, definitions
            ),
            editor=editor
        )
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
    .cdrgam_cli_edit_yaml(
        target, initial,
        validate=function(path) .cdrgam_cli_validate_checkout(
            .cdrgam_cli_read_yaml(path), path
        ),
        editor=editor
    )
    options(cdrgam.cli.checkout=checkout)
    .cdrgam_cli_checkout(checkout, create_root=TRUE)
    message('Updated site definition at ', target)
    invisible(target)
}

#' Create or edit CDR-GAM definitions
#'
#' @param project Project name, or `"site"` for checkout configuration.
#' @param type Optional definition type.
#' @param name Definition name when `type` is supplied.
#' @param copy_from_to Optional source and destination project names.
#' @param checkout Source checkout root.
#' @param editor Editor command or callback. The default uses `VISUAL`, then
#'   `EDITOR`, then the R `editor` option.
#' @return The created or updated path, invisibly.
#' @export
cdrgam_cli_def <- function(
        project=NULL, type=NULL, name=NULL, copy_from_to=NULL,
        checkout=NULL, editor=NULL
) {
    if (!is.null(copy_from_to)) {
        if (!is.null(project) || !is.null(type) || !is.null(name) ||
                !is.null(editor) || length(copy_from_to) != 2L) {
            .cdrgam_cli_abort(
                'copy_from_to must contain two project names and cannot be combined with other definition arguments'
            )
        }
        return(.cdrgam_cli_copy_project_definitions(
            copy_from_to[[1L]], copy_from_to[[2L]], checkout
        ))
    }
    project <- .cdrgam_cli_scalar_character(project, 'project')
    if (identical(project, 'site')) {
        if (!is.null(type) || !is.null(name)) {
            .cdrgam_cli_abort('The site definition cannot be combined with project selectors')
        }
        return(.cdrgam_cli_define_site(checkout, editor))
    }
    project <- .cdrgam_cli_name(project, 'project')
    if (xor(is.null(type), is.null(name))) {
        .cdrgam_cli_abort('Definition type and name must be supplied together')
    }
    if (is.null(type)) {
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
        .cdrgam_cli_edit_yaml(
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
            editor=editor
        )
        message('Updated project definition at ', target)
        return(invisible(target))
    }
    supported <- c('dataset', 'model', 'visualization', 'comparison')
    if (!(type %in% supported)) {
        .cdrgam_cli_abort(paste0(
            'Unsupported definition type: ', type, '. Expected: ',
            paste(supported, collapse=', ')
        ))
    }
    .cdrgam_cli_define_definition(project, type, name, checkout, editor)
}
