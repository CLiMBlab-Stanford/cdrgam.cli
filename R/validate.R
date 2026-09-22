#' Validate configured CDR-GAM projects
#'
#' @param projects Project selectors.
#' @param deep Read datasets and compile training model designs.
#' @param checkout Configured harness instance directory.
#' @return Validation reports grouped by project, invisibly.
#' @export
cdrgam_cli_validate <- function(projects=NULL, deep=FALSE, checkout=NULL) {
    selected <- .cdrgam_cli_select_projects(projects, checkout)
    reports <- lapply(stats::setNames(selected, selected), function(project) {
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=TRUE, checkout=checkout
        )
        external <- unlist(lapply(definitions$datasets, function(dataset) {
            names(dataset$sources)[vapply(
                dataset$sources, function(source) isTRUE(source$external), logical(1)
            )]
        }), use.names=FALSE)
        if (isTRUE(deep)) {
            for (name in names(definitions$datasets)) {
                dataset <- definitions$datasets[[name]]
                data <- .cdrgam_cli_load_dataset(dataset)
                .cdrgam_cli_check_dataset_columns(dataset, data)
                for (model in definitions$models) {
                    if (identical(model$datasets$train, name)) {
                        .cdrgam_cli_prepare_model(model, dataset, data)
                    }
                }
            }
        }
        list(
            valid=TRUE, root=definitions$root,
            datasets=length(definitions$datasets), models=length(definitions$models),
            visualizations=length(definitions$visualizations),
            comparisons=length(definitions$comparisons),
            external_sources=length(external), deep=isTRUE(deep)
        )
    })
    for (project in names(reports)) message(
        'Valid CDR-GAM project ', project, ': ', reports[[project]]$datasets,
        ' dataset(s), ', reports[[project]]$models, ' model(s), ',
        reports[[project]]$visualizations, ' visualization(s), ',
        reports[[project]]$comparisons, ' comparison(s)'
    )
    invisible(reports)
}

.cdrgam_cli_validate_definition <- function(
        project, type, name, deep=FALSE, checkout=NULL
) {
    root <- find_cdrgam_project(project, checkout)
    project_path <- file.path(root, .cdrgam_cli_marker)
    project_definition <- .cdrgam_cli_validate_project(
        .cdrgam_cli_read_yaml(project_path), project_path
    )
    project_definition$project$name <- basename(root)
    configuration <- .cdrgam_cli_checkout(checkout, create_root=FALSE)
    datasets <- new.env(parent=emptyenv())
    models <- new.env(parent=emptyenv())
    read_definition <- function(kind, definition_name) {
        path <- .cdrgam_cli_definition_target(root, kind, definition_name)
        if (!file.exists(path)) {
            .cdrgam_cli_abort(paste0(
                'The ', kind, ' definition does not exist: ', definition_name
            ))
        }
        value <- .cdrgam_cli_read_yaml(path)
        validated <- switch(
            kind,
            dataset=.cdrgam_cli_validate_dataset(
                value, path, root, check_exists=TRUE
            ),
            model=.cdrgam_cli_validate_model(value, path),
            visualization=.cdrgam_cli_validate_visualization(value, path),
            comparison=.cdrgam_cli_validate_comparison(value, path)
        )
        identity_field <- switch(
            kind, dataset='dataset', model='model',
            visualization='visualization', comparison='comparison'
        )
        if (!identical(validated[[identity_field]], definition_name)) {
            .cdrgam_cli_abort(paste0(
                path, ': ', identity_field, ' must match the file name ',
                sQuote(definition_name)
            ))
        }
        validated
    }
    read_dataset <- function(dataset_name) {
        if (!exists(dataset_name, envir=datasets, inherits=FALSE)) {
            assign(
                dataset_name,
                read_definition('dataset', dataset_name),
                envir=datasets
            )
        }
        get(dataset_name, envir=datasets, inherits=FALSE)
    }
    read_model <- function(model_name) {
        if (!exists(model_name, envir=models, inherits=FALSE)) {
            model <- read_definition('model', model_name)
            model_datasets <- lapply(
                unlist(model$datasets, use.names=FALSE), read_dataset
            )
            names(model_datasets) <- unlist(model$datasets, use.names=FALSE)
            if (isTRUE(deep)) {
                dataset <- model_datasets[[model$datasets$train]]
                data <- .cdrgam_cli_load_dataset(dataset)
                .cdrgam_cli_check_dataset_columns(dataset, data)
                .cdrgam_cli_prepare_model(model, dataset, data)
            }
            assign(model_name, model, envir=models)
        }
        get(model_name, envir=models, inherits=FALSE)
    }
    value <- switch(
        type,
        dataset={
            dataset <- read_dataset(name)
            if (isTRUE(deep)) {
                data <- .cdrgam_cli_load_dataset(dataset)
                .cdrgam_cli_check_dataset_columns(dataset, data)
            }
            dataset
        },
        model=read_model(name),
        visualization={
            visualization <- read_definition('visualization', name)
            model <- read_model(visualization$model)
            for (selector in .cdrgam_cli_null(
                    visualization$predictions, character()
            )) {
                dataset_name <- if (selector %in% names(model$datasets)) {
                    model$datasets[[selector]]
                } else selector
                read_dataset(dataset_name)
            }
            visualization
        },
        comparison={
            comparison <- read_definition('comparison', name)
            comparison_models <- lapply(comparison$models, read_model)
            if (!is.null(comparison$evaluation)) {
                selector <- comparison$evaluation$dataset
                for (model in comparison_models) {
                    dataset_name <- if (selector %in% names(model$datasets)) {
                        model$datasets[[selector]]
                    } else selector
                    read_dataset(dataset_name)
                }
            }
            comparison
        }
    )
    report <- list(
        valid=TRUE, project=project_definition$project$name,
        type=type, name=name, path=attr(value, 'path'),
        deep=isTRUE(deep), checkout=configuration$checkout
    )
    message(
        'Valid ', type, ' definition ', report$project, '/', name,
        if (isTRUE(deep)) ' (deep)' else ''
    )
    invisible(report)
}

.cdrgam_cli_validate_definitions <- function(
        project, type, patterns, deep=FALSE, checkout=NULL
) {
    root <- find_cdrgam_project(project, checkout)
    directory <- switch(
        type,
        dataset='datasets', model='models',
        visualization='visualizations', comparison='comparisons'
    )
    files <- .cdrgam_cli_definition_files(root, directory)
    available <- sub('\\.ya?ml$', '', basename(files))
    names <- .cdrgam_cli_match_names(patterns, available, type)
    reports <- vector('list', length(names))
    errors <- character()
    for (index in seq_along(names)) {
        name <- names[[index]]
        reports[index] <- list(tryCatch(
            .cdrgam_cli_validate_definition(
                project, type, name, deep=deep, checkout=checkout
            ),
            error=function(error) {
                errors[[name]] <<- conditionMessage(error)
                NULL
            }
        ))
    }
    names(reports) <- names
    if (length(errors)) {
        detail <- paste0(
            '- ', type, ' ', names(errors), ': ', unname(errors),
            collapse='\n'
        )
        .cdrgam_cli_abort(paste0(
            'Definition validation failed:\n', detail
        ))
    }
    invisible(reports)
}
