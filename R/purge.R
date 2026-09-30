#' Preview or remove generated artifacts
#'
#' @param projects,models,predictions,visualizations,comparisons,datasets
#'   Conjunctive selectors for generated results. Dataset selectors address
#'   dataset artifacts directly.
#' @param work,logs Include private attempts or logs.
#' @param yes Remove selected paths. The default previews them.
#' @param checkout Configured harness instance directory.
#' @return Selected generated paths, invisibly.
#' @details Registry records and private attempt directories for selected work
#'   items are removed even when their generated artifacts are already absent.
#' @export
cdrgam_cli_purge <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, datasets=NULL, work=FALSE, logs=FALSE,
        yes=FALSE, checkout=NULL
) {
    workload_selected <- any(c(
        length(models), length(predictions), length(visualizations),
        length(comparisons), length(datasets)
    ) > 0L)
    if (!workload_selected && !isTRUE(work) && !isTRUE(logs) &&
            (is.null(projects) || !length(projects))) {
        inferred <- tryCatch(
            .cdrgam_cli_infer_project(checkout),
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
    selected <- .cdrgam_cli_select_projects(projects, checkout)
    configuration <- .cdrgam_cli_checkout(checkout, create_root=TRUE)
    targets <- character()
    registry_keys <- character()
    for (project in selected) {
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=FALSE, checkout=checkout
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
    if (length(targets)) cat(
        'Selected generated artifacts:\n',
        paste0('  ', targets, collapse='\n'), '\n'
    )
    if (length(registry_keys)) cat(
        'Selected registry workloads:\n',
        paste0('  ', registry_keys, collapse='\n'), '\n'
    )
    if (!isTRUE(yes)) {
        message('Preview only; pass --yes to remove the selection')
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
