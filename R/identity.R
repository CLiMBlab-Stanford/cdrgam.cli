.cdrgam_cli_package_version <- function(package) {
    as.character(utils::packageVersion(package))
}

.cdrgam_cli_implementation <- function(package) {
    description <- utils::packageDescription(package)
    database <- system.file('R', paste0(package, '.rdb'), package=package)
    list(
        version=as.character(description$Version),
        revision=.cdrgam_cli_null(
            description$RemoteSha,
            .cdrgam_cli_null(description$GithubSHA1, NULL)
        ),
        code_md5=if (nzchar(database) && file.exists(database)) {
            .cdrgam_cli_source_hash(database)
        } else NULL
    )
}

.cdrgam_cli_artifact_contract <- function(kind, definition=NULL, inputs=list()) {
    kinds <- c('dataset', 'fit', 'prediction', 'effect', 'visualization', 'comparison')
    if (!(kind %in% kinds)) {
        .cdrgam_cli_abort(paste0('No artifact contract is defined for ', kind))
    }
    list(
        kind=kind,
        definition=definition,
        inputs=inputs
    )
}

.cdrgam_cli_scientific_definition <- function(value) {
    attr(value, 'path') <- NULL
    value$schema <- NULL
    value
}

.cdrgam_cli_model_identity <- function(model, dataset_identity) {
    scientific <- .cdrgam_cli_scientific_definition(model)
    resolved <- .cdrgam_cli_artifact_contract(
        'fit', scientific,
        list(training_data=dataset_identity)
    )
    list(identity=.cdrgam_cli_short_hash(resolved), resolved=resolved)
}

.cdrgam_cli_prediction_identity <- function(model_identity, dataset_identity) {
    resolved <- .cdrgam_cli_artifact_contract(
        'prediction',
        inputs=list(model=model_identity, dataset=dataset_identity)
    )
    list(identity=.cdrgam_cli_short_hash(resolved), resolved=resolved)
}

.cdrgam_cli_visualization_identity <- function(value, inputs) {
    scientific <- .cdrgam_cli_scientific_definition(value)
    resolved <- .cdrgam_cli_artifact_contract('visualization', scientific, inputs)
    list(identity=.cdrgam_cli_short_hash(resolved), resolved=resolved)
}

.cdrgam_cli_effect_identity <- function(query, layers, model_identity) {
    resolved <- .cdrgam_cli_artifact_contract(
        'effect',
        definition=list(query=query, layers=layers),
        inputs=list(model=model_identity)
    )
    list(identity=.cdrgam_cli_short_hash(resolved), resolved=resolved)
}

.cdrgam_cli_comparison_identity <- function(value, inputs) {
    scientific <- .cdrgam_cli_scientific_definition(value)
    resolved <- .cdrgam_cli_artifact_contract('comparison', scientific, inputs)
    list(identity=.cdrgam_cli_short_hash(resolved), resolved=resolved)
}

.cdrgam_cli_match_names <- function(patterns, choices, field) {
    if (is.null(patterns) || !length(patterns)) return(choices)
    selected <- character()
    for (pattern in patterns) {
        pattern <- .cdrgam_cli_scalar_character(pattern, field)
        if (identical(pattern, '*')) {
            matches <- choices
        } else if (grepl('*', pattern, fixed=TRUE)) {
            expression <- paste0(
                '^', gsub('\\*', '.*', gsub('([][{}()+?.^$|\\\\])', '\\\\\\1', pattern)), '$'
            )
            matches <- choices[grepl(expression, choices)]
        } else {
            matches <- choices[choices == pattern]
        }
        if (!length(matches)) {
            .cdrgam_cli_abort(paste0(
                field, ' selector matched nothing: ', pattern,
                if (length(choices)) paste0('; available: ', paste(choices, collapse=', ')) else ''
            ))
        }
        selected <- c(selected, matches)
    }
    unique(selected)
}

.cdrgam_cli_fit_item <- function(definitions, model_name) {
    model <- definitions$models[[model_name]]
    dataset_name <- model$datasets$train
    dataset <- definitions$datasets[[dataset_name]]
    dataset_identity <- .cdrgam_cli_dataset_identity(dataset)
    identity <- .cdrgam_cli_model_identity(model, dataset_identity$resolved)
    key <- paste('fit', definitions$project$project$id, model_name, identity$identity,
        sep=':')
    list(
        key=key, kind='fit', project=definitions$project$project$name,
        name=model_name, label=model_name, identity=identity$identity,
        resolved_identity=identity, dependencies=character(), model=model,
        dataset=dataset, dataset_name=dataset_name,
        dataset_identity=dataset_identity,
        output=.cdrgam_cli_path(definitions, 'model', model_name)
    )
}

.cdrgam_cli_resolve_prediction_dataset <- function(definitions, model, selector) {
    selector <- .cdrgam_cli_name(selector, 'prediction')
    if (selector %in% names(model$datasets)) return(model$datasets[[selector]])
    if (selector %in% names(definitions$datasets)) return(selector)
    .cdrgam_cli_abort(paste0(
        'Prediction selector ', sQuote(selector), ' matches neither a model partition ',
        'nor a dataset definition; partitions: ',
        paste(names(model$datasets), collapse=', '), '; datasets: ',
        paste(names(definitions$datasets), collapse=', ')
    ))
}

.cdrgam_cli_prediction_item <- function(definitions, model_name, selector, fit=NULL) {
    fit <- .cdrgam_cli_null(fit, .cdrgam_cli_fit_item(definitions, model_name))
    dataset_name <- .cdrgam_cli_resolve_prediction_dataset(
        definitions, fit$model, selector
    )
    dataset <- definitions$datasets[[dataset_name]]
    dataset_identity <- .cdrgam_cli_dataset_identity(dataset)
    identity <- .cdrgam_cli_prediction_identity(
        fit$resolved_identity$resolved, dataset_identity$resolved
    )
    key <- paste(
        'prediction', definitions$project$project$id, model_name, dataset_name,
        identity$identity, sep=':'
    )
    list(
        key=key, kind='prediction', project=definitions$project$project$name,
        name=paste(model_name, dataset_name, sep='_'), model_name=model_name,
        dataset_name=dataset_name, selector=selector, identity=identity$identity,
        resolved_identity=identity, dependencies=fit$key, model=fit$model,
        dataset=dataset, dataset_identity=dataset_identity, fit=fit,
        output=.cdrgam_cli_path(
            definitions, 'prediction', model_name, dataset=dataset_name
        )
    )
}

.cdrgam_cli_visualization_item <- function(definitions, name) {
    value <- definitions$visualizations[[name]]
    fit <- .cdrgam_cli_fit_item(definitions, value$model)
    prediction_selectors <- .cdrgam_cli_null(value$predictions, character())
    predictions <- lapply(prediction_selectors, function(selector) {
        .cdrgam_cli_prediction_item(definitions, value$model, selector, fit)
    })
    effect <- if (is.null(value$kind)) {
        .cdrgam_cli_effect_item(definitions, value$model, value, fit)
    } else NULL
    inputs <- if (is.null(effect)) c(
        list(model=fit$resolved_identity$resolved),
        lapply(predictions, function(item) item$resolved_identity$resolved)
    ) else list(effect=effect$resolved_identity$resolved)
    presentation <- value
    if (!is.null(effect)) {
        presentation$query <- NULL
        presentation$layers <- NULL
    }
    identity <- .cdrgam_cli_visualization_identity(presentation, inputs)
    key <- paste(
        'visualization', definitions$project$project$id, name, identity$identity,
        sep=':'
    )
    list(
        key=key, kind='visualization', project=definitions$project$project$name,
        name=name, model_name=value$model, identity=identity$identity,
        resolved_identity=identity,
        dependencies=if (is.null(effect)) {
            c(fit$key, vapply(predictions, `[[`, character(1), 'key'))
        } else effect$key,
        definition=value, fit=fit, effect=effect, predictions=predictions,
        output=.cdrgam_cli_path(
            definitions, 'visualization', value$model, dataset=name
        )
    )
}

.cdrgam_cli_effect_item <- function(definitions, model_name, value, fit=NULL) {
    fit <- .cdrgam_cli_null(fit, .cdrgam_cli_fit_item(definitions, model_name))
    identity <- .cdrgam_cli_effect_identity(
        value$query, value$layers, fit$resolved_identity$resolved
    )
    request_identity <- .cdrgam_cli_short_hash(list(
        query=value$query, layers=value$layers
    ))
    list(
        key=paste(
            'effect', definitions$project$project$id, model_name,
            identity$identity, sep=':'
        ),
        kind='effect', project=definitions$project$project$name,
        name=paste(model_name, request_identity, sep='_'),
        model_name=model_name, identity=identity$identity,
        resolved_identity=identity, dependencies=fit$key,
        query=value$query, layers=value$layers, fit=fit,
        output=.cdrgam_cli_path(
            definitions, 'effect', model_name, identity=identity$identity
        )
    )
}

.cdrgam_cli_comparison_item <- function(definitions, name) {
    value <- definitions$comparisons[[name]]
    if (is.null(value$evaluation)) {
        .cdrgam_cli_abort(paste0(
            'Comparison ', name, ' requires evaluation.dataset'
        ))
    }
    predictions <- lapply(value$models, function(model_name) {
        .cdrgam_cli_prediction_item(
            definitions, model_name, value$evaluation$dataset
        )
    })
    datasets <- unique(vapply(predictions, `[[`, character(1), 'dataset_name'))
    if (length(datasets) != 1L) {
        .cdrgam_cli_abort(paste0(
            'Comparison ', name, ' resolves its evaluation selector to different datasets: ',
            paste(datasets, collapse=', ')
        ))
    }
    identity <- .cdrgam_cli_comparison_identity(
        value, lapply(predictions, function(item) item$resolved_identity$resolved)
    )
    key <- paste(
        'comparison', definitions$project$project$id, name, identity$identity,
        sep=':'
    )
    list(
        key=key, kind='comparison', project=definitions$project$project$name,
        name=name, dataset_name=datasets[[1L]], identity=identity$identity,
        resolved_identity=identity,
        dependencies=vapply(predictions, `[[`, character(1), 'key'),
        definition=value, predictions=predictions,
        output=.cdrgam_cli_path(definitions, 'comparison', name)
    )
}

.cdrgam_cli_add_item <- function(graph, item) {
    graph[[item$key]] <- item
    children <- switch(
        item$kind,
        prediction=list(item$fit),
        effect=list(item$fit),
        visualization=if (is.null(item$effect)) {
            c(list(item$fit), item$predictions)
        } else list(item$effect),
        comparison=item$predictions,
        list()
    )
    for (child in children) graph <- .cdrgam_cli_add_item(graph, child)
    graph
}

.cdrgam_cli_reduce_targets <- function(targets, graph) {
    ancestors <- function(key, seen=character()) {
        dependencies <- graph[[key]]$dependencies
        dependencies <- setdiff(dependencies, seen)
        unique(c(dependencies, unlist(lapply(
            dependencies, ancestors, seen=c(seen, key)
        ), use.names=FALSE)))
    }
    redundant <- unique(unlist(lapply(targets, function(key) {
        intersect(ancestors(key), targets)
    }), use.names=FALSE))
    setdiff(unique(targets), redundant)
}

.cdrgam_cli_resolve_graph <- function(
        definitions, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, all=FALSE
) {
    explicit <- any(c(
        length(models), length(predictions), length(visualizations), length(comparisons)
    ) > 0L)
    if (!explicit) all <- TRUE
    model_names <- if (length(models)) {
        .cdrgam_cli_match_names(models, names(definitions$models), 'model')
    } else names(definitions$models)
    terminals <- list()
    if (isTRUE(all) || (length(models) && !length(predictions) &&
            !length(visualizations) && !length(comparisons))) {
        terminals <- c(terminals, lapply(model_names, function(name) {
            .cdrgam_cli_fit_item(definitions, name)
        }))
    }
    if (isTRUE(all)) {
        for (model_name in model_names) {
            partitions <- setdiff(names(definitions$models[[model_name]]$datasets), 'train')
            terminals <- c(terminals, lapply(partitions, function(partition) {
                .cdrgam_cli_prediction_item(definitions, model_name, partition)
            }))
        }
    } else if (length(predictions)) {
        for (model_name in model_names) {
            terminals <- c(terminals, lapply(predictions, function(partition) {
                .cdrgam_cli_prediction_item(definitions, model_name, partition)
            }))
        }
    }
    visualization_names <- if (isTRUE(all)) names(definitions$visualizations) else
        if (length(visualizations)) .cdrgam_cli_match_names(
            visualizations, names(definitions$visualizations), 'visualization'
        ) else character()
    if (length(models) && length(visualization_names)) {
        visualization_names <- visualization_names[vapply(
            definitions$visualizations[visualization_names],
            function(value) value$model %in% model_names, logical(1)
        )]
        if (!length(visualization_names)) {
            .cdrgam_cli_abort('Combined model and visualization selectors matched nothing')
        }
    }
    terminals <- c(terminals, lapply(visualization_names, function(name) {
        .cdrgam_cli_visualization_item(definitions, name)
    }))
    comparison_names <- if (isTRUE(all)) names(definitions$comparisons) else
        if (length(comparisons)) .cdrgam_cli_match_names(
            comparisons, names(definitions$comparisons), 'comparison'
        ) else character()
    if (length(models) && length(comparison_names)) {
        comparison_names <- comparison_names[vapply(
            definitions$comparisons[comparison_names],
            function(value) all(model_names %in% value$models), logical(1)
        )]
        if (!length(comparison_names)) {
            .cdrgam_cli_abort('Combined model and comparison selectors matched nothing')
        }
    }
    terminals <- c(terminals, lapply(comparison_names, function(name) {
        .cdrgam_cli_comparison_item(definitions, name)
    }))
    graph <- list()
    for (item in terminals) graph <- .cdrgam_cli_add_item(graph, item)
    targets <- .cdrgam_cli_reduce_targets(
        vapply(terminals, `[[`, character(1), 'key'), graph
    )
    list(items=graph, targets=targets)
}
