.cdrgam_cli_read_source <- function(source) {
    path <- source$resolved_path
    if (identical(source$format, 'rds')) {
        value <- readRDS(path)
    } else {
        separator <- .cdrgam_cli_null(
            source$separator,
            if (identical(source$format, 'tsv')) '\t' else ','
        )
        types <- if (is.null(source$types)) character() else
            unlist(source$types, use.names=TRUE)
        header <- utils::read.table(
            path, header=TRUE, sep=separator, quote='"', comment.char='',
            nrows=0L, check.names=FALSE, stringsAsFactors=FALSE
        )
        unknown <- setdiff(names(types), names(header))
        if (length(unknown)) {
            .cdrgam_cli_abort(paste0(
                'Delimited source types declare missing columns: ',
                paste(unknown, collapse=', ')
            ))
        }
        classes <- rep.int(NA_character_, length(header))
        names(classes) <- names(header)
        declared <- intersect(names(header), names(types))
        classes[declared] <- unname(c(
            character='character', double='numeric', integer='integer',
            logical='logical', factor='character'
        )[types[declared]])
        value <- utils::read.table(
            path, header=TRUE, sep=separator, quote='"', comment.char='',
            colClasses=classes, check.names=FALSE, stringsAsFactors=FALSE
        )
        factor_columns <- names(types)[types == 'factor']
        for (column in factor_columns) value[[column]] <- factor(value[[column]])
    }
    if (!is.data.frame(value)) {
        .cdrgam_cli_abort(paste0('Dataset source is not a data frame: ', path))
    }
    value
}

.cdrgam_cli_dataset_identity <- function(dataset) {
    source_identity <- lapply(dataset$sources, function(source) {
        info <- file.info(source$resolved_path)
        list(
            path=source$path,
            external=isTRUE(source$external),
            format=source$format,
            size=unname(info$size),
            content_md5=.cdrgam_cli_source_hash(source$resolved_path)
        )
    })
    preprocess <- dataset$preprocess
    if (!is.null(preprocess)) {
        preprocess$script_md5 <- .cdrgam_cli_source_hash(preprocess$resolved_script)
        preprocess$resolved_script <- NULL
    }
    scientific <- .cdrgam_cli_scientific_definition(dataset)
    for (stream in names(scientific$sources)) {
        scientific$sources[[stream]]$resolved_path <- NULL
        scientific$sources[[stream]]$external <- NULL
    }
    if (!is.null(scientific$preprocess)) {
        scientific$preprocess$resolved_script <- NULL
    }
    resolved <- .cdrgam_cli_artifact_contract(
        'dataset', scientific,
        list(sources=source_identity, preprocess=preprocess)
    )
    list(identity=.cdrgam_cli_short_hash(resolved), resolved=resolved)
}

.cdrgam_cli_apply_filters <- function(data, filters, dataset) {
    if (is.null(filters)) return(data)
    keep <- rep(TRUE, nrow(data))
    for (index in seq_along(filters)) {
        filter <- filters[[index]]
        field <- if (!is.null(filter$column)) filter$column else filter$factor
        if (!(field %in% names(data))) {
            .cdrgam_cli_abort(paste0(
                'Dataset ', dataset, ' filter [[', index,
                ']] references missing response column ', field
            ))
        }
        if (!is.null(filter$factor)) {
            counts <- table(data[[filter$factor]][keep])
            eligible <- names(counts[counts >= filter$min])
            selected <- as.character(data[[filter$factor]]) %in% eligible
            keep <- keep & !is.na(selected) & selected
            next
        }
        column <- data[[filter$column]]
        selected <- tryCatch(
            do.call(
                match.fun(filter$fun),
                c(list(column), list(filter$args))
            ),
            warning=function(warning) .cdrgam_cli_abort(paste0(
                'Dataset ', dataset, ' filter [[', index,
                ']] failed: ', conditionMessage(warning)
            )),
            error=function(error) .cdrgam_cli_abort(paste0(
                'Dataset ', dataset, ' filter [[', index,
                ']] failed: ', conditionMessage(error)
            ))
        )
        if (!is.logical(selected) || length(selected) != nrow(data)) {
            .cdrgam_cli_abort(paste0(
                'Dataset ', dataset, ' filter [[', index,
                ']] did not produce one logical value per row'
            ))
        }
        keep <- keep & !is.na(selected) & selected
    }
    data[keep, , drop=FALSE]
}

.cdrgam_cli_add_factor_interactions <- function(data, interactions, stream, dataset) {
    if (is.null(interactions)) return(data)
    for (name in names(interactions)) {
        columns <- interactions[[name]]
        present <- columns %in% names(data)
        if (!any(present)) next
        if (!all(present)) .cdrgam_cli_abort(paste0(
            'Dataset ', dataset, ' factor interaction ', name, ' is only ',
            'partially defined in ', stream, '; missing: ',
            paste(columns[!present], collapse=', ')
        ))
        if (name %in% names(data)) .cdrgam_cli_abort(paste0(
            'Dataset ', dataset, ' factor interaction ', name,
            ' would overwrite an existing ', stream, ' column'
        ))
        data[[name]] <- do.call(
            interaction,
            c(unname(data[columns]), list(drop=TRUE, sep='\034'))
        )
    }
    data
}

.cdrgam_cli_load_dataset <- function(dataset) {
    impulses <- .cdrgam_cli_read_source(dataset$sources$impulses)
    responses <- .cdrgam_cli_read_source(dataset$sources$responses)
    if (!is.null(dataset$filters)) {
        responses <- .cdrgam_cli_apply_filters(
            responses, dataset$filters, dataset$dataset
        )
    }
    if (!is.null(dataset$preprocess)) {
        environment <- new.env(parent=baseenv())
        sys.source(dataset$preprocess$resolved_script, envir=environment)
        function_name <- dataset$preprocess$`function`
        if (!exists(function_name, envir=environment, inherits=FALSE) ||
                !is.function(environment[[function_name]])) {
            .cdrgam_cli_abort(paste0(
                'Preprocessing script does not define function ', function_name
            ))
        }
        result <- environment[[function_name]](impulses, responses)
        if (!is.list(result) ||
                !all(c('impulses', 'responses') %in% names(result)) ||
                !is.data.frame(result$impulses) || !is.data.frame(result$responses)) {
            .cdrgam_cli_abort(
                'Preprocessing function must return a list with impulses and responses data frames'
            )
        }
        impulses <- result$impulses
        responses <- result$responses
    }
    interactions <- dataset$columns$factor_interactions
    impulses <- .cdrgam_cli_add_factor_interactions(
        impulses, interactions, 'impulses', dataset$dataset
    )
    responses <- .cdrgam_cli_add_factor_interactions(
        responses, interactions, 'responses', dataset$dataset
    )
    if (!is.null(interactions)) {
        missing <- names(interactions)[vapply(names(interactions), function(name) {
            !(name %in% names(impulses)) && !(name %in% names(responses))
        }, logical(1))]
        if (length(missing)) .cdrgam_cli_abort(paste0(
            'Dataset ', dataset$dataset,
            ' factor interactions have no source columns in either stream: ',
            paste(missing, collapse=', ')
        ))
    }
    factors <- .cdrgam_cli_null(dataset$columns$factors, character())
    for (column in factors) {
        if (column %in% names(impulses)) impulses[[column]] <- factor(impulses[[column]])
        if (column %in% names(responses)) responses[[column]] <- factor(responses[[column]])
    }
    list(impulses=impulses, responses=responses)
}

.cdrgam_cli_check_dataset_columns <- function(dataset, data) {
    columns <- dataset$columns
    impulse_required <- unique(c(
        .cdrgam_cli_null(columns$series, character()), columns$impulse_time
    ))
    response_required <- unique(c(
        .cdrgam_cli_null(columns$series, character()), columns$response_time,
        .cdrgam_cli_null(columns$row_id, character())
    ))
    missing_impulses <- setdiff(impulse_required, names(data$impulses))
    missing_responses <- setdiff(response_required, names(data$responses))
    if (length(missing_impulses) || length(missing_responses)) {
        .cdrgam_cli_abort(paste0(
            'Dataset ', dataset$dataset, ' is missing required columns',
            if (length(missing_impulses)) paste0(
                ' in impulses: ', paste(missing_impulses, collapse=', ')
            ) else '',
            if (length(missing_responses)) paste0(
                ' in responses: ', paste(missing_responses, collapse=', ')
            ) else ''
        ))
    }
    if (!is.null(columns$row_id)) {
        key <- data$responses[[columns$row_id]]
        if (anyNA(key) || anyDuplicated(key)) {
            .cdrgam_cli_abort(paste0(
                'Dataset ', dataset$dataset, ' response row key ', columns$row_id,
                ' must be complete and unique'
            ))
        }
    }
    invisible(TRUE)
}

.cdrgam_cli_formula <- function(text) {
    environment <- new.env(parent=asNamespace('mgcv'))
    environment$irf <- cdrgam::irf
    tryCatch(
        stats::as.formula(text, env=environment),
        error=function(error) .cdrgam_cli_abort(paste0(
            'Invalid model formula: ', conditionMessage(error)
        ))
    )
}

.cdrgam_cli_prepare_model <- function(model, dataset, data) {
    columns <- dataset$columns
    fit <- model$fit
    formula <- if (is.list(model$formula)) {
        lapply(model$formula, .cdrgam_cli_formula)
    } else .cdrgam_cli_formula(model$formula)
    arguments <- list(
        formula=formula,
        impulses=data$impulses,
        responses=data$responses,
        series=.cdrgam_cli_null(columns$series, character()),
        impulse_time=columns$impulse_time,
        response_time=columns$response_time,
        quiet=TRUE
    )
    if (!is.null(model$window)) arguments$window <- model$window
    for (field in intersect(.cdrgam_cli_irf_default_keys, names(model))) {
        arguments[field] <- model[field]
    }
    for (field in c(
            'history', 'chunk_size', 'rescale_predictors', 'drop.unused.levels'
    )) {
        value <- fit[[field, exact=TRUE]]
        if (!is.null(value)) arguments[[field]] <- value
    }
    do.call(cdrgam::prepare_cdrgam, arguments)
}
