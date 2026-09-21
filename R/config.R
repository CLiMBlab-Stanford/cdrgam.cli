.cdrgam_cli_definition_files <- function(root, kind) {
    directory <- file.path(root, 'definitions', kind)
    if (!dir.exists(directory)) return(character())
    sort(list.files(directory, pattern='\\.ya?ml$', full.names=TRUE))
}

.cdrgam_cli_validate_schema <- function(value, path) {
    if (is.null(value$schema) || !is.numeric(value$schema) ||
            length(value$schema) != 1L || value$schema != 1) {
        .cdrgam_cli_abort(paste0(path, ': schema must be 1'))
    }
    invisible(value)
}

.cdrgam_cli_validate_project <- function(value, path) {
    .cdrgam_cli_validate_schema(value, path)
    .cdrgam_cli_check_keys(
        value, c('schema', 'project'), path, c('schema', 'project')
    )
    .cdrgam_cli_check_keys(
        value$project, c('name', 'id'), paste0(path, ': project'), c('name', 'id')
    )
    .cdrgam_cli_name(value$project$name, paste0(path, ': project.name'))
    id <- .cdrgam_cli_scalar_character(value$project$id, paste0(path, ': project.id'))
    if (grepl('[/\\\\]', id) || id %in% c('.', '..')) {
        .cdrgam_cli_abort(paste0(path, ': project.id must be one safe path component'))
    }
    value
}

.cdrgam_cli_validate_source <- function(value, field, root, check_exists=TRUE) {
    .cdrgam_cli_check_keys(
        value, c('path', 'format', 'separator', 'types'), field,
        c('path', 'format')
    )
    path_text <- .cdrgam_cli_scalar_character(value$path, paste0(field, '.path'))
    format <- match.arg(
        .cdrgam_cli_scalar_character(value$format, paste0(field, '.format')),
        c('rds', 'csv', 'tsv')
    )
    resolved <- if (grepl('^(/|[A-Za-z]:[/\\\\])', path_text)) {
        path.expand(path_text)
    } else {
        file.path(root, path_text)
    }
    resolved <- .cdrgam_cli_normalize_path(resolved, must_work=FALSE)
    store_root <- dirname(dirname(root))
    if (grepl('^(/|[A-Za-z]:[/\\\\])', path_text) &&
            .cdrgam_cli_within(resolved, store_root)) {
        .cdrgam_cli_abort(paste0(
            field, '.path points inside cdrgam_root and must be relative to the project'
        ))
    }
    if (isTRUE(check_exists) && !file.exists(resolved)) {
        .cdrgam_cli_abort(paste0(field, '.path does not exist: ', resolved))
    }
    if (!is.null(value$separator)) {
        separator <- .cdrgam_cli_scalar_character(
            value$separator, paste0(field, '.separator'), allow_empty=TRUE
        )
        if (nchar(separator) != 1L) {
            .cdrgam_cli_abort(paste0(field, '.separator must be one character'))
        }
    }
    if (!is.null(value$types)) {
        if (!is.list(value$types) || is.null(names(value$types)) ||
                any(!nzchar(names(value$types)))) {
            .cdrgam_cli_abort(paste0(field, '.types must be a named mapping'))
        }
        allowed_types <- c('character', 'double', 'integer', 'logical', 'factor')
        invalid <- setdiff(unlist(value$types, use.names=FALSE), allowed_types)
        if (length(invalid)) {
            .cdrgam_cli_abort(paste0(
                field, '.types contains unsupported types: ',
                paste(unique(invalid), collapse=', ')
            ))
        }
    }
    value$format <- format
    value$resolved_path <- resolved
    value$external <- !.cdrgam_cli_within(resolved, root)
    value
}

.cdrgam_cli_validate_dataset <- function(value, path, root, check_exists=TRUE) {
    .cdrgam_cli_validate_schema(value, path)
    .cdrgam_cli_check_keys(
        value, c('schema', 'dataset', 'sources', 'columns', 'filters', 'preprocess'), path,
        c('schema', 'dataset', 'sources', 'columns')
    )
    value$dataset <- .cdrgam_cli_name(value$dataset, paste0(path, ': dataset'))
    .cdrgam_cli_check_keys(
        value$sources, c('impulses', 'responses'), paste0(path, ': sources'),
        c('impulses', 'responses')
    )
    value$sources$impulses <- .cdrgam_cli_validate_source(
        value$sources$impulses, paste0(path, ': sources.impulses'), root,
        check_exists
    )
    value$sources$responses <- .cdrgam_cli_validate_source(
        value$sources$responses, paste0(path, ': sources.responses'), root,
        check_exists
    )
    .cdrgam_cli_check_keys(
        value$columns,
        c(
            'series', 'impulse_time', 'response_time', 'factors',
            'factor_interactions', 'row_id'
        ),
        paste0(path, ': columns'),
        c('impulse_time', 'response_time')
    )
    for (field in c('impulse_time', 'response_time')) {
        .cdrgam_cli_scalar_character(
            value$columns[[field]], paste0(path, ': columns.', field)
        )
    }
    for (field in intersect(c('series', 'factors'), names(value$columns))) {
        if (!is.character(value$columns[[field]]) || anyNA(value$columns[[field]])) {
            .cdrgam_cli_abort(paste0(path, ': columns.', field, ' must be a string list'))
        }
    }
    if (!is.null(value$columns$row_id)) {
        .cdrgam_cli_scalar_character(value$columns$row_id, paste0(path, ': columns.row_id'))
    }
    if (!is.null(value$columns$factor_interactions)) {
        interactions <- value$columns$factor_interactions
        field <- paste0(path, ': columns.factor_interactions')
        if (!is.list(interactions) || !length(interactions) ||
                is.null(names(interactions)) || any(!nzchar(names(interactions))) ||
                anyDuplicated(names(interactions))) {
            .cdrgam_cli_abort(paste0(field, ' must be a nonempty named mapping'))
        }
        for (name in names(interactions)) {
            columns <- interactions[[name]]
            if (!is.character(columns) || length(columns) < 2L || anyNA(columns) ||
                    any(!nzchar(columns)) || anyDuplicated(columns)) {
                .cdrgam_cli_abort(paste0(
                    field, '.', name,
                    ' must list at least two distinct source columns'
                ))
            }
        }
    }
    if (!is.null(value$filters)) {
        filters <- value$filters
        field <- paste0(path, ': filters')
        if (!is.list(filters) || !length(filters) ||
                any(!vapply(filters, is.list, logical(1)))) {
            .cdrgam_cli_abort(paste0(field, ' must be a nonempty list of filters'))
        }
        for (index in seq_along(filters)) {
            filter <- filters[[index]]
            filter_field <- paste0(field, '[[', index, ']]')
            .cdrgam_cli_check_keys(
                filter, c('column', 'fun', 'args', 'factor', 'min'), filter_field
            )
            comparator <- c('column', 'fun', 'args')
            frequency <- c('factor', 'min')
            is_comparator <- all(comparator %in% names(filter)) &&
                !any(frequency %in% names(filter))
            is_frequency <- all(frequency %in% names(filter)) &&
                !any(comparator %in% names(filter))
            if (!is_comparator && !is_frequency) {
                .cdrgam_cli_abort(paste0(
                    filter_field, ' must contain either column, fun, and args; ',
                    'or factor and min'
                ))
            }
            if (is_comparator) {
                .cdrgam_cli_scalar_character(
                    filter$column, paste0(filter_field, '.column')
                )
                fun <- .cdrgam_cli_scalar_character(
                    filter$fun, paste0(filter_field, '.fun')
                )
                tryCatch(
                    match.fun(fun),
                    error=function(error) .cdrgam_cli_abort(paste0(
                        filter_field, '.fun does not resolve to an R function: ',
                        conditionMessage(error)
                    ))
                )
            } else {
                .cdrgam_cli_scalar_character(
                    filter$factor, paste0(filter_field, '.factor')
                )
                if (!is.numeric(filter$min) || length(filter$min) != 1L ||
                        is.na(filter$min) || !is.finite(filter$min) ||
                        filter$min < 1 || filter$min %% 1 != 0 ||
                        filter$min > .Machine$integer.max) {
                    .cdrgam_cli_abort(paste0(
                        filter_field, '.min must be a positive integer'
                    ))
                }
                filter$min <- as.integer(filter$min)
                value$filters[[index]] <- filter
            }
        }
    }
    if (!is.null(value$preprocess)) {
        .cdrgam_cli_check_keys(
            value$preprocess, c('script', 'function'), paste0(path, ': preprocess'),
            c('script', 'function')
        )
        script <- .cdrgam_cli_scalar_character(
            value$preprocess$script, paste0(path, ': preprocess.script')
        )
        script_path <- if (startsWith(script, '/')) script else file.path(root, script)
        script_path <- .cdrgam_cli_normalize_path(script_path, must_work=FALSE)
        if (grepl('^(/|[A-Za-z]:[/\\\\])', script) &&
                .cdrgam_cli_within(script_path, dirname(dirname(root)))) {
            .cdrgam_cli_abort(paste0(
                path, ': preprocess.script points inside cdrgam_root and must be ',
                'relative to the project'
            ))
        }
        if (check_exists && !file.exists(script_path)) {
            .cdrgam_cli_abort(paste0(path, ': preprocess.script does not exist: ', script_path))
        }
        value$preprocess$resolved_script <- script_path
        .cdrgam_cli_scalar_character(
            value$preprocess$`function`, paste0(path, ': preprocess.function')
        )
    }
    attr(value, 'path') <- path
    value
}

.cdrgam_cli_fit_keys <- c(
    'family', 'method', 'backend', 'engine', 'history', 'history_length',
    'chunk_size', 'rescale_predictors', 'sparse_control', 'rank_action',
    'rank_tol', 'rank_penalty', 'drop.unused.levels'
)

.cdrgam_cli_validate_model <- function(value, path) {
    .cdrgam_cli_validate_schema(value, path)
    .cdrgam_cli_check_keys(
        value, c('schema', 'model', 'datasets', 'formula', 'window', 'fit'), path,
        c('schema', 'model', 'datasets', 'formula')
    )
    value$model <- .cdrgam_cli_name(value$model, paste0(path, ': model'))
    if (!is.list(value$datasets) || is.null(names(value$datasets)) ||
            any(!nzchar(names(value$datasets))) || anyDuplicated(names(value$datasets))) {
        .cdrgam_cli_abort(paste0(path, ': datasets must be a named mapping'))
    }
    if (!('train' %in% names(value$datasets))) {
        .cdrgam_cli_abort(paste0(path, ': datasets must define reserved key train'))
    }
    names(value$datasets) <- vapply(
        names(value$datasets), .cdrgam_cli_name, character(1),
        field=paste0(path, ': datasets key')
    )
    value$datasets <- lapply(value$datasets, .cdrgam_cli_name,
        field=paste0(path, ': datasets value'))
    value$formula <- .cdrgam_cli_scalar_character(value$formula, paste0(path, ': formula'))
    if (!is.null(value$window)) {
        value$window <- unlist(value$window, use.names=FALSE)
        if (!is.numeric(value$window) || length(value$window) != 2L ||
                !is.finite(value$window[[1L]]) || is.na(value$window[[2L]]) ||
                value$window[[2L]] < value$window[[1L]]) {
            .cdrgam_cli_abort(paste0(
                path, ': window must contain minimum and maximum lag'
            ))
        }
    }
    if (is.null(value$fit)) value$fit <- list()
    .cdrgam_cli_check_keys(value$fit, .cdrgam_cli_fit_keys, paste0(path, ': fit'))
    if (!is.null(value$fit$backend) &&
            !(value$fit$backend %in% c('mgcv', 'block', 'sparse'))) {
        .cdrgam_cli_abort(paste0(path, ': fit.backend must be mgcv, block, or sparse'))
    }
    if (!is.null(value$fit$drop.unused.levels) &&
            (length(value$fit$drop.unused.levels) != 1L ||
                !is.logical(value$fit$drop.unused.levels) ||
                is.na(value$fit$drop.unused.levels))) {
        .cdrgam_cli_abort(paste0(
            path, ': fit.drop.unused.levels must be true or false'
        ))
    }
    if (!is.null(value$fit$family) &&
            !(value$fit$family %in% c('gaussian', 'binomial', 'poisson', 'Gamma'))) {
        .cdrgam_cli_abort(paste0(path, ': unsupported fit.family'))
    }
    attr(value, 'path') <- path
    value
}

.cdrgam_cli_validate_visualization <- function(value, path) {
    .cdrgam_cli_validate_schema(value, path)
    .cdrgam_cli_check_keys(
        value, c(
            'schema', 'visualization', 'model', 'kind', 'predictions', 'pages',
            'query', 'layers', 'render'
        ),
        path, c('schema', 'visualization', 'model')
    )
    value$visualization <- .cdrgam_cli_name(
        value$visualization, paste0(path, ': visualization')
    )
    value$model <- .cdrgam_cli_name(value$model, paste0(path, ': model'))
    legacy <- !is.null(value$kind)
    if (legacy) value$kind <- match.arg(
        .cdrgam_cli_scalar_character(value$kind, paste0(path, ': kind')),
        'booklet'
    )
    if (legacy && (!is.null(value$query) || !is.null(value$layers) ||
            !is.null(value$render))) {
        .cdrgam_cli_abort(paste0(path, ': booklet fields cannot be combined with query or render'))
    }
    if (!legacy && (is.null(value$query) || is.null(value$render))) {
        .cdrgam_cli_abort(paste0(path, ': effect visualizations require query and render'))
    }
    if (!legacy && (!is.null(value$predictions) || !is.null(value$pages))) {
        .cdrgam_cli_abort(paste0(
            path, ': predictions and pages are only valid for booklet visualizations'
        ))
    }
    if (!is.null(value$predictions)) {
        if (!is.character(value$predictions) || !length(value$predictions) ||
                anyNA(value$predictions)) {
            .cdrgam_cli_abort(paste0(path, ': predictions must be a nonempty string list'))
        }
        value$predictions <- vapply(
            value$predictions, .cdrgam_cli_name, character(1),
            field=paste0(path, ': predictions')
        )
    }
    if (!is.null(value$pages)) {
        value$pages <- .cdrgam_cli_positive_integer(value$pages, paste0(path, ': pages'))
    }
    if (!legacy) {
        value$query <- .cdrgam_cli_validate_effect_query(
            value$query, paste0(path, ': query')
        )
        if (!is.null(value$layers)) {
            if (!is.list(value$layers) || !length(value$layers) ||
                    any(!vapply(value$layers, is.list, logical(1)))) {
                .cdrgam_cli_abort(paste0(path, ': layers must be a nonempty list'))
            }
            ids <- character(length(value$layers))
            for (index in seq_along(value$layers)) {
                field <- paste0(path, ': layers[[', index, ']]')
                layer <- value$layers[[index]]
                .cdrgam_cli_check_keys(
                    layer,
                    c('id', 'composition', 'grouping', 'groups', 'interval', 'style'),
                    field, 'id'
                )
                layer$id <- .cdrgam_cli_name(layer$id, paste0(field, '.id'))
                if (!is.null(layer$composition)) layer$composition <- match.arg(
                    layer$composition, c('term', 'deviation', 'total')
                )
                if (!is.null(layer$grouping)) layer$grouping <- match.arg(
                    layer$grouping, c('population', 'deviation', 'conditional')
                )
                if (!is.null(layer$groups)) {
                    layer$groups <- as.character(unlist(layer$groups, use.names=FALSE))
                }
                if (!is.null(layer$interval)) layer$interval <- match.arg(
                    layer$interval, c('ribbon', 'lines', 'none')
                )
                if (!is.null(layer$style)) {
                    .cdrgam_cli_check_keys(
                        layer$style, c('color', 'fill', 'alpha', 'linewidth'),
                        paste0(field, '.style')
                    )
                    for (number in intersect(c('alpha', 'linewidth'), names(layer$style))) {
                        if (!is.numeric(layer$style[[number]]) ||
                                length(layer$style[[number]]) != 1L ||
                                !is.finite(layer$style[[number]])) {
                            .cdrgam_cli_abort(paste0(field, '.style.', number, ' must be numeric'))
                        }
                    }
                    for (name in intersect(c('color', 'fill'), names(layer$style))) {
                        layer$style[[name]] <- .cdrgam_cli_scalar_character(
                            layer$style[[name]], paste0(field, '.style.', name)
                        )
                    }
                }
                value$layers[[index]] <- layer
                ids[[index]] <- layer$id
            }
            if (anyDuplicated(ids)) .cdrgam_cli_abort(paste0(path, ': layer ids must be unique'))
        }
        value$render <- .cdrgam_cli_validate_render(
            value$render, paste0(path, ': render')
        )
    }
    attr(value, 'path') <- path
    value
}

.cdrgam_cli_validate_axis_request <- function(value, field) {
    if (is.numeric(value)) return(as.numeric(value))
    if (!is.list(value)) .cdrgam_cli_abort(paste0(field, ' must be numeric or a mapping'))
    .cdrgam_cli_check_keys(value, c('at', 'values', 'quantiles', 'grid', 'n'), field)
    modes <- intersect(c('at', 'values', 'quantiles', 'grid'), names(value))
    if (length(modes) != 1L) .cdrgam_cli_abort(paste0(
        field, ' must define exactly one of at, values, quantiles, or grid'
    ))
    if (!is.null(value$at)) {
        if (is.list(value$at)) {
            .cdrgam_cli_check_keys(
                value$at, c('summary', 'offset-sd'), paste0(field, '.at'), 'summary'
            )
            value$at$summary <- match.arg(value$at$summary, c('mean', 'median'))
            if (!is.null(value$at$`offset-sd`) &&
                    (!is.numeric(value$at$`offset-sd`) ||
                     length(value$at$`offset-sd`) != 1L ||
                     !is.finite(value$at$`offset-sd`))) {
                .cdrgam_cli_abort(paste0(field, '.at.offset-sd must be numeric'))
            }
        } else {
            value$at <- as.numeric(value$at)
            if (!length(value$at) || any(!is.finite(value$at))) {
                .cdrgam_cli_abort(paste0(field, '.at must contain finite numbers'))
            }
        }
    }
    for (name in intersect(c('values', 'quantiles'), names(value))) {
        value[[name]] <- as.numeric(unlist(value[[name]], use.names=FALSE))
        if (!length(value[[name]]) || any(!is.finite(value[[name]]))) {
            .cdrgam_cli_abort(paste0(field, '.', name, ' must contain finite numbers'))
        }
    }
    if (!is.null(value$quantiles) && any(value$quantiles < 0 | value$quantiles > 1)) {
        .cdrgam_cli_abort(paste0(field, '.quantiles must lie between zero and one'))
    }
    if (!is.null(value$grid) && !identical(value$grid, 'fitted')) {
        .cdrgam_cli_abort(paste0(field, '.grid must be fitted'))
    }
    if (!is.null(value$n)) {
        value$n <- .cdrgam_cli_positive_integer(value$n, paste0(field, '.n'))
        if (value$n < 2L) .cdrgam_cli_abort(paste0(field, '.n must be at least two'))
    }
    value
}

.cdrgam_cli_validate_effect_query <- function(value, field) {
    if (!is.list(value)) .cdrgam_cli_abort(paste0(field, ' must be a mapping'))
    .cdrgam_cli_check_keys(
        value,
        c('terms', 'composition', 'grouping', 'groups', 'axes', 'uncertainty', 'n'),
        field
    )
    if (!is.null(value$terms)) {
        if (is.character(value$terms)) {
            value$terms <- as.character(value$terms)
        } else if (is.list(value$terms)) {
            .cdrgam_cli_check_keys(
                value$terms, c('names', 'predictors', 'match', 'grouped'),
                paste0(field, '.terms')
            )
            for (name in intersect(c('names', 'predictors'), names(value$terms))) {
                value$terms[[name]] <- as.character(unlist(
                    value$terms[[name]], use.names=FALSE
                ))
            }
            if (!is.null(value$terms$match)) value$terms$match <- match.arg(
                value$terms$match, c('contains', 'exact')
            )
            if (!is.null(value$terms$grouped) &&
                    (!is.logical(value$terms$grouped) ||
                     length(value$terms$grouped) != 1L || is.na(value$terms$grouped))) {
                .cdrgam_cli_abort(paste0(field, '.terms.grouped must be true or false'))
            }
        } else .cdrgam_cli_abort(paste0(field, '.terms must be labels or a mapping'))
    }
    value$composition <- match.arg(
        .cdrgam_cli_null(value$composition, 'term'), c('term', 'deviation', 'total')
    )
    value$grouping <- match.arg(
        .cdrgam_cli_null(value$grouping, 'population'),
        c('population', 'deviation', 'conditional')
    )
    if (!is.null(value$groups)) value$groups <- as.character(unlist(
        value$groups, use.names=FALSE
    ))
    if (is.null(value$axes)) value$axes <- list()
    if (!is.list(value$axes)) .cdrgam_cli_abort(paste0(field, '.axes must be a mapping'))
    .cdrgam_cli_check_keys(value$axes, c('lag', 'time', 'predictors'), paste0(field, '.axes'))
    for (name in intersect(c('lag', 'time'), names(value$axes))) {
        value$axes[[name]] <- .cdrgam_cli_validate_axis_request(
            value$axes[[name]], paste0(field, '.axes.', name)
        )
    }
    if (!is.null(value$axes$predictors)) {
        if (!is.list(value$axes$predictors) || is.null(names(value$axes$predictors)) ||
                any(!nzchar(names(value$axes$predictors)))) {
            .cdrgam_cli_abort(paste0(field, '.axes.predictors must be a named mapping'))
        }
        for (name in names(value$axes$predictors)) {
            value$axes$predictors[[name]] <- .cdrgam_cli_validate_axis_request(
                value$axes$predictors[[name]],
                paste0(field, '.axes.predictors.', name)
            )
        }
    }
    if (is.null(value$uncertainty)) value$uncertainty <- list()
    .cdrgam_cli_check_keys(
        value$uncertainty, c('level', 'kind', 'unconditional'),
        paste0(field, '.uncertainty')
    )
    value$uncertainty$level <- .cdrgam_cli_null(value$uncertainty$level, 0.95)
    if (!is.numeric(value$uncertainty$level) || length(value$uncertainty$level) != 1L ||
            !is.finite(value$uncertainty$level) || value$uncertainty$level <= 0 ||
            value$uncertainty$level >= 1) {
        .cdrgam_cli_abort(paste0(field, '.uncertainty.level must lie between zero and one'))
    }
    value$uncertainty$kind <- match.arg(
        .cdrgam_cli_null(value$uncertainty$kind, 'pointwise'), 'pointwise'
    )
    value$uncertainty$unconditional <- .cdrgam_cli_null(
        value$uncertainty$unconditional, FALSE
    )
    if (!is.logical(value$uncertainty$unconditional) ||
            length(value$uncertainty$unconditional) != 1L ||
            is.na(value$uncertainty$unconditional)) {
        .cdrgam_cli_abort(paste0(field, '.uncertainty.unconditional must be true or false'))
    }
    value$n <- .cdrgam_cli_positive_integer(.cdrgam_cli_null(value$n, 200L), paste0(field, '.n'))
    if (value$n < 2L) .cdrgam_cli_abort(paste0(field, '.n must be at least two'))
    value
}

.cdrgam_cli_validate_render <- function(value, field) {
    if (!is.list(value)) .cdrgam_cli_abort(paste0(field, ' must be a mapping'))
    .cdrgam_cli_check_keys(
        value,
        c(
            'geometry', 'mappings', 'interval', 'theme', 'formats', 'width',
            'height', 'dpi', 'title', 'xlab', 'ylab'
        ),
        field, c('geometry', 'mappings')
    )
    value$geometry <- match.arg(
        value$geometry, c('line', 'raster', 'contour', 'raster-contour')
    )
    if (!is.list(value$mappings) || is.null(names(value$mappings))) {
        .cdrgam_cli_abort(paste0(field, '.mappings must be a named mapping'))
    }
    .cdrgam_cli_check_keys(
        value$mappings, c('x', 'y', 'color', 'fill', 'linetype', 'group', 'facet'),
        paste0(field, '.mappings'), c('x')
    )
    value$mappings <- lapply(value$mappings, .cdrgam_cli_scalar_character,
        field=paste0(field, '.mappings value'))
    value$interval <- match.arg(
        .cdrgam_cli_null(value$interval, 'ribbon'), c('ribbon', 'lines', 'companion', 'none')
    )
    if (is.null(value$mappings$y)) {
        .cdrgam_cli_abort(paste0(field, '.mappings.y is required'))
    }
    if (identical(value$geometry, 'line') && identical(value$interval, 'companion')) {
        .cdrgam_cli_abort(paste0(field, '.interval companion requires a surface geometry'))
    }
    if (!identical(value$geometry, 'line') &&
            value$interval %in% c('ribbon', 'lines')) {
        .cdrgam_cli_abort(paste0(
            field, '.interval ribbon and lines require line geometry'
        ))
    }
    if (value$geometry %in% c('raster', 'raster-contour') &&
            is.null(value$mappings$fill)) {
        .cdrgam_cli_abort(paste0(field, '.mappings.fill is required for raster geometry'))
    }
    value$theme <- match.arg(
        .cdrgam_cli_null(value$theme, 'paper'), c('paper', 'minimal', 'slides')
    )
    value$formats <- .cdrgam_cli_null(value$formats, 'pdf')
    value$formats <- unique(as.character(unlist(value$formats, use.names=FALSE)))
    if (!length(value$formats) || any(!(value$formats %in% c('pdf', 'png', 'svg')))) {
        .cdrgam_cli_abort(paste0(field, '.formats supports pdf, png, and svg'))
    }
    for (name in c('width', 'height', 'dpi')) if (!is.null(value[[name]]) &&
            (!is.numeric(value[[name]]) || length(value[[name]]) != 1L ||
             !is.finite(value[[name]]) || value[[name]] <= 0)) {
        .cdrgam_cli_abort(paste0(field, '.', name, ' must be positive'))
    }
    for (name in c('title', 'xlab', 'ylab')) if (!is.null(value[[name]])) {
        value[[name]] <- .cdrgam_cli_scalar_character(value[[name]], paste0(field, '.', name))
    }
    value
}

.cdrgam_cli_validate_comparison <- function(value, path) {
    .cdrgam_cli_validate_schema(value, path)
    .cdrgam_cli_check_keys(
        value, c('schema', 'comparison', 'models', 'evaluation', 'methods'),
        path, c('schema', 'comparison', 'models', 'methods')
    )
    value$comparison <- .cdrgam_cli_name(value$comparison, paste0(path, ': comparison'))
    if (!is.character(value$models) || length(value$models) < 2L) {
        .cdrgam_cli_abort(paste0(path, ': models must contain at least two names'))
    }
    value$models <- vapply(
        value$models, .cdrgam_cli_name, character(1), field=paste0(path, ': models')
    )
    if (!is.character(value$methods) || !length(value$methods)) {
        .cdrgam_cli_abort(paste0(path, ': methods must be a nonempty string list'))
    }
    unsupported <- setdiff(value$methods, c('mse', 'mae'))
    if (length(unsupported)) {
        .cdrgam_cli_abort(paste0(
            path, ': unsupported comparison methods: ',
            paste(unique(unsupported), collapse=', ')
        ))
    }
    if (!is.null(value$evaluation)) {
        .cdrgam_cli_check_keys(
            value$evaluation, c('dataset', 'row_id'), paste0(path, ': evaluation'),
            c('dataset', 'row_id')
        )
        .cdrgam_cli_name(value$evaluation$dataset, paste0(path, ': evaluation.dataset'))
        .cdrgam_cli_scalar_character(
            value$evaluation$row_id, paste0(path, ': evaluation.row_id')
        )
    }
    attr(value, 'path') <- path
    value
}

.cdrgam_cli_read_definitions <- function(project=NULL, check_sources=TRUE, checkout=NULL) {
    root <- find_cdrgam_project(project, checkout)
    project_path <- file.path(root, .cdrgam_cli_marker)
    project <- .cdrgam_cli_validate_project(
        .cdrgam_cli_read_yaml(project_path), project_path
    )
    project$project$name <- basename(root)
    read_kind <- function(directory, validator, ...) {
        files <- .cdrgam_cli_definition_files(root, directory)
        values <- lapply(files, function(path) {
            validator(.cdrgam_cli_read_yaml(path), path, ...)
        })
        if (!length(values)) return(list())
        names(values) <- vapply(values, function(value) {
            value[[switch(directory, datasets='dataset', models='model',
                visualizations='visualization', comparisons='comparison')]]
        }, character(1))
        duplicates <- unique(names(values)[duplicated(names(values))])
        if (length(duplicates)) {
            .cdrgam_cli_abort(paste0(
                'Duplicate ', directory, ' definitions: ',
                paste(duplicates, collapse=', ')
            ))
        }
        values
    }
    datasets <- read_kind('datasets', .cdrgam_cli_validate_dataset,
        root=root, check_exists=check_sources)
    models <- read_kind('models', .cdrgam_cli_validate_model)
    visualizations <- read_kind('visualizations', .cdrgam_cli_validate_visualization)
    comparisons <- read_kind('comparisons', .cdrgam_cli_validate_comparison)
    model_datasets <- unique(unlist(lapply(models, function(value) {
        unlist(value$datasets, use.names=FALSE)
    })))
    missing_datasets <- setdiff(model_datasets, names(datasets))
    if (length(missing_datasets)) {
        .cdrgam_cli_abort(paste0(
            'Models reference missing datasets: ', paste(unique(missing_datasets), collapse=', ')
        ))
    }
    missing_visualization_models <- setdiff(
        vapply(visualizations, `[[`, character(1), 'model'), names(models)
    )
    if (length(missing_visualization_models)) {
        .cdrgam_cli_abort(paste0(
            'Visualizations reference missing models: ',
            paste(unique(missing_visualization_models), collapse=', ')
        ))
    }
    comparison_models <- unique(unlist(lapply(comparisons, `[[`, 'models')))
    missing_models <- setdiff(comparison_models, names(models))
    if (length(missing_models)) {
        .cdrgam_cli_abort(paste0(
            'Comparisons reference missing models: ', paste(missing_models, collapse=', ')
        ))
    }
    list(
        root=root, project=project, datasets=datasets, models=models,
        visualizations=visualizations, comparisons=comparisons,
        checkout=.cdrgam_cli_checkout(checkout)
    )
}
