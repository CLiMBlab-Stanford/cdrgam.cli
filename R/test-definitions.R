# Test definitions: whole-function tests of native model terms, and
# statistical tests and interval estimates of summary statistics of fitted
# effects. `terms` selects native terms by their summary() labels. For
# derived statistics, a definition names effects, each a
# single-term effect query in the visualization value language, and
# estimands built from them in a fixed vocabulary. The core evaluates every
# effect on one joint distribution of their coefficients, through its
# effect_inference(), and tests whole functionals with Wood's statistic,
# through its test_effect().

.cdrgam_cli_test_forms <- c('weights', 'mean', 'area', 'peak_lag', 'peak_value', 'wood')
.cdrgam_cli_test_linear_forms <- c('weights', 'mean', 'area')
.cdrgam_cli_test_peak_forms <- c('peak_lag', 'peak_value')
.cdrgam_cli_test_default_draws <- 2000L
.cdrgam_cli_test_default_seed <- 1L

.cdrgam_cli_test_label <- function(value, field) {
    value <- .cdrgam_cli_scalar_character(value, field)
    if (!grepl('^[A-Za-z][A-Za-z0-9_.-]*$', value)) .cdrgam_cli_abort(paste0(
        field, ' must start with a letter and use letters, digits, ., _, or -'
    ))
    value
}

.cdrgam_cli_test_number <- function(value, field) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
        .cdrgam_cli_abort(paste0(field, ' must be one finite number'))
    }
    as.numeric(value)
}

.cdrgam_cli_validate_test_effect <- function(value, field) {
    if (!is.list(value)) .cdrgam_cli_abort(paste0(field, ' must be a mapping'))
    for (name in intersect(c('test', 'uncertainty'), names(value))) {
        .cdrgam_cli_abort(paste0(
            field, '.', name, ' is not an effect field; set uncertainty at the top level'
        ))
    }
    value <- .cdrgam_cli_validate_effect_query(value, field)
    value$uncertainty <- NULL
    if (!is.null(.cdrgam_cli_pointwise_direction(value$axes$lag))) {
        .cdrgam_cli_abort(paste0(
            field, '.axes.lag cannot be a pointwise peak; use peak or peak(TERM)'
        ))
    }
    value
}

.cdrgam_cli_validate_test_estimand <- function(value, field, effects) {
    if (!is.list(value) || is.null(names(value))) {
        .cdrgam_cli_abort(paste0(field, ' must be a mapping'))
    }
    .cdrgam_cli_check_keys(
        value, c(.cdrgam_cli_test_forms, 'direction', 'null'), field
    )
    form <- intersect(names(value), .cdrgam_cli_test_forms)
    if (length(form) != 1L) .cdrgam_cli_abort(paste0(
        field, ' must use exactly one of ',
        paste(.cdrgam_cli_test_forms, collapse=', ')
    ))
    effect_name <- function(name, subfield) {
        name <- .cdrgam_cli_scalar_character(name, subfield)
        if (!(name %in% effects)) .cdrgam_cli_abort(paste0(
            subfield, ' names unknown effect ', name
        ))
        name
    }
    output <- list(form=form)
    if (identical(form, 'weights')) {
        weights <- value$weights
        if (!is.list(weights) || !length(weights) || is.null(names(weights))) {
            .cdrgam_cli_abort(paste0(field, '.weights must map effect names to weights'))
        }
        output$weights <- list()
        for (name in names(weights)) {
            subfield <- paste0(field, '.weights.', name)
            effect_name(name, subfield)
            entry <- unlist(weights[[name]], use.names=FALSE)
            if (!is.numeric(entry) || !length(entry) || any(!is.finite(entry))) {
                .cdrgam_cli_abort(paste0(subfield, ' must be finite numbers'))
            }
            output$weights[[name]] <- as.numeric(entry)
        }
    } else {
        output$effect <- effect_name(value[[form]], paste0(field, '.', form))
    }
    if (!is.null(value$direction)) {
        if (!(form %in% .cdrgam_cli_test_peak_forms)) .cdrgam_cli_abort(paste0(
            field, '.direction applies only to peak_lag and peak_value'
        ))
        output$direction <- match.arg(
            .cdrgam_cli_scalar_character(value$direction, paste0(field, '.direction')),
            c('pos', 'neg')
        )
    } else if (form %in% .cdrgam_cli_test_peak_forms) output$direction <- 'pos'
    if (!is.null(value$null)) {
        if (identical(form, 'wood')) .cdrgam_cli_abort(paste0(
            field, '.null does not apply to wood, whose null is a zero functional'
        ))
        output$null <- .cdrgam_cli_test_number(value$null, paste0(field, '.null'))
    }
    output
}

.cdrgam_cli_validate_test <- function(value, path) {
    .cdrgam_cli_validate_schema(value, path)
    .cdrgam_cli_reject_definition_identity(value, 'test', path)
    .cdrgam_cli_check_keys(
        value,
        c('schema', 'model', 'terms', 'effects', 'estimands', 'null', 'uncertainty'),
        path, c('schema', 'model')
    )
    value$model <- .cdrgam_cli_name(value$model, paste0(path, ': model'))
    if (!is.null(value$terms)) {
        terms <- unlist(value$terms, use.names=FALSE)
        if (!is.character(terms) || !length(terms) || any(!nzchar(terms)) ||
                !is.null(names(value$terms))) {
            .cdrgam_cli_abort(paste0(path, ': terms must be a nonempty list of term labels'))
        }
        value$terms <- as.character(terms)
    }
    if (is.null(value$effects) != is.null(value$estimands)) {
        .cdrgam_cli_abort(paste0(
            path, ': effects and estimands must be given together'
        ))
    }
    if (is.null(value$terms) && is.null(value$estimands)) {
        .cdrgam_cli_abort(paste0(path, ': give terms, or effects and estimands'))
    }
    for (name in intersect(c('effects', 'estimands'), names(value))) {
        entries <- value[[name]]
        if (!is.list(entries) || !length(entries) || is.null(names(entries)) ||
                anyDuplicated(names(entries))) {
            .cdrgam_cli_abort(paste0(path, ': ', name, ' must be a nonempty named mapping'))
        }
        for (label in names(entries)) {
            .cdrgam_cli_test_label(label, paste0(path, ': ', name, ' name'))
        }
    }
    for (name in names(value$effects)) {
        value$effects[[name]] <- .cdrgam_cli_validate_test_effect(
            value$effects[[name]], paste0(path, ': effects.', name)
        )
    }
    for (name in names(value$estimands)) {
        value$estimands[[name]] <- .cdrgam_cli_validate_test_estimand(
            value$estimands[[name]], paste0(path, ': estimands.', name),
            names(value$effects)
        )
    }
    value$null <- .cdrgam_cli_test_number(
        .cdrgam_cli_null(value$null, 0), paste0(path, ': null')
    )
    uncertainty <- .cdrgam_cli_null(value$uncertainty, list())
    .cdrgam_cli_check_keys(
        uncertainty, c('level', 'unconditional', 'draws', 'seed'),
        paste0(path, ': uncertainty')
    )
    uncertainty$level <- .cdrgam_cli_null(uncertainty$level, 0.95)
    if (!is.numeric(uncertainty$level) || length(uncertainty$level) != 1L ||
            !is.finite(uncertainty$level) || uncertainty$level <= 0 ||
            uncertainty$level >= 1) {
        .cdrgam_cli_abort(paste0(path, ': uncertainty.level must lie between zero and one'))
    }
    uncertainty$unconditional <- .cdrgam_cli_null(uncertainty$unconditional, FALSE)
    if (!is.logical(uncertainty$unconditional) || length(uncertainty$unconditional) != 1L ||
            is.na(uncertainty$unconditional)) {
        .cdrgam_cli_abort(paste0(path, ': uncertainty.unconditional must be true or false'))
    }
    uncertainty$draws <- .cdrgam_cli_positive_integer(
        .cdrgam_cli_null(uncertainty$draws, .cdrgam_cli_test_default_draws),
        paste0(path, ': uncertainty.draws')
    )
    if (uncertainty$draws < 2L) {
        .cdrgam_cli_abort(paste0(path, ': uncertainty.draws must be at least two'))
    }
    uncertainty$seed <- .cdrgam_cli_positive_integer(
        .cdrgam_cli_null(uncertainty$seed, .cdrgam_cli_test_default_seed),
        paste0(path, ': uncertainty.seed')
    )
    value$uncertainty <- uncertainty
    attr(value, 'path') <- path
    value
}

# The draws and seed only set how the Monte Carlo parts are computed, so
# they are execution settings and do not enter the identity.
.cdrgam_cli_test_identity <- function(value, inputs) {
    scientific <- .cdrgam_cli_scientific_definition(value)
    scientific$uncertainty$draws <- NULL
    scientific$uncertainty$seed <- NULL
    resolved <- .cdrgam_cli_artifact_contract('test', scientific, inputs)
    list(identity=.cdrgam_cli_short_hash(resolved), resolved=resolved)
}

.cdrgam_cli_test_item <- function(definitions, name, fingerprints=NULL) {
    value <- definitions$tests[[name]]
    fit <- .cdrgam_cli_fit_item(definitions, value$model, fingerprints)
    identity <- .cdrgam_cli_test_identity(
        value, list(model=fit$resolved_identity$resolved)
    )
    key <- paste('test', definitions$project$project$id, name, identity$identity, sep=':')
    list(
        key=key, kind='test', project=definitions$project$project$name,
        name=name, model_name=value$model, identity=identity$identity,
        resolved_identity=identity, dependencies=fit$key,
        definition=value, fit=fit,
        output=.cdrgam_cli_path(definitions, 'test', name)
    )
}

# The effect_inference() specification of one test effect, with its axes and
# reference resolved from the value language for its single term.
.cdrgam_cli_test_effect_spec <- function(fit, name, effect, training, peaks) {
    labels <- .cdrgam_cli_select_terms(
        fit, training$catalog, effect$terms, training$impulses, training$responses
    )
    if (length(labels) != 1L) .cdrgam_cli_abort(paste0(
        'Test effect ', name, ' must select exactly one term; it selects ',
        paste(labels, collapse=', ')
    ))
    term <- .cdrgam_cli_catalog_row(training$catalog, match(labels, training$catalog$label))
    request <- .cdrgam_cli_term_request(
        fit, term, effect, training$impulses, training$responses, peaks,
        effect$composition
    )
    Filter(Negate(is.null), list(
        terms=labels, axes=request$axes, composition=effect$composition,
        grouping=effect$grouping, groups=effect$groups,
        reference=request$reference, n=effect$n
    ))
}

# A curve over lag: every axis but lag is constant and there is one group.
.cdrgam_cli_test_curve <- function(grid, estimand, effect) {
    groups <- unique(grid$group)
    others <- setdiff(names(grid), c(
        'lag', .cdrgam_cli_effect_columns, grep('_reference$', names(grid), value=TRUE)
    ))
    constant <- vapply(others, function(name) length(unique(grid[[name]])) <= 1L,
        logical(1))
    if (!('lag' %in% names(grid)) || length(unique(grid$lag)) < 2L ||
            length(groups) > 1L || !all(constant)) {
        .cdrgam_cli_abort(paste0(
            'Estimand ', estimand, ' needs effect ', effect,
            ' to vary over lag only, for one group'
        ))
    }
    invisible(TRUE)
}

# Linear weights of each linear estimand over its effects' rows.
.cdrgam_cli_test_weights <- function(estimand, label, grids) {
    if (identical(estimand$form, 'weights')) {
        for (name in names(estimand$weights)) {
            size <- nrow(grids[[name]])
            if (!(length(estimand$weights[[name]]) %in% c(1L, size))) {
                .cdrgam_cli_abort(paste0(
                    'Estimand ', label, ' gives ', length(estimand$weights[[name]]),
                    ' weights for effect ', name, ', which has ', size, ' rows'
                ))
            }
        }
        return(estimand$weights)
    }
    grid <- grids[[estimand$effect]]
    weights <- if (identical(estimand$form, 'mean')) {
        rep.int(1 / nrow(grid), nrow(grid))
    } else {
        .cdrgam_cli_test_curve(grid, label, estimand$effect)
        order <- order(grid$lag)
        lags <- grid$lag[order]
        steps <- diff(lags)
        trapezoid <- (c(steps, 0) + c(0, steps)) / 2
        weights <- numeric(nrow(grid))
        weights[order] <- trapezoid
        weights
    }
    stats::setNames(list(weights), estimand$effect)
}

.cdrgam_cli_test_columns <- c(
    'estimand', 'form', 'effects', 'edf', 'estimate', 'std_error', 'lower', 'upper',
    'null', 'statistic', 'df', 'residual_df', 'p_value', 'simultaneous_lower',
    'simultaneous_upper', 'adjusted_p_value', 'method'
)

.cdrgam_cli_test_rows <- function(result, estimands, family) {
    output <- as.data.frame(result)
    output$form <- vapply(estimands[output$estimand], `[[`, character(1), 'form')
    output$effects <- vapply(estimands[output$estimand], function(estimand) {
        paste(.cdrgam_cli_null(names(estimand$weights), estimand$effect), collapse=',')
    }, character(1))
    output$edf <- NA_real_
    output$df <- NA_real_
    output$residual_df <- NA_real_
    output$method <- paste0(output$method, ' (', family, ')')
    output[.cdrgam_cli_test_columns]
}

# Tests of native terms: parametric terms, from the fit summary's
# pTerms.table, then smooth terms, from its smooth table, selected by their
# summary() labels exactly, by * glob, or by re: regular expression.
.cdrgam_cli_test_term_rows <- function(fit, patterns) {
    summary <- .cdrgam_cli_core_summary(fit)
    empty <- function(count) as.data.frame(stats::setNames(
        rep(list(rep.int(NA, count)), length(.cdrgam_cli_test_columns)),
        .cdrgam_cli_test_columns
    ))
    rows <- list()
    residual_df <- .cdrgam_cli_null(summary$residual.df, NA_real_)
    parametric <- summary$pTerms.table
    if (!is.null(parametric) && nrow(parametric)) {
        chi_square <- 'Chi.sq' %in% colnames(parametric)
        output <- empty(nrow(parametric))
        output$estimand <- rownames(parametric)
        output$form <- 'term'
        output$statistic <- parametric[, if (chi_square) 'Chi.sq' else 'F']
        output$df <- parametric[, 'df']
        output$residual_df <- if (chi_square) NA_real_ else residual_df
        output$p_value <- parametric[, 'p-value']
        output$method <- 'Wald (parametric term)'
        rows[[length(rows) + 1L]] <- output
    }
    table <- summary$s.table
    if (!is.null(table) && nrow(table)) {
        status <- attr(table, 'test.status')
        if (is.null(status)) {
            status <- ifelse(
                is.finite(table[, 'p-value']), 'mgcv summary.gam', 'not computed by mgcv'
            )
        }
        chi_square <- 'Chi.sq' %in% colnames(table)
        output <- empty(nrow(table))
        output$estimand <- rownames(table)
        output$form <- 'term'
        output$edf <- table[, 'edf']
        output$statistic <- table[, if (chi_square) 'Chi.sq' else 'F']
        output$df <- table[, 'Ref.df']
        output$residual_df <- if (chi_square) NA_real_ else residual_df
        output$p_value <- table[, 'p-value']
        output$method <- unname(status)
        rows[[length(rows) + 1L]] <- output
    }
    if (!length(rows)) .cdrgam_cli_abort('The fitted model has no terms to test')
    available <- do.call(rbind, rows)
    labels <- .cdrgam_cli_match_names(patterns, available$estimand, 'test term')
    output <- available[available$estimand %in% labels, , drop=FALSE]
    rownames(output) <- NULL
    output
}

.cdrgam_cli_execute_test <- function(item, stage) {
    definition <- item$definition
    if (!is.null(definition$estimands) && !.cdrgam_cli_core_has_effect_inference()) {
        .cdrgam_cli_abort(paste(
            'Test estimands need a cdrgam release with effect_inference();',
            'update cdrgam'
        ))
    }
    fit <- readRDS(file.path(item$fit$output, 'fit.rds'))
    term_rows <- if (!is.null(definition$terms)) {
        .cdrgam_cli_test_term_rows(fit, definition$terms)
    }
    if (is.null(definition$estimands)) {
        utils::write.csv(term_rows, file.path(stage, 'tests.csv'), row.names=FALSE)
        .cdrgam_cli_write_yaml(list(terms=term_rows$estimand),
            file.path(stage, 'request.yml'))
        return(list(kind='test', rows=nrow(term_rows)))
    }
    training <- .cdrgam_cli_effect_training(fit, item$fit$dataset)
    peaks <- new.env()
    peaks$.catalog <- training$catalog
    specs <- lapply(names(definition$effects), function(name) {
        .cdrgam_cli_test_effect_spec(fit, name, definition$effects[[name]], training, peaks)
    })
    names(specs) <- names(definition$effects)
    uncertainty <- definition$uncertainty
    common <- list(
        fit, level=uncertainty$level, unconditional=uncertainty$unconditional,
        impulses=training$impulses, responses=training$responses,
        draws=uncertainty$draws, seed=uncertainty$seed
    )
    estimands <- definition$estimands
    forms <- vapply(estimands, `[[`, character(1), 'form')
    nulls <- vapply(estimands, function(estimand) {
        .cdrgam_cli_null(estimand$null, definition$null)
    }, numeric(1))
    used <- unique(unlist(lapply(estimands[forms != 'wood'], function(estimand) {
        .cdrgam_cli_null(names(estimand$weights), estimand$effect)
    })))
    # The effect grids, from one inference call over every effect an
    # estimand uses, give each effect's rows for the weights.
    grids <- if (length(used)) {
        attr(do.call(.cdrgam_cli_core_effect_inference, c(
            common, list(effects=specs[used])
        )), 'grids')
    } else list()
    rows <- list()
    linear <- names(estimands)[forms %in% .cdrgam_cli_test_linear_forms]
    if (length(linear)) {
        weights <- lapply(linear, function(label) {
            .cdrgam_cli_test_weights(estimands[[label]], label, grids)
        })
        names(weights) <- linear
        effects <- unique(unlist(lapply(weights, names)))
        result <- do.call(.cdrgam_cli_core_effect_inference, c(common, list(
            effects=specs[effects], estimand=weights, null=nulls[linear]
        )))
        rows[[length(rows) + 1L]] <- .cdrgam_cli_test_rows(result, estimands, 'linear')
        joint <- attr(result, 'joint_test')
    }
    peak <- names(estimands)[forms %in% .cdrgam_cli_test_peak_forms]
    if (length(peak)) {
        effects <- unique(vapply(estimands[peak], `[[`, character(1), 'effect'))
        for (label in peak) {
            .cdrgam_cli_test_curve(grids[[estimands[[label]]$effect]], label,
                estimands[[label]]$effect)
        }
        summary <- function(values, effect_grids) {
            vapply(peak, function(label) {
                estimand <- estimands[[label]]
                curve <- values[[estimand$effect]]
                index <- if (identical(estimand$direction, 'neg')) {
                    which.min(curve)
                } else which.max(curve)
                if (identical(estimand$form, 'peak_lag')) {
                    effect_grids[[estimand$effect]]$lag[[index]]
                } else curve[[index]]
            }, numeric(1))
        }
        result <- do.call(.cdrgam_cli_core_effect_inference, c(common, list(
            effects=specs[effects], estimand=summary, null=nulls[peak]
        )))
        rows[[length(rows) + 1L]] <- .cdrgam_cli_test_rows(result, estimands, 'peaks')
    }
    for (label in names(estimands)[forms == 'wood']) {
        spec <- specs[[estimands[[label]]$effect]]
        tested <- .cdrgam_cli_core_test_effect(
            fit, terms=spec$terms, composition=spec$composition,
            grouping=spec$grouping,
            groups=if (identical(spec$grouping, 'deviation')) NULL else spec$groups,
            impulses=training$impulses, responses=training$responses,
            unconditional=uncertainty$unconditional, method='wood'
        )
        tested <- as.data.frame(tested)
        output <- as.data.frame(stats::setNames(
            rep(list(rep.int(NA, nrow(tested))), length(.cdrgam_cli_test_columns)),
            .cdrgam_cli_test_columns
        ))
        output$estimand <- if (nrow(tested) > 1L) {
            paste0(label, '[', tested$group, ']')
        } else label
        output$form <- 'wood'
        output$effects <- estimands[[label]]$effect
        output$edf <- NA_real_
        output$statistic <- tested$statistic
        output$df <- tested$df
        output$residual_df <- tested$residual_df
        output$p_value <- tested$p_value
        output$method <- tested$method
        rows[[length(rows) + 1L]] <- output
    }
    table <- .cdrgam_cli_bind_tables(rows)
    table <- table[order(match(sub('\\[.*$', '', table$estimand), names(estimands))), ,
        drop=FALSE]
    if (!is.null(term_rows)) table <- .cdrgam_cli_bind_tables(list(term_rows, table))
    rownames(table) <- NULL
    utils::write.csv(table, file.path(stage, 'tests.csv'), row.names=FALSE)
    if (length(grids)) {
        grid_table <- .cdrgam_cli_bind_tables(lapply(names(grids), function(name) {
            transform(as.data.frame(grids[[name]]), effect=name)
        }))
        utils::write.csv(grid_table, file.path(stage, 'effects-grid.csv'), row.names=FALSE)
    }
    .cdrgam_cli_write_yaml(list(
        terms=if (!is.null(term_rows)) term_rows$estimand,
        effects=specs,
        joint_linear_test=if (length(linear)) joint else NULL,
        adjustment=paste(
            'Simultaneous intervals and adjusted p-values cover the linear',
            'estimands together and the peak estimands together'
        )
    ), file.path(stage, 'request.yml'))
    list(kind='test', rows=nrow(table))
}

# The term labels of each selected model, parametric then smooth, read from
# its latest fit artifact and printed as YAML lists that can be pasted under a
# test definition's terms. Parametric labels come from parametric-terms.csv;
# artifacts written before it existed give their coefficient names instead,
# which are the term labels of numeric terms but not of factors. Models
# without a fit artifact are reported as such. The artifact is read as it is,
# even if the model definition has changed since it was fitted.
.cdrgam_cli_model_terms <- function(project=NULL, models=NULL, checkout=NULL) {
    definitions <- .cdrgam_cli_read_definitions(
        project, check_sources=FALSE, checkout=checkout
    )
    if (!length(definitions$models)) .cdrgam_cli_abort('The project has no models')
    names <- .cdrgam_cli_match_names(models, names(definitions$models), 'model')
    read_terms <- function(path) {
        if (!file.exists(path) || !length(readLines(path, n=1L))) return(character())
        as.character(utils::read.csv(path, check.names=FALSE)$term)
    }
    output <- lapply(stats::setNames(names, names), function(name) {
        artifact <- .cdrgam_cli_path(definitions, 'model', name)
        smooth_path <- file.path(artifact, 'smooths.csv')
        if (!file.exists(smooth_path)) return(NULL)
        parametric_path <- file.path(artifact, 'parametric-terms.csv')
        parametric <- if (file.exists(parametric_path)) {
            read_terms(parametric_path)
        } else setdiff(read_terms(file.path(artifact, 'coefficients.csv')), '(Intercept)')
        c(parametric, read_terms(smooth_path))
    })
    for (name in names) {
        terms <- output[[name]]
        if (is.null(terms)) {
            cat(name, ': []  # not fitted\n', sep='')
        } else if (!length(terms)) {
            cat(name, ': []  # no terms\n', sep='')
        } else {
            cat(name, ':\n', paste0("  - '", gsub("'", "''", terms), "'\n"), sep='')
        }
    }
    invisible(output)
}
