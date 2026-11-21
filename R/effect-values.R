# Axis values written as arithmetic over an axis's training summaries, such
# as "mean + 2 * sd". Lag values may also name the lag at which a term's
# response peaks. Expressions are checked when a definition is read and
# resolved for each term when an effect is evaluated, because summaries and
# peaks belong to the fitted term.

.cdrgam_cli_value_names <- c('mean', 'sd', 'median')
.cdrgam_cli_peak_names <- c('peak_pos', 'peak_neg')
# Posterior draws per peak when a query does not set uncertainty.peak_draws.
.cdrgam_cli_peak_draws <- 1000L

.cdrgam_cli_value_help <- function(lag) paste0(
    'use numbers, mean, sd, median',
    if (lag) ', peak, peak-pos, peak-neg, peak(TERM)' else '',
    ', +, -, *, /, and parentheses'
)

# Rewrite the peak names into R syntax: `peak` and `peak-pos` become
# `peak_pos`, `peak-neg` becomes `peak_neg`, and a following parenthesized
# term label, quoted or not, becomes a string argument. Labels may contain
# balanced parentheses, as in `peak(s(lag, wlen))`.
.cdrgam_cli_value_rewrite_peaks <- function(text, field) {
    pattern <- '(?<![A-Za-z0-9_.])peak(-pos|-neg)?(?![A-Za-z0-9_.])'
    output <- ''
    rest <- text
    repeat {
        match <- regexpr(pattern, rest, perl=TRUE)
        if (match < 0L) break
        start <- as.integer(match)
        length <- attr(match, 'match.length')
        token <- substr(rest, start, start + length - 1L)
        name <- if (identical(token, 'peak-neg')) 'peak_neg' else 'peak_pos'
        output <- paste0(output, substr(rest, 1L, start - 1L), name)
        rest <- substring(rest, start + length)
        opening <- regexpr('^\\s*\\(', rest, perl=TRUE)
        if (opening < 0L) next
        characters <- strsplit(rest, '')[[1L]]
        first <- attr(opening, 'match.length')
        depth <- 0L
        close <- NA_integer_
        for (index in seq.int(first, length(characters))) {
            if (characters[[index]] == '(') depth <- depth + 1L
            if (characters[[index]] == ')') depth <- depth - 1L
            if (depth == 0L) {
                close <- index
                break
            }
        }
        if (is.na(close)) .cdrgam_cli_abort(paste0(
            field, ': ', text, ' has an unclosed parenthesis after ', token
        ))
        label <- trimws(paste(characters[seq_len(close - 1L)[-seq_len(first)]],
            collapse=''))
        if (grepl('^([\'"]).*\\1$', label)) {
            label <- substr(label, 2L, nchar(label) - 1L)
        }
        if (!nzchar(trimws(label))) .cdrgam_cli_abort(paste0(
            field, ': ', text, ' gives ', token, ' an empty term label'
        ))
        output <- paste0(output, '(', encodeString(label, quote='"'), ')')
        rest <- substring(rest, close + 1L)
    }
    paste0(output, rest)
}

# Parse an expression into R language, rejecting anything outside the value
# grammar. `peak` is an alias of `peak-pos`; `peak(LABEL)` names the peak of
# another term, which is checked against the fit only at evaluation.
.cdrgam_cli_value_parse <- function(text, field, lag=FALSE) {
    if (!is.character(text) || length(text) != 1L || is.na(text) ||
            !nzchar(trimws(text))) {
        .cdrgam_cli_abort(paste0(field, ' must be a number or a value expression'))
    }
    source <- .cdrgam_cli_value_rewrite_peaks(text, field)
    parsed <- tryCatch(
        parse(text=source, keep.source=FALSE), error=function(error) NULL
    )
    if (length(parsed) != 1L) .cdrgam_cli_abort(paste0(
        field, ' is not a valid value expression: ', text, '; ',
        .cdrgam_cli_value_help(lag)
    ))
    allowed <- c(.cdrgam_cli_value_names, if (lag) .cdrgam_cli_peak_names)
    check <- function(node) {
        if (is.numeric(node) && length(node) == 1L && is.finite(node)) return()
        if (is.symbol(node)) {
            name <- as.character(node)
            if (name %in% allowed) return()
            if (name %in% .cdrgam_cli_peak_names) .cdrgam_cli_abort(paste0(
                field, ': ', text, ' names a peak, which only lag values may use'
            ))
            shown <- name
            .cdrgam_cli_abort(paste0(
                field, ' uses unknown name ', shown, ' in ', text, '; ',
                .cdrgam_cli_value_help(lag)
            ))
        }
        if (is.call(node) && is.symbol(node[[1L]])) {
            operator <- as.character(node[[1L]])
            if (operator %in% .cdrgam_cli_peak_names) {
                if (!lag) .cdrgam_cli_abort(paste0(
                    field, ': ', text, ' names a peak, which only lag values may use'
                ))
                if (length(node) == 2L && is.character(node[[2L]])) return()
            }
            arity <- length(node) - 1L
            valid <- (operator %in% c('+', '-') && arity %in% 1:2) ||
                (operator %in% c('*', '/') && arity == 2L) ||
                (operator == '(' && arity == 1L)
            if (valid) {
                for (argument in as.list(node)[-1L]) check(argument)
                return()
            }
        }
        .cdrgam_cli_abort(paste0(
            field, ' is not a valid value expression: ', text, '; ',
            .cdrgam_cli_value_help(lag)
        ))
    }
    check(parsed[[1L]])
    parsed[[1L]]
}

.cdrgam_cli_value_evaluate <- function(node, lookup) {
    if (is.numeric(node)) return(as.numeric(node))
    if (is.symbol(node)) return(lookup(as.character(node)))
    operator <- as.character(node[[1L]])
    if (operator %in% .cdrgam_cli_peak_names) return(lookup(operator, node[[2L]]))
    values <- lapply(as.list(node)[-1L], .cdrgam_cli_value_evaluate, lookup=lookup)
    if (operator == '(') return(values[[1L]])
    if (length(values) == 1L) {
        return(if (operator == '-') -values[[1L]] else values[[1L]])
    }
    switch(
        operator,
        '+'=values[[1L]] + values[[2L]],
        '-'=values[[1L]] - values[[2L]],
        '*'=values[[1L]] * values[[2L]],
        '/'=values[[1L]] / values[[2L]]
    )
}

# Mean, standard deviation, and median of one axis from a core axis summary
# (mean, sd, and named quantiles). Missing statistics are NA.
.cdrgam_cli_axis_statistics <- function(summary) {
    number <- function(value) {
        value <- suppressWarnings(as.numeric(value))
        if (length(value) == 1L && is.finite(value)) value else NA_real_
    }
    if (is.null(summary)) return(list(mean=NA_real_, sd=NA_real_, median=NA_real_))
    median <- number(summary$median)
    quantiles <- as.numeric(summary$quantiles)
    probabilities <- suppressWarnings(as.numeric(names(summary$quantiles)))
    if (!is.finite(median) && length(quantiles) && all(is.finite(quantiles)) &&
            length(probabilities) == length(quantiles) && all(is.finite(probabilities))) {
        median <- stats::approx(
            probabilities, quantiles, 0.5, rule=2, ties='ordered'
        )$y
    }
    list(mean=number(summary$mean), sd=number(summary$sd), median=median)
}

# Resolve a number or expression for axis `axis`. `statistics` comes from
# .cdrgam_cli_axis_statistics(); `peak` is a function of a direction ("pos"
# or "neg") and an optional term label returning a lag, used only on the lag
# axis.
.cdrgam_cli_axis_value <- function(value, axis, statistics, peak=NULL, field=axis) {
    if (is.numeric(value)) return(as.numeric(value))
    lag <- identical(axis, 'lag')
    node <- .cdrgam_cli_value_parse(value, field, lag=lag)
    result <- .cdrgam_cli_value_evaluate(node, function(name, label=NULL) {
        if (name %in% .cdrgam_cli_peak_names) {
            if (is.null(peak)) .cdrgam_cli_abort(paste0(
                field, ': no fitted response is available to locate ', value
            ))
            direction <- sub('^peak_', '', name)
            return(if (is.null(label)) peak(direction) else peak(direction, label))
        }
        number <- statistics[[name]]
        if (is.null(number) || !is.finite(number)) .cdrgam_cli_abort(paste0(
            field, ': axis ', axis, ' has no stored training ', name,
            '; give a number instead'
        ))
        number
    })
    if (length(result) != 1L || !is.finite(result)) .cdrgam_cli_abort(paste0(
        field, ': ', value, ' does not resolve to a finite number'
    ))
    result
}

# The axis roles of one effect-catalog row: `predictors` are predictor axes,
# `time` the remaining non-lag axes.
.cdrgam_cli_term_axes <- function(row) {
    axes <- as.character(row$axes)
    predictors <- intersect(as.character(row$predictors), axes)
    list(
        axes=axes, predictors=predictors,
        time=setdiff(axes, c('lag', predictors))
    )
}

# A catalog row as a plain list.
.cdrgam_cli_catalog_row <- function(catalog, index) {
    list(
        label=catalog$label[[index]],
        axes=catalog$axes[[index]],
        predictors=catalog$predictors[[index]],
        summaries=catalog$axis_summaries[[index]]
    )
}

# The request whose lag profile locates a term's peak: the term alone with
# each of its predictors one training standard deviation above the mean,
# relative to the term at the means; for irf(1) it is irf(1) itself. Other
# axes are held at their training means.
.cdrgam_cli_peak_request <- function(term, n) {
    roles <- .cdrgam_cli_term_axes(term)
    if (!('lag' %in% roles$axes)) .cdrgam_cli_abort(paste0(
        'Term ', term$label, ' has no lag axis, so it has no peak'
    ))
    statistic <- function(name, field) {
        value <- .cdrgam_cli_axis_statistics(term$summaries[[name]])[[field]]
        if (!is.finite(value)) .cdrgam_cli_abort(paste0(
            'The peak of ', term$label, ' needs the training ', field,
            ' of ', name, ', which is not stored'
        ))
        value
    }
    axes <- list(lag=list(grid='fitted', n=as.integer(n)))
    for (name in roles$predictors) {
        axes[[name]] <- statistic(name, 'mean') + statistic(name, 'sd')
    }
    for (name in roles$time) axes[[name]] <- statistic(name, 'mean')
    reference <- if (length(roles$predictors)) list(reference=stats::setNames(
        lapply(roles$predictors, statistic, field='mean'), roles$predictors
    )) else list()
    c(list(
        terms=term$label, axes=axes, composition='term', grouping='population',
        n=as.integer(n)
    ), reference)
}

.cdrgam_cli_peak_index <- function(values, direction) {
    if (identical(direction, 'neg')) which.min(values) else which.max(values)
}

# The lag at which a term's fitted response, as defined by
# .cdrgam_cli_peak_request(), is largest (`direction = "pos"`) or smallest
# ("neg") over the fitted lag domain.
.cdrgam_cli_effect_peak <- function(
        fit, term, direction, impulses=NULL, responses=NULL, n=200L
) {
    value <- do.call(.cdrgam_cli_core_estimate_effect, c(
        list(fit, impulses=impulses, responses=responses, se=FALSE),
        .cdrgam_cli_peak_request(term, n)
    ))
    value$lag[[.cdrgam_cli_peak_index(value$estimate, direction)]]
}

# Posterior draws of the same peak: the peak lag of each simulated response
# curve. NULL when the installed core cannot simulate effects. Draws are
# limited to the lag grid, so their resolution is the grid spacing.
.cdrgam_cli_effect_peak_draws <- function(
        fit, term, direction, impulses=NULL, responses=NULL, n=200L,
        draws=1000L, seed=1L, unconditional=FALSE
) {
    if (!.cdrgam_cli_core_has_simulate_effect()) return(NULL)
    value <- do.call(.cdrgam_cli_core_simulate_effect, c(
        list(
            fit, impulses=impulses, responses=responses, draws=as.integer(draws),
            seed=seed, unconditional=unconditional
        ),
        .cdrgam_cli_peak_request(term, n)
    ))
    lags <- value$grid$lag
    lags[apply(value$draws, 2L, .cdrgam_cli_peak_index, direction=direction)]
}

# A peak lookup for `term`: a function of a direction and an optional term
# label, where NULL means `term` itself. Peaks are memoized by term and
# direction in `cache`, an environment that callers may share across the
# terms of one evaluation so that a peak borrowed by many terms is located
# once. Other terms are found in the fit's effect catalog.
.cdrgam_cli_peak_function <- function(
        fit, term, impulses=NULL, responses=NULL, n=200L, cache=new.env()
) {
    function(direction, label=NULL) {
        label <- .cdrgam_cli_null(label, term$label)
        key <- paste0(direction, '\r', label)
        if (is.null(cache[[key]])) {
            source <- if (identical(label, term$label)) term else {
                if (is.null(cache$.catalog)) {
                    cache$.catalog <- .cdrgam_cli_core_effect_catalog(
                        fit, impulses, responses
                    )
                }
                catalog <- cache$.catalog
                index <- match(label, catalog$label)
                if (is.na(index)) {
                    lagged <- catalog$label[vapply(
                        catalog$axes, function(axes) 'lag' %in% axes, logical(1)
                    )]
                    .cdrgam_cli_abort(paste0(
                        'peak(', label, '): the fit has no term ', label,
                        '; terms with a lag axis: ', paste(lagged, collapse=', ')
                    ))
                }
                .cdrgam_cli_catalog_row(catalog, index)
            }
            cache[[key]] <- .cdrgam_cli_effect_peak(
                fit, source, direction, impulses, responses, n
            )
        }
        used <- data.frame(
            term=term$label, source=label, direction=direction, lag=cache[[key]],
            stringsAsFactors=FALSE
        )
        cache$.used <- unique(rbind(cache$.used, used))
        cache[[key]]
    }
}

# The peaks that values resolved through `cache` used: one row per
# evaluated term, source term, and direction, with the point estimate `lag`.
# When `draws` is positive and the core can simulate effects, `lower` and
# `upper` give the central `level` interval of the posterior draws, and
# attribute `draws` holds them as a long table with columns source,
# direction, draw, and lag. Each source peak is simulated once, with the
# same seed, so lines that share a peak share its draws.
.cdrgam_cli_peak_summary <- function(
        fit, cache, impulses=NULL, responses=NULL, n=200L, draws=1000L,
        level=0.95, unconditional=FALSE, seed=1L
) {
    used <- cache$.used
    if (is.null(used) || !nrow(used)) return(NULL)
    rownames(used) <- NULL
    used$lower <- NA_real_
    used$upper <- NA_real_
    samples <- list()
    if (draws > 0L && .cdrgam_cli_core_has_simulate_effect()) {
        catalog <- cache$.catalog
        if (is.null(catalog)) {
            catalog <- .cdrgam_cli_core_effect_catalog(fit, impulses, responses)
        }
        sources <- unique(used[c('source', 'direction')])
        for (index in seq_len(nrow(sources))) {
            source <- sources$source[[index]]
            direction <- sources$direction[[index]]
            term <- .cdrgam_cli_catalog_row(catalog, match(source, catalog$label))
            lags <- .cdrgam_cli_effect_peak_draws(
                fit, term, direction, impulses, responses, n, draws, seed,
                unconditional
            )
            rows <- used$source == source & used$direction == direction
            bounds <- stats::quantile(
                lags, c((1 - level) / 2, (1 + level) / 2), names=FALSE, type=1
            )
            used$lower[rows] <- bounds[[1L]]
            used$upper[rows] <- bounds[[2L]]
            samples[[index]] <- data.frame(
                source=source, direction=direction, draw=seq_along(lags), lag=lags,
                stringsAsFactors=FALSE
            )
        }
    }
    attr(used, 'draws') <- if (length(samples)) do.call(rbind, samples) else NULL
    used
}

# Convert one validated axis request for one term into a form the core
# accepts: expressions become numbers and ranges become values. Other forms
# pass through unchanged. NULL leaves the axis at the core default.
.cdrgam_cli_resolve_axis_request <- function(request, axis, term, peak, n, field) {
    if (is.null(request) || is.numeric(request)) return(request)
    statistics <- .cdrgam_cli_axis_statistics(term$summaries[[axis]])
    resolve <- function(value, suffix='') .cdrgam_cli_axis_value(
        value, axis, statistics, peak, paste0(field, suffix)
    )
    if (is.character(request)) return(resolve(request))
    if (is.character(request$at)) {
        request$at <- resolve(request$at, '.at')
    }
    if (is.character(request$values)) {
        request$values <- vapply(
            request$values, resolve, numeric(1), suffix='.values', USE.NAMES=FALSE
        )
    }
    if (!is.null(request$range)) {
        limits <- vapply(
            request$range, resolve, numeric(1), suffix='.range', USE.NAMES=FALSE
        )
        if (limits[[1L]] == limits[[2L]]) .cdrgam_cli_abort(paste0(
            field, '.range resolves to a single value for ', term$label
        ))
        request <- list(values=seq(
            limits[[1L]], limits[[2L]],
            length.out=as.integer(.cdrgam_cli_null(request$n, n))
        ))
    }
    request
}

# The axis and reference requests that one query makes of one term, in the
# form passed to estimate_effect(). Predictor requests are flattened by name,
# so the "*" wildcard is resolved for each of the term's own predictors.
# A wildcard reference applies to every predictor of the term that has no
# explicit reference; a term without predictors gets none.
.cdrgam_cli_term_request <- function(
        fit, term, query, impulses=NULL, responses=NULL, peaks=new.env()
) {
    roles <- .cdrgam_cli_term_axes(term)
    n <- .cdrgam_cli_null(query$n, 200L)
    peak <- .cdrgam_cli_peak_function(fit, term, impulses, responses, n, peaks)
    requests <- list()
    for (name in roles$axes) {
        request <- if (identical(name, 'lag')) {
            query$axes$lag
        } else if (name %in% roles$predictors) {
            .cdrgam_cli_null(
                query$axes$predictors[[name]], query$axes$predictors[['*']]
            )
        } else query$axes$time
        # CDR default: hold unspecified non-lag axes at their training mean.
        if (is.null(request) && !identical(name, 'lag')) request <- 'mean'
        field <- paste0('query.axes.', if (name %in% roles$predictors) {
            paste0('predictors.', name)
        } else if (identical(name, 'lag')) 'lag' else 'time')
        resolved <- .cdrgam_cli_resolve_axis_request(
            request, name, term, peak, n, paste0(term$label, ': ', field)
        )
        if (!is.null(resolved)) requests[[name]] <- resolved
    }
    reference <- NULL
    if (length(query$reference)) {
        entries <- query$reference
        wildcard <- entries[['*']]
        entries[['*']] <- NULL
        if (!is.null(wildcard)) {
            for (name in setdiff(roles$predictors, names(entries))) {
                entries[[name]] <- wildcard
            }
        }
        reference <- list()
        for (name in names(entries)) {
            value <- entries[[name]]
            reference[[name]] <- if (identical(value, 'default')) value else {
                .cdrgam_cli_resolve_axis_request(
                    value, name, term, peak, n,
                    paste0(term$label, ': query.reference.', name)
                )
            }
        }
        if (!length(reference)) reference <- NULL
    }
    list(axes=requests, reference=reference)
}

# Labels of the terms a query selects, in fitted order. Core selector fields
# are passed to the core; `interacting` is applied here from the catalog:
# FALSE keeps terms with at most one predictor and TRUE keeps the rest.
.cdrgam_cli_select_terms <- function(fit, catalog, terms, impulses=NULL, responses=NULL) {
    interacting <- if (is.list(terms)) terms$interacting else NULL
    core <- terms
    if (is.list(core)) {
        core$interacting <- NULL
        if (!length(core)) core <- NULL
    }
    labels <- if (is.null(core)) catalog$label else {
        probe <- .cdrgam_cli_core_estimate_effect(
            fit, terms=core, impulses=impulses, responses=responses,
            se=FALSE, n=2L
        )
        catalog$label[catalog$label %in% unique(probe$term)]
    }
    if (!is.null(interacting)) {
        counts <- vapply(catalog$predictors, length, integer(1))
        keep <- catalog$label[if (isTRUE(interacting)) counts > 1L else counts <= 1L]
        labels <- intersect(labels, keep)
    }
    if (!length(labels)) .cdrgam_cli_abort('The effect term selector matched nothing')
    labels
}
