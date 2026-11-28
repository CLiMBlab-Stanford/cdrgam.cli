# Pointwise peaks: `lag: peak-pointwise` holds each evaluation point at the
# lag where the evaluated effect, including any reference subtraction, is
# largest at that point's other axis values (`peak-pointwise-neg`: smallest).
# Unlike `peak`, which gives one lag per term from a standard one-SD
# contrast, the lag can change along a line. `peak-pointwise(LABEL)` finds
# the lags on another term instead, evaluated at the plotted points' values
# of that term's predictors.

.cdrgam_cli_pointwise_peaks <- c('peak-pointwise'='pos', 'peak-pointwise-neg'='neg')

# Columns of an effect grid that are not evaluation axes.
.cdrgam_cli_effect_columns <- c(
    'term_id', 'term', 'group', 'composition', 'grouping', 'estimate', 'se',
    'lower', 'upper', 'layer'
)

# A lag request that names a pointwise peak, as a string or as `at`: a list
# with `direction` ("pos" or "neg") and `label`, the term named in
# parentheses or NULL for the plotted term. NULL for any other request.
.cdrgam_cli_pointwise_lag <- function(request) {
    if (is.list(request) && !is.null(request$at)) request <- request$at
    if (!is.character(request) || length(request) != 1L || is.na(request)) return(NULL)
    match <- regmatches(request, regexec(
        '^\\s*(peak-pointwise(-neg)?)\\s*(\\((.*)\\))?\\s*$', request
    ))[[1L]]
    if (!length(match)) return(NULL)
    label <- if (nzchar(match[[4L]])) {
        trimws(gsub('^\\s*["\']|["\']\\s*$', '', match[[5L]]))
    } else NULL
    if (!is.null(label) && !nzchar(label)) return(NULL)
    list(direction=unname(.cdrgam_cli_pointwise_peaks[match[[2L]]]), label=label)
}

.cdrgam_cli_pointwise_direction <- function(request) {
    .cdrgam_cli_pointwise_lag(request)$direction
}

# The estimate_effect() arguments that locate pointwise peaks on the term
# `source` for a plotted term evaluated with `arguments`: the plotted
# request restricted to the source's axes, with the plotted reference for
# the source's predictors. The source's predictors must be predictors of
# the plotted term, so each plotted point has one source point.
.cdrgam_cli_pointwise_source <- function(arguments, term, source) {
    plotted <- .cdrgam_cli_term_axes(term)
    roles <- .cdrgam_cli_term_axes(source)
    if (!('lag' %in% roles$axes)) .cdrgam_cli_abort(paste0(
        'peak-pointwise(', source$label, '): ', source$label,
        ' has no lag axis, so it has no peak'
    ))
    if (!length(roles$predictors)) .cdrgam_cli_abort(paste0(
        'peak-pointwise(', source$label, '): ', source$label, ' has no ',
        'predictors, so its peak does not vary by point; use lag: peak(',
        source$label, ')'
    ))
    missing <- setdiff(roles$predictors, plotted$predictors)
    if (length(missing)) .cdrgam_cli_abort(paste0(
        'peak-pointwise(', source$label, ') on ', term$label, ': ',
        paste(missing, collapse=', '), ' is not a predictor of ', term$label,
        ', so its points cannot be matched'
    ))
    search <- arguments
    search$terms <- source$label
    search$axes <- arguments$axes[intersect(names(arguments$axes), roles$axes)]
    for (name in setdiff(roles$axes, c('lag', names(search$axes)))) {
        search$axes[[name]] <- 'mean'
    }
    if (!is.null(search$reference)) {
        search$reference <- search$reference[
            intersect(names(search$reference), roles$predictors)
        ]
        if (!length(search$reference)) search$reference <- NULL
    }
    list(label=source$label, arguments=search, axes=setdiff(roles$axes, 'lag'))
}

# Evaluate one term with lag at each point's own peak. `arguments` are the
# named estimate_effect() arguments other than the fit, for the term and
# without a lag axis request;
# `arguments$n` sets the lag grid searched. The peak at each point is found
# on the point estimates over that grid; estimates and standard errors are
# then evaluated at the chosen (point, lag) pairs. With `draws` positive,
# each posterior draw of the same grid gives its own peak lag at each point,
# and the central `level` interval of those lags is stored. With
# `simultaneous = TRUE`, `lower` and `upper` of the effect become a
# simultaneous band over each draw's effect at its own per-point peaks (see
# .cdrgam_cli_draw_band()). Returns the
# effect grid with attribute `pointwise_peaks`: one row per point, with its
# group, axis values, direction, peak `lag`, and `lower` and `upper`.
.cdrgam_cli_pointwise_effect <- function(
        fit, arguments, direction, draws=0L, level=0.95, seed=1L, simultaneous=FALSE,
        source=NULL
) {
    if (!is.null(source)) return(.cdrgam_cli_pointwise_source_effect(
        fit, arguments, direction, source, draws, level, seed, simultaneous
    ))
    if (!.cdrgam_cli_core_has_points()) .cdrgam_cli_abort(paste(
        'peak-pointwise needs a cdrgam release whose estimate_effect() accepts',
        'points; update cdrgam'
    ))
    n <- as.integer(.cdrgam_cli_null(arguments$n, 200L))
    search <- arguments
    search$axes$lag <- list(grid='fitted', n=n)
    surface <- as.data.frame(do.call(
        .cdrgam_cli_core_estimate_effect, c(list(fit), search, list(se=FALSE))
    ))
    axes <- setdiff(names(surface), c(
        'lag', .cdrgam_cli_effect_columns, grep('_reference$', names(surface), value=TRUE)
    ))
    pick <- if (identical(direction, 'neg')) which.min else which.max
    keys <- function(grid) {
        do.call(paste, c(unname(as.list(grid[c('group', axes)])), sep='\r'))
    }
    key <- keys(surface)
    order <- unique(key)
    rows <- split(seq_len(nrow(surface)), factor(key, levels=order))
    best <- vapply(rows, function(index) index[[pick(surface$estimate[index])]], integer(1))
    chosen <- surface[best, c('group', axes, 'lag'), drop=FALSE]
    rownames(chosen) <- NULL
    tables <- lapply(unique(chosen$group), function(group) {
        keep <- if (is.na(group)) is.na(chosen$group) else chosen$group %in% group
        request <- arguments
        request$axes <- list()
        if (!is.na(group)) request$groups <- group
        do.call(.cdrgam_cli_core_estimate_effect, c(list(fit), request, list(
            points=chosen[keep, c(axes, 'lag'), drop=FALSE], se=TRUE, level=level
        )))
    })
    value <- .cdrgam_cli_bind_tables(tables)
    peaks <- data.frame(
        term=arguments$terms, chosen[c('group', axes)], direction=direction,
        lag=chosen$lag, lower=NA_real_, upper=NA_real_,
        stringsAsFactors=FALSE, check.names=FALSE
    )
    if (simultaneous && draws < 2L) .cdrgam_cli_abort(paste(
        'Simultaneous intervals at a peak need at least two draws;',
        'raise uncertainty.peak_draws'
    ))
    if (simultaneous && !.cdrgam_cli_core_has_simulate_effect()) .cdrgam_cli_abort(paste(
        'Simultaneous intervals at a peak need a cdrgam release with',
        'simulate_effect(); update cdrgam'
    ))
    if (draws > 0L && .cdrgam_cli_core_has_simulate_effect()) {
        if (as.numeric(nrow(surface)) * draws > 1e8) .cdrgam_cli_abort(paste0(
            'peak-pointwise intervals would draw ', nrow(surface), ' grid points ',
            draws, ' times; lower n or uncertainty.peak_draws'
        ))
        simulated <- do.call(.cdrgam_cli_core_simulate_effect, c(
            list(fit), search[setdiff(names(search), 'level')],
            list(draws=as.integer(draws), seed=seed)
        ))
        grid <- as.data.frame(simulated$grid)
        rows <- split(seq_len(nrow(grid)), factor(keys(grid), levels=order))
        bounds <- vapply(rows, function(index) {
            lags <- grid$lag[index][apply(
                simulated$draws[index, , drop=FALSE], 2L, pick
            )]
            stats::quantile(
                lags, c((1 - level) / 2, (1 + level) / 2), names=FALSE, type=1
            )
        }, numeric(2))
        peaks$lower <- bounds[1L, ]
        peaks$upper <- bounds[2L, ]
        if (simultaneous) {
            extreme <- if (identical(direction, 'neg')) min else max
            effect <- t(vapply(rows, function(index) {
                apply(simulated$draws[index, , drop=FALSE], 2L, extreme)
            }, numeric(ncol(simulated$draws))))
            value <- as.data.frame(value)
            matched <- match(keys(value), order)
            band <- .cdrgam_cli_draw_band(
                value$estimate, effect[matched, , drop=FALSE],
                .cdrgam_cli_band_groups(value), level
            )
            value$lower <- band$lower
            value$upper <- band$upper
            value$critical_value <- band$critical_value
        }
    }
    attr(value, 'pointwise_peaks') <- peaks
    value
}

# Evaluate one term with lag at each point's peak of another term, `source`
# from .cdrgam_cli_pointwise_source(). Peaks are found on the source's
# surface at the plotted points' values of its predictors; a plotted point
# takes the lag of the source point it matches. With `draws` positive, one
# coefficient sample serves both terms: each draw re-finds the source's peak
# lags, which give the peak intervals, and with `simultaneous = TRUE` the
# plotted effect of the same draw at those lags gives a simultaneous band
# (see .cdrgam_cli_draw_band()).
.cdrgam_cli_pointwise_source_effect <- function(
        fit, arguments, direction, source, draws=0L, level=0.95, seed=1L,
        simultaneous=FALSE
) {
    if (!.cdrgam_cli_core_has_points()) .cdrgam_cli_abort(paste(
        'peak-pointwise needs a cdrgam release whose estimate_effect() accepts',
        'points; update cdrgam'
    ))
    if (simultaneous && draws < 2L) .cdrgam_cli_abort(paste(
        'Simultaneous intervals at a peak need at least two draws;',
        'raise uncertainty.peak_draws'
    ))
    if (simultaneous && !.cdrgam_cli_core_has_shared_draws()) .cdrgam_cli_abort(paste(
        'Simultaneous intervals at peak-pointwise(', source$label, ') need a',
        'cdrgam release with sample_effect_coefficients(); update cdrgam'
    ))
    n <- as.integer(.cdrgam_cli_null(arguments$n, 200L))
    pick <- if (identical(direction, 'neg')) which.min else which.max
    search <- source$arguments
    search$axes$lag <- list(grid='fitted', n=n)
    surface <- as.data.frame(do.call(
        .cdrgam_cli_core_estimate_effect, c(list(fit), search, list(se=FALSE))
    ))
    # The source is matched by value on its axes, and by group when it has
    # groups of its own.
    grouped <- any(!is.na(surface$group))
    keys <- function(grid) {
        columns <- c(if (grouped) 'group', source$axes)
        do.call(paste, c(unname(as.list(grid[columns])), sep='\r'))
    }
    order <- unique(keys(surface))
    rows <- split(seq_len(nrow(surface)), factor(keys(surface), levels=order))
    lags <- vapply(rows, function(index) {
        surface$lag[index][[pick(surface$estimate[index])]]
    }, numeric(1))
    # The plotted points: the plotted term's grid without its lag axis.
    probe <- arguments
    probe$axes$lag <- list(grid='fitted', n=2L)
    plotted <- as.data.frame(do.call(
        .cdrgam_cli_core_estimate_effect, c(list(fit), probe, list(se=FALSE))
    ))
    axes <- setdiff(names(plotted), c(
        'lag', .cdrgam_cli_effect_columns, grep('_reference$', names(plotted), value=TRUE)
    ))
    points <- unique(plotted[c('group', axes)])
    rownames(points) <- NULL
    matched <- match(keys(points), order)
    if (anyNA(matched)) .cdrgam_cli_abort(paste0(
        'peak-pointwise(', source$label, ') on ', arguments$terms,
        ': some plotted points have no matching point of ', source$label
    ))
    points$lag <- lags[matched]
    by_group <- function(table, evaluate) {
        lapply(unique(table$group), function(group) {
            keep <- if (is.na(group)) is.na(table$group) else table$group %in% group
            request <- arguments
            request$axes <- list()
            if (!is.na(group)) request$groups <- group
            list(keep=which(keep), value=evaluate(request, table[keep, , drop=FALSE]))
        })
    }
    tables <- by_group(points, function(request, table) {
        do.call(.cdrgam_cli_core_estimate_effect, c(list(fit), request, list(
            points=table[c(axes, 'lag')], se=TRUE, level=level
        )))
    })
    value <- .cdrgam_cli_bind_tables(lapply(tables, `[[`, 'value'))
    bounds <- matrix(NA_real_, 2L, length(order))
    if (draws > 0L && .cdrgam_cli_core_has_simulate_effect()) {
        if (as.numeric(nrow(surface)) * draws > 1e8) .cdrgam_cli_abort(paste0(
            'peak-pointwise intervals would draw ', nrow(surface), ' grid points ',
            draws, ' times; lower n or uncertainty.peak_draws'
        ))
        shared <- .cdrgam_cli_core_has_shared_draws()
        sample <- if (shared) .cdrgam_cli_peak_band_sample(
            fit, c(arguments$terms, source$label),
            .cdrgam_cli_null(arguments$composition, 'term'),
            .cdrgam_cli_null(arguments$grouping, 'population'),
            arguments$impulses, arguments$responses,
            isTRUE(arguments$unconditional), draws, seed
        )
        simulated <- do.call(.cdrgam_cli_core_simulate_effect, c(
            list(fit), search[setdiff(names(search), 'level')],
            if (shared) list(coefficients=sample) else list(
                draws=as.integer(draws), seed=seed
            )
        ))
        grid <- as.data.frame(simulated$grid)
        index <- split(seq_len(nrow(grid)), factor(keys(grid), levels=order))
        # The source's peak lag at each source point in each draw.
        drawn <- t(vapply(index, function(rows) {
            grid$lag[rows][apply(simulated$draws[rows, , drop=FALSE], 2L, pick)]
        }, numeric(ncol(simulated$draws))))
        bounds <- apply(drawn, 1L, stats::quantile,
            probs=c((1 - level) / 2, (1 + level) / 2), names=FALSE, type=1)
        if (simultaneous) {
            # Each draw's plotted effect at that draw's source peak lags,
            # evaluated once per distinct (point, lag) pair.
            effect <- matrix(NA_real_, nrow(points), ncol(drawn))
            for (table in by_group(points, function(request, table) request)) {
                rows <- table$keep
                pairs <- unique(data.frame(
                    point=rep(rows, ncol(drawn)),
                    lag=as.vector(drawn[matched[rows], , drop=FALSE])
                ))
                evaluated <- do.call(.cdrgam_cli_core_simulate_effect, c(
                    list(fit), table$value[setdiff(names(table$value), 'level')],
                    list(
                        points=cbind(points[pairs$point, axes, drop=FALSE], lag=pairs$lag),
                        coefficients=sample
                    )
                ))
                pair_key <- paste(pairs$point, pairs$lag, sep='\r')
                for (draw in seq_len(ncol(drawn))) {
                    at <- match(
                        paste(rows, drawn[matched[rows], draw], sep='\r'), pair_key
                    )
                    effect[rows, draw] <- evaluated$draws[cbind(at, draw)]
                }
            }
            # Estimates are bound group by group, so draws follow that order.
            effect <- effect[unlist(lapply(tables, `[[`, 'keep')), , drop=FALSE]
            value <- as.data.frame(value)
            band <- .cdrgam_cli_draw_band(
                value$estimate, effect, .cdrgam_cli_band_groups(value), level
            )
            value$lower <- band$lower
            value$upper <- band$upper
            value$critical_value <- band$critical_value
        }
    }
    attr(value, 'pointwise_peaks') <- data.frame(
        term=arguments$terms, source=source$label, points[c('group', axes)],
        direction=direction, lag=points$lag, lower=bounds[1L, matched],
        upper=bounds[2L, matched], stringsAsFactors=FALSE, check.names=FALSE
    )
    value
}
