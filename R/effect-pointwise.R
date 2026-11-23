# Pointwise peaks: `lag: peak-pointwise` holds each evaluation point at the
# lag where the evaluated effect, including any reference subtraction, is
# largest at that point's other axis values (`peak-pointwise-neg`: smallest).
# Unlike `peak`, which gives one lag per term from a standard one-SD
# contrast, the lag can change along a line.

.cdrgam_cli_pointwise_peaks <- c('peak-pointwise'='pos', 'peak-pointwise-neg'='neg')

# Columns of an effect grid that are not evaluation axes.
.cdrgam_cli_effect_columns <- c(
    'term_id', 'term', 'group', 'composition', 'grouping', 'estimate', 'se',
    'lower', 'upper', 'layer'
)

# The direction ("pos" or "neg") of a lag request that names a pointwise
# peak, as a string or as `at`, or NULL for any other request.
.cdrgam_cli_pointwise_direction <- function(request) {
    if (is.list(request) && !is.null(request$at)) request <- request$at
    if (!is.character(request) || length(request) != 1L || is.na(request)) return(NULL)
    direction <- .cdrgam_cli_pointwise_peaks[trimws(request)]
    if (is.na(direction)) NULL else unname(direction)
}

# Evaluate one term with lag at each point's own peak. `arguments` are the
# named estimate_effect() arguments other than the fit, for the term and
# without a lag axis request;
# `arguments$n` sets the lag grid searched. The peak at each point is found
# on the point estimates over that grid; estimates and standard errors are
# then evaluated at the chosen (point, lag) pairs. With `draws` positive,
# each posterior draw of the same grid gives its own peak lag at each point,
# and the central `level` interval of those lags is stored. Returns the
# effect grid with attribute `pointwise_peaks`: one row per point, with its
# group, axis values, direction, peak `lag`, and `lower` and `upper`.
.cdrgam_cli_pointwise_effect <- function(
        fit, arguments, direction, draws=0L, level=0.95, seed=1L
) {
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
    }
    attr(value, 'pointwise_peaks') <- peaks
    value
}
