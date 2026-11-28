# Simultaneous bands for lines whose values use peaks. A peak lag is itself
# estimated, so the band draws the effect from the coefficient posterior and
# re-locates each peak on every draw before evaluating the effect at the
# values those peaks give. The band then covers every point of a term and
# group jointly, including the uncertainty about where each peak lies.
# Lines without peaks keep the analytic band of
# .cdrgam_cli_simultaneous_band().

# The joint-band group of each row of an effect grid: its group, with NA
# (the population) as one group.
.cdrgam_cli_band_groups <- function(value) {
    if (is.null(value$group)) rep.int('', nrow(value)) else paste(value$group)
}

# A simultaneous band from posterior draws: `estimate` at each row, `draws`
# a matrix with one row per row and one column per draw, and `groups` the
# rows of each joint band. Each row's scale is its draw standard deviation,
# and the core gives each group's max-modulus critical value. Returns
# lower, upper, and critical_value per row.
.cdrgam_cli_draw_band <- function(estimate, draws, groups, level) {
    .cdrgam_cli_core_require_simultaneous()
    scale <- apply(draws, 1L, stats::sd)
    scale[!is.finite(scale) | scale <= 0] <- NA_real_
    critical <- .cdrgam_cli_core_simultaneous_critical_value(
        draws=draws, estimate=estimate, groups=groups, level=level
    )
    width <- critical * ifelse(is.na(scale), 0, scale)
    list(lower=estimate - width, upper=estimate + width, critical_value=critical)
}

# Coefficient draws shared by every simulation of one line, for `terms`
# under `composition`. Population peaks and grouped lines both draw from
# the conditional coefficients when the line is grouped.
.cdrgam_cli_peak_band_sample <- function(
        fit, terms, composition, grouping, impulses, responses, unconditional, draws,
        seed
) {
    .cdrgam_cli_core_sample_coefficients(
        fit, terms=unique(terms), impulses=impulses, responses=responses,
        composition=composition,
        grouping=if (identical(grouping, 'population')) 'population' else 'conditional',
        unconditional=unconditional, draws=as.integer(draws), seed=seed
    )
}

# The columns of `sample` for draws `columns`, as a sample.
.cdrgam_cli_peak_band_columns <- function(sample, columns) {
    sample$draws <- sample$draws[, columns, drop=FALSE]
    sample
}

# The posterior draws of each peak that a line's values used: a list named
# by peak-cache key, each a vector of one lag per draw. `used` is the
# cache's `.used` table for the line's term.
.cdrgam_cli_peak_band_peak_lags <- function(
        fit, used, catalog, sample, impulses, responses, n
) {
    keys <- paste0(used$direction, '\r', used$source, '\r', used$composition)
    lags <- lapply(seq_len(nrow(used)), function(row) {
        term <- .cdrgam_cli_catalog_row(catalog, match(used$source[[row]], catalog$label))
        value <- do.call(.cdrgam_cli_core_simulate_effect, c(
            list(fit, impulses=impulses, responses=responses, coefficients=sample),
            .cdrgam_cli_peak_request(term, n, used$composition[[row]])
        ))
        value$grid$lag[apply(
            value$draws, 2L, .cdrgam_cli_peak_index, direction=used$direction[[row]]
        )]
    })
    stats::setNames(lags, keys)
}

# The simultaneous band of one line that uses peaks. `value` is its point-estimate effect
# grid; `arguments` the named estimate_effect() arguments, without the fit,
# that produced it; `used` the peaks it used (the `.used` rows of its peak
# cache for this term, or NULL); and `resolve(cache)` re-resolves the axes
# and reference with the peaks in `cache`, returning list(axes, reference).
# Returns `value` with `lower`, `upper`, and `critical_value` from the
# draws; see .cdrgam_cli_draw_band().
.cdrgam_cli_peak_band <- function(
        fit, value, arguments, used, resolve, catalog, draws, level, seed=1L
) {
    if (!.cdrgam_cli_core_has_shared_draws()) .cdrgam_cli_abort(paste(
        'Simultaneous intervals at a peak need a cdrgam release with',
        'sample_effect_coefficients(); update cdrgam'
    ))
    draws <- as.integer(draws)
    if (draws < 2L) .cdrgam_cli_abort(paste(
        'Simultaneous intervals at a peak need at least two draws;',
        'raise uncertainty.peak_draws'
    ))
    sample <- .cdrgam_cli_peak_band_sample(
        fit, c(arguments$terms, used$source), arguments$composition,
        arguments$grouping, arguments$impulses, arguments$responses,
        isTRUE(arguments$unconditional), draws, seed
    )
    value <- as.data.frame(value)
    axes <- setdiff(names(value), c(
        .cdrgam_cli_effect_columns, grep('_reference$', names(value), value=TRUE)
    ))
    # The axes and reference of each draw, from that draw's peaks.
    lags <- .cdrgam_cli_peak_band_peak_lags(
        fit, used, catalog, sample, arguments$impulses, arguments$responses,
        arguments$n
    )
    settings <- lapply(seq_len(draws), function(draw) {
        cache <- new.env()
        cache$.catalog <- catalog
        for (key in names(lags)) cache[[key]] <- lags[[key]][[draw]]
        resolve(cache)
    })
    signature <- vapply(settings, digest::digest, character(1))
    distinct <- unique(signature)
    settings <- settings[match(distinct, signature)]
    assignment <- match(signature, distinct)
    values <- matrix(NA_real_, nrow(value), draws)
    for (index in seq_along(settings)) {
        columns <- which(assignment == index)
        setting <- settings[[index]]
        request <- arguments
        request$axes <- setting$axes
        request$reference <- setting$reference
        request <- Filter(Negate(is.null), request)
        simulated <- do.call(.cdrgam_cli_core_simulate_effect, c(
            list(fit), request[setdiff(names(request), c('level', 'se'))],
            list(coefficients=.cdrgam_cli_peak_band_columns(sample, columns))
        ))
        grid <- as.data.frame(simulated$grid)
        # Rows match the point-estimate grid on every axis but a fixed lag,
        # which moves with the draw's peak.
        varying <- if ('lag' %in% axes && length(unique(value$lag)) == 1L) {
            setdiff(axes, 'lag')
        } else axes
        key <- function(table) do.call(
            paste, c(unname(as.list(table[c('group', varying)])), sep='\r')
        )
        rows <- match(key(value), key(grid))
        if (anyNA(rows)) .cdrgam_cli_abort(
            'Peak draws did not cover every point of the line'
        )
        values[, columns] <- simulated$draws[rows, , drop=FALSE]
    }
    band <- .cdrgam_cli_draw_band(
        value$estimate, values, .cdrgam_cli_band_groups(value), level
    )
    value$lower <- band$lower
    value$upper <- band$upper
    value$critical_value <- band$critical_value
    value
}
