# Simultaneous confidence bands for effect grids. The core evaluates the
# effect, with its reference, on a subset of the grid together with the
# joint covariance of those points, and computes the max-modulus critical
# value from that covariance. This file chooses the subset and applies the
# critical value to the full grid.

.cdrgam_cli_simultaneous_draws <- 10000L
.cdrgam_cli_simultaneous_seed <- 20261119L

# Critical value c such that a centered Gaussian vector with covariance
# `covariance` stays within c standard errors at every coordinate with
# probability `level`. A fixed seed gives the same value for the same
# covariance and leaves the caller's random stream unchanged.
.cdrgam_cli_simultaneous_multiplier <- function(covariance, level) {
    .cdrgam_cli_core_require_simultaneous()
    .cdrgam_cli_core_simultaneous_critical_value(
        covariance=covariance, level=level,
        simulations=.cdrgam_cli_simultaneous_draws,
        seed=.cdrgam_cli_simultaneous_seed
    )[[1L]]
}

# Up to `count` evenly spaced values of an axis, always including its ends.
.cdrgam_cli_axis_subset <- function(values, count) {
    if (length(values) <= count) return(values)
    values[unique(round(seq(1, length(values), length.out=count)))]
}

# Replace the pointwise limits of `data`, one term's effect grid as returned
# by estimate_effect(), with simultaneous limits at `level`. Coverage holds
# jointly over every grid point of each group. The critical value is
# computed on at most about `points` grid points, chosen evenly along each
# varying axis, from one estimate_effect() call. `call` holds the term,
# axes, reference, composition, grouping, groups, and unconditional
# arguments that produced `data`; reference axes must have numeric values.
# Adds a `critical_value` column.
.cdrgam_cli_simultaneous_band <- function(fit, data, call, level, points=40L) {
    .cdrgam_cli_core_require_simultaneous()
    axes <- intersect(as.character(call$axes_names), names(data))
    reference_axes <- names(call$reference)
    defaults <- reference_axes[vapply(reference_axes, function(name) {
        column <- paste0(name, '_reference')
        !(column %in% names(data)) || anyNA(data[[column]])
    }, logical(1))]
    if (length(defaults)) .cdrgam_cli_abort(paste0(
        'Simultaneous intervals need a numeric reference for ',
        paste(defaults, collapse=', '), ' in ', call$term,
        '; replace default with a value such as mean'
    ))
    reference <- lapply(reference_axes, function(name) {
        data[[paste0(name, '_reference')]][[1L]]
    })
    names(reference) <- reference_axes
    observed <- lapply(axes, function(name) sort(unique(data[[name]])))
    names(observed) <- axes
    varying <- axes[lengths(observed) > 1L]
    per_axis <- if (length(varying)) {
        max(2L, floor(points^(1 / length(varying))))
    } else 1L
    request <- lapply(observed, function(values) {
        list(values=.cdrgam_cli_axis_subset(values, per_axis))
    })
    subset <- .cdrgam_cli_core_estimate_effect(
        fit, terms=call$term, axes=request,
        impulses=call$impulses, responses=call$responses,
        composition=call$composition, grouping=call$grouping,
        groups=call$groups, reference=reference, se=TRUE,
        unconditional=call$unconditional, level=level, n=2L, covariance=TRUE
    )
    subset_groups <- .cdrgam_cli_band_groups(subset)
    critical <- .cdrgam_cli_core_simultaneous_critical_value(
        covariance=attr(subset, 'covariance'), groups=subset_groups, level=level,
        simulations=.cdrgam_cli_simultaneous_draws,
        seed=.cdrgam_cli_simultaneous_seed
    )
    multiplier <- critical[match(.cdrgam_cli_band_groups(data), subset_groups)]
    data$lower <- data$estimate - multiplier * data$se
    data$upper <- data$estimate + multiplier * data$se
    data$critical_value <- multiplier
    data
}
