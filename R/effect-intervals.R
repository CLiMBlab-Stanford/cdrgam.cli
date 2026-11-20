# Simultaneous confidence bands for effect grids, computed only through
# estimate_effect(). The core reports pointwise standard errors. With a
# reference at grid point b, it also reports the standard error of
# f(a) - f(b) for every grid point a. Together these give the covariance
#   Cov(f(a), f(b)) = (Var f(a) + Var f(b) - Var(f(a) - f(b))) / 2
# of the effect over a subset of the grid, which fixes the distribution of
# the maximum absolute t statistic and so the band's critical value.

.cdrgam_cli_simultaneous_draws <- 10000L
.cdrgam_cli_simultaneous_seed <- 20261119L

# Critical value c such that a centered Gaussian vector with covariance
# `covariance` stays within c standard errors at every coordinate with
# probability `level`. It is estimated from a fixed-seed simulation, so the
# same covariance always gives the same value, and the caller's random
# number stream is left unchanged. Coordinates with no variance are ignored.
.cdrgam_cli_simultaneous_multiplier <- function(
        covariance, level, draws=.cdrgam_cli_simultaneous_draws,
        seed=.cdrgam_cli_simultaneous_seed
) {
    pointwise <- stats::qnorm((1 + level) / 2)
    covariance <- as.matrix(covariance)
    se <- sqrt(pmax(diag(covariance), 0))
    keep <- is.finite(se) & se > max(c(se, 0), na.rm=TRUE) * 1e-8
    if (sum(keep) < 2L) return(pointwise)
    correlation <- covariance[keep, keep, drop=FALSE] / outer(se[keep], se[keep])
    decomposition <- eigen((correlation + t(correlation)) / 2, symmetric=TRUE)
    root <- decomposition$vectors %*%
        diag(sqrt(pmax(decomposition$values, 0)), nrow=sum(keep))
    environment <- globalenv()
    saved <- if (exists('.Random.seed', envir=environment, inherits=FALSE)) {
        get('.Random.seed', envir=environment, inherits=FALSE)
    } else NULL
    on.exit({
        if (is.null(saved)) {
            if (exists('.Random.seed', envir=environment, inherits=FALSE)) {
                rm('.Random.seed', envir=environment)
            }
        } else assign('.Random.seed', saved, envir=environment)
    }, add=TRUE)
    set.seed(seed, kind='Mersenne-Twister', normal.kind='Inversion')
    draws <- matrix(stats::rnorm(draws * sum(keep)), nrow=draws) %*% t(root)
    maxima <- apply(abs(draws), 1L, max)
    max(pointwise, stats::quantile(maxima, level, names=FALSE, type=8))
}

# Up to `count` evenly spaced values of an axis, always including its ends.
.cdrgam_cli_axis_subset <- function(values, count) {
    if (length(values) <= count) return(values)
    values[unique(round(seq(1, length(values), length.out=count)))]
}

.cdrgam_cli_grid_key <- function(frame, axes) {
    columns <- lapply(axes, function(name) sprintf('%.17g', frame[[name]]))
    group <- if ('group' %in% names(frame)) {
        ifelse(is.na(frame$group), '', as.character(frame$group))
    } else rep.int('', nrow(frame))
    do.call(paste, c(columns, list(group, sep='\r')))
}

# Replace the pointwise limits of `data`, one term's effect grid as returned
# by estimate_effect(), with simultaneous limits at `level`. Coverage holds
# jointly over every grid point of each group. The covariance is estimated on
# at most about `points` grid points, chosen evenly along each varying axis,
# and costs one estimate_effect() call per point and reference value. `call`
# holds the term, axes, reference, composition, grouping, groups, and
# unconditional arguments that produced `data`; reference axes must have
# numeric values. Adds a `critical_value` column.
.cdrgam_cli_simultaneous_band <- function(fit, data, call, level, points=40L) {
    axes <- intersect(as.character(call$axes_names), names(data))
    reference <- call$reference
    reference_axes <- names(reference)
    defaults <- reference_axes[vapply(reference_axes, function(name) {
        column <- paste0(name, '_reference')
        !(column %in% names(data)) || anyNA(data[[column]])
    }, logical(1))]
    if (length(defaults)) .cdrgam_cli_abort(paste0(
        'Simultaneous intervals need a numeric reference for ',
        paste(defaults, collapse=', '), ' in ', call$term,
        '; replace default with a value such as mean'
    ))
    reference_values <- lapply(reference_axes, function(name) {
        data[[paste0(name, '_reference')]][[1L]]
    })
    names(reference_values) <- reference_axes
    observed <- lapply(axes, function(name) sort(unique(data[[name]])))
    names(observed) <- axes
    varying <- axes[lengths(observed) > 1L]
    per_axis <- if (length(varying)) {
        max(2L, floor(points^(1 / length(varying))))
    } else 1L
    evaluation <- lapply(axes, function(name) {
        .cdrgam_cli_axis_subset(observed[[name]], per_axis)
    })
    names(evaluation) <- axes
    support <- evaluation
    for (name in reference_axes) {
        support[[name]] <- sort(unique(c(support[[name]], reference_values[[name]])))
    }
    grid <- do.call(expand.grid, c(
        support, list(KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
    ))
    request <- lapply(support, function(values) list(values=values))
    estimate <- function(extra=list()) {
        do.call(.cdrgam_cli_core_estimate_effect, c(list(
            fit, terms=call$term, axes=request,
            impulses=call$impulses, responses=call$responses,
            composition=call$composition, grouping=call$grouping,
            groups=call$groups, se=TRUE, unconditional=call$unconditional,
            level=level, n=2L
        ), extra))
    }
    base <- estimate()
    keys <- .cdrgam_cli_grid_key(base, axes)
    groups <- if (all(is.na(base$group))) NA_character_ else unique(base$group)
    in_group <- function(frame, group) {
        if (is.na(group)) rep.int(TRUE, nrow(frame)) else frame$group %in% group
    }
    grid_keys <- .cdrgam_cli_grid_key(grid, axes)
    variance <- matrix(NA_real_, nrow(grid), length(groups))
    differences <- array(NA_real_, c(nrow(grid), nrow(grid), length(groups)))
    for (g in seq_along(groups)) {
        rows <- in_group(base, groups[[g]])
        group_keys <- .cdrgam_cli_grid_key(
            transform(grid, group=groups[[g]]), axes
        )
        variance[, g] <- base$se[rows][match(group_keys, keys[rows])]^2
    }
    for (index in seq_len(nrow(grid))) {
        point <- as.list(grid[index, , drop=FALSE])
        shifted <- estimate(list(reference=point))
        shifted_keys <- .cdrgam_cli_grid_key(shifted, axes)
        for (g in seq_along(groups)) {
            rows <- in_group(shifted, groups[[g]])
            group_keys <- .cdrgam_cli_grid_key(
                transform(grid, group=groups[[g]]), axes
            )
            differences[, index, g] <- shifted$se[rows][
                match(group_keys, shifted_keys[rows])
            ]^2
        }
    }
    targets <- do.call(expand.grid, c(
        evaluation, list(KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
    ))
    baselines <- targets
    for (name in reference_axes) baselines[[name]] <- reference_values[[name]]
    target_index <- match(.cdrgam_cli_grid_key(targets, axes), grid_keys)
    baseline_index <- match(.cdrgam_cli_grid_key(baselines, axes), grid_keys)
    data$critical_value <- NA_real_
    for (g in seq_along(groups)) {
        covariance <- (outer(variance[, g], variance[, g], '+') -
            differences[, , g]) / 2
        covariance <- (covariance + t(covariance)) / 2
        effect <- covariance[target_index, target_index, drop=FALSE]
        if (length(reference_axes)) {
            effect <- effect -
                covariance[target_index, baseline_index, drop=FALSE] -
                covariance[baseline_index, target_index, drop=FALSE] +
                covariance[baseline_index, baseline_index, drop=FALSE]
        }
        multiplier <- .cdrgam_cli_simultaneous_multiplier(effect, level)
        rows <- in_group(data, groups[[g]])
        data$lower[rows] <- data$estimate[rows] - multiplier * data$se[rows]
        data$upper[rows] <- data$estimate[rows] + multiplier * data$se[rows]
        data$critical_value[rows] <- multiplier
    }
    data
}
