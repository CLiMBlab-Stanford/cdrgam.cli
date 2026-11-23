# Plots of lines: each line is one effect evaluation, with its own model,
# term, axes, and reference, drawn over a shared horizontal axis. The viewer
# and visualization definitions with `lines` both build their figures here.
# An entry is list(spec=list(id, label, varying), data), where `data` is the
# line's effect grid with attributes `peaks` and `pointwise_peaks` from
# .cdrgam_cli_compute_effect() and `varying[[1]]` names its horizontal axis.

# The plot specification of lines under a validated `render` whose
# mappings use the columns .cdrgam_cli_lines_grid() adds, with legend
# entries labeled by line label.
.cdrgam_cli_lines_spec <- function(entries, render) {
    data <- if (identical(render$statistic, 'peak-lag')) {
        .cdrgam_cli_lines_peak_rows(entries)
    } else .cdrgam_cli_lines_grid(entries)
    spec <- .cdrgam_cli_plot_spec(data, list(render=render))
    labels <- vapply(entries, function(entry) entry$spec$label, character(1))
    spec$color$labels <- labels[match(spec$color$levels, .cdrgam_cli_lines_ids(entries))]
    spec$render <- render
    spec
}

# The mappings a lines plot uses when a definition gives none.
.cdrgam_cli_lines_mappings <- function(statistic=NULL) {
    if (identical(statistic, 'peak-lag')) {
        list(x='peak', y='estimate', color='line')
    } else list(x='.line_x', y='estimate', color='line', group='line')
}

# The figure of lines: the plot specification, the peak timeline, and the
# pointwise-peak panel, either NULL when not drawn. Backends draw the three
# and stack them.
.cdrgam_cli_lines_figure <- function(entries, render) {
    spec <- .cdrgam_cli_lines_spec(entries, render)
    list(
        spec=spec, strip=.cdrgam_cli_lines_strip_spec(entries, spec),
        pointwise=.cdrgam_cli_lines_pointwise_spec(entries, spec)
    )
}

.cdrgam_cli_lines_ids <- function(entries) {
    vapply(entries, function(entry) entry$spec$id, character(1))
}

# The lines as one effect grid: each line's rows, with its id in
# `line` (a factor in line order), its horizontal axis name in `.cdrgam_axis`,
# and that axis's values in `.line_x`, so lines over different axes share
# the horizontal axis.
.cdrgam_cli_lines_grid <- function(entries) {
    tables <- lapply(entries, function(entry) {
        data <- as.data.frame(entry$data)
        axis <- entry$spec$varying[[1L]]
        data$.line_x <- data[[axis]]
        data$.cdrgam_axis <- axis
        data$line <- entry$spec$id
        data$layer <- 'estimate'
        data
    })
    data <- .cdrgam_cli_bind_tables(tables)
    data$line <- factor(data$line, levels=.cdrgam_cli_lines_ids(entries))
    data
}

# The peaks the lines used, one row per line and peak, with the
# lane label each gets: the line's legend label, adding the peak only when
# the line used more than one. `term` and `line` hold
# the line id, and attribute `draws` the posterior draws with their line.
# NULL when no line used a peak.
.cdrgam_cli_lines_peaks <- function(entries) {
    peaks <- list()
    draws <- list()
    for (entry in entries) {
        table <- attr(entry$data, 'peaks')
        if (is.null(table) || !nrow(table)) next
        sampled <- attr(table, 'draws')
        table <- as.data.frame(table)
        lanes <- .cdrgam_cli_peak_lanes(table)
        label <- entry$spec$label
        table$.label <- ifelse(
            nrow(table) > 1L, paste0(label, ' · ', sub('^.* · ', '', lanes)), label
        )
        table$term <- entry$spec$id
        table$line <- entry$spec$id
        peaks[[length(peaks) + 1L]] <- table[c(
            'term', 'line', 'source', 'direction', 'lag', 'lower', 'upper', '.label'
        )]
        if (!is.null(sampled) && nrow(sampled)) {
            sampled$line <- entry$spec$id
            draws[[length(draws) + 1L]] <- sampled
        }
    }
    if (!length(peaks)) return(NULL)
    table <- do.call(rbind, peaks)
    attr(table, 'draws') <- if (length(draws)) do.call(rbind, draws) else NULL
    table
}

# The rows of a peak-lag plot of the lines: one per line and peak,
# labeled by lane in line order.
.cdrgam_cli_lines_peak_rows <- function(entries) {
    peaks <- .cdrgam_cli_lines_peaks(entries)
    if (is.null(peaks)) .cdrgam_cli_abort(
        'No line uses a peak; set lag to peak to plot peak lags'
    )
    data.frame(
        term=peaks$term, line=factor(peaks$line, levels=.cdrgam_cli_lines_ids(entries)),
        peak=factor(peaks$.label, levels=unique(peaks$.label)), source=peaks$source,
        direction=peaks$direction, estimate=peaks$lag, lower=peaks$lower,
        upper=peaks$upper, group=NA_character_, layer='estimate',
        stringsAsFactors=FALSE
    )
}

# The peak timeline of the lines, or NULL when none is drawn: as
# for rendered plots, only under lines or points of estimates, when
# render.peaks allows it (see .cdrgam_cli_show_peaks()). Lanes are colored
# like their lines; the strip shares the x-axis when every line runs over
# lag at linear spacing. `spec` is the lines' plot specification.
.cdrgam_cli_lines_strip_spec <- function(entries, spec) {
    render <- spec$render
    if (!(render$geometry %in% c('line', 'point')) ||
            identical(render$statistic, 'peak-lag')) return(NULL)
    table <- .cdrgam_cli_lines_peaks(entries)
    if (!.cdrgam_cli_show_peaks(table, render$peaks, lines=TRUE)) return(NULL)
    shared <- all(unique(spec$rows$.cdrgam_axis) == 'lag') &&
        !identical(render$spacing, 'uniform')
    .cdrgam_cli_peak_strip_spec(
        table, colors=spec$color$values, labels=table$.label,
        xlim=if (shared) range(c(spec$rows$.line_x, table$lag), na.rm=TRUE),
        theme=spec$theme
    )
}

# The pointwise peaks of the lines that hold lag at
# peak-pointwise, one row per point, with the line id in `line` and the
# point's horizontal axis value in `.line_x`; NULL when no line does.
.cdrgam_cli_lines_pointwise <- function(entries) {
    tables <- lapply(entries, function(entry) {
        table <- attr(entry$data, 'pointwise_peaks')
        if (is.null(table) || !nrow(table)) return(NULL)
        table <- as.data.frame(table)
        table$.line_x <- table[[entry$spec$varying[[1L]]]]
        table$line <- entry$spec$id
        table$layer <- 'estimate'
        table
    })
    tables <- Filter(Negate(is.null), tables)
    if (!length(tables)) return(NULL)
    .cdrgam_cli_bind_tables(tables)
}

# The pointwise-peak panel of the lines, or NULL when none is
# drawn: as for rendered plots, under lines or points of estimates unless
# render.peaks is never. `spec` is the lines' plot specification.
.cdrgam_cli_lines_pointwise_spec <- function(entries, spec) {
    render <- spec$render
    if (!(render$geometry %in% c('line', 'point')) ||
            identical(render$statistic, 'peak-lag') ||
            identical(render$peaks, 'never')) return(NULL)
    .cdrgam_cli_pointwise_spec(.cdrgam_cli_lines_pointwise(entries), spec, render)
}


# A visualization definition of lines, as list(model, query, lines,
# render): the inverse of .cdrgam_cli_validate_lines_visualization().
# `lines` is a list of list(id, label, model, query), each with a validated
# one-term query; `render` is the render block. Settings every line shares
# move to the definition's `query` for the lines to inherit, and settings
# at their defaults are left out.
.cdrgam_cli_lines_definition <- function(lines, render) {
    models <- vapply(lines, `[[`, character(1), 'model')
    model <- names(sort(table(models), decreasing=TRUE))[[1L]]
    queries <- lapply(lines, function(line) {
        query <- line$query
        query$terms <- NULL
        query
    })
    # Split each query into named settings: one per scalar field, axis,
    # predictor, reference axis, and uncertainty setting.
    flatten <- function(query) {
        values <- list()
        for (name in setdiff(names(query), c('axes', 'reference', 'uncertainty'))) {
            values[[name]] <- query[[name]]
        }
        for (name in setdiff(names(query$axes), 'predictors')) {
            values[[paste0('axes\r', name)]] <- query$axes[[name]]
        }
        for (name in names(query$axes$predictors)) {
            values[[paste0('axes\rpredictors\r', name)]] <- query$axes$predictors[[name]]
        }
        for (name in names(query$reference)) {
            values[[paste0('reference\r', name)]] <- query$reference[[name]]
        }
        for (name in names(query$uncertainty)) {
            values[[paste0('uncertainty\r', name)]] <- query$uncertainty[[name]]
        }
        values
    }
    unflatten <- function(values) {
        query <- list()
        for (key in names(values)) {
            path <- strsplit(key, '\r', fixed=TRUE)[[1L]]
            if (length(path) == 1L) query[[path]] <- values[[key]]
            else if (length(path) == 2L) query[[path[[1L]]]][[path[[2L]]]] <- values[[key]]
            else query[[path[[1L]]]][[path[[2L]]]][[path[[3L]]]] <- values[[key]]
        }
        query
    }
    flat <- lapply(queries, flatten)
    keys <- unique(unlist(lapply(flat, names)))
    shared <- Filter(function(key) {
        all(vapply(flat, function(values) {
            key %in% names(values) && identical(values[[key]], flat[[1L]][[key]])
        }, logical(1)))
    }, keys)
    defaults <- list(
        composition='term', grouping='population', n=200L,
        'uncertainty\rlevel'=0.95, 'uncertainty\rkind'='pointwise',
        'uncertainty\runconditional'=FALSE
    )
    common <- flat[[1L]][shared]
    common <- common[!vapply(names(common), function(key) {
        key %in% names(defaults) && identical(common[[key]], defaults[[key]])
    }, logical(1))]
    entries <- lapply(seq_along(lines), function(index) {
        line <- lines[[index]]
        own <- flat[[index]][setdiff(names(flat[[index]]), shared)]
        c(
            list(id=line$id, label=line$label, term=line$query$terms$names),
            if (!identical(line$model, model)) list(model=line$model),
            unflatten(own)
        )
    })
    Filter(Negate(is.null), list(
        schema=1L, model=model,
        query=if (length(common)) unflatten(common), lines=entries, render=render
    ))
}
