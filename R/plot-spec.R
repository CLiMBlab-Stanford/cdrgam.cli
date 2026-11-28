# Backend-neutral plot specifications. A specification resolves everything a
# plot shows (row positions after spacing and dodging, colors, axis ticks,
# labels, facets, and the peak strip) from an effect grid and a validated
# render, so the ggplot2 renderer and the viewer's plotly renderer draw the
# same thing and differ only in drawing calls.

.cdrgam_cli_mapping_column <- function(value) {
    sub('^predictor:', '', value)
}

.cdrgam_cli_layer_style <- function(layer) {
    style <- .cdrgam_cli_null(layer$style, list())
    style$alpha <- .cdrgam_cli_null(style$alpha, 1)
    style$linewidth <- .cdrgam_cli_null(style$linewidth, 0.8)
    style
}

# Evaluation-axis columns, other than the x and y mappings, that take several
# values. Line series are split on these so that, for example, curves at
# different moderator values are not joined into one path.
.cdrgam_cli_series_axes <- function(data, mappings) {
    reserved <- c(
        'term_id', 'term', 'group', 'composition', 'grouping', 'estimate',
        'se', 'lower', 'upper', 'layer', 'predictor', 'predictor_value',
        'critical_value'
    )
    mapped <- vapply(
        mappings[intersect(c('x', 'y'), names(mappings))],
        .cdrgam_cli_mapping_column, character(1)
    )
    # A mapped predictor value stands for each term's own predictor column.
    if ('predictor_value' %in% mapped && 'predictor' %in% names(data)) {
        mapped <- c(mapped, unique(stats::na.omit(as.character(data$predictor))))
    }
    candidates <- setdiff(names(data), c(reserved, mapped))
    candidates <- candidates[!startsWith(candidates, '.') &
        !endsWith(candidates, '_reference')]
    # An axis of only some terms is missing elsewhere; it splits series only
    # where it takes several values within one term.
    candidates[vapply(candidates, function(name) {
        values <- data[[name]]
        is.numeric(values) && any(tapply(values, data$term, function(value) {
            length(unique(value[!is.na(value)])) > 1L
        }), na.rm=TRUE)
    }, logical(1))]
}

# Numeric color and linetype mappings with few distinct values are shown as
# discrete legends, one entry per evaluated value.
.cdrgam_cli_discrete_mappings <- function(data, mappings) {
    for (aesthetic in intersect(c('color', 'linetype'), names(mappings))) {
        column <- .cdrgam_cli_mapping_column(mappings[[aesthetic]])
        values <- data[[column]]
        if (!is.numeric(values)) next
        levels <- sort(unique(values))
        if (length(levels) > 12L && identical(aesthetic, 'color')) next
        labels <- format(signif(levels, 4L), trim=TRUE, drop0trailing=TRUE)
        data[[column]] <- factor(match(values, levels), seq_along(levels), labels)
    }
    data
}

# Rainbow colors from red through violet, darkened enough to read on white.
# irf(1), the response to an average event, is drawn in black.
.cdrgam_cli_rainbow <- function(levels) {
    constant <- levels == 'irf(1)'
    colors <- rep.int('black', length(levels))
    colors[!constant] <- grDevices::hsv(
        seq(0, 0.8, length.out=sum(!constant)), 0.9, 0.8
    )
    names(colors) <- levels
    colors
}

# Named palettes accepted by render.palette, besides `default` (ggplot2's hue
# palette) and `rainbow`. ColorBrewer palettes are listed with the number of
# colors each provides.
.cdrgam_cli_brewer_palettes <- c(
    Blues=9L, BuGn=9L, BuPu=9L, GnBu=9L, Greens=9L, Greys=9L, Oranges=9L,
    OrRd=9L, PuBu=9L, PuBuGn=9L, PuRd=9L, Purples=9L, RdPu=9L, Reds=9L,
    YlGn=9L, YlGnBu=9L, YlOrBr=9L, YlOrRd=9L,
    BrBG=11L, PiYG=11L, PRGn=11L, PuOr=11L, RdBu=11L, RdGy=11L, RdYlBu=11L,
    RdYlGn=11L, Spectral=11L,
    Accent=8L, Dark2=8L, Paired=12L, Pastel1=9L, Pastel2=8L, Set1=9L,
    Set2=8L, Set3=12L
)
# The viewer's line colors: the Okabe-Ito colors without yellow, which is
# hard to read on white, ordered blue, vermillion, green, reddish purple,
# orange, and sky blue, then grey and black.
.cdrgam_cli_okabe_ito <- c(
    '#0072B2', '#D55E00', '#009E73', '#CC79A7', '#E69F00', '#56B4E9',
    '#7F7F7F', '#000000'
)
.cdrgam_cli_viridis_palettes <- c(
    'magma', 'inferno', 'plasma', 'viridis', 'cividis', 'rocket', 'mako', 'turbo'
)

# The canonical spelling of a palette name. Matching ignores case; `hue` is
# an alias of `default` and `gray` of `grey`.
.cdrgam_cli_palette_name <- function(value, field) {
    names <- c(
        'default', 'rainbow', 'grey', 'okabe-ito', .cdrgam_cli_viridis_palettes,
        names(.cdrgam_cli_brewer_palettes)
    )
    key <- tolower(value)
    key <- switch(key, hue='default', gray='grey', key)
    index <- match(key, tolower(names))
    if (is.na(index)) .cdrgam_cli_abort(paste0(
        field, ' must be default, rainbow, grey, okabe-ito, a viridis option (',
        paste(.cdrgam_cli_viridis_palettes, collapse=', '),
        '), or a ColorBrewer palette (',
        paste(names(.cdrgam_cli_brewer_palettes), collapse=', '), ')'
    ))
    names[[index]]
}

# A color written as hex (#RGB, #RRGGBB, or #RRGGBBAA), as rgb(R, G, B) with
# channels from 0 to 255, or as an R color name, in uppercase #RRGGBB form
# (#RRGGBBAA when not opaque). NA when the text is not a color.
.cdrgam_cli_color_hex <- function(value) {
    text <- trimws(value)
    pattern <- '^rgb\\(\\s*([0-9.]+)\\s*,\\s*([0-9.]+)\\s*,\\s*([0-9.]+)\\s*\\)$'
    if (grepl(pattern, text, ignore.case=TRUE, perl=TRUE)) {
        channels <- as.numeric(regmatches(
            text, regexec(pattern, text, ignore.case=TRUE, perl=TRUE)
        )[[1L]][-1L])
        if (any(!is.finite(channels) | channels > 255)) return(NA_character_)
        return(grDevices::rgb(channels[[1L]], channels[[2L]], channels[[3L]], maxColorValue=255))
    }
    if (grepl('^#([0-9A-Fa-f]{3})$', text)) {
        digits <- strsplit(substring(text, 2L), '')[[1L]]
        text <- paste0('#', paste(rep(digits, each=2L), collapse=''))
    }
    channels <- tryCatch(
        grDevices::col2rgb(text, alpha=TRUE), error=function(error) NULL
    )
    if (is.null(channels) || grepl('^[0-9]+$', text)) return(NA_character_)
    alpha <- channels[[4L]]
    grDevices::rgb(
        channels[[1L]], channels[[2L]], channels[[3L]],
        alpha=if (alpha < 255) alpha else NULL, maxColorValue=255
    )
}

# A palette is a gradient when it lists colors rather than naming a palette.
.cdrgam_cli_gradient_palette <- function(palette) length(palette) > 1L

# One color per level of a discrete mapping, so that the main plot and the
# peak strip color the same level alike. A gradient places the levels at
# equal steps along the listed colors, from the first color to the last.
# NULL or `default` gives ggplot2's hue colors.
# `reverse = TRUE` reads the palette from its other end. A palette with a
# fixed list of colors, okabe-ito or a Brewer palette, then starts from its
# last color, so three levels take the last three colors, last first. Other
# palettes spread over the levels, and their spread is reversed.
.cdrgam_cli_palette_colors <- function(palette, levels, reverse=FALSE) {
    if (!isTRUE(reverse)) return(.cdrgam_cli_palette_level_colors(palette, levels))
    fixed <- if (length(palette) == 1L && identical(palette, 'okabe-ito')) {
        length(.cdrgam_cli_okabe_ito)
    } else if (length(palette) == 1L && palette %in% names(.cdrgam_cli_brewer_palettes)) {
        .cdrgam_cli_brewer_palettes[[palette]]
    }
    colors <- if (is.null(fixed)) {
        rev(unname(.cdrgam_cli_palette_level_colors(palette, levels)))
    } else {
        if (length(levels) > fixed) {
            .cdrgam_cli_palette_level_colors(palette, levels)
        }
        full <- .cdrgam_cli_palette_level_colors(palette, paste0('level', seq_len(fixed)))
        rev(unname(full))[seq_along(levels)]
    }
    names(colors) <- levels
    colors
}

.cdrgam_cli_palette_level_colors <- function(palette, levels) {
    count <- length(levels)
    if (is.null(palette)) palette <- 'default'
    colors <- if (.cdrgam_cli_gradient_palette(palette)) {
        if (count == 1L) palette[[1L]] else grDevices::colorRampPalette(palette)(count)
    } else if (identical(palette, 'default')) {
        ggplot2::scale_colour_hue()$palette(count)
    } else if (identical(palette, 'rainbow')) {
        .cdrgam_cli_rainbow(levels)
    } else if (identical(palette, 'grey')) {
        ggplot2::scale_colour_grey()$palette(count)
    } else if (palette %in% .cdrgam_cli_viridis_palettes) {
        ggplot2::scale_colour_viridis_d(option=palette)$palette(count)
    } else if (identical(palette, 'okabe-ito')) {
        available <- length(.cdrgam_cli_okabe_ito)
        if (count > available) .cdrgam_cli_abort(paste0(
            'Palette okabe-ito has ', available, ' colors, but the plot has ',
            count, ' color levels; choose a viridis palette or rainbow'
        ))
        .cdrgam_cli_okabe_ito[seq_len(count)]
    } else {
        available <- .cdrgam_cli_brewer_palettes[[palette]]
        if (count > available) .cdrgam_cli_abort(paste0(
            'Palette ', palette, ' has ', available, ' colors, but the plot has ',
            count, ' color levels; choose a viridis palette or rainbow'
        ))
        ggplot2::scale_colour_brewer(palette=palette)$palette(max(count, 3L))[
            seq_len(count)
        ]
    }
    stats::setNames(unname(colors), levels)
}

# The colors a numeric mapping interpolates between, from the lowest value
# to the highest. These reproduce each palette's ggplot2 continuous scale.
.cdrgam_cli_palette_stops <- function(palette, reverse=FALSE) {
    stops <- .cdrgam_cli_palette_gradient_stops(palette)
    if (isTRUE(reverse)) rev(stops) else stops
}

.cdrgam_cli_palette_gradient_stops <- function(palette) {
    if (is.null(palette)) palette <- 'default'
    if (.cdrgam_cli_gradient_palette(palette)) return(palette)
    switch(
        palette,
        default=c('#132B43', '#56B1F7'),
        rainbow=grDevices::hsv(seq(0, 0.8, length.out=7L), 0.9, 0.8),
        grey=vapply(c('grey80', 'grey10'), .cdrgam_cli_color_hex, character(1),
            USE.NAMES=FALSE),
        'okabe-ito'=.cdrgam_cli_okabe_ito[c(1L, 6L, 5L, 2L)],
        if (palette %in% .cdrgam_cli_viridis_palettes) {
            ggplot2::scale_colour_viridis_d(option=palette)$palette(6L)
        } else rev(ggplot2::scale_colour_brewer(palette=palette)$palette(7L))
    )
}

# The colors of one mapped column: discrete level colors with their legend
# labels, or continuous stops over the column's range. NULL when no column is
# given.
.cdrgam_cli_color_spec <- function(data, column, palette, reverse=FALSE) {
    if (is.null(column)) return(NULL)
    values <- data[[column]]
    if (is.numeric(values)) return(list(
        column=column, kind='continuous',
        stops=.cdrgam_cli_palette_stops(palette, reverse),
        limits=range(values, na.rm=TRUE)
    ))
    levels <- if (is.factor(values)) levels(droplevels(values)) else {
        sort(unique(as.character(values[!is.na(values)])))
    }
    list(
        column=column, kind='discrete', levels=levels, labels=levels,
        values=.cdrgam_cli_palette_colors(palette, levels, reverse)
    )
}

# Where rows sit on the x-axis. A numeric x drawn at its evaluated values has
# one tick per distinct value: `uniform` spacing places the ticks at 1, 2, ...
# in sorted order and `linear` spacing at the values themselves. Bars over a
# numeric x use uniform spacing unless `render.spacing` is set. A categorical
# x has one tick per level, as ggplot2's discrete scales do. Otherwise x is a
# continuous range with automatic breaks, or with ticks at `render.breaks`.
.cdrgam_cli_x_axis <- function(values, render) {
    # render.tick_labels replaces the labels of the axis's ticks: the breaks
    # when given (validation has paired them), and otherwise each distinct
    # x value in order, which must then match the labels in number.
    labelled <- function(labels) {
        given <- render$tick_labels
        if (is.null(given)) return(labels)
        if (length(given) != length(labels)) .cdrgam_cli_abort(paste0(
            'render.tick_labels has ', length(given), ' labels for ', length(labels),
            if (is.null(render$breaks)) ' x values' else ' breaks'
        ))
        given
    }
    if (!is.numeric(values)) {
        levels <- if (is.factor(values)) levels(droplevels(values)) else {
            sort(unique(as.character(values[!is.na(values)])))
        }
        return(list(
            kind='discrete', position=match(as.character(values), levels),
            breaks=seq_along(levels), labels=labelled(levels)
        ))
    }
    spacing <- render$spacing
    if (!is.null(render$breaks) && !identical(spacing, 'uniform')) {
        return(list(
            kind='breaks', position=values, breaks=render$breaks,
            labels=labelled(
                format(signif(render$breaks, 4L), trim=TRUE, drop0trailing=TRUE)
            )
        ))
    }
    if (is.null(spacing) && (identical(render$geometry, 'bar') ||
            !is.null(render$tick_labels))) {
        # Labels name ticks, so a labelled axis without breaks gets one tick
        # per value: evenly spaced for bars, at the values otherwise.
        spacing <- if (identical(render$geometry, 'bar')) 'uniform' else 'linear'
    }
    if (is.null(spacing)) return(list(kind='continuous', position=values))
    ticks <- sort(unique(values[is.finite(values)]))
    breaks <- if (identical(spacing, 'uniform')) seq_along(ticks) else ticks
    list(
        kind='ticks', position=breaks[match(values, ticks)], breaks=breaks,
        labels=labelled(format(signif(ticks, 4L), trim=TRUE, drop0trailing=TRUE))
    )
}

# The width one x position may occupy: 80% of the smallest gap between
# distinct positions, or 0.8 when there is only one.
.cdrgam_cli_x_width <- function(values) {
    gaps <- diff(sort(unique(values[is.finite(values)])))
    if (length(gaps)) 0.8 * min(gaps) else 0.8
}

# The width of a cluster of points sharing an x position: one point's width
# per series, so the series sit in adjacent columns, but no more than
# `width`, which leaves a gap between clusters. A point is taken as
# .cdrgam_cli_point_columns_px pixels of a panel `figure_width` inches wide
# (7 when unset, as in the viewer) at 96 pixels per inch.
.cdrgam_cli_point_columns_px <- 9
.cdrgam_cli_point_cluster_width <- function(position, series, key, width, figure_width=NULL) {
    span <- diff(range(position[is.finite(position)]))
    if (!length(span) || !is.finite(span) || span <= 0) return(width)
    slot <- paste(key, sprintf('%.17g', position), sep='\r')
    count <- max(vapply(split(series, slot), function(value) length(unique(value)),
        integer(1)))
    panel <- 0.85 * 96 * .cdrgam_cli_null(figure_width, 7)
    min(width, count * .cdrgam_cli_point_columns_px * span / panel)
}

# Offsets that place the series sharing an x position side by side within
# `width`, ordered by series, as ggplot2's position_dodge() does. `key`
# identifies the panel and layer, within which positions are shared.
# Returns each row's center and the count of series sharing its position.
.cdrgam_cli_dodge <- function(position, series, key, width) {
    slot <- paste(key, sprintf('%.17g', position), sep='\r')
    count <- integer(length(position))
    rank <- integer(length(position))
    for (rows in split(seq_along(position), slot)) {
        present <- sort(unique(series[rows]))
        count[rows] <- length(present)
        rank[rows] <- match(series[rows], present)
    }
    list(center=position + width * ((rank - 0.5) / count - 0.5), count=count)
}

# The specification of one effect plot. `data` is an effect grid, or for
# `statistic: peak-lag` the rows from .cdrgam_cli_peak_data(); `definition`
# holds the validated `render` and optionally `query` and `layers`.
.cdrgam_cli_plot_spec <- function(data, definition) {
    render <- definition$render
    mappings <- render$mappings
    geometry <- render$geometry
    peak_rows <- identical(render$statistic, 'peak-lag')
    columns <- lapply(mappings, .cdrgam_cli_mapping_column)
    missing <- setdiff(unlist(columns[setdiff(names(columns), 'facet')]), names(data))
    if (length(missing)) .cdrgam_cli_abort(paste0(
        'Visualization mappings reference unavailable effect-grid columns: ',
        paste(missing, collapse=', ')
    ))
    facet <- if (!is.null(columns$facet)) {
        if (!(columns$facet %in% names(data))) .cdrgam_cli_abort(paste0(
            'Visualization facet references unavailable column: ', columns$facet
        ))
        list(column=columns$facet, scales=.cdrgam_cli_null(render$scales, 'fixed'))
    }
    # Peak lags are located once per evaluation, not per layer.
    layers <- .cdrgam_cli_effect_layers(
        definition$query, if (peak_rows) NULL else definition$layers
    )
    layers <- lapply(layers, function(layer) list(
        id=layer$id, interval=.cdrgam_cli_null(layer$interval, render$interval),
        style=.cdrgam_cli_layer_style(layer)
    ))
    spec <- list(
        geometry=geometry, theme=render$theme, y_zero=isTRUE(render$y_zero),
        ylim=render$ylim, zero_line=!isFALSE(render$zero_line),
        columns=columns, facet=facet,
        layers=layers,
        labels=list(
            title=render$title, x=.cdrgam_cli_null(render$xlab, mappings$x),
            y=.cdrgam_cli_null(
                render$ylab, if (peak_rows) 'Peak lag' else 'Effect on linear predictor'
            )
        )
    )
    if (geometry %in% .cdrgam_cli_surface_geometries) {
        # A surface's vertical axis is an evaluation axis, not the effect.
        spec$labels$y <- .cdrgam_cli_null(render$ylab, mappings$y)
        spec$rows <- data
        spec$surface <- list(
            raster=geometry %in% c('raster', 'raster-contour'),
            contour=geometry %in% c('contour', 'raster-contour'),
            z=.cdrgam_cli_null(columns$fill, 'estimate'),
            scale=if (identical(columns$fill, 'se')) 'se' else 'diverging'
        )
        return(spec)
    }
    series_axes <- .cdrgam_cli_series_axes(data, mappings)
    x <- .cdrgam_cli_x_axis(data[[columns$x]], render)
    data$.cdrgam_x <- x$position
    data <- .cdrgam_cli_discrete_mappings(data, mappings)
    data$.cdrgam_series <- do.call(interaction, c(
        list(
            data$term,
            if (peak_rows) data$peak else
                ifelse(is.na(data$group), '<population>', as.character(data$group)),
            data$layer
        ),
        unname(lapply(data[series_axes], function(value) {
            ifelse(is.na(value), '<none>', format(value, digits=15L))
        })),
        list(drop=TRUE, lex.order=TRUE)
    ))
    group <- .cdrgam_cli_null(columns$group, '.cdrgam_series')
    if (geometry %in% c(.cdrgam_cli_point_geometries, 'bar')) {
        width <- .cdrgam_cli_x_width(data$.cdrgam_x)
        key <- paste(
            data$layer, if (is.null(facet)) '' else as.character(data[[facet$column]]),
            sep='\r'
        )
        series <- as.integer(interaction(data[[group]], drop=TRUE))
        if (geometry %in% .cdrgam_cli_point_geometries) {
            width <- .cdrgam_cli_point_cluster_width(
                data$.cdrgam_x, series, key, width, render$width
            )
        }
        dodge <- .cdrgam_cli_dodge(data$.cdrgam_x, series, key, width)
        data$.cdrgam_x <- dodge$center
        data$.cdrgam_width <- width / dodge$count
        data$.cdrgam_whisker <- data$.cdrgam_width *
            if (identical(geometry, 'bar')) 0.3 else 0.8
        if (identical(geometry, 'bar')) {
            data$.cdrgam_xmin <- data$.cdrgam_x - data$.cdrgam_width / 2
            data$.cdrgam_xmax <- data$.cdrgam_x + data$.cdrgam_width / 2
            data$.cdrgam_ymin <- pmin(data[[columns$y]], 0)
            data$.cdrgam_ymax <- pmax(data[[columns$y]], 0)
        }
    }
    spec$rows <- data
    spec$group <- group
    spec$x <- x[setdiff(names(x), 'position')]
    # Bars are filled by the color mapping, so fill follows color unless
    # fill is mapped to another column.
    spec$color <- .cdrgam_cli_color_spec(
        data, .cdrgam_cli_null(columns$color, columns$fill), render$palette,
        render$reverse_palette
    )
    spec$fill_follows_color <- is.null(columns$fill) ||
        (is.null(columns$color) && !is.null(columns$fill))
    spec
}

# Whether to draw the peak strip. `auto` draws it when the plotted terms
# were held at peaks that differ, since the curves then sit at different
# lags; `always` draws it whenever a peak was used. `lines = TRUE` treats
# `auto` as `always`, since a line at a peak lag needs the strip to show
# where that peak is.
.cdrgam_cli_show_peaks <- function(peaks, setting=NULL, lines=FALSE) {
    setting <- .cdrgam_cli_null(setting, 'auto')
    if (is.null(peaks) || !nrow(peaks) || identical(setting, 'never')) return(FALSE)
    lines || identical(setting, 'always') || length(unique(signif(peaks$lag, 10L))) > 1L
}

# One label per strip row. A term held at its own positive peak is labeled
# by the term alone; other rows also name the peak, as in
# "x <middle dot> peak-neg(irf(1))".
.cdrgam_cli_peak_lanes <- function(peaks) {
    own <- peaks$source == peaks$term
    expression <- paste0(
        ifelse(peaks$direction == 'neg', 'peak-neg', 'peak'),
        ifelse(own, '', paste0('(', peaks$source, ')'))
    )
    plain <- own & peaks$direction == 'pos' &
        !(peaks$term %in% peaks$term[duplicated(peaks$term)])
    ifelse(plain, peaks$term, paste0(peaks$term, ' \u00b7 ', expression))
}

# The effect grid that plots a peak-lag summary: one row per evaluated term,
# source term, and direction, with the peak lag as `estimate` and its
# posterior interval as `lower` and `upper`. `peak` labels the row as the
# peak strip does.
.cdrgam_cli_peak_data <- function(peaks) {
    if (is.null(peaks) || !nrow(peaks)) .cdrgam_cli_abort(paste0(
        'render.statistic peak-lag needs a query whose lag values use a peak, ',
        'such as lag: peak'
    ))
    peaks <- as.data.frame(peaks)
    data.frame(
        term=peaks$term, source=peaks$source, direction=peaks$direction,
        peak=.cdrgam_cli_peak_lanes(peaks), estimate=peaks$lag,
        lower=peaks$lower, upper=peaks$upper, group=NA_character_,
        layer='estimate', stringsAsFactors=FALSE
    )
}

# The specification of the peak timeline: one lane per peak, listed top to
# bottom in `lanes` order; the peaks with their lane; the peaks with a
# finite interval; and the posterior draws with their lane and an opacity
# that keeps dense draws readable. `colors` names a color per term, or is
# NULL to draw every lane in black. `xlim` aligns the timeline with a lag
# x-axis, or is NULL for its own axis. `labels` gives each peak's lane label
# in place of .cdrgam_cli_peak_lanes(). Draws join their peaks on every
# column they share other than draw and lag, so draws that carry a line
# column stay with that line.
.cdrgam_cli_peak_strip_spec <- function(
        peaks, colors=NULL, xlim=NULL, theme='paper', labels=NULL
) {
    draws <- attr(peaks, 'draws')
    peaks <- as.data.frame(peaks)
    lanes <- .cdrgam_cli_null(labels, .cdrgam_cli_peak_lanes(peaks))
    peaks$.lane <- factor(lanes, levels=rev(unique(lanes)))
    alpha <- NULL
    if (!is.null(draws) && nrow(draws)) {
        by <- intersect(setdiff(names(draws), c('draw', 'lag')), names(peaks))
        draws <- merge(
            draws, peaks[unique(c(by, 'term', '.lane'))], by=by
        )
        alpha <- min(1, 25 / max(table(paste(draws$.lane))))
    } else draws <- NULL
    list(
        peaks=peaks, lanes=rev(levels(peaks$.lane)),
        bounded=peaks[is.finite(peaks$lower) & is.finite(peaks$upper), , drop=FALSE],
        draws=draws, draw_alpha=alpha, colors=colors, xlim=xlim,
        label=if (is.null(xlim)) 'Peak lag' else NULL, theme=theme
    )
}

# The peak timeline for a figure, or NULL when none is drawn. The strip
# shares the main plot's x-axis when that axis is lag, unfaceted, and not
# uniformly spaced; its lanes take the main plot's colors when color maps to
# term.
.cdrgam_cli_figure_peaks <- function(data, peaks, render) {
    if (!(render$geometry %in% c('line', .cdrgam_cli_point_geometries)) ||
            identical(render$statistic, 'peak-lag') ||
            !.cdrgam_cli_show_peaks(peaks, render$peaks)) return(NULL)
    mappings <- render$mappings
    shared <- identical(.cdrgam_cli_mapping_column(mappings$x), 'lag') &&
        is.null(mappings$facet) && 'lag' %in% names(data) &&
        !identical(render$spacing, 'uniform')
    by_term <- identical(
        .cdrgam_cli_mapping_column(.cdrgam_cli_null(mappings$color, '')), 'term'
    )
    colors <- if (by_term) .cdrgam_cli_palette_colors(
        render$palette, sort(unique(as.character(data$term))), render$reverse_palette
    )
    .cdrgam_cli_peak_strip_spec(
        peaks, colors,
        xlim=if (shared) range(c(data$lag, peaks$lag), na.rm=TRUE) else NULL,
        theme=.cdrgam_cli_null(render$theme, 'paper')
    )
}

# The specification of the pointwise-peak panel drawn below a plot whose
# lines hold lag at peak-pointwise: each series' peak lag across the main
# plot's x-axis, with the central interval of its posterior draws. `table`
# has one row per point, as from .cdrgam_cli_pointwise_effect(), with the
# main plot's x and color columns; `main` is the main plot specification.
# Series follow the main plot's color column and layer, and take its
# colors. NULL when the main plot is faceted or its x-axis is not numeric
# or not in `table`.
.cdrgam_cli_pointwise_spec <- function(table, main, render) {
    x <- main$columns$x
    if (is.null(table) || !nrow(table) || !is.null(main$facet) ||
            !(x %in% names(table)) || !is.numeric(table[[x]]) ||
            !is.numeric(main$rows[[x]])) {
        return(NULL)
    }
    table <- as.data.frame(table)
    axis <- .cdrgam_cli_x_axis(c(main$rows[[x]], table[[x]]), render)
    color <- main$color$column
    discrete <- !is.null(color) && identical(main$color$kind, 'discrete') &&
        color %in% names(table)
    key <- if (discrete) as.character(table[[color]]) else rep.int('', nrow(table))
    layer <- if ('layer' %in% names(table)) as.character(table$layer) else ''
    rows <- data.frame(
        .x=axis$position[-seq_len(nrow(main$rows))], lag=table$lag,
        lower=table$lower, upper=table$upper, .color=key,
        .series=paste(key, layer, sep='\r'), stringsAsFactors=FALSE
    )
    rows <- rows[order(rows$.series, rows$.x), , drop=FALSE]
    rownames(rows) <- NULL
    list(
        rows=rows, colors=if (discrete) main$color$values else NULL,
        x=main$x, theme=main$theme,
        labels=list(x=main$labels$x, y='Peak lag')
    )
}

# The pointwise-peak panel for a figure, or NULL when none is drawn: under
# lines or points of estimates whose query used peak-pointwise, unless
# render.peaks is never.
.cdrgam_cli_figure_pointwise <- function(data, pointwise, definition) {
    render <- definition$render
    if (is.null(pointwise) ||
            !(render$geometry %in% c('line', .cdrgam_cli_point_geometries)) ||
            identical(render$statistic, 'peak-lag') ||
            identical(render$peaks, 'never')) return(NULL)
    .cdrgam_cli_pointwise_spec(
        pointwise, .cdrgam_cli_plot_spec(data, definition), render
    )
}

# The vertical display limits of a plot specification, or NULL when
# render.ylim is unset. An NA end takes the extent of the plotted values and
# their intervals, padded by 5% of their span, and includes zero under
# y_zero. Limits only zoom the view: no row is dropped.
.cdrgam_cli_spec_ylim <- function(spec) {
    limits <- spec$ylim
    if (is.null(limits)) return(NULL)
    if (anyNA(limits)) {
        rows <- spec$rows
        values <- c(rows[[spec$columns$y]], rows$lower, rows$upper,
            rows$.cdrgam_ymin, rows$.cdrgam_ymax, if (isTRUE(spec$y_zero)) 0)
        values <- values[is.finite(values)]
        extent <- if (length(values)) range(values) else c(0, 1)
        known <- limits[is.finite(limits)]
        extent <- range(c(extent, known))
        pad <- 0.05 * max(diff(extent), abs(extent), 1e-8)
        if (is.na(limits[[1L]])) limits[[1L]] <- min(extent[[1L]] - pad, limits[[2L]] - pad)
        if (is.na(limits[[2L]])) limits[[2L]] <- max(extent[[2L]] + pad, limits[[1L]] + pad)
    }
    limits
}

# The facet values whose vertical axis includes zero, for the zero line:
# NA for an unfaceted plot when its axis does. A panel's axis is
# render.ylim when set, and otherwise the extent of its estimates and
# intervals with ggplot2's 5% padding, per facet under free y scales and
# over all rows otherwise.
.cdrgam_cli_zero_panels <- function(spec) {
    if (!isTRUE(spec$zero_line) || is.null(spec$rows) || !nrow(spec$rows)) {
        return(character())
    }
    covers <- function(rows) {
        limits <- .cdrgam_cli_spec_ylim(c(spec[setdiff(names(spec), 'rows')], list(rows=rows)))
        if (is.null(limits)) {
            # y_zero extends the axis to zero, as the renderers do.
            values <- c(rows[[spec$columns$y]], rows$lower, rows$upper,
                if (isTRUE(spec$y_zero)) 0)
            values <- values[is.finite(values)]
            if (!length(values)) return(FALSE)
            # The drawn axis: ggplot2 pads a continuous range by 5% of its
            # span on each side.
            limits <- range(values)
            limits <- limits + c(-1, 1) * 0.05 * diff(limits)
        }
        limits[[1L]] <= 0 && 0 <= limits[[2L]]
    }
    facet <- spec$facet
    if (is.null(facet)) return(if (covers(spec$rows)) NA_character_ else character())
    values <- as.character(spec$rows[[facet$column]])
    if (!(facet$scales %in% c('free', 'free_y'))) {
        return(if (covers(spec$rows)) unique(values) else character())
    }
    Filter(function(value) covers(spec$rows[values == value, , drop=FALSE]), unique(values))
}
