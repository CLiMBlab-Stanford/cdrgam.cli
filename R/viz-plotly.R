# The viewer's plotly renderer of plot specifications from R/plot-spec.R.
# It draws what the ggplot2 renderer draws, with plotly's own line widths and
# marker sizes. Facets and linetype mappings are not drawn: the viewer never
# sets them.

# Sizes in plotly pixels per ggplot2 unit: a 0.8 linewidth is drawn 2 px wide,
# and a point of ggplot2 size 2 is drawn 6 px across.
.cdrgam_cli_plotly_line_scale <- 2.5
.cdrgam_cli_plotly_point_scale <- 3

.cdrgam_cli_viz_rgba <- function(color, alpha) {
    value <- grDevices::col2rgb(color)
    sprintf('rgba(%d,%d,%d,%.2f)', value[1L], value[2L], value[3L], alpha)
}

# The color of each row under the specification's color mapping, or the
# fixed color when one is given. `default` is used when color is unmapped.
.cdrgam_cli_spec_colors <- function(spec, rows, fixed=NULL, default='#000000') {
    if (!is.null(fixed)) return(rep.int(fixed, nrow(rows)))
    color <- spec$color
    if (is.null(color)) return(rep.int(default, nrow(rows)))
    values <- rows[[color$column]]
    if (identical(color$kind, 'continuous')) {
        span <- diff(color$limits)
        scaled <- if (span > 0) (values - color$limits[[1L]]) / span else rep(0.5, length(values))
        ramp <- grDevices::colorRamp(color$stops, space='Lab')
        return(grDevices::rgb(ramp(pmin(pmax(scaled, 0), 1)), maxColorValue=255))
    }
    unname(color$values[as.character(values)])
}

# Hover text for rows: the x value under the axis it belongs to, the
# estimate, and its interval.
.cdrgam_cli_plotly_hover <- function(spec, rows) {
    axis <- if ('.cdrgam_axis' %in% names(rows)) rows$.cdrgam_axis else spec$labels$x
    x <- rows[[spec$columns$x]]
    shown <- if (is.numeric(x)) signif(x, 4L) else as.character(x)
    sprintf(
        '%s = %s<br>estimate %s<br>CI [%s, %s]', axis, shown,
        signif(rows[[spec$columns$y]], 4L), signif(rows$lower, 4L), signif(rows$upper, 4L)
    )
}

# Error bars as one trace: a vertical bar and two caps per row, separated by
# gaps, with caps as wide in data units as the specification's whiskers.
.cdrgam_cli_plotly_errorbars <- function(plot, rows, color, width, opacity, group) {
    rows <- rows[is.finite(rows$lower) & is.finite(rows$upper), , drop=FALSE]
    if (!nrow(rows)) return(plot)
    half <- rows$.cdrgam_whisker / 2
    x <- rows$.cdrgam_x
    segment <- function(a, b) as.vector(rbind(a, b, NA))
    plotly::add_trace(
        plot, type='scatter', mode='lines',
        x=c(segment(x, x), segment(x - half, x + half), segment(x - half, x + half)),
        y=c(segment(rows$lower, rows$upper), segment(rows$lower, rows$lower),
            segment(rows$upper, rows$upper)),
        line=list(color=color, width=width), opacity=opacity,
        legendgroup=group, showlegend=FALSE, hoverinfo='skip'
    )
}

# The traces of one series in one layer.
.cdrgam_cli_plotly_series <- function(plot, spec, rows, layer, name, group, legend) {
    style <- layer$style
    width <- style$linewidth * .cdrgam_cli_plotly_line_scale
    bounded <- any(is.finite(rows$lower))
    hover <- .cdrgam_cli_plotly_hover(spec, rows)
    if (identical(spec$geometry, 'line')) {
        rows <- rows[order(rows$.cdrgam_x), , drop=FALSE]
        hover <- .cdrgam_cli_plotly_hover(spec, rows)
        color <- .cdrgam_cli_spec_colors(spec, rows, style$color)[[1L]]
        if (identical(layer$interval, 'ribbon') && bounded) {
            fill <- .cdrgam_cli_null(style$fill, color)
            plot <- plotly::add_ribbons(
                plot, x=rows$.cdrgam_x, ymin=rows$lower, ymax=rows$upper,
                fillcolor=.cdrgam_cli_viz_rgba(fill, 0.18), line=list(width=0),
                legendgroup=group, showlegend=FALSE, hoverinfo='skip', name=name
            )
        }
        plot <- plotly::add_lines(
            plot, x=rows$.cdrgam_x, y=rows[[spec$columns$y]], name=name,
            legendgroup=group, showlegend=legend, opacity=style$alpha,
            line=list(color=color, width=width), text=hover, hoverinfo='text'
        )
        if (identical(layer$interval, 'lines') && bounded) {
            for (bound in c('lower', 'upper')) plot <- plotly::add_lines(
                plot, x=rows$.cdrgam_x, y=rows[[bound]], name=name,
                legendgroup=group, showlegend=FALSE, opacity=style$alpha * 0.65,
                line=list(color=color, width=width * 0.7, dash='dash'), hoverinfo='skip'
            )
        }
        return(plot)
    }
    errorbars <- identical(layer$interval, 'errorbar') && bounded
    if (identical(spec$geometry, 'bar')) {
        fill <- .cdrgam_cli_spec_colors(
            spec, rows, .cdrgam_cli_null(style$fill, style$color), default='grey35'
        )
        plot <- plotly::add_bars(
            plot, x=rows$.cdrgam_x, y=rows$.cdrgam_ymax - rows$.cdrgam_ymin,
            base=rows$.cdrgam_ymin, width=rows$.cdrgam_xmax - rows$.cdrgam_xmin,
            marker=list(color=fill, line=list(width=0)), opacity=style$alpha,
            name=name, legendgroup=group, showlegend=legend, text=hover, hoverinfo='text'
        )
        if (errorbars) plot <- .cdrgam_cli_plotly_errorbars(
            plot, rows, 'grey20', width * 0.7, style$alpha, group
        )
        return(plot)
    }
    color <- .cdrgam_cli_spec_colors(spec, rows, style$color)
    if (errorbars) plot <- .cdrgam_cli_plotly_errorbars(
        plot, rows, color[[1L]], width * 0.7, style$alpha, group
    )
    if (identical(spec$geometry, 'connected-point')) {
        ordered <- order(rows$.cdrgam_x)
        plot <- plotly::add_lines(
            plot, x=rows$.cdrgam_x[ordered], y=rows[[spec$columns$y]][ordered],
            name=name, legendgroup=group, showlegend=FALSE, opacity=style$alpha,
            line=list(color=color[[1L]], width=width * 0.5), hoverinfo='skip'
        )
    }
    plotly::add_markers(
        plot, x=rows$.cdrgam_x, y=rows[[spec$columns$y]], name=name,
        marker=list(
            color=color, size=style$linewidth * 2.5 * .cdrgam_cli_plotly_point_scale
        ),
        opacity=style$alpha, legendgroup=group, showlegend=legend,
        text=hover, hoverinfo='text'
    )
}

.cdrgam_cli_plotly_font <- function(theme) {
    list(size=if (identical(theme, 'slides')) 16 else 12)
}

# The plotly counterpart of a ggplot2 theme: panel background, grid, and
# axis lines, as list(panel, axis), where `axis` styles both axes.
.cdrgam_cli_plotly_theme <- function(theme) {
    theme <- .cdrgam_cli_null(theme, 'paper')
    grid <- '#EBEBEB'
    box <- function(color) list(showline=TRUE, linecolor=color, mirror=TRUE)
    style <- switch(
        theme,
        grey=, gray=list(panel='#EBEBEB', axis=list(gridcolor='#FFFFFF')),
        dark=list(panel='#7F7F7F', axis=list(gridcolor='#A8A8A8')),
        # paper keeps the viewer's plain grid, without a panel outline.
        paper=list(panel='#FFFFFF', axis=list(gridcolor=grid)),
        bw=, test=list(panel='#FFFFFF', axis=c(list(gridcolor=grid), box('#333333'))),
        linedraw=list(panel='#FFFFFF', axis=c(list(gridcolor='#BFBFBF'), box('#000000'))),
        light=list(panel='#FFFFFF', axis=c(list(gridcolor='#DEDEDE'), box('#B3B3B3'))),
        classic=list(panel='#FFFFFF', axis=list(
            showgrid=FALSE, showline=TRUE, linecolor='#000000', mirror=FALSE
        )),
        void=list(panel='#FFFFFF', axis=list(
            showgrid=FALSE, showline=FALSE, showticklabels=FALSE, title=list(text='')
        )),
        list(panel='#FFFFFF', axis=list(gridcolor=grid))
    )
    style
}

# Apply a theme to a plotly plot: `xaxis` and `yaxis` are the plot's own
# axis settings, which take precedence over the theme's.
.cdrgam_cli_plotly_themed <- function(plot, theme, xaxis=list(), yaxis=list(), ...) {
    style <- .cdrgam_cli_plotly_theme(theme)
    merge <- function(axis) {
        value <- utils::modifyList(style$axis, axis)
        if (identical(theme, 'void')) value <- utils::modifyList(value, style$axis)
        value
    }
    plotly::layout(
        plot, font=.cdrgam_cli_plotly_font(theme), plot_bgcolor=style$panel,
        xaxis=merge(xaxis), yaxis=merge(yaxis), ...
    )
}

# The diverging or standard-error color scale of a surface, as in the
# ggplot2 renderer, with its limits.
.cdrgam_cli_plotly_colorscale <- function(scale, z) {
    if (identical(scale, 'se')) return(list(
        colorscale=list(list(0, '#FFFFFF'), list(1, '#2166AC')),
        zmin=min(z, na.rm=TRUE), zmax=max(z, na.rm=TRUE)
    ))
    limit <- max(abs(z), na.rm=TRUE)
    if (!is.finite(limit) || limit == 0) limit <- 1
    list(
        colorscale=list(list(0, '#2166AC'), list(0.5, '#FFFFFF'), list(1, '#B2182B')),
        zmin=-limit, zmax=limit
    )
}

.cdrgam_cli_plotly_surface <- function(spec, colorbar=list()) {
    columns <- spec$columns
    values <- .cdrgam_cli_viz_surface_matrix(
        spec$rows, columns$x, columns$y, spec$surface$z
    )
    scale <- .cdrgam_cli_plotly_colorscale(spec$surface$scale, values$z)
    plot <- plotly::plot_ly()
    if (spec$surface$raster) plot <- plotly::add_trace(
        plot, type='heatmap', x=values$x, y=values$y, z=values$z,
        colorscale=scale$colorscale, zmin=scale$zmin, zmax=scale$zmax,
        colorbar=utils::modifyList(list(title=spec$surface$z), colorbar)
    )
    if (spec$surface$contour) plot <- plotly::add_trace(
        plot, type='contour', x=values$x, y=values$y, z=values$z,
        contours=list(coloring='none'), line=list(color='#333333', width=1),
        showscale=FALSE, hoverinfo='skip'
    )
    plot
}

# Draw a plot specification with plotly. `colorbar` adjusts a surface's
# color bar, for stacking surfaces.
.cdrgam_cli_plotly_plot <- function(spec, colorbar=list()) {
    if (!is.null(spec$facet) || !is.null(spec$columns$linetype)) .cdrgam_cli_abort(
        'The viewer does not draw facet or linetype mappings'
    )
    if (!is.null(spec$surface)) {
        plot <- .cdrgam_cli_plotly_surface(spec, colorbar)
        return(plotly::layout(
            plot, title=spec$labels$title, font=.cdrgam_cli_plotly_font(spec$theme),
            xaxis=list(title=spec$labels$x), yaxis=list(title=spec$labels$y)
        ))
    }
    plot <- plotly::plot_ly()
    color <- spec$color
    shown <- character()
    for (layer in spec$layers) {
        rows <- spec$rows[spec$rows$layer == layer$id, , drop=FALSE]
        series <- rows[[spec$group]]
        for (key in levels(droplevels(as.factor(series)))) {
            selected <- rows[as.character(series) == key, , drop=FALSE]
            # One legend entry per color level, shared by its series.
            group <- if (identical(color$kind, 'discrete')) {
                as.character(selected[[color$column]][[1L]])
            } else key
            name <- if (identical(color$kind, 'discrete')) {
                color$labels[[match(group, color$levels)]]
            } else key
            plot <- .cdrgam_cli_plotly_series(
                plot, spec, selected, layer, name, group,
                legend=!is.null(color) && !(group %in% shown)
            )
            shown <- c(shown, group)
        }
    }
    xaxis <- list(title=spec$labels$x, zeroline=FALSE)
    if (!identical(spec$x$kind, 'continuous')) {
        xaxis$tickvals <- spec$x$breaks
        xaxis$ticktext <- spec$x$labels
    }
    yaxis <- list(title=spec$labels$y, zeroline=!isFALSE(spec$zero_line))
    if (isTRUE(spec$y_zero)) yaxis$rangemode <- 'tozero'
    ylim <- .cdrgam_cli_spec_ylim(spec)
    if (!is.null(ylim)) yaxis$range <- ylim
    .cdrgam_cli_plotly_themed(
        plot, spec$theme, xaxis=xaxis, yaxis=yaxis, title=spec$labels$title,
        legend=list(orientation='h', y=-0.2), hovermode='closest', barmode='overlay'
    )
}

# Draw a peak-strip specification with plotly: the draws as faint ticks, the
# central interval as a bar, and the point estimate as a long tick, in each
# lane's color. Lanes sit at 1, 2, ... from the bottom, and every tick spans
# at most its own lane, so lanes never overlap.
.cdrgam_cli_plotly_peak_strip <- function(strip) {
    plot <- plotly::plot_ly()
    peaks <- strip$peaks
    lanes <- strip$lanes
    level <- function(lane) length(lanes) + 1L - match(lane, lanes)
    color_of <- function(term) {
        if (is.null(strip$colors)) '#000000' else unname(strip$colors[[term]])
    }
    # Vertical ticks at `x`, each from y - half to y + half, as one trace.
    ticks <- function(plot, x, y, half, color, width, opacity=1) {
        plotly::add_trace(
            plot, type='scatter', mode='lines',
            x=as.vector(rbind(x, x, NA)), y=as.vector(rbind(y - half, y + half, NA)),
            line=list(color=color, width=width), opacity=opacity,
            hoverinfo='skip', showlegend=FALSE
        )
    }
    for (row in seq_len(nrow(peaks))) {
        peak <- peaks[row, , drop=FALSE]
        lane <- as.character(peak$.lane)
        y <- level(lane)
        color <- color_of(peak$term)
        draws <- if (is.null(strip$draws)) numeric() else {
            keep <- as.character(strip$draws$.lane) == lane &
                strip$draws$source == peak$source & strip$draws$direction == peak$direction
            strip$draws$lag[keep]
        }
        if (length(draws)) plot <- ticks(
            plot, draws, rep.int(y, length(draws)), 0.2, color, 1,
            .cdrgam_cli_null(strip$draw_alpha, 1)
        )
        if (is.finite(peak$lower) && is.finite(peak$upper)) {
            plot <- plotly::add_segments(
                plot, x=peak$lower, xend=peak$upper, y=y, yend=y,
                line=list(color=color, width=2), hoverinfo='skip', showlegend=FALSE
            )
        }
        plot <- ticks(plot, peak$lag, y, 0.4, color, 3)
        plot <- plotly::add_markers(
            plot, x=peak$lag, y=y, marker=list(size=12, color=color, opacity=0),
            text=sprintf(
                '%s<br>peak lag %s%s', lane, signif(peak$lag, 4L),
                if (is.finite(peak$lower)) sprintf(
                    '<br>draws [%s, %s]', signif(peak$lower, 4L), signif(peak$upper, 4L)
                ) else ''
            ),
            hoverinfo='text', showlegend=FALSE
        )
    }
    .cdrgam_cli_plotly_themed(
        plot, strip$theme,
        xaxis=list(title=.cdrgam_cli_null(strip$label, ''), zeroline=FALSE),
        yaxis=list(
            title='', range=c(0.5, length(lanes) + 0.5), zeroline=FALSE,
            showgrid=FALSE, fixedrange=TRUE,
            # Label every lane; automatic ticks skip some in a short strip.
            tickmode='array', tickvals=rev(seq_along(lanes)), ticktext=lanes
        )
    )
}

# A plot with its pointwise-peak panel and peak strip below. Panels whose
# x-axis matches the panel below drop their x title and tick labels, which
# the lower panel shows; every other panel keeps room below for its own.
.cdrgam_cli_plotly_figure <- function(plot, strip, spec, pointwise=NULL) {
    if (is.null(strip) && is.null(pointwise)) return(plot)
    plots <- list(plot)
    heights <- 1
    # Whether each panel's x-axis is the same as the next panel's.
    linked <- logical()
    if (!is.null(pointwise)) {
        plots <- c(plots, list(.cdrgam_cli_plotly_pointwise(pointwise)))
        heights <- c(heights, 0.45)
        linked <- c(linked, TRUE)
    }
    shared <- !is.null(strip$xlim)
    if (!is.null(strip)) {
        if (shared) strip$label <- spec$labels$x
        plots <- c(plots, list(.cdrgam_cli_plotly_peak_strip(strip)))
        heights <- c(heights, min(0.4, 0.1 + 0.06 * length(strip$lanes)))
        linked <- c(linked, shared && is.null(pointwise))
    }
    linked <- c(linked, FALSE)
    figure <- plotly::subplot(
        plots, nrows=length(plots), titleX=TRUE, titleY=TRUE
    )
    # Gaps below each panel, as fractions of the figure: tick labels and a
    # title, or a sliver above a panel that repeats the axis.
    gaps <- ifelse(linked, 0.03, 0.13)[-length(plots)]
    space <- 1 - sum(gaps)
    sizes <- space * heights / sum(heights)
    top <- 1
    axes <- list()
    for (index in seq_along(plots)) {
        suffix <- if (index == 1L) '' else as.character(index)
        bottom <- top - sizes[[index]]
        axes[[paste0('yaxis', suffix)]] <- list(domain=c(max(0, bottom), top))
        if (linked[[index]]) {
            axes[[paste0('xaxis', suffix)]] <- list(
                title=list(text=''), showticklabels=FALSE,
                matches=paste0('x', if (index + 1L == 1L) '' else index + 1L)
            )
        }
        if (index < length(plots)) top <- bottom - gaps[[index]]
    }
    do.call(plotly::layout, c(list(figure), axes))
}

# Draw a pointwise-peak panel specification with plotly: each series' peak
# lag as a line over its posterior interval band.
.cdrgam_cli_plotly_pointwise <- function(panel) {
    plot <- plotly::plot_ly()
    rows <- panel$rows
    for (series in unique(rows$.series)) {
        selected <- rows[rows$.series == series, , drop=FALSE]
        color <- if (is.null(panel$colors)) '#000000' else
            unname(panel$colors[[selected$.color[[1L]]]])
        bounded <- selected[is.finite(selected$lower) & is.finite(selected$upper), ,
            drop=FALSE]
        if (nrow(bounded)) plot <- plotly::add_ribbons(
            plot, x=bounded$.x, ymin=bounded$lower, ymax=bounded$upper,
            fillcolor=.cdrgam_cli_viz_rgba(color, 0.2), line=list(width=0),
            hoverinfo='skip', showlegend=FALSE
        )
        plot <- plotly::add_lines(
            plot, x=selected$.x, y=selected$lag, line=list(color=color, width=2),
            text=sprintf('peak lag %s', signif(selected$lag, 4L)),
            hoverinfo='x+text', showlegend=FALSE
        )
    }
    xaxis <- list(title=panel$labels$x, zeroline=FALSE)
    if (!identical(panel$x$kind, 'continuous')) {
        xaxis$tickvals <- panel$x$breaks
        xaxis$ticktext <- panel$x$labels
    }
    .cdrgam_cli_plotly_themed(
        plot, panel$theme, xaxis=xaxis, yaxis=list(title=panel$labels$y, zeroline=FALSE)
    )
}
