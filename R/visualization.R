.cdrgam_cli_effect_layers <- function(query, layers) {
    if (is.null(layers)) return(list(list(
        id='estimate', composition=query$composition,
        grouping=query$grouping, groups=query$groups
    )))
    lapply(layers, function(layer) {
        layer$composition <- .cdrgam_cli_null(layer$composition, query$composition)
        layer$grouping <- .cdrgam_cli_null(layer$grouping, query$grouping)
        layer$groups <- .cdrgam_cli_null(layer$groups, query$groups)
        layer
    })
}

# Columns added for a term with exactly one predictor: its name and value,
# so that terms with different predictors can share a predictor-value axis.
.cdrgam_cli_predictor_columns <- function(value, term) {
    predictors <- .cdrgam_cli_term_axes(term)$predictors
    if (length(predictors) != 1L) return(value)
    value$predictor <- predictors
    value$predictor_value <- value[[predictors]]
    value
}

# Evaluate one layer of a query for one term. Expressions, ranges, and
# wildcard references are resolved for the term before the core is called.
.cdrgam_cli_evaluate_term <- function(
        fit, term, query, layer, training, peaks=new.env()
) {
    request <- .cdrgam_cli_term_request(
        fit, term, query, training$impulses, training$responses, peaks
    )
    # References need cdrgam > 0.2.0; omit the argument otherwise so that
    # other queries still run against older core releases.
    reference <- if (is.null(request$reference)) list() else list(
        reference=request$reference
    )
    arguments <- c(list(
        fit,
        terms=term$label,
        axes=request$axes,
        impulses=training$impulses,
        responses=training$responses,
        composition=layer$composition,
        grouping=layer$grouping,
        groups=layer$groups,
        unconditional=query$uncertainty$unconditional,
        n=query$n
    ), reference)
    value <- do.call(.cdrgam_cli_core_estimate_effect, c(
        arguments, list(se=TRUE, level=query$uncertainty$level)
    ))
    if (identical(query$uncertainty$kind, 'simultaneous')) {
        value <- .cdrgam_cli_simultaneous_band(fit, value, list(
            term=term$label, axes_names=term$axes,
            reference=request$reference, composition=layer$composition,
            grouping=layer$grouping, groups=layer$groups,
            unconditional=query$uncertainty$unconditional,
            impulses=training$impulses, responses=training$responses
        ), query$uncertainty$level, query$uncertainty$points)
    }
    test <- if (isTRUE(query$test)) {
        as.data.frame(do.call(.cdrgam_cli_core_test_effect, arguments))
    } else NULL
    list(estimate=.cdrgam_cli_predictor_columns(value, term), test=test)
}

.cdrgam_cli_bind_tables <- function(tables) {
    columns <- unique(unlist(lapply(tables, names), use.names=FALSE))
    tables <- lapply(tables, function(value) {
        value <- as.data.frame(value)
        for (name in setdiff(columns, names(value))) value[[name]] <- NA
        value[columns]
    })
    output <- do.call(rbind, tables)
    rownames(output) <- NULL
    output
}

.cdrgam_cli_execute_effect <- function(item, stage) {
    fit <- readRDS(file.path(item$fit$output, 'fit.rds'))
    catalog <- .cdrgam_cli_core_effect_catalog(fit)
    summaries_complete <- vapply(seq_len(nrow(catalog)), function(index) {
        axes <- catalog$axes[[index]]
        summaries <- catalog$axis_summaries[[index]]
        all(axes %in% names(summaries)) &&
            all(vapply(summaries[axes], function(value) !is.null(value), logical(1)))
    }, logical(1))
    training <- if (all(summaries_complete)) {
        list(impulses=NULL, responses=NULL)
    } else .cdrgam_cli_load_dataset(item$fit$dataset)
    if (!all(summaries_complete)) {
        catalog <- .cdrgam_cli_core_effect_catalog(
            fit, training$impulses, training$responses
        )
    }
    query <- item$query
    layers <- .cdrgam_cli_effect_layers(query, item$layers)
    labels <- .cdrgam_cli_select_terms(
        fit, catalog, query$terms, training$impulses, training$responses
    )
    terms <- lapply(match(labels, catalog$label), function(index) {
        .cdrgam_cli_catalog_row(catalog, index)
    })
    tables <- list()
    tests <- list()
    peaks <- new.env()
    peaks$.catalog <- catalog
    for (layer in layers) {
        for (term in terms) {
            value <- .cdrgam_cli_evaluate_term(fit, term, query, layer, training, peaks)
            value$estimate$layer <- layer$id
            tables[[length(tables) + 1L]] <- value$estimate
            if (!is.null(value$test)) {
                value$test$layer <- rep.int(layer$id, nrow(value$test))
                tests[[length(tests) + 1L]] <- value$test
            }
        }
    }
    if (isTRUE(query$test)) {
        utils::write.csv(
            .cdrgam_cli_bind_tables(tests), file.path(stage, 'effect-tests.csv'),
            row.names=FALSE
        )
    }
    output <- .cdrgam_cli_bind_tables(tables)
    peaks <- .cdrgam_cli_peak_summary(
        fit, peaks, training$impulses, training$responses, query$n,
        .cdrgam_cli_null(query$uncertainty$peak_draws, .cdrgam_cli_peak_draws),
        query$uncertainty$level, query$uncertainty$unconditional
    )
    if (!is.null(peaks)) {
        saveRDS(peaks, file.path(stage, 'effect-peaks.rds'), version=3)
        utils::write.csv(
            peaks, file.path(stage, 'effect-peaks.csv'), row.names=FALSE
        )
        if (!is.null(attr(peaks, 'draws'))) utils::write.csv(
            attr(peaks, 'draws'), file.path(stage, 'effect-peak-draws.csv'),
            row.names=FALSE
        )
    }
    saveRDS(output, file.path(stage, 'effect-grid.rds'), version=3)
    utils::write.csv(output, file.path(stage, 'effect-grid.csv'), row.names=FALSE)
    .cdrgam_cli_write_yaml(
        list(query=query, layers=layers), file.path(stage, 'request.yml')
    )
    list(
        rows=nrow(output), terms=length(unique(output$term_id)),
        layers=length(unique(output$layer)), scale='linear-predictor',
        interval=query$uncertainty$kind, level=query$uncertainty$level
    )
}

# The ggplot2 renderer of plot specifications from R/plot-spec.R.

.cdrgam_cli_ggplot_mapping <- function(mappings, data, include_y=TRUE) {
    mappings <- lapply(mappings, .cdrgam_cli_mapping_column)
    if (!include_y) mappings$y <- NULL
    missing <- setdiff(unlist(mappings, use.names=FALSE), names(data))
    if (length(missing)) .cdrgam_cli_abort(paste0(
        'Visualization mappings reference unavailable effect-grid columns: ',
        paste(missing, collapse=', ')
    ))
    arguments <- lapply(mappings[intersect(
        c('x', 'y', 'color', 'fill', 'linetype', 'group'), names(mappings)
    )], as.name)
    do.call(ggplot2::aes, arguments)
}

# An aes() call from aesthetic = column pairs; NULL columns are left out.
.cdrgam_cli_ggplot_aes <- function(...) {
    columns <- Filter(Negate(is.null), list(...))
    do.call(ggplot2::aes, lapply(columns, as.name))
}

.cdrgam_cli_ggplot_theme <- function(theme) switch(
    .cdrgam_cli_null(theme, 'paper'),
    paper=ggplot2::theme_bw(base_size=10),
    minimal=ggplot2::theme_minimal(base_size=10),
    slides=ggplot2::theme_minimal(base_size=16)
)

# A geom call with fixed aesthetics that are given.
.cdrgam_cli_ggplot_geom <- function(geom, arguments, fixed=list()) {
    do.call(geom, c(arguments, Filter(Negate(is.null), fixed)))
}

# The ggplot2 layers of one effect layer drawn as lines, with the
# aesthetics each maps: a ribbon or dashed bound lines for the interval and
# one line per series.
.cdrgam_cli_ggplot_curves <- function(rows, spec, layer) {
    columns <- spec$columns
    style <- layer$style
    colour <- if (is.null(style$color)) columns$color
    bounded <- any(is.finite(rows$lower))
    layers <- list()
    used <- list()
    if (identical(layer$interval, 'ribbon') && bounded) {
        fill <- .cdrgam_cli_null(columns$fill, columns$color)
        layers <- c(layers, list(.cdrgam_cli_ggplot_geom(ggplot2::geom_ribbon, list(
            data=rows, mapping=.cdrgam_cli_ggplot_aes(
                x='.cdrgam_x', ymin='lower', ymax='upper', fill=fill,
                linetype=columns$linetype, group=spec$group
            ),
            alpha=0.18, inherit.aes=FALSE, linewidth=0
        ), list(fill=style$fill))))
        if (is.null(style$fill)) used$fill <- fill
    }
    layers <- c(layers, list(.cdrgam_cli_ggplot_geom(ggplot2::geom_line, list(
        data=rows, mapping=.cdrgam_cli_ggplot_aes(
            x='.cdrgam_x', y=columns$y, colour=colour,
            linetype=columns$linetype, group=spec$group
        ),
        alpha=style$alpha, linewidth=style$linewidth, inherit.aes=FALSE
    ), list(colour=style$color))))
    used$colour <- colour
    if (identical(layer$interval, 'lines') && bounded) {
        for (bound in c('lower', 'upper')) {
            layers <- c(layers, list(.cdrgam_cli_ggplot_geom(ggplot2::geom_line, list(
                data=rows, mapping=.cdrgam_cli_ggplot_aes(
                    x='.cdrgam_x', y=bound, colour=colour,
                    linetype=columns$linetype, group=spec$group
                ),
                alpha=style$alpha * 0.65, linewidth=style$linewidth * 0.7,
                linetype=2, inherit.aes=FALSE
            ), list(colour=style$color))))
        }
    }
    list(layers=layers, used=used)
}

# The ggplot2 layers of one effect layer drawn as points or bars, at the
# dodged positions the specification gives.
.cdrgam_cli_ggplot_points <- function(rows, spec, layer) {
    columns <- spec$columns
    style <- layer$style
    bar <- identical(spec$geometry, 'bar')
    layers <- list()
    used <- list()
    if (bar) {
        fixed <- .cdrgam_cli_null(style$fill, style$color)
        fill <- if (is.null(fixed)) .cdrgam_cli_null(columns$fill, columns$color)
        layers <- c(layers, list(.cdrgam_cli_ggplot_geom(ggplot2::geom_rect, list(
            data=rows, mapping=.cdrgam_cli_ggplot_aes(
                xmin='.cdrgam_xmin', xmax='.cdrgam_xmax', ymin='.cdrgam_ymin',
                ymax='.cdrgam_ymax', fill=fill, group=spec$group
            ),
            alpha=style$alpha, inherit.aes=FALSE
        ), list(fill=fixed))))
        used$fill <- fill
    }
    colour <- if (!bar && is.null(style$color)) columns$color
    if (identical(layer$interval, 'errorbar') && any(is.finite(rows$lower))) {
        layers <- c(layers, list(.cdrgam_cli_ggplot_geom(ggplot2::geom_errorbar, list(
            data=rows, mapping=.cdrgam_cli_ggplot_aes(
                x='.cdrgam_x', ymin='lower', ymax='upper', width='.cdrgam_whisker',
                colour=colour, linetype=columns$linetype, group=spec$group
            ),
            alpha=style$alpha, linewidth=style$linewidth * 0.7, inherit.aes=FALSE
        ), list(colour=if (bar) 'grey20' else style$color))))
    }
    if (!bar) {
        layers <- c(layers, list(.cdrgam_cli_ggplot_geom(ggplot2::geom_point, list(
            data=rows, mapping=.cdrgam_cli_ggplot_aes(
                x='.cdrgam_x', y=columns$y, colour=colour,
                linetype=columns$linetype, group=spec$group
            ),
            alpha=style$alpha, size=style$linewidth * 2.5, inherit.aes=FALSE
        ), list(colour=style$color))))
    }
    used$colour <- colour
    list(layers=layers, used=used)
}

# Color and fill scales for the aesthetics that map the specification's
# color column.
.cdrgam_cli_ggplot_color_scales <- function(color, aesthetics) {
    if (is.null(color) || !length(aesthetics)) return(list())
    list(if (identical(color$kind, 'continuous')) {
        ggplot2::scale_colour_gradientn(colours=color$stops, aesthetics=aesthetics)
    } else ggplot2::scale_colour_manual(
        values=color$values, breaks=color$levels, labels=color$labels,
        aesthetics=aesthetics
    ))
}

.cdrgam_cli_ggplot_surface <- function(spec) {
    data <- spec$rows
    columns <- spec$columns
    plot <- ggplot2::ggplot()
    if (spec$surface$raster) {
        plot <- plot + ggplot2::geom_tile(
            data=data, mapping=.cdrgam_cli_ggplot_mapping(
                columns[setdiff(names(columns), 'facet')], data
            )
        )
    }
    if (spec$surface$contour) {
        contour <- list(x=columns$x, y=columns$y, z=spec$surface$z)
        missing <- setdiff(unlist(contour), names(data))
        if (length(missing)) .cdrgam_cli_abort(paste0(
            'Contour mappings reference unavailable columns: ',
            paste(missing, collapse=', ')
        ))
        plot <- plot + ggplot2::geom_contour(
            data=data, mapping=do.call(ggplot2::aes, lapply(contour, as.name)),
            color='grey20', linewidth=0.35, inherit.aes=FALSE
        )
    }
    plot + if (identical(spec$surface$scale, 'se')) {
        ggplot2::scale_fill_gradient(low='white', high='#2166AC')
    } else ggplot2::scale_fill_gradient2(
        low='#2166AC', mid='white', high='#B2182B', midpoint=0
    )
}

# Draw a plot specification with ggplot2.
.cdrgam_cli_ggplot_plot <- function(spec) {
    if (!is.null(spec$surface)) {
        plot <- .cdrgam_cli_ggplot_surface(spec)
    } else {
        plot <- ggplot2::ggplot()
        used <- character()
        for (layer in spec$layers) {
            rows <- spec$rows[spec$rows$layer == layer$id, , drop=FALSE]
            drawn <- if (identical(spec$geometry, 'line')) {
                .cdrgam_cli_ggplot_curves(rows, spec, layer)
            } else .cdrgam_cli_ggplot_points(rows, spec, layer)
            for (item in drawn$layers) plot <- plot + item
            mapped <- unlist(drawn$used)
            used <- union(used, names(mapped)[mapped %in% spec$color$column])
        }
        if (!identical(spec$x$kind, 'continuous')) {
            plot <- plot + ggplot2::scale_x_continuous(
                breaks=spec$x$breaks, labels=spec$x$labels,
                expand=if (identical(spec$x$kind, 'discrete')) {
                    ggplot2::expansion(add=0.6)
                } else ggplot2::waiver()
            )
        }
        if (identical(spec$geometry, 'bar')) {
            plot <- plot + ggplot2::geom_hline(yintercept=0, linewidth=0.3)
        }
        plot <- plot + .cdrgam_cli_ggplot_color_scales(spec$color, used)
    }
    if (!is.null(spec$facet)) plot <- plot + ggplot2::facet_wrap(
        stats::as.formula(paste('~', spec$facet$column)), scales=spec$facet$scales
    )
    plot + ggplot2::labs(title=spec$labels$title, x=spec$labels$x, y=spec$labels$y) +
        .cdrgam_cli_ggplot_theme(spec$theme)
}

.cdrgam_cli_visualization_plot <- function(data, definition) {
    .cdrgam_cli_ggplot_plot(.cdrgam_cli_plot_spec(data, definition))
}

# Draw a peak-strip specification with ggplot2: a long tick at each peak's
# point estimate, the central interval of its posterior draws as a bar, and
# the draws as faint ticks, so that draws split between separate lags show
# both clusters.
.cdrgam_cli_ggplot_peak_strip <- function(strip) {
    by_term <- !is.null(strip$colors)
    aesthetics <- function(...) .cdrgam_cli_ggplot_aes(
        ..., colour=if (by_term) 'term'
    )
    fixed <- list(colour=if (by_term) NULL else 'black')
    plot <- ggplot2::ggplot()
    if (!is.null(strip$draws)) plot <- plot + .cdrgam_cli_ggplot_geom(
        ggplot2::geom_point, list(
            data=strip$draws, mapping=aesthetics(x='lag', y='.lane'),
            shape='|', size=3, alpha=strip$draw_alpha
        ), fixed
    )
    if (nrow(strip$bounded)) plot <- plot + .cdrgam_cli_ggplot_geom(
        ggplot2::geom_linerange, list(
            data=strip$bounded, mapping=aesthetics(xmin='lower', xmax='upper', y='.lane'),
            linewidth=0.6
        ), fixed
    )
    plot <- plot + .cdrgam_cli_ggplot_geom(ggplot2::geom_point, list(
        data=strip$peaks, mapping=aesthetics(x='lag', y='.lane'), shape='|', size=7
    ), fixed)
    if (by_term) plot <- plot + ggplot2::scale_colour_manual(
        values=strip$colors, limits=names(strip$colors), guide='none'
    )
    # Lanes keep the specification's order whichever lanes have draws.
    plot <- plot + ggplot2::scale_y_discrete(limits=levels(strip$peaks$.lane))
    if (!is.null(strip$xlim)) plot <- plot + ggplot2::coord_cartesian(xlim=strip$xlim)
    plot + ggplot2::labs(x=strip$label, y=NULL) + .cdrgam_cli_ggplot_theme(strip$theme)
}

# Stack ggplots into one gtable whose panels share left and right edges.
# Each plot is padded on the left and right to the widest plot's axis and
# legend widths. `heights` are relative. Grobs are built on a null device,
# so no default device file is opened.
.cdrgam_cli_stack_plots <- function(plots, heights) {
    current <- grDevices::dev.cur()
    grDevices::pdf(NULL)
    device <- grDevices::dev.cur()
    on.exit({
        grDevices::dev.off(device)
        if (current > 1L) grDevices::dev.set(current)
    }, add=TRUE)
    grobs <- lapply(plots, ggplot2::ggplotGrob)
    edges <- lapply(grobs, function(grob) {
        panels <- grob$layout[grepl('^panel', grob$layout$name), , drop=FALSE]
        left <- min(panels$l)
        right <- max(panels$r)
        list(
            left=sum(grid::convertWidth(grob$widths[seq_len(left - 1L)], 'cm', TRUE)),
            right=sum(grid::convertWidth(
                grob$widths[seq.int(right + 1L, length.out=length(grob$widths) - right)],
                'cm', TRUE
            ))
        )
    })
    left <- max(vapply(edges, `[[`, numeric(1), 'left'))
    right <- max(vapply(edges, `[[`, numeric(1), 'right'))
    output <- gtable::gtable(
        widths=grid::unit(1, 'null'), heights=grid::unit(heights, 'null')
    )
    for (index in seq_along(grobs)) {
        grob <- gtable::gtable_add_cols(
            grobs[[index]], grid::unit(left - edges[[index]]$left, 'cm'), pos=0L
        )
        grob <- gtable::gtable_add_cols(
            grob, grid::unit(right - edges[[index]]$right, 'cm'), pos=-1L
        )
        output <- gtable::gtable_add_grob(output, grob, t=index, l=1L, name=paste0('plot-', index))
    }
    output
}

# The rendered figure: the main plot alone, or with the peak strip below.
.cdrgam_cli_peak_figure <- function(plot, data, peaks, definition) {
    strip <- .cdrgam_cli_figure_peaks(data, peaks, definition$render)
    if (is.null(strip)) return(NULL)
    drawn <- .cdrgam_cli_ggplot_peak_strip(strip)
    if (!is.null(strip$xlim)) plot <- plot + ggplot2::coord_cartesian(xlim=strip$xlim)
    list(
        strip=drawn,
        figure=.cdrgam_cli_stack_plots(
            list(plot, drawn), c(1, min(0.6, 0.1 + 0.05 * length(strip$lanes)))
        )
    )
}

.cdrgam_cli_save_ggplot <- function(plot, stage, render, stem='visualization') {
    width <- .cdrgam_cli_null(render$width, 8)
    height <- .cdrgam_cli_null(render$height, 5)
    dpi <- .cdrgam_cli_null(render$dpi, 300)
    paths <- character()
    for (format in render$formats) {
        path <- file.path(stage, paste0(stem, '.', format))
        device <- switch(
            format,
            pdf=grDevices::pdf,
            png=grDevices::png,
            svg=grDevices::svg
        )
        ggplot2::ggsave(
            path, plot=plot, device=device,
            width=width, height=height, dpi=dpi, units='in'
        )
        paths <- c(paths, basename(path))
    }
    paths
}

.cdrgam_cli_render_effect <- function(item, stage) {
    data <- readRDS(file.path(item$effect$output, 'effect-grid.rds'))
    peaks_path <- file.path(item$effect$output, 'effect-peaks.rds')
    peaks <- if (file.exists(peaks_path)) readRDS(peaks_path) else NULL
    if (identical(item$definition$render$statistic, 'peak-lag')) {
        data <- .cdrgam_cli_peak_data(peaks)
    }
    plot <- .cdrgam_cli_visualization_plot(data, item$definition)
    saveRDS(plot, file.path(stage, 'plot.rds'), version=3)
    figure <- .cdrgam_cli_peak_figure(plot, data, peaks, item$definition)
    if (!is.null(figure)) {
        saveRDS(figure$strip, file.path(stage, 'plot-peaks.rds'), version=3)
    }
    paths <- .cdrgam_cli_save_ggplot(
        if (is.null(figure)) plot else figure$figure, stage, item$definition$render
    )
    uncertainty_paths <- character()
    if (identical(item$definition$render$interval, 'companion')) {
        uncertainty <- item$definition
        uncertainty$render$mappings$fill <- 'se'
        uncertainty$render$title <- paste(
            .cdrgam_cli_null(uncertainty$render$title, item$name),
            'pointwise standard error'
        )
        uncertainty_plot <- .cdrgam_cli_visualization_plot(data, uncertainty)
        uncertainty_paths <- .cdrgam_cli_save_ggplot(
            uncertainty_plot, stage, uncertainty$render,
            stem='visualization-uncertainty'
        )
    }
    list(
        kind='effect', rows=nrow(data), files=c(paths, uncertainty_paths),
        effect_identity=item$effect$identity
    )
}
