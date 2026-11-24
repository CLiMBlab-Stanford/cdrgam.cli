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
        fit, term, query, training$impulses, training$responses, peaks,
        layer$composition
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
    direction <- .cdrgam_cli_pointwise_direction(query$axes$lag)
    if (!is.null(direction)) {
        value <- .cdrgam_cli_pointwise_effect(
            fit, arguments[-1L], direction,
            draws=.cdrgam_cli_null(query$uncertainty$peak_draws, .cdrgam_cli_peak_draws),
            level=query$uncertainty$level,
            simultaneous=identical(query$uncertainty$kind, 'simultaneous')
        )
        pointwise <- attr(value, 'pointwise_peaks')
        pointwise$layer <- layer$id
        return(list(
            estimate=.cdrgam_cli_predictor_columns(value, term), test=NULL,
            pointwise=.cdrgam_cli_predictor_columns(pointwise, term)
        ))
    }
    value <- do.call(.cdrgam_cli_core_estimate_effect, c(
        arguments, list(se=TRUE, level=query$uncertainty$level)
    ))
    used <- peaks$.used
    if (!is.null(used)) used <- used[used$term == term$label, , drop=FALSE]
    # A query whose values use peaks gets a band that re-finds them on every
    # draw; other queries keep the analytic band.
    peaked <- identical(query$uncertainty$kind, 'simultaneous') && !is.null(used) &&
        nrow(used) > 0L
    if (peaked) {
        if (is.null(peaks$.catalog)) {
            peaks$.catalog <- .cdrgam_cli_core_effect_catalog(
                fit, training$impulses, training$responses
            )
        }
        value <- .cdrgam_cli_peak_band(
            fit, value, arguments[-1L], used, function(cache) {
                resolved <- .cdrgam_cli_term_request(
                    fit, term, query, training$impulses, training$responses, cache,
                    layer$composition
                )
                list(axes=resolved$axes, reference=resolved$reference)
            }, peaks$.catalog,
            .cdrgam_cli_null(query$uncertainty$peak_draws, .cdrgam_cli_peak_draws),
            query$uncertainty$level
        )
    }
    if (!peaked && identical(query$uncertainty$kind, 'simultaneous')) {
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

# The training data an effect needs: none when the fit's effect catalog
# stores every axis summary, else the fit's training dataset, with the
# catalog recomputed from it.
.cdrgam_cli_effect_training <- function(fit, dataset) {
    catalog <- .cdrgam_cli_core_effect_catalog(fit)
    summaries_complete <- vapply(seq_len(nrow(catalog)), function(index) {
        axes <- catalog$axes[[index]]
        summaries <- catalog$axis_summaries[[index]]
        all(axes %in% names(summaries)) &&
            all(vapply(summaries[axes], function(value) !is.null(value), logical(1)))
    }, logical(1))
    if (all(summaries_complete)) {
        return(list(impulses=NULL, responses=NULL, catalog=catalog))
    }
    training <- .cdrgam_cli_load_dataset(dataset)
    training$catalog <- .cdrgam_cli_core_effect_catalog(
        fit, training$impulses, training$responses
    )
    training
}

# Evaluate a validated effect query in memory: the effect grid of every
# selected term and layer, with attribute `peaks` (the peaks its values
# used, from .cdrgam_cli_peak_summary(), or NULL), attribute
# `pointwise_peaks` (for lag: peak-pointwise, or NULL), and attribute
# `tests` (for test: true, or NULL). `training` holds impulses, responses,
# and the effect catalog, as from .cdrgam_cli_effect_training(). Both
# `cdrgam run` and the viewer evaluate effects here.
.cdrgam_cli_compute_effect <- function(fit, query, layers=NULL, training) {
    catalog <- training$catalog
    query_layers <- .cdrgam_cli_effect_layers(query, layers)
    labels <- .cdrgam_cli_select_terms(
        fit, catalog, query$terms, training$impulses, training$responses
    )
    terms <- lapply(match(labels, catalog$label), function(index) {
        .cdrgam_cli_catalog_row(catalog, index)
    })
    tables <- list()
    tests <- list()
    pointwise <- list()
    peaks <- new.env()
    peaks$.catalog <- catalog
    for (layer in query_layers) {
        for (term in terms) {
            value <- .cdrgam_cli_evaluate_term(fit, term, query, layer, training, peaks)
            value$estimate$layer <- layer$id
            tables[[length(tables) + 1L]] <- value$estimate
            if (!is.null(value$test)) {
                value$test$layer <- rep.int(layer$id, nrow(value$test))
                tests[[length(tests) + 1L]] <- value$test
            }
            if (!is.null(value$pointwise)) {
                pointwise[[length(pointwise) + 1L]] <- value$pointwise
            }
        }
    }
    output <- .cdrgam_cli_bind_tables(tables)
    attr(output, 'pointwise_peaks') <- if (length(pointwise)) {
        .cdrgam_cli_bind_tables(pointwise)
    }
    attr(output, 'tests') <- if (length(tests)) .cdrgam_cli_bind_tables(tests)
    attr(output, 'peaks') <- .cdrgam_cli_peak_summary(
        fit, peaks, training$impulses, training$responses, query$n,
        .cdrgam_cli_null(query$uncertainty$peak_draws, .cdrgam_cli_peak_draws),
        query$uncertainty$level, query$uncertainty$unconditional
    )
    output
}

.cdrgam_cli_execute_effect <- function(item, stage) {
    fit <- readRDS(file.path(item$fit$output, 'fit.rds'))
    training <- .cdrgam_cli_effect_training(fit, item$fit$dataset)
    query <- item$query
    output <- .cdrgam_cli_compute_effect(fit, query, item$layers, training)
    pointwise <- attr(output, 'pointwise_peaks')
    tests <- attr(output, 'tests')
    peaks <- attr(output, 'peaks')
    for (name in c('pointwise_peaks', 'tests', 'peaks')) attr(output, name) <- NULL
    if (!is.null(pointwise)) {
        saveRDS(pointwise, file.path(stage, 'effect-pointwise-peaks.rds'), version=3)
        utils::write.csv(
            pointwise, file.path(stage, 'effect-pointwise-peaks.csv'), row.names=FALSE
        )
    }
    if (isTRUE(query$test)) {
        utils::write.csv(tests, file.path(stage, 'effect-tests.csv'), row.names=FALSE)
    }
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
        list(query=query, layers=.cdrgam_cli_effect_layers(query, item$layers)),
        file.path(stage, 'request.yml')
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
    slides=ggplot2::theme_minimal(base_size=16),
    getExportedValue('ggplot2', paste0('theme_', theme))(base_size=10)
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
    name <- if (isTRUE(color$untitled)) NULL else ggplot2::waiver()
    list(if (identical(color$kind, 'continuous')) {
        ggplot2::scale_colour_gradientn(
            name=name, colours=color$stops, aesthetics=aesthetics
        )
    } else ggplot2::scale_colour_manual(
        name=name, values=color$values, breaks=color$levels,
        labels=color$labels, aesthetics=aesthetics
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
    if (isTRUE(spec$y_zero)) plot <- plot + ggplot2::expand_limits(y=0)
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

# Draw a pointwise-peak panel specification with ggplot2: each series' peak
# lag as a line over its posterior interval band.
.cdrgam_cli_ggplot_pointwise <- function(panel) {
    rows <- panel$rows
    by_color <- !is.null(panel$colors)
    aesthetics <- function(...) .cdrgam_cli_ggplot_aes(
        ..., colour=if (by_color) '.color', group='.series'
    )
    plot <- ggplot2::ggplot()
    bounded <- rows[is.finite(rows$lower) & is.finite(rows$upper), , drop=FALSE]
    if (nrow(bounded)) plot <- plot + .cdrgam_cli_ggplot_geom(
        ggplot2::geom_ribbon, list(
            data=bounded, mapping=.cdrgam_cli_ggplot_aes(
                x='.x', ymin='lower', ymax='upper', fill=if (by_color) '.color',
                group='.series'
            ),
            alpha=0.2, colour=NA
        ), list(fill=if (by_color) NULL else 'black')
    )
    plot <- plot + .cdrgam_cli_ggplot_geom(ggplot2::geom_line, list(
        data=rows, mapping=aesthetics(x='.x', y='lag'), linewidth=0.8
    ), list(colour=if (by_color) NULL else 'black'))
    if (by_color) plot <- plot + ggplot2::scale_colour_manual(
        values=panel$colors, limits=names(panel$colors), guide='none',
        aesthetics=c('colour', 'fill')
    )
    if (!identical(panel$x$kind, 'continuous')) {
        plot <- plot + ggplot2::scale_x_continuous(
            breaks=panel$x$breaks, labels=panel$x$labels
        )
    }
    plot + ggplot2::labs(x=panel$labels$x, y=panel$labels$y) +
        .cdrgam_cli_ggplot_theme(panel$theme)
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

# The rendered figure: the main plot alone (NULL), or with the
# pointwise-peak panel and the peak strip below it.
.cdrgam_cli_peak_figure <- function(plot, data, peaks, definition, pointwise=NULL) {
    .cdrgam_cli_ggplot_figure(
        plot, .cdrgam_cli_figure_peaks(data, peaks, definition$render),
        .cdrgam_cli_figure_pointwise(data, pointwise, definition)
    )
}

# Draw a main plot with its pointwise-peak panel and peak strip
# specifications below it, either NULL when not drawn; NULL when neither
# is drawn.
.cdrgam_cli_ggplot_figure <- function(plot, strip, panel) {
    if (is.null(strip) && is.null(panel)) return(NULL)
    plots <- list(plot)
    heights <- 1
    drawn_panel <- if (!is.null(panel)) .cdrgam_cli_ggplot_pointwise(panel)
    if (!is.null(drawn_panel)) {
        plots <- c(plots, list(drawn_panel))
        heights <- c(heights, 0.45)
    }
    drawn_strip <- if (!is.null(strip)) .cdrgam_cli_ggplot_peak_strip(strip)
    if (!is.null(drawn_strip)) {
        if (!is.null(strip$xlim)) {
            plots[[1L]] <- plots[[1L]] + ggplot2::coord_cartesian(xlim=strip$xlim)
        }
        plots <- c(plots, list(drawn_strip))
        heights <- c(heights, min(0.6, 0.1 + 0.05 * length(strip$lanes)))
    }
    list(
        strip=drawn_strip, pointwise=drawn_panel,
        figure=.cdrgam_cli_stack_plots(plots, heights)
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

# The horizontal axis of one line: the one axis its effect grid varies.
.cdrgam_cli_line_axis <- function(data, line) {
    columns <- setdiff(names(data), c(
        .cdrgam_cli_effect_columns, 'predictor', 'predictor_value', 'critical_value',
        grep('_reference$', names(data), value=TRUE)
    ))
    varying <- columns[vapply(columns, function(name) {
        length(unique(data[[name]])) > 1L
    }, logical(1))]
    if (length(varying) != 1L) .cdrgam_cli_abort(paste0(
        'Line ', line$id, ' varies ',
        if (length(varying)) paste(varying, collapse=', ') else 'no axis',
        '; a line needs exactly one varying axis'
    ))
    varying
}

# Render a visualization of lines from each line's effect artifact, with
# the figure the viewer draws for the same lines.
.cdrgam_cli_render_lines <- function(item, stage) {
    definition <- item$definition
    entries <- lapply(seq_along(definition$lines), function(index) {
        line <- definition$lines[[index]]
        output <- item$effects[[index]]$output
        data <- readRDS(file.path(output, 'effect-grid.rds'))
        for (name in c('peaks', 'pointwise_peaks')) {
            path <- file.path(output, paste0(
                'effect-', if (identical(name, 'peaks')) 'peaks' else 'pointwise-peaks',
                '.rds'
            ))
            if (file.exists(path)) attr(data, name) <- readRDS(path)
        }
        list(spec=list(
            id=line$id, label=line$label, varying=.cdrgam_cli_line_axis(data, line)
        ), data=data)
    })
    render <- definition$render
    if (is.null(render$xlab) && identical(render$mappings$x, '.line_x')) {
        render$xlab <- paste(unique(vapply(
            entries, function(entry) entry$spec$varying, character(1)
        )), collapse=' / ')
    }
    figure <- .cdrgam_cli_lines_figure(entries, render)
    plot <- .cdrgam_cli_ggplot_plot(figure$spec)
    saveRDS(plot, file.path(stage, 'plot.rds'), version=3)
    stacked <- .cdrgam_cli_ggplot_figure(plot, figure$strip, figure$pointwise)
    if (!is.null(stacked$strip)) {
        saveRDS(stacked$strip, file.path(stage, 'plot-peaks.rds'), version=3)
    }
    if (!is.null(stacked$pointwise)) {
        saveRDS(stacked$pointwise, file.path(stage, 'plot-pointwise.rds'), version=3)
    }
    paths <- .cdrgam_cli_save_ggplot(
        if (is.null(stacked)) plot else stacked$figure, stage, render
    )
    list(
        kind='lines', rows=nrow(figure$spec$rows), files=paths,
        effect_identities=vapply(item$effects, `[[`, character(1), 'identity')
    )
}

.cdrgam_cli_render_effect <- function(item, stage) {
    if (!is.null(item$effects)) return(.cdrgam_cli_render_lines(item, stage))
    data <- readRDS(file.path(item$effect$output, 'effect-grid.rds'))
    peaks_path <- file.path(item$effect$output, 'effect-peaks.rds')
    peaks <- if (file.exists(peaks_path)) readRDS(peaks_path) else NULL
    pointwise_path <- file.path(item$effect$output, 'effect-pointwise-peaks.rds')
    pointwise <- if (file.exists(pointwise_path)) readRDS(pointwise_path) else NULL
    if (identical(item$definition$render$statistic, 'peak-lag')) {
        data <- .cdrgam_cli_peak_data(peaks)
    }
    plot <- .cdrgam_cli_visualization_plot(data, item$definition)
    saveRDS(plot, file.path(stage, 'plot.rds'), version=3)
    figure <- .cdrgam_cli_peak_figure(plot, data, peaks, item$definition, pointwise)
    if (!is.null(figure$strip)) {
        saveRDS(figure$strip, file.path(stage, 'plot-peaks.rds'), version=3)
    }
    if (!is.null(figure$pointwise)) {
        saveRDS(figure$pointwise, file.path(stage, 'plot-pointwise.rds'), version=3)
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
