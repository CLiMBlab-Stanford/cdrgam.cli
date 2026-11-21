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

.cdrgam_cli_mapping_column <- function(value) {
    sub('^predictor:', '', value)
}

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

.cdrgam_cli_layer_style <- function(layer) {
    style <- .cdrgam_cli_null(layer$style, list())
    style$alpha <- .cdrgam_cli_null(style$alpha, 1)
    style$linewidth <- .cdrgam_cli_null(style$linewidth, 0.8)
    style
}

.cdrgam_cli_add_curve_layer <- function(plot, data, mappings, interval, style) {
    line_mappings <- mappings
    if (!is.null(style$color)) line_mappings$color <- NULL
    line_mapping <- .cdrgam_cli_ggplot_mapping(line_mappings, data)
    if (identical(interval, 'ribbon') && any(is.finite(data$lower))) {
        ribbon_mappings <- mappings
        ribbon_mappings$y <- NULL
        if (!is.null(ribbon_mappings$color) && is.null(ribbon_mappings$fill)) {
            ribbon_mappings$fill <- ribbon_mappings$color
        }
        ribbon_mappings$color <- NULL
        ribbon_mapping <- .cdrgam_cli_ggplot_mapping(
            ribbon_mappings, data, include_y=FALSE
        )
        ribbon_mapping$ymin <- as.name('lower')
        ribbon_mapping$ymax <- as.name('upper')
        arguments <- list(
            data=data, mapping=ribbon_mapping, alpha=0.18,
            inherit.aes=FALSE, linewidth=0
        )
        if (!is.null(style$fill)) arguments$fill <- style$fill
        plot <- plot + do.call(ggplot2::geom_ribbon, arguments)
    }
    arguments <- list(
        data=data, mapping=line_mapping, alpha=style$alpha,
        linewidth=style$linewidth, inherit.aes=FALSE
    )
    if (!is.null(style$color)) arguments$color <- style$color
    plot <- plot + do.call(ggplot2::geom_line, arguments)
    if (identical(interval, 'lines') && any(is.finite(data$lower))) {
        for (bound in c('lower', 'upper')) {
            bound_mappings <- mappings
            bound_mappings$y <- bound
            bound_mapping <- .cdrgam_cli_ggplot_mapping(bound_mappings, data)
            arguments <- list(
                data=data, mapping=bound_mapping, alpha=style$alpha * 0.65,
                linewidth=style$linewidth * 0.7, linetype=2,
                inherit.aes=FALSE
            )
            if (!is.null(style$color)) arguments$color <- style$color
            plot <- plot + do.call(ggplot2::geom_line, arguments)
        }
    }
    plot
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

.cdrgam_cli_rainbow_scales <- function(plot, data, mappings) {
    if (is.null(mappings$color)) return(plot)
    values <- data[[.cdrgam_cli_mapping_column(mappings$color)]]
    if (is.numeric(values)) {
        colors <- grDevices::hsv(seq(0, 0.8, length.out=7L), 0.9, 0.8)
        return(plot + ggplot2::scale_color_gradientn(colors=colors) +
            ggplot2::scale_fill_gradientn(colors=colors))
    }
    levels <- if (is.factor(values)) levels(values) else sort(unique(as.character(values)))
    colors <- .cdrgam_cli_rainbow(levels)
    plot + ggplot2::scale_color_manual(values=colors, breaks=levels) +
        ggplot2::scale_fill_manual(values=colors, breaks=levels)
}

.cdrgam_cli_visualization_plot <- function(data, definition) {
    render <- definition$render
    mappings <- render$mappings
    series_axes <- .cdrgam_cli_series_axes(data, mappings)
    data <- .cdrgam_cli_discrete_mappings(data, mappings)
    data$.cdrgam_series <- do.call(interaction, c(
        list(
            data$term,
            ifelse(is.na(data$group), '<population>', as.character(data$group)),
            data$layer
        ),
        unname(lapply(data[series_axes], function(value) {
            ifelse(is.na(value), '<none>', format(value, digits=15L))
        })),
        list(drop=TRUE, lex.order=TRUE)
    ))
    if (is.null(mappings$group)) mappings$group <- '.cdrgam_series'
    plot <- ggplot2::ggplot()
    layers <- .cdrgam_cli_effect_layers(definition$query, definition$layers)
    if (identical(render$geometry, 'line')) {
        for (layer in layers) {
            selected <- data[data$layer == layer$id, , drop=FALSE]
            interval <- .cdrgam_cli_null(layer$interval, render$interval)
            plot <- .cdrgam_cli_add_curve_layer(
                plot, selected, mappings, interval,
                .cdrgam_cli_layer_style(layer)
            )
        }
    } else {
        mapping <- .cdrgam_cli_ggplot_mapping(mappings, data)
        if (render$geometry %in% c('raster', 'raster-contour')) {
            plot <- plot + ggplot2::geom_tile(data=data, mapping=mapping)
        }
        if (render$geometry %in% c('contour', 'raster-contour')) {
            contour_mappings <- list(
                x=mappings$x, y=mappings$y,
                z=.cdrgam_cli_null(mappings$fill, 'estimate')
            )
            contour_columns <- lapply(contour_mappings, .cdrgam_cli_mapping_column)
            missing <- setdiff(unlist(contour_columns), names(data))
            if (length(missing)) .cdrgam_cli_abort(paste0(
                'Contour mappings reference unavailable columns: ',
                paste(missing, collapse=', ')
            ))
            contour_mapping <- do.call(
                ggplot2::aes, lapply(contour_columns, as.name)
            )
            plot <- plot + ggplot2::geom_contour(
                data=data, mapping=contour_mapping,
                color='grey20', linewidth=0.35, inherit.aes=FALSE
            )
        }
        if (identical(.cdrgam_cli_mapping_column(mappings$fill), 'se')) {
            plot <- plot + ggplot2::scale_fill_gradient(
                low='white', high='#2166AC'
            )
        } else {
            plot <- plot + ggplot2::scale_fill_gradient2(
                low='#2166AC', mid='white', high='#B2182B', midpoint=0
            )
        }
    }
    facet <- mappings$facet
    if (!is.null(facet)) {
        facet <- .cdrgam_cli_mapping_column(facet)
        if (!(facet %in% names(data))) .cdrgam_cli_abort(paste0(
            'Visualization facet references unavailable column: ', facet
        ))
        plot <- plot + ggplot2::facet_wrap(
            stats::as.formula(paste('~', facet)),
            scales=.cdrgam_cli_null(render$scales, 'fixed')
        )
    }
    if (identical(render$palette, 'rainbow') && identical(render$geometry, 'line')) {
        plot <- .cdrgam_cli_rainbow_scales(plot, data, mappings)
    }
    plot <- plot + ggplot2::labs(
        title=render$title,
        x=.cdrgam_cli_null(render$xlab, mappings$x),
        y=.cdrgam_cli_null(render$ylab, 'Effect on linear predictor')
    )
    plot + switch(
        render$theme,
        paper=ggplot2::theme_bw(base_size=10),
        minimal=ggplot2::theme_minimal(base_size=10),
        slides=ggplot2::theme_minimal(base_size=16)
    )
}

# Whether to draw the peak strip. `auto` draws it when the plotted terms
# were held at peaks that differ, since the curves then sit at different
# lags; `always` draws it whenever a peak was used.
.cdrgam_cli_show_peaks <- function(peaks, setting=NULL) {
    setting <- .cdrgam_cli_null(setting, 'auto')
    if (is.null(peaks) || !nrow(peaks) || identical(setting, 'never')) return(FALSE)
    identical(setting, 'always') || length(unique(signif(peaks$lag, 10L))) > 1L
}

# One label per strip row. A term held at its own positive peak is labeled
# by the term alone; other rows also name the peak, as in
# "x · peak-neg(irf(1))".
.cdrgam_cli_peak_lanes <- function(peaks) {
    own <- peaks$source == peaks$term
    expression <- paste0(
        ifelse(peaks$direction == 'neg', 'peak-neg', 'peak'),
        ifelse(own, '', paste0('(', peaks$source, ')'))
    )
    plain <- own & peaks$direction == 'pos' &
        !(peaks$term %in% peaks$term[duplicated(peaks$term)])
    ifelse(plain, peaks$term, paste0(peaks$term, ' · ', expression))
}

# A timeline of the peak lags used by each term: a long tick at the point
# estimate, the central interval of the posterior draws as a bar, and the
# draws as faint ticks, so that a peak whose draws split between separate
# lags shows both clusters. Rows are colored like the main plot's lines
# when color maps to term. `xlim` aligns the timeline with a lag x-axis.
.cdrgam_cli_peak_strip <- function(peaks, data, render, xlim=NULL) {
    peaks$.lane <- .cdrgam_cli_peak_lanes(peaks)
    peaks$.lane <- factor(peaks$.lane, levels=rev(unique(peaks$.lane)))
    by_term <- identical(
        .cdrgam_cli_mapping_column(.cdrgam_cli_null(render$mappings$color, '')), 'term'
    )
    aesthetics <- function(...) {
        columns <- c(list(...), if (by_term) list(colour='term'))
        do.call(ggplot2::aes, lapply(columns, as.name))
    }
    fixed <- if (by_term) list() else list(colour='black')
    plot <- ggplot2::ggplot()
    draws <- attr(peaks, 'draws')
    if (!is.null(draws) && nrow(draws)) {
        draws <- merge(
            draws, peaks[c('term', 'source', 'direction', '.lane')],
            by=c('source', 'direction')
        )
        count <- max(table(paste(draws$.lane)))
        plot <- plot + do.call(ggplot2::geom_point, c(list(
            data=draws,
            mapping=aesthetics(x='lag', y='.lane'), shape='|', size=3, alpha=min(1, 25 / count)
        ), fixed))
    }
    bounded <- peaks[is.finite(peaks$lower) & is.finite(peaks$upper), , drop=FALSE]
    if (nrow(bounded)) plot <- plot + do.call(ggplot2::geom_linerange, c(list(
        data=bounded, mapping=aesthetics(xmin='lower', xmax='upper', y='.lane'),
        linewidth=0.6
    ), fixed))
    plot <- plot + do.call(ggplot2::geom_point, c(list(
        data=peaks,
        mapping=aesthetics(x='lag', y='.lane'), shape='|', size=7
    ), fixed))
    if (by_term) {
        levels <- sort(unique(as.character(data$term)))
        plot <- plot + if (identical(render$palette, 'rainbow')) {
            ggplot2::scale_colour_manual(
                values=.cdrgam_cli_rainbow(levels), limits=levels, guide='none'
            )
        } else ggplot2::scale_colour_discrete(limits=levels, guide='none')
    }
    if (!is.null(xlim)) plot <- plot + ggplot2::coord_cartesian(xlim=xlim)
    plot + ggplot2::labs(x=if (is.null(xlim)) 'Peak lag' else NULL, y=NULL) +
        switch(
            .cdrgam_cli_null(render$theme, 'paper'),
            paper=ggplot2::theme_bw(base_size=10),
            minimal=ggplot2::theme_minimal(base_size=10),
            slides=ggplot2::theme_minimal(base_size=16)
        )
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
    render <- definition$render
    if (!identical(render$geometry, 'line') ||
            !.cdrgam_cli_show_peaks(peaks, render$peaks)) return(NULL)
    shared <- identical(.cdrgam_cli_mapping_column(render$mappings$x), 'lag') &&
        is.null(render$mappings$facet) && 'lag' %in% names(data)
    xlim <- if (shared) range(c(data$lag, peaks$lag), na.rm=TRUE) else NULL
    strip <- .cdrgam_cli_peak_strip(peaks, data, render, xlim)
    if (shared) plot <- plot + ggplot2::coord_cartesian(xlim=xlim)
    rows <- length(unique(.cdrgam_cli_peak_lanes(peaks)))
    list(
        strip=strip,
        figure=.cdrgam_cli_stack_plots(
            list(plot, strip), c(1, min(0.6, 0.1 + 0.05 * rows))
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
    plot <- .cdrgam_cli_visualization_plot(data, item$definition)
    saveRDS(plot, file.path(stage, 'plot.rds'), version=3)
    peaks_path <- file.path(item$effect$output, 'effect-peaks.rds')
    peaks <- if (file.exists(peaks_path)) readRDS(peaks_path) else NULL
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
