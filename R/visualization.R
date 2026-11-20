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
.cdrgam_cli_evaluate_term <- function(fit, term, query, layer, training) {
    request <- .cdrgam_cli_term_request(
        fit, term, query, training$impulses, training$responses
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
    for (layer in layers) {
        for (term in terms) {
            value <- .cdrgam_cli_evaluate_term(fit, term, query, layer, training)
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
    paths <- .cdrgam_cli_save_ggplot(plot, stage, item$definition$render)
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
