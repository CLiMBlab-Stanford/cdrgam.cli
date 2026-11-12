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
    query <- item$query
    layers <- .cdrgam_cli_effect_layers(query, item$layers)
    # References and tests need cdrgam > 0.2.0; omit the argument otherwise so
    # that other queries still run against older core releases.
    reference <- if (is.null(query$reference)) list() else list(reference=query$reference)
    tables <- lapply(layers, function(layer) {
        value <- do.call(.cdrgam_cli_core_estimate_effect, c(list(
            fit,
            terms=query$terms,
            axes=query$axes,
            impulses=training$impulses,
            responses=training$responses,
            composition=layer$composition,
            grouping=layer$grouping,
            groups=layer$groups,
            se=TRUE,
            unconditional=query$uncertainty$unconditional,
            level=query$uncertainty$level,
            n=query$n
        ), reference))
        value$layer <- layer$id
        value
    })
    if (isTRUE(query$test)) {
        tests <- lapply(layers, function(layer) {
            value <- do.call(.cdrgam_cli_core_test_effect, c(list(
                fit,
                terms=query$terms,
                axes=query$axes,
                impulses=training$impulses,
                responses=training$responses,
                composition=layer$composition,
                grouping=layer$grouping,
                groups=layer$groups,
                unconditional=query$uncertainty$unconditional,
                n=query$n
            ), reference))
            value$layer <- rep.int(layer$id, nrow(value))
            as.data.frame(value)
        })
        tests <- do.call(rbind, tests)
        rownames(tests) <- NULL
        utils::write.csv(tests, file.path(stage, 'effect-tests.csv'), row.names=FALSE)
    }
    columns <- unique(unlist(lapply(tables, names), use.names=FALSE))
    tables <- lapply(tables, function(value) {
        for (name in setdiff(columns, names(value))) value[[name]] <- NA
        value[columns]
    })
    output <- do.call(rbind, tables)
    rownames(output) <- NULL
    saveRDS(output, file.path(stage, 'effect-grid.rds'), version=3)
    utils::write.csv(output, file.path(stage, 'effect-grid.csv'), row.names=FALSE)
    .cdrgam_cli_write_yaml(
        list(query=query, layers=layers), file.path(stage, 'request.yml')
    )
    list(
        rows=nrow(output), terms=length(unique(output$term_id)),
        layers=length(unique(output$layer)), scale='linear-predictor',
        interval='pointwise', level=query$uncertainty$level
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
        'se', 'lower', 'upper', 'layer'
    )
    mapped <- vapply(
        mappings[intersect(c('x', 'y'), names(mappings))],
        .cdrgam_cli_mapping_column, character(1)
    )
    candidates <- setdiff(names(data), c(reserved, mapped))
    candidates <- candidates[!startsWith(candidates, '.') &
        !endsWith(candidates, '_reference')]
    candidates[vapply(candidates, function(name) {
        is.numeric(data[[name]]) && length(unique(data[[name]])) > 1L
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
        unname(as.list(data[series_axes])),
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
        plot <- plot + ggplot2::facet_wrap(stats::as.formula(paste('~', facet)))
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
