.cdrgam_cli_viz_input_id <- function(axis) {
    paste0('fixed_', gsub('[^A-Za-z0-9]', '_', axis), '_', substr(
        digest::digest(axis, algo='md5'), 1L, 6L
    ))
}

.cdrgam_cli_viz_reference_id <- function(axis) {
    sub('^fixed_', 'reference_', .cdrgam_cli_viz_input_id(axis))
}

# The axes a reference may set: the fixed ones. A varying axis keeps its
# plotted value in the reference, so the line shows the change from moving
# only the fixed axes to their reference values.
.cdrgam_cli_viz_reference_axes <- function(term, varying) {
    setdiff(names(term$axes), varying)
}

# An empty reference field (NULL) leaves its axis at the line's fixed value,
# so the axis is left out of the reference; NA, typed as `default`, leaves a
# predictor at the model default. Lag and time have no model default, so NA
# also leaves them at their fixed value.
.cdrgam_cli_viz_reference_values <- function(values) {
    keep <- vapply(names(values), function(name) {
        value <- values[[name]]
        !is.null(value) && (is.character(value) || !is.na(value) ||
            !(name %in% c('lag', 'time')))
    }, logical(1))
    values <- values[keep]
    if (length(values)) values else NULL
}

.cdrgam_cli_viz_format <- function(value) {
    if (is.character(value)) return(value)
    if (!is.finite(value)) return('NA')
    format(signif(value, 4L), trim=TRUE, scientific=abs(value) >= 1e5 ||
        (abs(value) < 1e-3 && value != 0))
}

.cdrgam_cli_viz_label <- function(spec, term) {
    fixed <- spec$fixed[setdiff(names(spec$fixed), spec$varying)]
    fixed <- fixed[vapply(fixed, function(value) {
        is.character(value) || is.finite(value)
    }, logical(1))]
    parts <- c(
        paste0(spec$model_id, ': ', spec$term),
        if (length(fixed)) paste0('[', paste(
            names(fixed), vapply(fixed, .cdrgam_cli_viz_format, character(1)),
            sep='=', collapse=', '
        ), ']'),
        if (!identical(spec$grouping, 'population')) paste0(
            spec$grouping, ' ', spec[['group']]
        ),
        if (!identical(spec$composition, 'term')) spec$composition,
        if (length(spec$reference)) paste0(
            'minus [', paste(
                names(spec$reference),
                vapply(spec$reference, function(value) {
                    if (!is.character(value) && is.na(value)) 'default' else
                        .cdrgam_cli_viz_format(value)
                }, character(1)),
                sep='=', collapse=', '
            ), ']'
        )
    )
    paste(parts, collapse=' ')
}

.cdrgam_cli_viz_surface_matrix <- function(data, x, y, value='estimate') {
    xs <- sort(unique(data[[x]]))
    ys <- sort(unique(data[[y]]))
    z <- matrix(NA_real_, length(ys), length(xs))
    z[cbind(match(data[[y]], ys), match(data[[x]], xs))] <- data[[value]]
    list(x=xs, y=ys, z=z)
}

# The render fields the viewer has controls for, in registry order.
.cdrgam_cli_viz_control_fields <- function() {
    fields <- .cdrgam_cli_render_fields
    names(fields)[vapply(fields, function(spec) identical(spec$viewer, 'control'), logical(1))]
}

# The geometries the viewer offers in each mode. `surface` is a 3D surface,
# which only the viewer draws.
.cdrgam_cli_viz_geometries <- list(
    line=c(
        'Lines'='line', 'Points'='point', 'Connected points'='connected-point',
        'Bars'='bar'
    ),
    surface=c(
        '3D surface'='surface', 'Heatmap'='raster', 'Contour lines'='contour',
        'Heatmap with contours'='raster-contour'
    )
)

# The interval choices the viewer offers for a geometry. On a surface,
# `companion` adds the standard error: a second heatmap, or bounding
# surfaces around a 3D surface.
.cdrgam_cli_viz_intervals <- function(geometry) {
    labels <- c(
        ribbon='Ribbon', lines='Boundary lines', errorbar='Error bars',
        companion=if (identical(geometry, 'surface')) 'Bounding surfaces' else
            'Standard-error heatmap',
        none='None'
    )
    choices <- if (identical(geometry, 'surface')) c('companion', 'none') else {
        .cdrgam_cli_geometry_intervals(geometry)
    }
    stats::setNames(choices, labels[choices])
}

# Colors typed as a list, separated by commas, semicolons, or spaces;
# rgb(R, G, B) entries keep their commas. NULL when nothing is typed.
.cdrgam_cli_viz_gradient <- function(text) {
    text <- trimws(.cdrgam_cli_null(text, ''))
    if (!nzchar(text)) return(NULL)
    colors <- regmatches(text, gregexpr('rgb\\([^)]*\\)|[^,;[:space:]]+', text))[[1L]]
    if (length(colors) < 2L) .cdrgam_cli_abort('List two or more gradient colors')
    colors
}

# The appearance settings from the viewer's inputs: each control field's
# value, leaving out empty ones. The custom palette is read from its color
# list.
.cdrgam_cli_viz_settings <- function(input) {
    values <- lapply(.cdrgam_cli_viz_control_fields(), function(name) {
        value <- input[[paste0('render_', name)]]
        if (is.null(value) || !length(value) || identical(value, '')) NULL else value
    })
    names(values) <- .cdrgam_cli_viz_control_fields()
    if (identical(values$palette, 'custom')) {
        values$palette <- .cdrgam_cli_viz_gradient(input$render_palette_colors)
    }
    # Show peak lags, under the plot, sets the peak timeline.
    values$peaks <- if (isFALSE(input$show_peaks)) 'never' else NULL
    # Tick values label a line's axis; points and bars are evaluated at them.
    if (!.cdrgam_cli_viz_ticks_evaluated(input) && !identical(input$mode, 'surface')) {
        values$breaks <- tryCatch(
            .cdrgam_cli_viz_ticks(input$x_ticks), error=function(error) NULL
        )
    }
    # Tick labels name the typed tick values in order, so they need them.
    if (!identical(input$mode, 'surface') &&
            nzchar(trimws(.cdrgam_cli_null(input$x_ticks, '')))) {
        values$tick_labels <- tryCatch(.cdrgam_cli_viz_tick_labels(
            input$x_tick_labels, input$x_ticks
        ), error=function(error) NULL)
    }
    Filter(Negate(is.null), values)
}

# Whether the viewer evaluates lines at the typed tick values: for points
# and bars, which are drawn only where they are evaluated. Lines are
# evaluated on the grid and the tick values only label the axis.
.cdrgam_cli_viz_ticks_evaluated <- function(input) {
    .cdrgam_cli_null(input$render_geometry, 'line') %in%
        c(.cdrgam_cli_point_geometries, 'bar') &&
        nzchar(trimws(.cdrgam_cli_null(input$x_ticks, '')))
}

# The x values typed into the viewer's tick field, sorted and unique, or
# NULL when the field is empty.
.cdrgam_cli_viz_ticks <- function(text) {
    text <- trimws(.cdrgam_cli_null(text, ''))
    if (!nzchar(text)) return(NULL)
    parts <- strsplit(text, '[,[:space:]]+')[[1L]]
    parts <- parts[nzchar(parts)]
    values <- suppressWarnings(as.numeric(parts))
    if (!length(values) || any(!is.finite(values))) .cdrgam_cli_abort(paste0(
        'X tick values must be numbers separated by commas or spaces, not "',
        text, '"'
    ))
    sort(unique(values))
}

# The typed X tick labels, separated by commas, or NULL when empty, in the
# sorted order of the tick values `ticks` that they label as typed. Spaces
# within a label are kept; an empty entry, as in "a,,c", leaves that tick
# unlabelled. A repeated tick value keeps its first label.
.cdrgam_cli_viz_tick_labels <- function(text, ticks) {
    text <- .cdrgam_cli_null(text, '')
    if (!nzchar(trimws(text))) return(NULL)
    labels <- trimws(strsplit(text, ',', fixed=TRUE)[[1L]])
    if (grepl(',\\s*$', text)) labels <- c(labels, '')
    .cdrgam_cli_viz_ticks(ticks)
    parts <- strsplit(trimws(ticks), '[,[:space:]]+')[[1L]]
    written <- as.numeric(parts[nzchar(parts)])
    if (length(labels) != length(written)) .cdrgam_cli_abort(paste0(
        'X tick labels has ', length(labels), ' labels for ', length(written),
        ' tick values'
    ))
    keep <- !duplicated(written)
    labels[keep][order(written[keep])]
}

# Up to eight colors of each named palette, for the palette menu, as a list
# named by palette; a palette with fewer colors gives all of them, and
# `custom` none.
.cdrgam_cli_viz_palette_swatches <- function(palettes) {
    swatches <- lapply(palettes, function(palette) {
        if (identical(palette, 'custom')) return(character())
        count <- if (palette %in% names(.cdrgam_cli_brewer_palettes)) {
            min(8L, .cdrgam_cli_brewer_palettes[[palette]])
        } else 8L
        unname(.cdrgam_cli_palette_colors(palette, paste0('s', seq_len(count))))
    })
    stats::setNames(swatches, palettes)
}

# The selectize render functions that draw a palette's swatches before its
# name, in the menu and in the chosen item; `%s` takes the swatches as JSON.
.cdrgam_cli_viz_swatch_render <- paste0(
    '(function() { var swatches = %s; function draw(item, escape) {',
    ' var colors = swatches[item.value] || []; var blocks = colors.map(function(color) {',
    ' return \'<span style="display:inline-block;width:14px;height:14px;background:\'',
    ' + escape(color) + \';"></span>\'; }).join(\'\');',
    ' return \'<div><div style="display:flex;margin-bottom:2px;">\' + blocks +',
    ' \'</div><div class="small">\' + escape(item.label) + \'</div></div>\'; }',
    ' return {option: draw, item: draw}; })()'
)

# The viewer's palette for `count` lines: okabe-ito, or viridis when there
# are more lines than okabe-ito has colors. The switch recolors every line.
.cdrgam_cli_viz_palette <- function(count) {
    if (count > length(.cdrgam_cli_okabe_ito)) 'viridis' else 'okabe-ito'
}

# Line colors named by line id, in the order the lines were added, under a
# chosen palette; okabe-ito switches as .cdrgam_cli_viz_palette() says.
.cdrgam_cli_viz_line_colors <- function(ids, palette='okabe-ito', reverse=FALSE) {
    if (identical(palette, 'okabe-ito')) palette <- .cdrgam_cli_viz_palette(length(ids))
    .cdrgam_cli_palette_colors(palette, ids, reverse)
}

# The render the viewer draws its lines with, validated like a definition's.
# `settings` holds appearance settings from .cdrgam_cli_viz_settings(); an
# interval that does not suit the geometry falls back to its default, and
# `show_interval = FALSE` means no interval when none is chosen.
.cdrgam_cli_viz_render <- function(entries, show_interval=TRUE, settings=list()) {
    geometry <- .cdrgam_cli_null(settings$geometry, 'line')
    peak <- identical(settings$statistic, 'peak-lag')
    axes <- unique(vapply(entries, function(entry) entry$spec$varying[[1L]], character(1)))
    interval <- .cdrgam_cli_null(settings$interval, if (!show_interval) 'none')
    if (!is.null(interval) && !(interval %in% .cdrgam_cli_geometry_intervals(geometry))) {
        interval <- NULL
    }
    palette <- .cdrgam_cli_null(settings$palette, 'okabe-ito')
    if (identical(palette, 'okabe-ito')) palette <- .cdrgam_cli_viz_palette(length(entries))
    render <- list(
        geometry=geometry,
        mappings=.cdrgam_cli_lines_mappings(settings$statistic),
        interval=interval, palette=palette,
        xlab=.cdrgam_cli_null(settings$xlab, if (peak) 'Peak' else paste(axes, collapse=' / ')),
        ylab=.cdrgam_cli_null(settings$ylab, if (peak) 'Peak lag' else 'Effect (linear predictor)')
    )
    for (name in c(
        'title', 'theme', 'peaks', 'spacing', 'statistic', 'y_zero', 'reverse_palette',
        'breaks', 'tick_labels'
    )) {
        render[[name]] <- settings[[name]]
    }
    render$theme <- .cdrgam_cli_null(render$theme, 'classic')
    .cdrgam_cli_validate_render(Filter(Negate(is.null), render), 'viewer render')
}

# The plot specification of the viewer's lines under its appearance
# settings; see .cdrgam_cli_lines_spec().
.cdrgam_cli_viz_line_spec <- function(entries, show_interval=TRUE, settings=list()) {
    .cdrgam_cli_lines_spec(entries, .cdrgam_cli_viz_render(entries, show_interval, settings))
}

.cdrgam_cli_viz_line_plot <- function(entries, show_interval=TRUE, settings=list()) {
    figure <- .cdrgam_cli_lines_figure(
        entries, .cdrgam_cli_viz_render(entries, show_interval, settings)
    )
    .cdrgam_cli_plotly_figure(
        .cdrgam_cli_plotly_plot(figure$spec), figure$strip, figure$spec, figure$pointwise
    )
}

# The viewer's peak timeline alone: its lane labels, one per peak, and its
# plot. NULL when no strip is drawn.
.cdrgam_cli_viz_peak_strip <- function(entries) {
    spec <- .cdrgam_cli_viz_line_spec(entries)
    strip <- .cdrgam_cli_lines_strip_spec(entries, spec)
    if (is.null(strip)) return(NULL)
    list(
        rows=as.character(strip$peaks$.lane),
        plot=.cdrgam_cli_plotly_peak_strip(strip)
    )
}

# A surface over two axes. `settings$geometry` (or `display`, where
# `heatmap` is raster and `contour` is raster-contour) chooses a 3D surface
# or a raster or contour render; `settings$interval` companion (or
# `show_interval`) adds the standard error. Title, axis titles, and theme
# come from `settings`.
.cdrgam_cli_viz_surface_plot <- function(
        data, spec, display='surface', show_interval=FALSE, settings=list()
) {
    x <- spec$varying[[1L]]
    y <- spec$varying[[2L]]
    data <- as.data.frame(data)
    geometry <- .cdrgam_cli_null(settings$geometry, switch(
        display, heatmap='raster', contour='raster-contour', display
    ))
    if (!(geometry %in% .cdrgam_cli_viz_geometries$surface)) geometry <- 'surface'
    companion <- identical(.cdrgam_cli_null(settings$interval,
        if (show_interval) 'companion' else 'none'), 'companion') && any(is.finite(data$se))
    appearance <- settings[intersect(c('title', 'xlab', 'ylab', 'theme'), names(settings))]
    if (!identical(geometry, 'surface')) {
        surface <- function(fill, colorbar) {
            render <- .cdrgam_cli_validate_render(c(list(
                geometry=geometry, mappings=list(x=x, y=y, fill=fill), interval='none'
            ), appearance), 'viewer render')
            .cdrgam_cli_plotly_plot(
                .cdrgam_cli_plot_spec(data, list(render=render)), colorbar=colorbar
            )
        }
        if (!companion) return(surface('estimate', list()))
        return(plotly::subplot(
            surface('estimate', list(len=0.45, y=1, yanchor='top')),
            surface('se', list(len=0.45, y=0, yanchor='bottom')),
            nrows=2L, shareX=TRUE, titleX=TRUE, titleY=TRUE
        ))
    }
    estimate <- .cdrgam_cli_viz_surface_matrix(data, x, y)
    scale <- .cdrgam_cli_plotly_colorscale('diverging', estimate$z)
    plot <- plotly::plot_ly(
        x=estimate$x, y=estimate$y, z=estimate$z, type='surface',
        colorscale=scale$colorscale, cmin=scale$zmin, cmax=scale$zmax,
        colorbar=list(title='Effect'), name='estimate'
    )
    if (companion) {
        for (bound in c('lower', 'upper')) {
            value <- .cdrgam_cli_viz_surface_matrix(data, x, y, bound)
            plot <- plotly::add_surface(
                plot, x=value$x, y=value$y, z=value$z, opacity=0.3,
                showscale=FALSE, colorscale=list(c(0, '#888888'), c(1, '#888888')),
                name=bound
            )
        }
    }
    plotly::layout(
        plot, title=appearance$title,
        font=.cdrgam_cli_plotly_font(appearance$theme),
        scene=list(
            xaxis=list(title=.cdrgam_cli_null(appearance$xlab, x)),
            yaxis=list(title=.cdrgam_cli_null(appearance$ylab, y)),
            zaxis=list(title='Effect (linear predictor)')
        )
    )
}

# The appearance panel for a mode: one input per registry control field
# that applies, with ids render_FIELD, holding `values` (raw input values)
# where they are still valid. Surfaces have no palette, spacing, statistic,
# or peak-timeline controls.
.cdrgam_cli_viz_appearance <- function(mode, values=list()) {
    fields <- .cdrgam_cli_render_fields
    mode <- if (identical(mode, 'surface')) 'surface' else 'line'
    geometries <- .cdrgam_cli_viz_geometries[[mode]]
    geometry <- values$geometry
    if (is.null(geometry) || !(geometry %in% geometries)) geometry <- geometries[[1L]]
    pick <- function(value, choices, default) {
        if (!is.null(value) && value %in% choices) value else default
    }
    choose <- function(name, choices, default=choices[[1L]]) shiny::selectInput(
        paste0('render_', name), fields[[name]]$label, choices,
        selected=pick(values[[name]], choices, default)
    )
    text <- function(name) shiny::textInput(
        paste0('render_', name), fields[[name]]$label,
        value=.cdrgam_cli_null(values[[name]], ''), placeholder='Automatic'
    )
    # The palette menu shows each palette's colors beside its name.
    palette_input <- function(choices) {
        swatches <- .cdrgam_cli_viz_palette_swatches(unname(choices))
        shiny::selectizeInput(
            'render_palette', fields$palette$label, choices,
            selected=pick(values$palette, choices, choices[[1L]]),
            options=list(render=I(sprintf(.cdrgam_cli_viz_swatch_render, as.character(
                jsonlite::toJSON(swatches, auto_unbox=FALSE)
            ))))
        )
    }
    intervals <- .cdrgam_cli_viz_intervals(geometry)
    default_interval <- if (identical(mode, 'surface')) 'none' else {
        fields$interval$default(list(geometry=geometry))
    }
    controls <- list(
        geometry=choose('geometry', geometries),
        statistic=if (identical(mode, 'line')) choose(
            'statistic', c('Effect estimates'='estimate', 'Peak lags'='peak-lag')
        ),
        interval=choose('interval', intervals, default_interval),
        spacing=if (identical(mode, 'line')) choose(
            'spacing', c('Continuous axis'='', 'Uniform ticks'='uniform', 'Linear ticks'='linear')
        ),
        palette=if (identical(mode, 'line')) shiny::tagList(
            palette_input(c(
                'Okabe-Ito (viridis above 8 lines)'='okabe-ito', 'ggplot2 hue'='default',
                'Rainbow'='rainbow', 'Grey'='grey',
                stats::setNames(.cdrgam_cli_viridis_palettes, .cdrgam_cli_viridis_palettes),
                stats::setNames(
                    names(.cdrgam_cli_brewer_palettes), names(.cdrgam_cli_brewer_palettes)
                ),
                'Custom gradient'='custom'
            )),
            shiny::conditionalPanel(
                "input.render_palette == 'custom'",
                shiny::textInput(
                    'render_palette_colors', 'Gradient colors',
                    value=.cdrgam_cli_null(values$palette_colors, ''),
                    placeholder='#2166AC, rgb(178, 24, 43)'
                )
            )
        ),
        # The peak timeline is toggled by the Show peak lags box under the plot.
        title=text('title'), xlab=text('xlab'), ylab=text('ylab'),
        # The viewer starts in classic; definitions keep their paper default.
        theme=choose('theme', fields$theme$choices, 'classic'),
        y_zero=if (identical(mode, 'line')) shiny::checkboxInput(
            'render_y_zero', fields$y_zero$label, isTRUE(values$y_zero)
        ),
        reverse_palette=if (identical(mode, 'line')) shiny::checkboxInput(
            'render_reverse_palette', fields$reverse_palette$label,
            isTRUE(values$reverse_palette)
        )
    )
    shiny::tagList(
        Filter(Negate(is.null), controls[.cdrgam_cli_viz_control_fields()]),
        shiny::actionLink(
            'show_render_yaml', 'Show render YAML', icon=shiny::icon('code')
        )
    )
}

# The viewer's appearance as a render block for a visualization definition:
# the settings that were chosen, in registry order, and the palette, since
# the viewer's default palette differs from a definition's. Mappings are
# left to the definition, whose query decides which columns exist. A 3D
# surface has no render geometry, so its geometry is left out with a note.
# The viewer's lines as a visualization definition in YAML (see
# .cdrgam_cli_lines_definition()). `lines` are the viewer's line records,
# `terms` a function of a model id giving that model's viewer terms, and
# `options` and `settings` the evaluation options and appearance. Lines
# must come from one project, since a definition belongs to one.
.cdrgam_cli_viz_definition_yaml <- function(lines, terms, options, settings) {
    if (!length(lines)) .cdrgam_cli_abort('Add a line first')
    ids <- vapply(lines, `[[`, character(1), 'model_id')
    projects <- unique(sub('/.*$', '', ids))
    if (length(projects) != 1L) .cdrgam_cli_abort(paste0(
        'The lines come from projects ', paste(projects, collapse=', '),
        '; a visualization definition belongs to one project'
    ))
    entries <- lapply(seq_along(lines), function(index) {
        line <- lines[[index]]
        match <- Filter(function(term) identical(term$label, line$term), terms(line$model_id))
        if (length(match) != 1L) .cdrgam_cli_abort(paste0(
            'Term ', line$term, ' is no longer in ', line$model_id
        ))
        list(
            id=paste0('line', index), label=line$label, model=sub('^[^/]*/', '', line$model_id),
            query=.cdrgam_cli_viz_line_query(match[[1L]], line, options)
        )
    })
    render <- .cdrgam_cli_viz_render_values(settings)
    render <- c(list(geometry=.cdrgam_cli_null(render$geometry, 'line')),
        render[setdiff(names(render), 'geometry')])
    definition <- .cdrgam_cli_lines_definition(entries, render)
    paste(c(
        paste0('# Visualization definition from cdrgam viz, for project ', projects, '.'),
        '# Save it under definitions/visualizations/ in that project.',
        sub('\n$', '', yaml::as.yaml(definition))
    ), collapse='\n')
}

# The viewer's appearance as a render block's values: the settings that
# were chosen, in registry order, and the palette, since the viewer's
# default palette differs from a definition's.
.cdrgam_cli_viz_render_values <- function(settings, mode='line') {
    surface <- identical(mode, 'surface')
    fields <- c(.cdrgam_cli_viz_control_fields(), 'breaks', 'tick_labels')
    if (surface) {
        fields <- setdiff(fields, c(
            'palette', 'spacing', 'statistic', 'peaks', 'y_zero', 'reverse_palette',
            'breaks', 'tick_labels'
        ))
    }
    values <- settings[intersect(fields, names(settings))]
    if (!surface) {
        palette <- .cdrgam_cli_null(values$palette, 'okabe-ito')
        values$palette <- .cdrgam_cli_validate_render_palette(palette, 'palette')
    }
    values[intersect(fields, names(values))]
}

.cdrgam_cli_viz_render_yaml <- function(settings, mode='line') {
    surface <- identical(mode, 'surface')
    fields <- .cdrgam_cli_viz_control_fields()
    fields <- c(fields, 'breaks', 'tick_labels')
    values <- .cdrgam_cli_viz_render_values(settings, mode)
    three_d <- surface && identical(.cdrgam_cli_null(values$geometry, 'surface'), 'surface')
    if (three_d) values$geometry <- NULL
    if (surface && !three_d && is.null(values$interval)) values$interval <- 'none'
    values <- values[intersect(fields, names(values))]
    paste(c(
        '# Render settings from cdrgam viz. Add mappings for the definition\'s',
        '# query, such as',
        '#   mappings: {x: lag, y: estimate, color: term}',
        if (three_d) c(
            '# 3D surfaces are drawn only by the viewer; set geometry to raster,',
            '# contour, or raster-contour.'
        ),
        sub('\n$', '', yaml::as.yaml(list(render=values)))
    ), collapse='\n')
}

# The raw values of the appearance inputs, as kept in session storage.
.cdrgam_cli_viz_appearance_values <- function(input) {
    names <- c(.cdrgam_cli_viz_control_fields(), 'palette_colors')
    values <- lapply(names, function(name) input[[paste0('render_', name)]])
    names(values) <- names
    values$show_peaks <- input$show_peaks
    Filter(Negate(is.null), values)
}

# Lines are kept in the browser tab's session storage so that they survive a
# server restart. These convert them to and from that JSON form.
.cdrgam_cli_viz_lines_json <- function(lines) {
    as.character(jsonlite::toJSON(
        unname(lines), auto_unbox=TRUE, null='null', na='null', digits=NA
    ))
}

.cdrgam_cli_viz_lines_parse <- function(text) {
    values <- jsonlite::fromJSON(text, simplifyVector=FALSE)
    lapply(values, function(line) {
        line$varying <- as.character(unlist(line$varying))
        value <- function(value) {
            if (is.null(value)) NA_real_ else if (is.character(value)) value else
                as.numeric(value)
        }
        fixed <- lapply(line$fixed, value)
        line$fixed <- if (length(fixed)) fixed else list()
        reference <- lapply(line$reference, value)
        line$reference <- if (length(reference)) reference else NULL
        line
    })
}

.cdrgam_cli_viz_script_tag <- function() shiny::tags$script(shiny::HTML("
$(function() {
  var key = 'cdrgam-viz-lines';
  $(document).on('shiny:connected', function() {
    var saved = null;
    try { saved = window.sessionStorage.getItem(key); } catch (error) {}
    if (saved) Shiny.setInputValue('restore_lines', saved, {priority: 'event'});
  });
  Shiny.addCustomMessageHandler('cdrgam-lines', function(value) {
    try { window.sessionStorage.setItem(key, value); } catch (error) {}
  });
  var formKey = 'cdrgam-viz-form';
  $(document).on('shiny:connected', function() {
    var saved = null;
    try { saved = window.sessionStorage.getItem(formKey); } catch (error) {}
    if (saved) Shiny.setInputValue('restore_form', saved, {priority: 'event'});
  });
  Shiny.addCustomMessageHandler('cdrgam-form', function(value) {
    try { window.sessionStorage.setItem(formKey, value); } catch (error) {}
  });
  var appearanceKey = 'cdrgam-viz-appearance';
  $(document).on('shiny:connected', function() {
    var saved = null;
    try { saved = window.sessionStorage.getItem(appearanceKey); } catch (error) {}
    if (saved) Shiny.setInputValue('restore_appearance', saved, {priority: 'event'});
  });
  Shiny.addCustomMessageHandler('cdrgam-appearance', function(value) {
    try { window.sessionStorage.setItem(appearanceKey, value); } catch (error) {}
  });
  Shiny.addCustomMessageHandler('cdrgam-restart', function(value) {
    var overlay = document.createElement('div');
    overlay.style.cssText = 'position:fixed;inset:0;z-index:100000;display:flex;' +
      'align-items:center;justify-content:center;font-size:1.25rem;' +
      'background:rgba(255,255,255,0.9);color:#222;';
    overlay.textContent = 'Restarting server\u2026';
    document.body.appendChild(overlay);
    var poll = function() {
      fetch(window.location.href, {cache: 'no-store'}).then(function(response) {
        if (response.ok) window.location.reload(); else setTimeout(poll, 1000);
      }).catch(function() { setTimeout(poll, 1000); });
    };
    setTimeout(poll, 3000);
  });
});
"))

# Read a fixed or reference text field: empty is NA, a plain number is
# numeric, and anything else must be a value expression for that axis. A
# fixed lag (`pointwise = TRUE`) may also be peak-pointwise or
# peak-pointwise-neg.
.cdrgam_cli_viz_input_value <- function(text, name, pointwise=FALSE) {
    if (is.null(text) || (length(text) == 1L && is.na(text))) return(NA_real_)
    if (is.numeric(text)) return(as.numeric(text))
    text <- trimws(as.character(text))
    if (!nzchar(text)) return(NA_real_)
    if (pointwise && identical(name, 'lag') &&
            !is.null(.cdrgam_cli_pointwise_direction(text))) return(text)
    number <- suppressWarnings(as.numeric(text))
    if (is.finite(number)) return(number)
    .cdrgam_cli_value_parse(text, name, lag=identical(name, 'lag'))
    text
}

.cdrgam_cli_viz_ui <- function(restartable=FALSE) {
    bslib::page_sidebar(
        title='cdrgam viz',
        theme=bslib::bs_theme(version=5L),
        sidebar=bslib::sidebar(
            width=360,
            .cdrgam_cli_viz_script_tag(),
            shiny::div(
                style='display:flex; gap:0.5rem;',
                shiny::actionButton(
                    'refresh', 'Refresh data', icon=shiny::icon('rotate'),
                    class='btn-sm', title='Rescan completed fits and reload changed ones'
                ),
                if (restartable) shiny::actionButton(
                    'restart', 'Restart server', icon=shiny::icon('power-off'),
                    class='btn-sm', title='Restart the server with the installed code'
                )
            ),
            shiny::radioButtons(
                'mode', NULL, c('Line plot'='line', 'Surface plot'='surface'),
                inline=TRUE
            ),
            shiny::div(
                style='display:flex; gap:0.5rem; align-items:end;',
                shiny::div(style='flex:1;', shiny::selectInput('model', 'Model', NULL))
            ),
            shiny::selectInput('term', 'Term', NULL),
            shiny::uiOutput('axis_controls'),
            shiny::uiOutput('group_controls'),
            shiny::selectInput(
                'composition', 'Composition',
                c('Term alone'='term', 'Total'='total')
            ),
            shiny::div(
                class='form-text mt-n2 mb-2',
                paste(
                    'Total adds the lower-order terms, including irf(1).',
                    'To isolate one predictor, subtract a reference.'
                )
            ),
            shiny::uiOutput('fixed_controls'),
            shiny::uiOutput('reference_controls'),
            shiny::conditionalPanel(
                "input.mode == 'line'",
                shiny::textInput('label', 'Line label', placeholder='Automatic'),
                shiny::div(
                    style='display:flex; gap:0.5rem;',
                    shiny::uiOutput('line_buttons', inline=TRUE)
                )
            ),
            shiny::conditionalPanel(
                "input.mode == 'surface'",
                shiny::actionButton('draw_surface', 'Draw surface', class='btn-primary')
            ),
            bslib::accordion(
                open=FALSE,
                bslib::accordion_panel(
                    'Appearance', shiny::uiOutput('appearance_controls')
                ),
                bslib::accordion_panel(
                    'Options',
                    # Points and bars at typed tick values have no grid.
                    shiny::conditionalPanel(
                        paste(
                            "!(input.mode == 'line' && (input.x_ticks || '').trim() &&",
                            "['point', 'connected-point', 'bar']",
                            ".includes(input.render_geometry))"
                        ),
                        shiny::numericInput(
                            'n', 'Grid points per axis', 200, min=2, max=2000
                        )
                    ),
                    shiny::conditionalPanel(
                        "input.mode == 'line'",
                        shiny::textInput(
                            'x_ticks', 'X tick values', placeholder='Automatic'
                        ),
                        shiny::div(
                            class='form-text mt-n2 mb-2',
                            paste(
                                'Numbers separated by commas or spaces. Lines get',
                                'a tick at each; points and bars are evaluated at',
                                'these values instead of the grid.'
                            )
                        ),
                        shiny::conditionalPanel(
                            "(input.x_ticks || '').trim()",
                            shiny::textInput(
                                'x_tick_labels', 'X tick labels', placeholder='The values'
                            ),
                            shiny::div(
                                class='form-text mt-n2 mb-2',
                                paste(
                                    'One label per tick value, in order, separated',
                                    'by commas.'
                                )
                            )
                        )
                    ),
                    shiny::numericInput(
                        'level', 'Interval level', 0.95,
                        min=0.5, max=0.999, step=0.01
                    ),
                    shiny::radioButtons(
                        'interval', 'Intervals',
                        c('Pointwise'='pointwise', 'Simultaneous'='simultaneous'),
                        inline=TRUE
                    ),
                    shiny::div(
                        class='form-text mt-n2 mb-2',
                        paste(
                            'Simultaneous bands cover the whole line. At a peak lag',
                            'they come from 1000 posterior draws that each find their',
                            'own peak, so they include uncertainty about the peak.'
                        )
                    ),
                    shiny::checkboxInput(
                        'unconditional', 'Include smoothing-parameter uncertainty', FALSE
                    )
                )
            )
        ),
        bslib::card(
            full_screen=TRUE, fill=TRUE,
            bslib::card_body(plotly::plotlyOutput('plot', height='100%'), min_height='420px'),
            bslib::card_footer(
                shiny::div(
                    style='display:flex; justify-content:space-between; align-items:center;',
                    shiny::textOutput('status', inline=TRUE),
                    shiny::div(
                        style='display:flex; gap:1rem; align-items:center;',
                        shiny::conditionalPanel(
                            "input.mode == 'line'",
                            shiny::div(
                                class='mb-n3',
                                shiny::checkboxInput('show_peaks', 'Show peak lags', TRUE)
                            )
                        ),
                        shiny::downloadButton('download', 'Data (CSV)', class='btn-sm')
                    )
                )
            )
        ),
        # The lines card has a fixed height and scrolls, so the plot keeps its
        # size however many lines there are.
        shiny::conditionalPanel(
            "input.mode == 'line'",
            style='flex:0 0 auto;',
            bslib::card(
                fill=FALSE, height='220px',
                bslib::card_header(
                    shiny::div(
                        style=paste(
                            'display:flex; justify-content:space-between;',
                            'align-items:center; width:100%;'
                        ),
                        shiny::span('Lines'),
                        shiny::div(
                            class='ms-auto',
                            shiny::actionLink(
                                'show_definition_yaml', 'Definition YAML',
                                icon=shiny::icon('code'), class='me-3'
                            ),
                            shiny::actionLink('clear_lines', 'Remove all')
                        )
                    )
                ),
                bslib::card_body(
                    fillable=FALSE, class='p-0', style='overflow-y:auto;',
                    shiny::uiOutput('line_list', style='min-height:2.5em;')
                )
            )
        )
    )
}

.cdrgam_cli_viz_server <- function(cdrgam_root, control=NULL) {
    force(cdrgam_root)
    fits <- new.env(parent=emptyenv())
    results <- new.env(parent=emptyenv())
    load_fit <- function(path) {
        key <- paste(path, unclass(file.info(path)$mtime))
        if (!exists(key, envir=fits, inherits=FALSE)) {
            fit <- readRDS(path)
            assign(key, list(
                fit=fit, terms=.cdrgam_cli_viz_terms(fit),
                catalog=.cdrgam_cli_core_effect_catalog(fit)
            ), envir=fits)
        }
        get(key, envir=fits, inherits=FALSE)
    }
    function(input, output, session) {
        models <- shiny::reactiveVal(.cdrgam_cli_viz_models(cdrgam_root=cdrgam_root))
        lines <- shiny::reactiveVal(list())
        editing <- shiny::reactiveVal(NULL)
        surface <- shiny::reactiveVal(NULL)
        status_message <- shiny::reactiveVal('')
        data_version <- shiny::reactiveVal(0L)

        shiny::observeEvent(input$refresh, {
            available <- .cdrgam_cli_viz_models(cdrgam_root=cdrgam_root)
            models(available)
            data_version(data_version() + 1L)
            shiny::showNotification(paste0(
                'Found ', nrow(available), ' completed fit',
                if (nrow(available) == 1L) '' else 's'
            ), duration=3)
        })
        shiny::observeEvent(input$restart, {
            if (is.null(control) || !isTRUE(control$restartable)) return()
            control$restart <- TRUE
            session$sendCustomMessage('cdrgam-restart', TRUE)
            later::later(function() shiny::stopApp(), 0.5)
        })
        shiny::observe({
            session$sendCustomMessage('cdrgam-lines', .cdrgam_cli_viz_lines_json(lines()))
        })
        # Appearance settings survive a restart in session storage. Restored
        # values are applied once, when the controls are next drawn; later
        # redraws keep the controls' current values.
        restored_appearance <- shiny::reactiveVal(NULL)
        appearance_state <- new.env(parent=emptyenv())
        shiny::observeEvent(input$restore_appearance, {
            values <- tryCatch(
                jsonlite::fromJSON(input$restore_appearance, simplifyVector=TRUE),
                error=function(error) NULL
            )
            if (is.list(values) && length(values)) {
                restored_appearance(values)
                if (isFALSE(values$show_peaks)) {
                    shiny::updateCheckboxInput(session, 'show_peaks', value=FALSE)
                }
            }
        }, once=TRUE)
        output$appearance_controls <- shiny::renderUI({
            mode <- input$mode
            input$render_geometry
            restored <- restored_appearance()
            values <- shiny::isolate(.cdrgam_cli_viz_appearance_values(input))
            if (!is.null(restored) && !isTRUE(appearance_state$applied)) {
                appearance_state$applied <- TRUE
                values <- restored
            }
            .cdrgam_cli_viz_appearance(mode, values)
        })
        shiny::outputOptions(output, 'appearance_controls', suspendWhenHidden=FALSE)
        shiny::observe({
            values <- .cdrgam_cli_viz_appearance_values(input)
            if (length(values)) session$sendCustomMessage(
                'cdrgam-appearance',
                as.character(jsonlite::toJSON(values, auto_unbox=TRUE))
            )
        })
        settings <- shiny::reactive(.cdrgam_cli_viz_settings(input))
        shiny::observeEvent(input$show_render_yaml, {
            text <- tryCatch(
                .cdrgam_cli_viz_render_yaml(settings(), input$mode),
                error=function(error) {
                    shiny::showNotification(conditionMessage(error), type='error')
                    NULL
                }
            )
            if (is.null(text)) return()
            shiny::showModal(shiny::modalDialog(
                title='Render settings', shiny::tags$pre(text),
                shiny::div(class='form-text', paste(
                    'Paste this into a visualization definition. File-only',
                    'settings (formats, width, height, dpi) and facet scales',
                    'are not set by the viewer.'
                )),
                easyClose=TRUE, footer=shiny::modalButton('Close')
            ))
        })
        shiny::observeEvent(input$show_definition_yaml, {
            text <- tryCatch(
                .cdrgam_cli_viz_definition_yaml(
                    lines(), function(id) load_fit(model_path(id))$terms, options(),
                    settings()
                ),
                error=function(error) {
                    shiny::showNotification(conditionMessage(error), type='error')
                    NULL
                }
            )
            if (is.null(text)) return()
            shiny::showModal(shiny::modalDialog(
                title='Visualization definition', size='l', shiny::tags$pre(text),
                shiny::div(class='form-text', paste(
                    'cdrgam run renders this definition with the same lines,',
                    'options, and appearance. Add formats, width, height, or dpi',
                    'under render for the files.'
                )),
                easyClose=TRUE, footer=shiny::modalButton('Close')
            ))
        })
        shiny::observeEvent(input$restore_lines, {
            if (length(shiny::isolate(lines()))) return()
            restored <- tryCatch(
                .cdrgam_cli_viz_lines_parse(input$restore_lines),
                error=function(error) list()
            )
            if (length(restored)) lines(restored)
        }, once=TRUE)
        shiny::observe({
            available <- models()
            choices <- stats::setNames(available$id, available$id)
            selected <- shiny::isolate(input$model)
            if (is.null(selected) || !(selected %in% choices)) selected <- choices[1L]
            shiny::updateSelectInput(session, 'model', choices=choices, selected=selected)
            if (!nrow(available)) status_message('No completed fits were found')
        })

        model_path <- function(id) {
            available <- models()
            available$fit[match(id, available$id)]
        }
        current <- shiny::reactive({
            shiny::req(input$model)
            path <- model_path(input$model)
            shiny::req(!is.na(path))
            shiny::withProgress(message='Loading fit', value=0.5, load_fit(path))
        })
        shiny::observe({
            terms <- current()$terms
            labels <- vapply(terms, `[[`, character(1), 'label')
            pending <- shiny::isolate(session$userData$pending_term)
            selected <- if (!is.null(pending) && pending %in% labels) pending else {
                previous <- shiny::isolate(input$term)
                if (!is.null(previous) && previous %in% labels) previous else labels[1L]
            }
            session$userData$pending_term <- NULL
            shiny::updateSelectInput(session, 'term', choices=labels, selected=selected)
        })
        term <- shiny::reactive({
            terms <- current()$terms
            shiny::req(input$term)
            match <- Filter(function(value) identical(value$label, input$term), terms)
            shiny::req(length(match) == 1L)
            match[[1L]]
        })

        output$axis_controls <- shiny::renderUI({
            info <- term()
            axes <- names(info$axes)
            # Without a pending line, keep the axes already chosen, so a data
            # refresh leaves the form as it was.
            pending <- shiny::isolate(.cdrgam_cli_null(
                session$userData$pending_varying, c(input$x_axis, input$y_axis)
            ))
            if (identical(input$mode, 'surface')) {
                if (length(axes) < 2L) return(shiny::div(
                    class='text-danger small mb-3',
                    'This term has one axis; a surface needs two.'
                ))
                x <- if (length(pending) >= 1L && pending[[1L]] %in% axes) {
                    pending[[1L]]
                } else axes[[1L]]
                y <- if (length(pending) >= 2L && pending[[2L]] %in% axes) pending[[2L]] else
                    setdiff(axes, x)[[1L]]
                shiny::tagList(
                    shiny::selectInput('x_axis', 'Horizontal axis', axes, selected=x),
                    shiny::selectInput('y_axis', 'Depth axis', axes, selected=y)
                )
            } else {
                x <- if (length(pending) && pending[[1L]] %in% axes) pending[[1L]] else axes[[1L]]
                shiny::selectInput('x_axis', 'Horizontal axis', axes, selected=x)
            }
        })
        varying <- shiny::reactive({
            info <- term()
            axes <- names(info$axes)
            x <- input$x_axis
            shiny::req(x, x %in% axes)
            if (identical(input$mode, 'surface')) {
                y <- input$y_axis
                shiny::req(y, y %in% axes)
                shiny::validate(shiny::need(
                    !identical(x, y), 'Choose two different axes'
                ))
                c(x, y)
            } else x
        })

        output$group_controls <- shiny::renderUI({
            info <- term()
            if (!isTRUE(info$grouped)) return(NULL)
            pending <- shiny::isolate(.cdrgam_cli_null(
                session$userData$pending_group,
                list(grouping=input$grouping, group=input$group)
            ))
            shiny::tagList(
                shiny::selectInput(
                    'grouping', 'Grouping',
                    c('Population'='population', 'Group deviation'='deviation',
                      'Conditional on group'='conditional'),
                    selected=.cdrgam_cli_null(pending$grouping, 'population')
                ),
                shiny::conditionalPanel(
                    "input.grouping != 'population'",
                    shiny::selectInput(
                        'group', paste('Level of', info$group), info$groups,
                        selected=pending[['group']]
                    )
                )
            )
        })

        output$fixed_controls <- shiny::renderUI({
            info <- term()
            fixed_axes <- setdiff(names(info$axes), varying())
            if (!length(fixed_axes)) return(NULL)
            pending <- shiny::isolate(session$userData$pending_fixed)
            controls <- lapply(fixed_axes, function(name) {
                axis <- info$axes[[name]]
                # A pending line's value, else the value already typed.
                value <- if (is.null(pending)) {
                    typed <- shiny::isolate(input[[.cdrgam_cli_viz_input_id(name)]])
                    if (is.null(typed) || !nzchar(trimws(typed))) NULL else typed
                } else pending[[name]]
                if (is.null(value) || (!is.character(value) && is.na(value))) {
                    value <- .cdrgam_cli_viz_default(name)
                }
                hint <- paste0(
                    'mean ', .cdrgam_cli_viz_format(axis$mean),
                    ', sd ', .cdrgam_cli_viz_format(axis$sd), '; range ',
                    .cdrgam_cli_viz_format(axis$min), ' to ',
                    .cdrgam_cli_viz_format(axis$max)
                )
                shiny::div(
                    shiny::textInput(
                        .cdrgam_cli_viz_input_id(name), name,
                        value=.cdrgam_cli_viz_format(value)
                    ),
                    shiny::div(class='form-text mt-n2 mb-2', hint)
                )
            })
            shiny::tagList(
                shiny::tags$h6('Fixed values', class='mt-2'), controls,
                shiny::div(
                    class='form-text mt-n2 mb-2',
                    paste(
                        'Enter a number or an expression in mean, sd, and median,',
                        'such as mean + 2 * sd. Lag also accepts peak (or',
                        'peak-pos) and peak-neg: the lag at which the term\'s',
                        'response to a one-SD increase, under the chosen',
                        'composition, is largest or smallest.',
                        'Add a term label, as in peak(irf(1)), to use that',
                        'term\'s peak instead. peak-pointwise (or',
                        'peak-pointwise-neg) holds each point at its own peak',
                        'lag for this line\'s contrast, shown in a panel below.',
                        'peak-pointwise(x) uses the peak of term x at each',
                        'point\'s values of x\'s predictors instead.'
                    )
                )
            )
        })

        output$reference_controls <- shiny::renderUI({
            info <- term()
            axes <- .cdrgam_cli_viz_reference_axes(info, varying())
            if (!length(axes)) return(NULL)
            pending <- shiny::isolate(session$userData$pending_reference)
            # Without a pending line, keep what is already typed and checked.
            typed <- if (is.null(pending)) shiny::isolate(stats::setNames(lapply(
                axes, function(name) input[[.cdrgam_cli_viz_reference_id(name)]]
            ), axes))
            checked <- if (is.null(pending)) {
                isTRUE(shiny::isolate(input$use_reference))
            } else length(pending) > 0L
            fields <- lapply(axes, function(name) {
                value <- if (is.null(pending)) typed[[name]] else pending[[name]]
                value <- if (is.null(value)) '' else if (!is.character(value) && is.na(value)) {
                    'default'
                } else .cdrgam_cli_viz_format(value)
                shiny::tagAppendAttributes(
                    shiny::textInput(
                        .cdrgam_cli_viz_reference_id(name), name, value=value
                    ),
                    placeholder='plotted value', .cssSelector='input'
                )
            })
            shiny::tagList(
                shiny::tags$h6('Reference', class='mt-2'),
                shiny::checkboxInput('use_reference', 'Subtract a reference', checked),
                shiny::conditionalPanel(
                    'input.use_reference',
                    fields,
                    shiny::div(
                        class='form-text mt-n2 mb-2',
                        paste(
                            'The line shows the effect minus the effect with these',
                            'fixed values changed; plotted axes keep their values.',
                            'An empty field keeps the line\'s fixed value. A',
                            'predictor set to default takes the model default: terms',
                            'involving it drop out of the reference.'
                        )
                    )
                )
            )
        })

        spec <- function() {
            info <- term()
            axes <- varying()
            fixed <- lapply(setdiff(names(info$axes), axes), function(name) {
                .cdrgam_cli_viz_input_value(
                    input[[.cdrgam_cli_viz_input_id(name)]], name, pointwise=TRUE
                )
            })
            names(fixed) <- setdiff(names(info$axes), axes)
            grouping <- if (isTRUE(info$grouped)) {
                .cdrgam_cli_null(input$grouping, 'population')
            } else 'population'
            value <- list(
                model_id=input$model, fit=model_path(input$model), term=info$label,
                varying=axes, fixed=fixed, composition=input$composition,
                grouping=grouping,
                group=if (identical(grouping, 'population')) NULL else input$group
            )
            if (isTRUE(input$use_reference)) {
                names <- .cdrgam_cli_viz_reference_axes(info, axes)
                value$reference <- .cdrgam_cli_viz_reference_values(
                    stats::setNames(lapply(names, function(name) {
                        text <- trimws(.cdrgam_cli_null(
                            input[[.cdrgam_cli_viz_reference_id(name)]], ''
                        ))
                        if (!nzchar(text)) return(NULL)
                        if (identical(text, 'default')) return(NA_real_)
                        .cdrgam_cli_viz_input_value(text, name)
                    }), names)
                )
            }
            value$label <- if (nzchar(trimws(.cdrgam_cli_null(input$label, '')))) {
                trimws(input$label)
            } else .cdrgam_cli_viz_label(value, info)
            value
        }

        options <- shiny::reactive(list(
            n={
                n <- suppressWarnings(as.integer(.cdrgam_cli_null(input$n, NA)))
                if (length(n) != 1L || is.na(n)) 200L else max(2L, n)
            },
            level=.cdrgam_cli_null(input$level, 0.95),
            unconditional=isTRUE(input$unconditional),
            interval=.cdrgam_cli_null(input$interval, 'pointwise'),
            ticks=if (!identical(input$mode, 'surface') &&
                    .cdrgam_cli_viz_ticks_evaluated(input)) {
                tryCatch(.cdrgam_cli_viz_ticks(input$x_ticks), error=function(error) NULL)
            }
        ))

        evaluate <- function(value, options) {
            key <- digest::digest(list(
                value[setdiff(names(value), c('label', 'id'))], options,
                unclass(file.info(value$fit)$mtime)
            ))
            if (exists(key, envir=results, inherits=FALSE)) {
                return(get(key, envir=results, inherits=FALSE))
            }
            loaded <- load_fit(value$fit)
            match <- Filter(function(term) identical(term$label, value$term), loaded$terms)
            if (length(match) != 1L) .cdrgam_cli_abort(paste0(
                'Term ', value$term, ' is no longer in ', value$model_id
            ))
            data <- .cdrgam_cli_viz_estimate(
                loaded$fit, match[[1L]], value, options, loaded$catalog
            )
            assign(key, data, envir=results)
            data
        }

        restore <- function(value) {
            session$userData$pending_term <- value$term
            session$userData$pending_varying <- value$varying
            session$userData$pending_fixed <- .cdrgam_cli_null(value$fixed, list())
            session$userData$pending_reference <- .cdrgam_cli_null(value$reference, list())
            session$userData$pending_group <- list(
                grouping=value$grouping, group=value[['group']]
            )
            shiny::updateSelectInput(session, 'composition', selected=value$composition)
            if (!identical(input$model, value$model_id)) {
                shiny::updateSelectInput(session, 'model', selected=value$model_id)
            } else {
                shiny::updateSelectInput(session, 'term', selected=value$term)
            }
        }
        # The form and options survive a page reload or server restart in
        # session storage: the model, term, axes, fixed and reference values,
        # composition, grouping, label, and the Options panel.
        option_inputs <- c(
            'n', 'level', 'interval', 'unconditional', 'x_ticks', 'x_tick_labels'
        )
        shiny::observe({
            info <- tryCatch(term(), error=function(error) NULL)
            axes <- if (is.null(info)) character() else names(info$axes)
            chosen <- c(input$x_axis, if (identical(input$mode, 'surface')) input$y_axis)
            fixed <- setdiff(axes, chosen)
            references <- if (is.null(info)) character() else {
                .cdrgam_cli_viz_reference_axes(info, chosen)
            }
            state <- list(
                mode=input$mode, model_id=input$model, term=input$term, varying=chosen,
                fixed=stats::setNames(lapply(fixed, function(name) {
                    .cdrgam_cli_null(input[[.cdrgam_cli_viz_input_id(name)]], '')
                }), fixed),
                reference=if (isTRUE(input$use_reference)) stats::setNames(lapply(
                    references, function(name) {
                        .cdrgam_cli_null(input[[.cdrgam_cli_viz_reference_id(name)]], '')
                    }
                ), references),
                composition=input$composition, grouping=input$grouping,
                group=input$group, label=input$label,
                options=stats::setNames(lapply(option_inputs, function(name) input[[name]]),
                    option_inputs)
            )
            if (is.null(state$model_id)) return()
            session$sendCustomMessage('cdrgam-form', as.character(jsonlite::toJSON(
                state, auto_unbox=TRUE, null='null', digits=NA
            )))
        })
        shiny::observeEvent(input$restore_form, {
            state <- tryCatch(
                jsonlite::fromJSON(input$restore_form, simplifyVector=FALSE),
                error=function(error) NULL
            )
            if (!is.list(state) || is.null(state$model_id)) return()
            options <- state$options
            if (!is.null(options$n)) shiny::updateNumericInput(session, 'n', value=options$n)
            if (!is.null(options$level)) {
                shiny::updateNumericInput(session, 'level', value=options$level)
            }
            if (!is.null(options$interval)) {
                shiny::updateRadioButtons(session, 'interval', selected=options$interval)
            }
            if (!is.null(options$unconditional)) shiny::updateCheckboxInput(
                session, 'unconditional', value=isTRUE(options$unconditional)
            )
            if (!is.null(options$x_ticks)) {
                shiny::updateTextInput(session, 'x_ticks', value=options$x_ticks)
            }
            if (!is.null(options$x_tick_labels)) shiny::updateTextInput(
                session, 'x_tick_labels', value=options$x_tick_labels
            )
            if (!is.null(state$mode)) {
                shiny::updateRadioButtons(session, 'mode', selected=state$mode)
            }
            if (!is.null(state$label)) shiny::updateTextInput(session, 'label', value=state$label)
            state$varying <- as.character(unlist(state$varying))
            text <- function(values) lapply(values, function(value) {
                if (is.null(value)) '' else as.character(value)
            })
            state$fixed <- text(state$fixed)
            if (!is.null(state$reference)) state$reference <- text(state$reference)
            if (is.null(state$grouping)) state$grouping <- 'population'
            restore(state)
        }, once=TRUE)

        shiny::observeEvent(input$term, {
            session$userData$pending_varying <- NULL
            session$userData$pending_fixed <- NULL
            session$userData$pending_group <- NULL
            session$userData$pending_reference <- NULL
        }, ignoreInit=TRUE, priority=-10)

        shiny::observeEvent(input$add_line, {
            value <- tryCatch(spec(), error=function(error) {
                shiny::showNotification(conditionMessage(error), type='error')
                NULL
            })
            if (is.null(value)) return()
            value$id <- .cdrgam_cli_random_id('line')
            lines(c(lines(), list(value)))
            shiny::updateTextInput(session, 'label', value='')
        })
        # While a line is being edited, Update and Cancel replace Add.
        output$line_buttons <- shiny::renderUI({
            if (is.null(editing())) {
                return(shiny::actionButton('add_line', 'Add line', class='btn-primary'))
            }
            shiny::tagList(
                shiny::actionButton('update_line', 'Update line', class='btn-primary'),
                shiny::actionLink('cancel_edit', 'Cancel', class='ms-2 align-self-center')
            )
        })
        # The buttons start in a panel hidden in surface mode; keep them drawn
        # so they are there when the panel appears.
        shiny::outputOptions(output, 'line_buttons', suspendWhenHidden=FALSE)
        shiny::observeEvent(input$update_line, {
            target <- editing()
            value <- tryCatch(spec(), error=function(error) {
                shiny::showNotification(conditionMessage(error), type='error')
                NULL
            })
            if (is.null(value) || is.null(target)) return()
            value$id <- target
            lines(lapply(lines(), function(line) if (identical(line$id, target)) value else line))
            editing(NULL)
            shiny::updateTextInput(session, 'label', value='')
        })
        shiny::observeEvent(input$cancel_edit, editing(NULL))
        shiny::observeEvent(input$remove_line, {
            lines(Filter(function(line) !identical(line$id, input$remove_line), lines()))
            if (identical(editing(), input$remove_line)) editing(NULL)
        })
        shiny::observeEvent(input$edit_line, {
            match <- Filter(function(line) identical(line$id, input$edit_line), lines())
            if (!length(match)) return()
            editing(input$edit_line)
            shiny::updateTextInput(session, 'label', value=match[[1L]]$label)
            restore(match[[1L]])
        })
        # Move a line one place up (`step = -1`) or down (`step = 1`); its
        # place sets its legend order and its color.
        move_line <- function(id, step) {
            values <- lines()
            ids <- vapply(values, `[[`, character(1), 'id')
            from <- match(id, ids)
            to <- from + step
            if (is.na(from) || to < 1L || to > length(values)) return()
            values[c(from, to)] <- values[c(to, from)]
            lines(values)
        }
        shiny::observeEvent(input$move_line_up, move_line(input$move_line_up, -1L))
        shiny::observeEvent(input$move_line_down, move_line(input$move_line_down, 1L))
        shiny::observeEvent(input$clear_lines, {
            lines(list())
            editing(NULL)
        })

        output$line_list <- shiny::renderUI({
            values <- lines()
            if (!length(values)) return(shiny::div(
                class='text-muted small p-2',
                'Choose a term and fixed values, then press Add line.'
            ))
            ids <- vapply(values, `[[`, character(1), 'id')
            chosen <- tryCatch(settings(), error=function(error) list())
            colors <- tryCatch(
                .cdrgam_cli_viz_line_colors(
                    ids, .cdrgam_cli_null(chosen$palette, 'okabe-ito'),
                    isTRUE(chosen$reverse_palette)
                ),
                error=function(error) .cdrgam_cli_viz_line_colors(ids)
            )
            shiny::tags$ul(class='list-group list-group-flush', lapply(
                seq_along(values), function(index) {
                line <- values[[index]]
                color <- colors[[index]]
                action <- function(input_id, text, title=NULL, enabled=TRUE) shiny::tags$a(
                    href='#', title=title, text,
                    class=paste('ms-2 small', if (!enabled) 'invisible' else ''),
                    onclick=sprintf(
                        "Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                        input_id, line$id
                    )
                )
                shiny::tags$li(
                    class=paste(
                        'list-group-item d-flex align-items-center',
                        if (identical(editing(), line$id)) 'active' else ''
                    ),
                    shiny::span(style=sprintf(
                        paste0(
                            'display:inline-block;width:1.5em;height:3px;',
                            'background:%s;margin-right:0.6em;'
                        ),
                        color
                    )),
                    shiny::span(class='flex-grow-1', line$label),
                    action('move_line_up', '\u2191', 'Move up', index > 1L),
                    action('move_line_down', '\u2193', 'Move down', index < length(values)),
                    action('edit_line', 'Edit'), action('remove_line', 'Remove')
                )
            }))
        })

        # An empty list has zero height, which Shiny would treat as hidden and
        # never render, so keep this output live.
        shiny::outputOptions(output, 'line_list', suspendWhenHidden=FALSE)

        shiny::observeEvent(input$draw_surface, {
            value <- tryCatch(spec(), error=function(error) {
                shiny::showNotification(conditionMessage(error), type='error')
                NULL
            })
            if (!is.null(value)) surface(value)
        })

        plotted <- shiny::reactive({
            data_version()
            settings <- options()
            if (identical(input$mode, 'surface')) {
                value <- surface()
                shiny::validate(shiny::need(
                    !is.null(value), 'Choose two axes, then press Draw surface.'
                ))
                surface_options <- settings
                surface_options$n <- min(settings$n, 100L)
                data <- shiny::withProgress(
                    message='Evaluating surface', value=0.5,
                    evaluate(value, surface_options)
                )
                list(mode='surface', entries=list(list(spec=value, data=data)))
            } else {
                values <- lines()
                shiny::validate(shiny::need(
                    length(values) > 0L, 'Add a line to start the plot.'
                ))
                entries <- shiny::withProgress(message='Evaluating lines', value=0, {
                    lapply(seq_along(values), function(index) {
                        shiny::incProgress(1 / length(values))
                        data <- tryCatch(
                            evaluate(values[[index]], settings),
                            error=function(error) {
                            shiny::showNotification(paste0(
                                values[[index]]$label, ': ', conditionMessage(error)
                            ), type='error', duration=10)
                            NULL
                        })
                        if (is.null(data)) NULL else list(spec=values[[index]], data=data)
                    })
                })
                list(mode='line', entries=Filter(Negate(is.null), entries))
            }
        })

        shiny::observeEvent(input$x_tick_labels, {
            message <- tryCatch({
                .cdrgam_cli_viz_tick_labels(input$x_tick_labels, input$x_ticks)
                NULL
            }, error=conditionMessage)
            if (!is.null(message)) shiny::showNotification(message, type='error')
        }, ignoreInit=TRUE)

        shiny::observeEvent(input$x_ticks, {
            message <- tryCatch({
                .cdrgam_cli_viz_ticks(input$x_ticks)
                NULL
            }, error=conditionMessage)
            if (!is.null(message)) shiny::showNotification(message, type='error')
        }, ignoreInit=TRUE)

        output$plot <- plotly::renderPlotly({
            value <- plotted()
            shiny::validate(shiny::need(length(value$entries) > 0L, 'Nothing to plot.'))
            # Appearance errors, such as a palette with too few colors, are
            # shown in place of the plot.
            plot <- tryCatch({
                chosen <- settings()
                if (identical(value$mode, 'surface')) {
                    .cdrgam_cli_viz_surface_plot(
                        value$entries[[1L]]$data, value$entries[[1L]]$spec,
                        settings=chosen
                    )
                } else {
                    # Points and bars at chosen x values put a tick at each.
                    if (length(options()$ticks) && is.null(chosen$spacing)) {
                        chosen$spacing <- 'linear'
                    }
                    .cdrgam_cli_viz_line_plot(value$entries, settings=chosen)
                }
            }, error=function(error) conditionMessage(error))
            shiny::validate(shiny::need(!is.character(plot), plot))
            plotly::config(plot, displaylogo=FALSE, toImageButtonOptions=list(
                format='svg', filename='cdrgam-effect'
            ))
        })

        output$status <- shiny::renderText({
            value <- tryCatch(plotted(), error=function(error) NULL)
            if (is.null(value) || !length(value$entries)) return(status_message())
            level <- options()$level
            paste0(
                'Linear-predictor scale; ', options()$interval, ' ',
                round(100 * level), '% intervals',
                if (isTRUE(options()$unconditional)) ', smoothing-parameter adjusted' else '',
                if (identical(value$mode, 'line') && length(value$entries) >
                        length(.cdrgam_cli_okabe_ito) && identical(
                            .cdrgam_cli_null(
                                tryCatch(settings()$palette, error=function(e) NULL),
                                'okabe-ito'
                            ), 'okabe-ito'
                        )) {
                    paste0('; more than ', length(.cdrgam_cli_okabe_ito),
                        ' lines: using viridis')
                } else ''
            )
        })

        output$download <- shiny::downloadHandler(
            filename=function() paste0(
                'cdrgam-effect-', format(Sys.time(), '%Y%m%d-%H%M%S'), '.csv'
            ),
            content=function(file) {
                value <- plotted()
                tables <- lapply(value$entries, function(entry) {
                    data <- as.data.frame(entry$data)
                    data$model <- entry$spec$model_id
                    data$line <- entry$spec$label
                    data
                })
                columns <- unique(unlist(lapply(tables, names)))
                tables <- lapply(tables, function(data) {
                    for (name in setdiff(columns, names(data))) data[[name]] <- NA
                    data[columns]
                })
                utils::write.csv(do.call(rbind, tables), file, row.names=FALSE)
            }
        )
    }
}

.cdrgam_cli_viz_app <- function(cdrgam_root, control=NULL) {
    restartable <- !is.null(control) && isTRUE(control$restartable)
    shiny::shinyApp(
        .cdrgam_cli_viz_ui(restartable), .cdrgam_cli_viz_server(cdrgam_root, control)
    )
}
