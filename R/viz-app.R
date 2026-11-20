.cdrgam_cli_viz_colors <- c(
    '#0072B2', '#D55E00', '#009E73', '#CC79A7', '#E69F00', '#56B4E9',
    '#7F7F7F', '#000000'
)

.cdrgam_cli_viz_rgba <- function(color, alpha) {
    value <- grDevices::col2rgb(color)
    sprintf('rgba(%d,%d,%d,%.2f)', value[1L], value[2L], value[3L], alpha)
}

.cdrgam_cli_viz_input_id <- function(axis) {
    paste0('fixed_', gsub('[^A-Za-z0-9]', '_', axis), '_', substr(
        digest::digest(axis, algo='md5'), 1L, 6L
    ))
}

.cdrgam_cli_viz_reference_id <- function(axis) {
    sub('^fixed_', 'reference_', .cdrgam_cli_viz_input_id(axis))
}

# Predictor axes can be left at the model default; lag and time cannot.
.cdrgam_cli_viz_reference_axes <- function(term) {
    setdiff(names(term$axes), c('lag', 'time'))
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

.cdrgam_cli_viz_line_plot <- function(entries, show_interval=TRUE) {
    plot <- plotly::plot_ly()
    axes <- character()
    for (index in seq_along(entries)) {
        entry <- entries[[index]]
        data <- entry$data
        axis <- entry$spec$varying[[1L]]
        axes <- c(axes, axis)
        order <- order(data[[axis]])
        x <- data[[axis]][order]
        color <- .cdrgam_cli_viz_colors[
            (index - 1L) %% length(.cdrgam_cli_viz_colors) + 1L
        ]
        group <- entry$spec$id
        if (show_interval && any(is.finite(data$se))) {
            plot <- plotly::add_ribbons(
                plot, x=x, ymin=data$lower[order], ymax=data$upper[order],
                fillcolor=.cdrgam_cli_viz_rgba(color, 0.18),
                line=list(width=0), legendgroup=group, showlegend=FALSE,
                hoverinfo='skip', name=entry$spec$label
            )
        }
        plot <- plotly::add_lines(
            plot, x=x, y=data$estimate[order], name=entry$spec$label,
            legendgroup=group, line=list(color=color, width=2),
            text=sprintf(
                '%s = %s<br>estimate %s<br>CI [%s, %s]', axis,
                signif(x, 4L), signif(data$estimate[order], 4L),
                signif(data$lower[order], 4L), signif(data$upper[order], 4L)
            ),
            hoverinfo='text'
        )
    }
    plotly::layout(
        plot,
        xaxis=list(title=paste(unique(axes), collapse=' / '), zeroline=FALSE),
        yaxis=list(title='Effect (linear predictor)', zeroline=TRUE),
        legend=list(orientation='h', y=-0.2),
        hovermode='closest'
    )
}

.cdrgam_cli_viz_surface_plot <- function(data, spec, display='surface', show_interval=FALSE) {
    x <- spec$varying[[1L]]
    y <- spec$varying[[2L]]
    estimate <- .cdrgam_cli_viz_surface_matrix(data, x, y)
    limit <- max(abs(estimate$z), na.rm=TRUE)
    if (!is.finite(limit) || limit == 0) limit <- 1
    if (identical(display, 'surface')) {
        plot <- plotly::plot_ly(
            x=estimate$x, y=estimate$y, z=estimate$z, type='surface',
            colorscale='RdBu', reversescale=TRUE, cmin=-limit, cmax=limit,
            colorbar=list(title='Effect'), name='estimate'
        )
        if (show_interval && any(is.finite(data$se))) {
            for (bound in c('lower', 'upper')) {
                value <- .cdrgam_cli_viz_surface_matrix(data, x, y, bound)
                plot <- plotly::add_surface(
                    plot, x=value$x, y=value$y, z=value$z, opacity=0.3,
                    showscale=FALSE, colorscale=list(c(0, '#888888'), c(1, '#888888')),
                    name=bound
                )
            }
        }
        return(plotly::layout(plot, scene=list(
            xaxis=list(title=x), yaxis=list(title=y),
            zaxis=list(title='Effect (linear predictor)')
        )))
    }
    plot <- plotly::plot_ly(
        x=estimate$x, y=estimate$y, z=estimate$z,
        type=if (identical(display, 'contour')) 'contour' else 'heatmap',
        colorscale='RdBu', reversescale=TRUE, zmin=-limit, zmax=limit,
        colorbar=list(title='Effect')
    )
    plotly::layout(plot, xaxis=list(title=x), yaxis=list(title=y))
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
# numeric, and anything else must be a value expression for that axis.
.cdrgam_cli_viz_input_value <- function(text, name) {
    if (is.null(text) || (length(text) == 1L && is.na(text))) return(NA_real_)
    if (is.numeric(text)) return(as.numeric(text))
    text <- trimws(as.character(text))
    if (!nzchar(text)) return(NA_real_)
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
                    shiny::actionButton('add_line', 'Add line', class='btn-primary'),
                    shiny::uiOutput('update_button', inline=TRUE)
                )
            ),
            shiny::conditionalPanel(
                "input.mode == 'surface'",
                shiny::radioButtons(
                    'display', 'Display',
                    c('3D surface'='surface', 'Heatmap'='heatmap', 'Contour'='contour'),
                    inline=TRUE
                ),
                shiny::actionButton('draw_surface', 'Draw surface', class='btn-primary')
            ),
            bslib::accordion(
                open=FALSE,
                bslib::accordion_panel(
                    'Options',
                    shiny::numericInput('n', 'Grid points per axis', 200, min=2, max=2000),
                    shiny::numericInput(
                        'level', 'Interval level', 0.95,
                        min=0.5, max=0.999, step=0.01
                    ),
                    shiny::checkboxInput('show_interval', 'Show intervals', TRUE),
                    shiny::radioButtons(
                        'interval', 'Intervals',
                        c('Pointwise'='pointwise', 'Simultaneous'='simultaneous'),
                        inline=TRUE
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
                    shiny::downloadButton('download', 'Data (CSV)', class='btn-sm')
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
                        style='display:flex; justify-content:space-between;',
                        'Lines',
                        shiny::actionLink('clear_lines', 'Remove all')
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
            assign(key, list(fit=fit, terms=.cdrgam_cli_viz_terms(fit)), envir=fits)
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
            pending <- shiny::isolate(session$userData$pending_varying)
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
            pending <- shiny::isolate(session$userData$pending_group)
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
                value <- pending[[name]]
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
                        'response to a one-SD increase is largest or smallest.'
                    )
                )
            )
        })

        output$reference_controls <- shiny::renderUI({
            info <- term()
            axes <- .cdrgam_cli_viz_reference_axes(info)
            if (!length(axes)) return(NULL)
            pending <- shiny::isolate(session$userData$pending_reference)
            fields <- lapply(axes, function(name) {
                value <- pending[[name]]
                value <- if (is.null(value) || (!is.character(value) && is.na(value))) {
                    ''
                } else .cdrgam_cli_viz_format(value)
                shiny::tagAppendAttributes(
                    shiny::textInput(
                        .cdrgam_cli_viz_reference_id(name), name, value=value
                    ),
                    placeholder='model default', .cssSelector='input'
                )
            })
            shiny::tagList(
                shiny::tags$h6('Reference', class='mt-2'),
                shiny::checkboxInput(
                    'use_reference', 'Subtract a reference', length(pending) > 0L
                ),
                shiny::conditionalPanel(
                    'input.use_reference',
                    fields,
                    shiny::div(
                        class='form-text mt-n2 mb-2',
                        paste(
                            'The line shows the effect minus the effect at these values.',
                            'An empty field uses the model default: terms involving',
                            'that predictor drop out of the reference. To hold a',
                            'predictor at its plotted value, enter the same value.'
                        )
                    )
                )
            )
        })

        spec <- function() {
            info <- term()
            axes <- varying()
            fixed <- lapply(setdiff(names(info$axes), axes), function(name) {
                .cdrgam_cli_viz_input_value(input[[.cdrgam_cli_viz_input_id(name)]], name)
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
                names <- .cdrgam_cli_viz_reference_axes(info)
                value$reference <- stats::setNames(lapply(names, function(name) {
                    .cdrgam_cli_viz_input_value(
                        input[[.cdrgam_cli_viz_reference_id(name)]], name
                    )
                }), names)
            }
            value$label <- if (nzchar(trimws(.cdrgam_cli_null(input$label, '')))) {
                trimws(input$label)
            } else .cdrgam_cli_viz_label(value, info)
            value
        }

        options <- shiny::reactive(list(
            n=max(2L, as.integer(.cdrgam_cli_null(input$n, 200L))),
            level=.cdrgam_cli_null(input$level, 0.95),
            unconditional=isTRUE(input$unconditional),
            interval=.cdrgam_cli_null(input$interval, 'pointwise')
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
            data <- .cdrgam_cli_viz_estimate(loaded$fit, match[[1L]], value, options)
            assign(key, data, envir=results)
            data
        }

        restore <- function(value) {
            session$userData$pending_term <- value$term
            session$userData$pending_varying <- value$varying
            session$userData$pending_fixed <- value$fixed
            session$userData$pending_reference <- value$reference
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
        output$update_button <- shiny::renderUI({
            if (is.null(editing())) return(NULL)
            shiny::tagList(
                shiny::actionButton('update_line', 'Update line'),
                shiny::actionLink('cancel_edit', 'Cancel', class='ms-2 align-self-center')
            )
        })
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
            shiny::tags$ul(class='list-group list-group-flush', lapply(
                seq_along(values), function(index) {
                line <- values[[index]]
                color <- .cdrgam_cli_viz_colors[
                    (index - 1L) %% length(.cdrgam_cli_viz_colors) + 1L
                ]
                action <- function(input_id, text) shiny::tags$a(
                    href='#', class='ms-2 small', text,
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

        output$plot <- plotly::renderPlotly({
            value <- plotted()
            shiny::validate(shiny::need(length(value$entries) > 0L, 'Nothing to plot.'))
            plot <- if (identical(value$mode, 'surface')) {
                .cdrgam_cli_viz_surface_plot(
                    value$entries[[1L]]$data, value$entries[[1L]]$spec,
                    display=input$display, show_interval=isTRUE(input$show_interval)
                )
            } else .cdrgam_cli_viz_line_plot(value$entries, isTRUE(input$show_interval))
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
                if (isTRUE(options()$unconditional)) ', smoothing-parameter adjusted' else ''
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
