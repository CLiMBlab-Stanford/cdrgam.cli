library(cdrgam.cli)

internal <- function(name) getFromNamespace(name, 'cdrgam.cli')
validate_render <- internal('.cdrgam_cli_validate_render')
validate_visualization <- internal('.cdrgam_cli_validate_visualization')
short_hash <- internal('.cdrgam_cli_short_hash')

# Render settings are part of visualization identity, so validation must
# keep producing the same normalized values, in the same field order, for
# every render it accepted before. The expected hashes were recorded from
# the validator as it stood before render fields moved into a registry.
render_cases <- list(
    line_minimal=list(geometry='line', mappings=list(x='lag', y='estimate')),
    line_full=list(
        geometry='line',
        mappings=list(
            x='lag', y='estimate', color='term', linetype='layer',
            group='term', facet='term'
        ),
        interval='lines', theme='slides', formats=list('pdf', 'png', 'pdf'),
        width=6, height=4, dpi=150, title='T', xlab='X', ylab='Y',
        palette='rainbow', scales='free', peaks='always'
    ),
    line_yaml_key=list(
        geometry='line', mappings=list(x='lag', 'TRUE'='estimate', color='term')
    ),
    line_ordered=list(
        theme='minimal', formats='svg', geometry='line', interval='none',
        mappings=list(y='estimate', x='lag'), palette='HUE'
    ),
    raster=list(
        geometry='raster-contour',
        mappings=list(x='lag', y='predictor:x', fill='estimate', facet='term'),
        interval='companion', formats='pdf'
    ),
    contour=list(
        geometry='contour', mappings=list(x='lag', y='x'), interval='none',
        palette='gray'
    ),
    point=list(geometry='point', mappings=list(x='lag', y='estimate')),
    point_spaced=list(
        geometry='point', mappings=list(x='lag', y='estimate', color='term'),
        spacing='linear', palette='set1', interval='none'
    ),
    bar_peak=list(
        geometry='bar', mappings=list(x='peak', y='estimate', color='term'),
        statistic='peak-lag', palette=list('#00f', 'rgb(255, 0, 0)')
    ),
    bar_estimate=list(
        geometry='bar', mappings=list(x='term', y='estimate'),
        statistic='estimate', spacing='uniform', palette='Magma'
    )
)

visualization_cases <- list(
    preset_lag=list(schema=1L, model='main', preset='irf-lag'),
    preset_predictor=list(
        schema=1L, model='main', preset='irf-predictor',
        options=list(hold=list(lag='peak')),
        render=list(formats=list('pdf', 'png'), palette='Dark2')
    ),
    layers=list(
        schema=1L, model='main',
        query=list(terms=list(predictors='x', grouped=TRUE)),
        layers=list(
            list(id='subjects', grouping='conditional', interval='none',
                style=list(color='grey70', alpha=0.2, linewidth=0.3)),
            list(id='population', grouping='population',
                style=list(color='black', linewidth=1))
        ),
        render=list(geometry='line', mappings=list(x='lag', y='estimate'))
    ),
    point_layers=list(
        schema=1L, model='main', query=list(terms='x'),
        layers=list(list(id='a', interval='errorbar'), list(id='b')),
        render=list(geometry='point', mappings=list(x='lag', y='estimate'))
    )
)

render_errors <- list(
    missing_geometry=list(mappings=list(x='lag', y='estimate')),
    unknown_field=list(geometry='line', mappings=list(x='lag', y='estimate'), size=2),
    bad_geometry=list(geometry='area', mappings=list(x='lag', y='estimate')),
    mappings_unnamed=list(geometry='line', mappings=list('lag')),
    mapping_unknown=list(geometry='line', mappings=list(x='lag', y='estimate', size='se')),
    mapping_missing_x=list(geometry='line', mappings=list(y='estimate')),
    mapping_missing_y=list(geometry='line', mappings=list(x='lag')),
    mapping_not_scalar=list(geometry='line', mappings=list(x=c('a', 'b'), y='estimate')),
    line_companion=list(
        geometry='line', mappings=list(x='lag', y='estimate'), interval='companion'
    ),
    surface_ribbon=list(geometry='contour', mappings=list(x='lag', y='x')),
    line_errorbar=list(
        geometry='line', mappings=list(x='lag', y='estimate'), interval='errorbar'
    ),
    bar_companion=list(
        geometry='bar', mappings=list(x='term', y='estimate'), interval='companion'
    ),
    raster_fill=list(
        geometry='raster', mappings=list(x='lag', y='x'), interval='none'
    ),
    bad_spacing=list(
        geometry='line', mappings=list(x='lag', y='estimate'), spacing='log'
    ),
    surface_spacing=list(
        geometry='contour', mappings=list(x='lag', y='x'), interval='none',
        spacing='uniform'
    ),
    bad_statistic=list(
        geometry='bar', mappings=list(x='term', y='estimate'), statistic='mean'
    ),
    line_peak=list(
        geometry='line', mappings=list(x='lag', y='estimate'), statistic='peak-lag'
    ),
    bad_theme=list(geometry='line', mappings=list(x='lag', y='estimate'), theme='neon'),
    bad_palette=list(geometry='line', mappings=list(x='lag', y='estimate'), palette='nope'),
    bad_gradient=list(
        geometry='line', mappings=list(x='lag', y='estimate'),
        palette=list('#0000FF', 'rgb(300, 0, 0)')
    ),
    numeric_gradient=list(
        geometry='line', mappings=list(x='lag', y='estimate'), palette=list(1, 2)
    ),
    bad_peaks=list(geometry='line', mappings=list(x='lag', y='estimate'), peaks='some'),
    bad_scales=list(geometry='line', mappings=list(x='lag', y='estimate'), scales='x'),
    bad_format=list(geometry='line', mappings=list(x='lag', y='estimate'), formats='jpg'),
    empty_formats=list(geometry='line', mappings=list(x='lag', y='estimate'), formats=list()),
    bad_width=list(geometry='line', mappings=list(x='lag', y='estimate'), width=-1),
    bad_dpi=list(geometry='line', mappings=list(x='lag', y='estimate'), dpi='high'),
    bad_title=list(geometry='line', mappings=list(x='lag', y='estimate'), title=1),
    not_mapping='line'
)

error_message <- function(expression) tryCatch({
    force(expression)
    NA_character_
}, error=function(error) conditionMessage(error))

observed <- list(
    render=vapply(render_cases, function(value) {
        short_hash(validate_render(value, 'render'))
    }, character(1)),
    order=vapply(render_cases, function(value) {
        paste(names(validate_render(value, 'render')), collapse=',')
    }, character(1)),
    visualization=vapply(visualization_cases, function(value) {
        short_hash(validate_visualization(value, 'v.yml')$render)
    }, character(1)),
    errors=vapply(render_errors, function(value) {
        error_message(validate_render(value, 'render'))
    }, character(1))
)

if (nzchar(Sys.getenv('CDRGAM_RECORD_RENDER'))) {
    dput(observed)
    quit(save='no')
}

expected <- list(
    render=c(
        line_minimal='2c53172643d1ee15',
        line_full='3a01d606ce5e50fb',
        line_yaml_key='f93cd51f9b56ecfb',
        line_ordered='ca3cb766ad944179',
        raster='268e4442f66ca64a',
        contour='b4da412240b766d6',
        point='d8fad4d151913560',
        point_spaced='7e85ac578c65dce4',
        bar_peak='c97756d53642ebf4',
        bar_estimate='9fc219441870b5d5'
    ),
    order=c(
        line_minimal='geometry,mappings,interval,theme,formats',
        line_full=paste0(
            'geometry,mappings,interval,theme,formats,width,height,dpi,title,xlab,y',
            'lab,palette,scales,peaks'
        ),
        line_yaml_key='geometry,mappings,interval,theme,formats',
        line_ordered='theme,formats,geometry,interval,mappings,palette',
        raster='geometry,mappings,interval,formats,theme',
        contour='geometry,mappings,interval,palette,theme,formats',
        point='geometry,mappings,interval,theme,formats',
        point_spaced='geometry,mappings,spacing,palette,interval,theme,formats',
        bar_peak='geometry,mappings,statistic,palette,interval,theme,formats',
        bar_estimate='geometry,mappings,spacing,palette,interval,theme,formats'
    ),
    visualization=c(
        preset_lag='f505e496d0145ea2',
        preset_predictor='61545e225ad928db',
        layers='2c53172643d1ee15',
        point_layers='d8fad4d151913560'
    ),
    errors=c(
        missing_geometry='render is missing required field: geometry',
        unknown_field=paste0(
            'render contains unknown field: size. Allowed fields: geometry, mapping',
            's, interval, theme, formats, width, height, dpi, panel_width, panel_heig',
            'ht, title, xlab, ylab, pa',
            'lette, scales, peaks, spacing, breaks, tick_labels, statistic, y_zero, re',
            'verse_palette, ylim, zero_line'
        ),
        bad_geometry=paste0(
            'render.geometry must be one of line, point, connected-point, bar, ',
            'raster, contour, raster-contour'
        ),
        mappings_unnamed='render.mappings must be a named mapping',
        mapping_unknown=paste0(
            'render.mappings contains unknown field: size. Allowed fields: x, y, co',
            'lor, fill, linetype, group, facet'
        ),
        mapping_missing_x='render.mappings is missing required field: x',
        mapping_missing_y='render.mappings.y is required',
        mapping_not_scalar='render.mappings value must be a character scalar',
        line_companion='render.interval companion requires a surface geometry',
        surface_ribbon='render.interval ribbon requires line geometry',
        line_errorbar=paste0(
            'render.interval errorbar requires point, connected-point, or bar ',
            'geometry'
        ),
        bar_companion='render.interval companion requires a surface geometry',
        raster_fill='render.mappings.fill is required for raster geometry',
        bad_spacing='render.spacing must be one of uniform, linear',
        surface_spacing=paste0(
            'render.spacing requires line, point, connected-point, or bar geometry'
        ),
        bad_statistic='render.statistic must be one of estimate, peak-lag',
        line_peak=paste0(
            'render.statistic peak-lag requires point, connected-point, or bar ',
            'geometry'
        ),
        bad_theme=paste0(
            'render.theme must be one of paper, slides, grey, gray, bw, linedraw, ',
            'light, dark, minimal, classic, void, test'
        ),
        bad_palette=paste0(
            'render.palette must be default, rainbow, grey, okabe-ito, a viridis op',
            'tion (magma, inferno, plasma, viridis, cividis, rocket, mako, turbo), ',
            'or a ColorBrewer palette (Blues, BuGn, BuPu, GnBu, Greens, Greys, Oran',
            'ges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOr',
            'Br, YlOrRd, BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectr',
            'al, Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3)'
        ),
        bad_gradient=paste0(
            'render.palette has colors that are not hex (#RRGGBB), rgb(R, G, B), or',
            ' R color names: rgb(300, 0, 0)'
        ),
        numeric_gradient='render.palette must be a palette name or a list of colors',
        bad_peaks='render.peaks must be one of auto, always, never',
        bad_scales='render.scales must be one of fixed, free, free_x, free_y',
        bad_format='render.formats supports pdf, png, and svg',
        empty_formats='render.formats supports pdf, png, and svg',
        bad_width='render.width must be positive',
        bad_dpi='render.dpi must be positive',
        bad_title='render.title must be a character scalar',
        not_mapping='render must be a mapping'
    )
)

# breaks puts ticks at given values on a continuous x-axis without changing
# where the effect is evaluated; uniform spacing already ticks every value.
broken <- internal('.cdrgam_cli_validate_render')(list(geometry='line',
    mappings=list(x='lag', y='estimate'), breaks=list(10, 0, 5, 5)), 'render')
stopifnot(identical(broken$breaks, c(0, 5, 10)))
axis <- internal('.cdrgam_cli_x_axis')(seq(0, 10, length.out=200), broken)
stopifnot(
    identical(axis$kind, 'breaks'), identical(axis$breaks, c(0, 5, 10)),
    length(axis$position) == 200L, identical(axis$labels, c('0', '5', '10'))
)
stopifnot(grepl('cannot be combined', tryCatch(
    internal('.cdrgam_cli_validate_render')(list(geometry='line',
        mappings=list(x='lag', y='estimate'), breaks=list(1), spacing='uniform'),
        'render'),
    error=conditionMessage
)))

# tick_labels name the breaks in the order written, and otherwise each
# distinct x value in sorted order; a mismatch in number is an error.
labelled <- internal('.cdrgam_cli_validate_render')(list(geometry='line',
    mappings=list(x='lag', y='estimate'), breaks=list(10, 0, 5),
    tick_labels=list('late', 'onset', 'mid')), 'render')
stopifnot(identical(labelled$tick_labels, c('onset', 'mid', 'late')))
axis <- internal('.cdrgam_cli_x_axis')(seq(0, 10, length.out=200), labelled)
stopifnot(identical(axis$labels, c('onset', 'mid', 'late')))
stopifnot(grepl('3 labels for 2 breaks', tryCatch(
    internal('.cdrgam_cli_validate_render')(list(geometry='line',
        mappings=list(x='lag', y='estimate'), breaks=list(0, 5),
        tick_labels=list('a', 'b', 'c')), 'render'),
    error=conditionMessage
)))
points_labelled <- internal('.cdrgam_cli_validate_render')(list(geometry='point',
    mappings=list(x='lag', y='estimate'), tick_labels=list('low', 'high')), 'render')
axis <- internal('.cdrgam_cli_x_axis')(c(2, 1, 2, 1), points_labelled)
stopifnot(
    identical(axis$kind, 'ticks'), identical(axis$breaks, c(1, 2)),
    identical(axis$labels, c('low', 'high'))
)
stopifnot(grepl('2 labels for 3 x values', tryCatch(
    internal('.cdrgam_cli_x_axis')(c(1, 2, 3), points_labelled),
    error=conditionMessage
)))

# ylim takes [low, high] with null for an automatic end.
validate_ylim <- function(value) tryCatch(internal('.cdrgam_cli_validate_render')(
    list(geometry='line', mappings=list(x='lag', y='estimate'), ylim=value), 'render'
)$ylim, error=conditionMessage)
stopifnot(
    identical(validate_ylim(list(-1, 2)), c(-1, 2)),
    identical(validate_ylim(list(NULL, 2)), c(NA, 2)),
    grepl('low end below', validate_ylim(list(2, 1))),
    grepl('at least one finite', validate_ylim(list(NULL, NULL))),
    grepl('must be \\[low, high\\]', validate_ylim(list(1)))
)

# panel_width and panel_height fix the panel, not the figure: they exclude
# width and height and are set together. The saved size fits the panel plus
# its labels and legend, so a longer legend widens the file, not the panel.
panel_error <- function(extra) tryCatch(internal('.cdrgam_cli_validate_render')(
    c(list(geometry='line', mappings=list(x='lag', y='estimate')), extra), 'render'
), error=conditionMessage)
stopifnot(
    grepl('not both', panel_error(list(width=4, panel_width=3, panel_height=3))),
    grepl('set together', panel_error(list(panel_width=3))),
    is.list(panel_error(list(panel_width=3, panel_height=2)))
)
panel_plot <- function(labels) ggplot2::ggplot(
    data.frame(x=rep(1:3, length(labels)), y=seq_along(labels), g=rep(labels, each=3)),
    ggplot2::aes(x, y, colour=g)
) + ggplot2::geom_line()
panel_inches <- function(figure) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    grob <- figure$plot
    panels <- grob$layout[grepl('^panel', grob$layout$name), , drop=FALSE]
    c(
        grid::convertWidth(grob$widths[panels$l[[1L]]], 'in', TRUE),
        grid::convertHeight(grob$heights[panels$t[[1L]]], 'in', TRUE)
    )
}
panel_figure <- internal('.cdrgam_cli_panel_figure')
short_legend <- panel_figure(panel_plot(c('a', 'b')), list(width=3, height=2))
long_legend <- panel_figure(
    panel_plot(c('a much longer legend entry', 'another long legend entry')),
    list(width=3, height=2)
)
stopifnot(
    isTRUE(all.equal(panel_inches(short_legend), c(3, 2))),
    isTRUE(all.equal(panel_inches(long_legend), c(3, 2))),
    long_legend$width > short_legend$width + 0.5,
    short_legend$width > 3, short_legend$height > 2
)
# A stack under a fixed panel keeps the main panel's size and aligns the
# panels below it.
stacked <- internal('.cdrgam_cli_stack_plots')(
    list(panel_plot(c('a', 'b')), panel_plot('c') + ggplot2::theme(legend.position='none')),
    c(1, 0.5), list(width=3, height=2)
)
grDevices::pdf(NULL)
stack_heights <- grid::convertHeight(stacked$heights, 'in', TRUE)
stack_width <- grid::convertWidth(stacked$widths, 'in', TRUE)
grDevices::dev.off()
stopifnot(length(stack_heights) == 2L, stack_heights[[1L]] > 2,
    stack_heights[[2L]] > 1, stack_heights[[2L]] < stack_heights[[1L]],
    stack_width > 3)
saved <- internal('.cdrgam_cli_save_ggplot')(
    panel_plot(c('a', 'b')), tempdir(),
    list(formats='svg', panel_width=3, panel_height=2), stem='panel-test'
)
svg_head <- paste(readLines(file.path(tempdir(), saved), n=3L), collapse=' ')
svg_width <- as.numeric(sub('.*width="([0-9.]+)pt".*', '\\1', svg_head)) / 72
stopifnot(abs(svg_width - short_legend$width) < 0.05)

# zero_line is on unless false, and only false is stored. The line is drawn
# only in panels whose range includes zero, so it never moves an axis.
zero_spec <- function(y, render=list()) {
    data <- data.frame(term_id='t', term='t', group=NA, layer='estimate', lag=1:3,
        estimate=y, lower=y - 0.1, upper=y + 0.1)
    internal('.cdrgam_cli_plot_spec')(data, list(render=internal(
        '.cdrgam_cli_validate_render'
    )(c(list(geometry='line', mappings=list(x='lag', y='estimate')), render), 'render')))
}
has_hline <- function(spec) any(vapply(
    internal('.cdrgam_cli_ggplot_plot')(spec)$layers,
    function(layer) inherits(layer$geom, 'GeomHline'), logical(1)
))
stopifnot(
    is.null(validate_render(list(geometry='line', mappings=list(x='lag', y='estimate'),
        zero_line=TRUE), 'render')$zero_line),
    isFALSE(validate_render(list(geometry='line', mappings=list(x='lag', y='estimate'),
        zero_line=FALSE), 'render')$zero_line),
    has_hline(zero_spec(c(-1, 0.5, 1))),
    !has_hline(zero_spec(c(1, 2, 3))),
    !has_hline(zero_spec(c(-1, 0.5, 1), list(zero_line=FALSE))),
    has_hline(zero_spec(c(1, 2, 3), list(ylim=list(-1, 4)))),
    # Zero inside the padded axis but below the data still gets the line.
    has_hline(zero_spec(c(0.15, 2, 3))),
    !has_hline(zero_spec(c(0.5, 2, 3)))
)

# y_zero is a flag that keeps zero on the vertical axis of line geometries;
# false is the same as leaving it out. Every native ggplot2 theme renders.
stopifnot(
    isTRUE(validate_render(list(geometry='point', mappings=list(x='lag', y='estimate'),
        y_zero=TRUE), 'render')$y_zero),
    is.null(validate_render(list(geometry='line', mappings=list(x='lag', y='estimate'),
        y_zero=FALSE), 'render')$y_zero),
    grepl('must be true or false', tryCatch(validate_render(list(geometry='line',
        mappings=list(x='lag', y='estimate'), y_zero='yes'), 'render'),
        error=conditionMessage), fixed=TRUE)
)
for (theme in internal('.cdrgam_cli_render_fields')$theme$choices) {
    stopifnot(inherits(internal('.cdrgam_cli_ggplot_theme')(theme), 'theme'))
}

for (part in names(expected)) {
    differs <- names(expected[[part]])[
        expected[[part]] != observed[[part]][names(expected[[part]])]
    ]
    if (length(differs)) stop(
        'render validation changed for ', part, ': ',
        paste(differs, collapse=', ')
    )
    stopifnot(setequal(names(expected[[part]]), names(observed[[part]])))
}

# Every render field is described in the registry, and every field without
# a viewer control says why.
fields <- internal('.cdrgam_cli_render_fields')
stopifnot(
    all(vapply(fields, function(spec) spec$viewer %in% c('control', 'derived', 'file'),
        logical(1))),
    all(vapply(Filter(function(spec) spec$viewer != 'control', fields),
        function(spec) is.character(spec$reason) && nzchar(spec$reason), logical(1))),
    all(vapply(Filter(function(spec) spec$viewer == 'control', fields),
        function(spec) is.character(spec$label) && nzchar(spec$label), logical(1)))
)
unknown <- error_message(validate_render(
    list(geometry='line', mappings=list(x='lag', y='estimate'), nope=1), 'render'
))
stopifnot(identical(
    sub('^.*Allowed fields: ', '', unknown), paste(names(fields), collapse=', ')
))
# Fields without defaults stay absent, so new fields keep old identities.
stopifnot(identical(
    names(validate_render(render_cases$line_minimal, 'render')),
    c('geometry', 'mappings', 'interval', 'theme', 'formats')
))

# Plot specifications resolve positions and colors for every backend.
plot_spec <- internal('.cdrgam_cli_plot_spec')
validated <- function(render) validate_render(render, 'render')
grid <- expand.grid(lag=c(0, 0.5, 2), term=c('x', 'y'), stringsAsFactors=FALSE)
grid <- transform(grid, term_id=term, group=NA_character_, layer='estimate',
    estimate=lag, lower=lag - 0.1, upper=lag + 0.1)

# Two series of points at each tick sit in adjacent columns one point wide,
# centered on the tick, so clusters keep a gap; bars fill 80% of the gap.
spec <- plot_spec(grid, list(render=validated(list(
    geometry='point', spacing='uniform', mappings=list(x='lag', y='estimate', color='term')
))))
column <- 9 * 2 / (0.85 * 96 * 7)
stopifnot(
    identical(spec$x$kind, 'ticks'), identical(spec$x$labels, c('0', '0.5', '2')),
    isTRUE(all.equal(sort(spec$rows$.cdrgam_x),
        rep(1:3, each=2) + c(-1, 1) * column / 2)),
    isTRUE(all.equal(unique(spec$rows$.cdrgam_width), column))
)
# A wide figure or few ticks never widens a cluster past 80% of the gap.
wide <- plot_spec(grid, list(render=validated(list(
    geometry='bar', spacing='uniform', mappings=list(x='lag', y='estimate', color='term')
))))
stopifnot(isTRUE(all.equal(unique(wide$rows$.cdrgam_width), 0.4)))
single <- plot_spec(grid[grid$term == 'x', ], list(render=validated(list(
    geometry='bar', mappings=list(x='lag', y='estimate')
))))
stopifnot(
    identical(single$rows$.cdrgam_x, c(1, 2, 3)),
    isTRUE(all.equal(single$rows$.cdrgam_xmax - single$rows$.cdrgam_xmin, rep(0.8, 3))),
    identical(single$rows$.cdrgam_ymin, c(0, 0, 0))
)
# A categorical x has one tick per sorted level; a continuous x is unchanged.
by_term <- plot_spec(grid[grid$lag == 0, ], list(render=validated(list(
    geometry='bar', mappings=list(x='term', y='estimate', color='term')
))))
stopifnot(
    identical(by_term$x$kind, 'discrete'), identical(by_term$x$labels, c('x', 'y')),
    identical(by_term$rows$.cdrgam_x, c(1, 2))
)
line <- plot_spec(grid, list(render=validated(list(
    geometry='line', mappings=list(x='lag', y='estimate', color='term')
))))
stopifnot(identical(line$x$kind, 'continuous'), identical(line$rows$.cdrgam_x, grid$lag))

# The default palette resolves to ggplot2's hue colors, and continuous stops
# reproduce each palette's ggplot2 scale.
stopifnot(identical(
    unname(line$color$values), ggplot2::scale_colour_hue()$palette(2)
))
stops <- internal('.cdrgam_cli_palette_stops')
built_colour <- function(scale) {
    mapping <- do.call(ggplot2::aes, lapply(list(x='x', y='x', colour='x'), as.name))
    plot <- ggplot2::ggplot(data.frame(x=1:5), mapping) + ggplot2::geom_point()
    ggplot2::ggplot_build(plot + scale)$data[[1L]]$colour
}
for (case in list(
    list(NULL, ggplot2::scale_colour_continuous()),
    list('magma', ggplot2::scale_colour_viridis_c(option='magma')),
    list('RdBu', ggplot2::scale_colour_distiller(palette='RdBu')),
    list('grey', ggplot2::scale_colour_gradient(low='grey80', high='grey10'))
)) stopifnot(identical(
    built_colour(ggplot2::scale_colour_gradientn(colours=stops(case[[1L]]))),
    built_colour(case[[2L]])
))
okabe <- internal('.cdrgam_cli_palette_colors')('okabe-ito', c('a', 'b'))
stopifnot(identical(unname(okabe), c('#0072B2', '#D55E00')))

# Backend parity: each specification drawn with ggplot2 and with plotly
# shows the same marks at the same positions in the same colors, with the
# same intervals and axis ticks. Facts are compared after drawing, from
# ggplot_build() and plotly_build().
if (requireNamespace('plotly', quietly=TRUE)) {
    ggplot_draw <- internal('.cdrgam_cli_ggplot_plot')
    plotly_draw <- internal('.cdrgam_cli_plotly_plot')
    hex <- function(color) {
        color <- as.character(color)
        rgba <- grepl('^rgba\\(', color)
        color[rgba] <- vapply(strsplit(gsub('rgba\\(|\\)', '', color[rgba]), ','),
            function(part) {
                part <- as.numeric(part)
                grDevices::rgb(part[[1L]], part[[2L]], part[[3L]], maxColorValue=255)
            },
            character(1))
        values <- grDevices::col2rgb(color)
        grDevices::rgb(values[1L, ], values[2L, ], values[3L, ], maxColorValue=255)
    }
    # One row per mark, sorted, with positions rounded so that both
    # backends' floating-point arithmetic compares equal.
    facts <- function(...) {
        frame <- data.frame(..., stringsAsFactors=FALSE)
        for (name in names(frame)) if (is.numeric(frame[[name]])) {
            frame[[name]] <- round(frame[[name]], 9L)
        }
        frame <- frame[do.call(order, unname(as.list(frame))), , drop=FALSE]
        rownames(frame) <- NULL
        frame
    }
    bind <- function(frames) {
        frames <- Filter(function(frame) !is.null(frame) && nrow(frame), frames)
        if (length(frames)) facts(do.call(rbind, frames)) else NULL
    }
    ribbon_rows <- function(x, lower, upper, fill) {
        data.frame(x=x, lower=lower, upper=upper, color=hex(fill))
    }
    ggplot_facts <- function(spec) {
        built <- ggplot2::ggplot_build(ggplot_draw(spec))
        geoms <- vapply(ggplot_draw(spec)$layers, function(layer) class(layer$geom)[[1L]],
            character(1))
        marks <- list(); intervals <- list(); bounds <- list()
        for (index in seq_along(geoms)) {
            data <- built$data[[index]]
            switch(
                geoms[[index]],
                GeomLine=if (all(data$linetype == 2 | data$linetype == 'dashed')) {
                    bounds[[length(bounds) + 1L]] <- data.frame(
                        x=data$x, y=data$y, color=hex(data$colour))
                } else marks[[length(marks) + 1L]] <- data.frame(
                    x=data$x, y=data$y, color=hex(data$colour)),
                GeomPoint=marks[[length(marks) + 1L]] <- data.frame(
                    x=data$x, y=data$y, color=hex(data$colour)),
                GeomRect=marks[[length(marks) + 1L]] <- data.frame(
                    x=(data$xmin + data$xmax) / 2, y=data$ymax - data$ymin,
                    width=data$xmax - data$xmin, color=hex(data$fill)),
                GeomRibbon=intervals[[length(intervals) + 1L]] <- ribbon_rows(
                    data$x, data$ymin, data$ymax, data$fill),
                GeomErrorbar=intervals[[length(intervals) + 1L]] <- data.frame(
                    x=data$x, lower=data$ymin, upper=data$ymax,
                    whisker=data$xmax - data$xmin, color=hex(data$colour))
            )
        }
        params <- built$layout$panel_params[[1L]]
        list(marks=bind(marks), intervals=bind(intervals), bounds=bind(bounds),
            ticks=if (identical(spec$x$kind, 'continuous')) NULL else
                as.character(params$x$get_labels()))
    }
    plotly_facts <- function(spec) {
        built <- plotly::plotly_build(plotly_draw(spec))$x
        marks <- list(); intervals <- list(); bounds <- list()
        for (trace in built$data) {
            x <- as.numeric(unlist(trace$x)); y <- as.numeric(unlist(trace$y))
            if (identical(trace$type, 'bar')) {
                marks[[length(marks) + 1L]] <- data.frame(x=x, y=y,
                    width=as.numeric(unlist(trace$width)),
                    color=hex(rep_len(unlist(trace$marker$color), length(x))))
            } else if (identical(trace$fill, 'toself')) {
                lower <- tapply(y, x, min); upper <- tapply(y, x, max)
                intervals[[length(intervals) + 1L]] <- ribbon_rows(
                    as.numeric(names(lower)), lower, upper, trace$fillcolor)
            } else if (anyNA(x)) {
                segments <- matrix(c(x, NA)[seq_len(3L * ((length(x) + 1L) %/% 3L))], nrow=3L)
                heights <- matrix(c(y, NA)[seq_len(3L * ((length(y) + 1L) %/% 3L))], nrow=3L)
                vertical <- segments[1L, ] == segments[2L, ]
                caps <- which(!vertical)
                intervals[[length(intervals) + 1L]] <- data.frame(
                    x=segments[1L, vertical], lower=heights[1L, vertical],
                    upper=heights[2L, vertical],
                    whisker=(segments[2L, caps] - segments[1L, caps])[seq_len(sum(vertical))],
                    color=hex(trace$line$color))
            } else if (identical(trace$mode, 'markers')) {
                marks[[length(marks) + 1L]] <- data.frame(x=x, y=y,
                    color=hex(rep_len(unlist(trace$marker$color), length(x))))
            } else if (identical(trace$line$dash, 'dash')) {
                bounds[[length(bounds) + 1L]] <- data.frame(x=x, y=y, color=hex(trace$line$color))
            } else marks[[length(marks) + 1L]] <- data.frame(x=x, y=y,
                color=hex(trace$line$color))
        }
        list(marks=bind(marks), intervals=bind(intervals), bounds=bind(bounds),
            ticks=if (identical(spec$x$kind, 'continuous')) NULL else
                as.character(unlist(built$layout$xaxis$ticktext)))
    }
    parity <- function(name, data, render, layers=NULL) {
        spec <- plot_spec(data, list(render=validated(render), layers=layers))
        a <- ggplot_facts(spec); b <- plotly_facts(spec)
        for (part in names(a)) if (!isTRUE(all.equal(a[[part]], b[[part]]))) stop(
            'backends differ for ', name, ' in ', part, ': ',
            paste(all.equal(a[[part]], b[[part]]), collapse='; ')
        )
    }
    cases <- 0L
    fixture <- expand.grid(lag=c(0, 0.25, 1, 3), term=c('x', 'y', 'irf(1)'),
        stringsAsFactors=FALSE)
    fixture <- transform(fixture, term_id=term, group=NA_character_, layer='estimate',
        estimate=sin(lag + nchar(term)), lower=sin(lag + nchar(term)) - 0.3,
        upper=sin(lag + nchar(term)) + 0.3)
    palettes <- list('okabe-ito', 'Set1', 'viridis', 'rainbow', list('#000000', '#FF0000'))
    for (geometry in c('line', 'point', 'bar')) {
        for (interval in internal('.cdrgam_cli_geometry_intervals')(geometry)) {
            for (spacing in list(NULL, 'uniform', 'linear')) {
                for (palette in palettes) {
                    render <- list(geometry=geometry, interval=interval,
                        mappings=list(x='lag', y='estimate', color='term'), palette=palette)
                    render$spacing <- spacing
                    parity(paste(geometry, interval, spacing, palette[[1L]]), fixture, render)
                    cases <- cases + 1L
                }
            }
        }
    }
    # A categorical x, an unmapped color, a numeric color with a continuous
    # scale, fixed layer styles, and peak-lag rows.
    parity('bar by term', fixture[fixture$lag == 0, ], list(geometry='bar',
        mappings=list(x='term', y='estimate', color='term'), palette='Dark2'))
    parity('point uncolored', fixture[fixture$term == 'x', ], list(geometry='point',
        mappings=list(x='lag', y='estimate')))
    parity('bar uncolored', fixture[fixture$term == 'x', ], list(geometry='bar',
        mappings=list(x='lag', y='estimate')))
    continuous <- transform(expand.grid(lag=c(0, 1), m=1:13), term='x:m', term_id='x:m',
        group=NA_character_, layer='estimate')
    continuous <- transform(continuous, estimate=lag * m, lower=lag * m - 1, upper=lag * m + 1)
    for (palette in list(NULL, 'magma', 'RdBu')) {
        render <- list(geometry='line', mappings=list(x='lag', y='estimate', color='m'))
        render$palette <- palette
        spec <- plot_spec(continuous, list(render=validated(render)))
        a <- ggplot_facts(spec)$marks; b <- plotly_facts(spec)$marks
        # ggplot2 and grDevices interpolate in Lab slightly differently.
        distance <- abs(grDevices::col2rgb(a$color) - grDevices::col2rgb(b$color))
        stopifnot(identical(a[c('x', 'y')], b[c('x', 'y')]), max(distance) <= 3)
    }
    layered <- rbind(transform(fixture, layer='a'),
        transform(fixture, layer='b', estimate=estimate / 2))
    for (geometry in c('line', 'point', 'bar')) parity(
        paste(geometry, 'layers'), layered,
        list(geometry=geometry, mappings=list(x='lag', y='estimate', color='term')),
        layers=list(list(id='a', style=list(color='grey50', fill='grey80')), list(id='b'))
    )
    peaks <- data.frame(term=c('x', 'y', 'y'), source=c('x', 'y', 'y'),
        direction=c('pos', 'pos', 'neg'), lag=c(0.2, 0.4, 0.9),
        lower=c(0.1, 0.3, 0.7), upper=c(0.3, 0.5, 1.1))
    for (geometry in c('point', 'bar')) parity(
        paste(geometry, 'peak lags'), internal('.cdrgam_cli_peak_data')(peaks),
        list(geometry=geometry, statistic='peak-lag',
            mappings=list(x='peak', y='estimate', color='term'))
    )
    stopifnot(cases >= 80L)

    # The peak strip shows the same lanes, top to bottom, with the same peak
    # lags, intervals, and colors.
    attr(peaks, 'draws') <- data.frame(source=c('x', 'x', 'y'), direction='pos',
        draw=c(1L, 2L, 1L), lag=c(0.15, 0.25, 0.4))
    strip <- internal('.cdrgam_cli_figure_peaks')(fixture, peaks, validated(list(
        geometry='line', mappings=list(x='lag', y='estimate', color='term'),
        palette='okabe-ito', peaks='always'
    )))
    gg <- ggplot2::ggplot_build(internal('.cdrgam_cli_ggplot_peak_strip')(strip))
    gg_peaks <- gg$data[[length(gg$data)]]
    gg_lanes <- levels(strip$peaks$.lane)[as.integer(gg_peaks$y)]
    py_strip <- plotly::plotly_build(internal('.cdrgam_cli_plotly_peak_strip')(strip))$x
    py <- Filter(function(trace) identical(trace$marker$size, 12), py_strip$data)
    # Lanes sit at 1, 2, ... from the bottom; the top lane is the first.
    py_lane <- function(trace) strip$lanes[[length(strip$lanes) + 1L - unlist(trace$y)]]
    stopifnot(
        identical(
            facts(lane=gg_lanes, lag=gg_peaks$x, color=hex(gg_peaks$colour)),
            facts(lane=vapply(py, py_lane, character(1)),
                lag=vapply(py, function(trace) unlist(trace$x), numeric(1)),
                color=hex(vapply(py, function(trace) trace$marker$color, character(1))))
        ),
        identical(unlist(py_strip$layout$yaxis$ticktext), strip$lanes),
        identical(as.integer(unlist(py_strip$layout$yaxis$tickvals)),
            rev(seq_along(strip$lanes))),
        # Each tick spans at most its own lane.
        all(vapply(Filter(function(trace) identical(trace$mode, 'lines'), py_strip$data),
            function(trace) {
                y <- unlist(trace$y)
                y <- y[is.finite(y)]
                !length(y) || diff(range(y)) <= 0.8 + 1e-9
            }, logical(1)))
    )
}

# The pointwise-peak panel: each line's peak lag over the main x-axis, in
# the line's color, with its interval band. ggplot2 and plotly draw the same
# values, and the rendered figure stacks it under the main plot.
pointwise_data <- expand.grid(x=c(-1, 0, 2), term=c('a', 'b'), stringsAsFactors=FALSE)
pointwise_data <- transform(pointwise_data, lag=0.5, estimate=x, se=0.1,
    lower=x - 0.2, upper=x + 0.2, group=NA_character_, layer='estimate',
    term_id=term)
pointwise_table <- transform(pointwise_data[c('term', 'x', 'layer')],
    lag=c(0, 0.2, 0.4, 1, 1.2, 1.4), lower=c(0, 0.1, 0.3, 0.9, 1, 1.3),
    upper=c(0.1, 0.3, 0.5, 1.1, 1.4, 1.5))
for (spacing in list(NULL, 'uniform')) {
    render <- validated(Filter(Negate(is.null), list(
        geometry='line', mappings=list(x='x', y='estimate', color='term'),
        palette='Set1', spacing=spacing
    )))
    definition <- list(render=render)
    panel <- internal('.cdrgam_cli_figure_pointwise')(
        pointwise_data, pointwise_table, definition
    )
    expected_x <- if (is.null(spacing)) c(-1, 0, 2) else 1:3
    stopifnot(
        identical(panel$rows$.x, rep(expected_x, 2L)),
        identical(panel$rows$lag, pointwise_table$lag),
        identical(names(panel$colors), c('a', 'b'))
    )
    drawn <- ggplot2::ggplot_build(internal('.cdrgam_cli_ggplot_pointwise')(panel))
    lines <- drawn$data[[2L]]
    traces <- plotly::plotly_build(internal('.cdrgam_cli_plotly_pointwise')(panel))$x$data
    plotted <- Filter(function(trace) identical(trace$mode, 'lines') &&
        is.null(trace$fill), traces)
    stopifnot(
        isTRUE(all.equal(sort(lines$y), sort(pointwise_table$lag))),
        isTRUE(all.equal(sort(unlist(lapply(plotted, `[[`, 'y'))),
            sort(pointwise_table$lag))),
        identical(toupper(unname(panel$colors)),
            toupper(substr(unique(lines$colour), 1L, 7L)))
    )
    figure <- internal('.cdrgam_cli_peak_figure')(
        internal('.cdrgam_cli_visualization_plot')(pointwise_data, definition),
        pointwise_data, NULL, definition, pointwise_table
    )
    stopifnot(
        inherits(figure$pointwise, 'ggplot'), is.null(figure$strip),
        inherits(figure$figure, 'gtable'), nrow(figure$figure$layout) == 2L
    )
}
never <- list(render=validated(list(
    geometry='line', mappings=list(x='x', y='estimate', color='term'), peaks='never'
)))
stopifnot(is.null(internal('.cdrgam_cli_figure_pointwise')(
    pointwise_data, pointwise_table, never
)))

cat('render: ok\n')
