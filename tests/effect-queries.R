library(cdrgam.cli)

validate <- getFromNamespace('.cdrgam_cli_validate_effect_query', 'cdrgam.cli')
series_axes <- getFromNamespace('.cdrgam_cli_series_axes', 'cdrgam.cli')
discrete <- getFromNamespace('.cdrgam_cli_discrete_mappings', 'cdrgam.cli')
plot_effect <- getFromNamespace('.cdrgam_cli_visualization_plot', 'cdrgam.cli')

expect_error <- function(expression, pattern) {
    message <- tryCatch({
        force(expression)
        NULL
    }, error=function(error) conditionMessage(error))
    stopifnot(!is.null(message), grepl(pattern, message))
}

base <- list(
    terms='wlen:repetition_index', composition='total',
    axes=list(
        lag=list(grid='fitted', n=5L),
        predictors=list(
            wlen=list(at=list(summary='mean', 'offset-sd'=1)),
            repetition_index=list(values=1:5)
        )
    )
)

# Queries without a reference keep their normalized form, and therefore their
# effect identity.
plain <- validate(base, 'query')
stopifnot(is.null(plain$reference), is.null(plain$test))

query <- base
query$reference <- list(wlen=list(at=list(summary='mean')))
query$test <- TRUE
query <- validate(query, 'query')
stopifnot(
    identical(query$reference$wlen$at$summary, 'mean'),
    isTRUE(query$test)
)
numeric <- base
numeric$reference <- list(repetition_index=1)
stopifnot(identical(validate(numeric, 'query')$reference$repetition_index, 1))

bad <- base
bad$reference <- list(wlen=list(values=c(1, 2)))
expect_error(validate(bad, 'query'), 'must resolve to one value')
bad$reference <- list(wlen=list(grid='fitted'))
expect_error(validate(bad, 'query'), 'must resolve to one value')
bad$reference <- list(1)
expect_error(validate(bad, 'query'), 'nonempty named mapping')
bad$reference <- list(lag='default')
expect_error(validate(bad, 'query'), 'no model default')
defaulted <- base
defaulted$reference <- list(wlen='default', repetition_index=2)
defaulted <- validate(defaulted, 'query')
stopifnot(identical(defaulted$reference$wlen, 'default'))
bad <- base
bad$test <- 'yes'
expect_error(validate(bad, 'query'), 'test must be true or false')

# Line series split on every varying evaluation axis except the x mapping.
grid <- expand.grid(lag=seq(0, 1, length.out=4), wlen=5, repetition_index=1:5)
grid <- transform(
    grid, term_id='irf-6', term='wlen:repetition_index', group=NA_character_,
    composition='total', grouping='population', estimate=lag * repetition_index,
    se=0.1, lower=lag * repetition_index - 0.2, upper=lag * repetition_index + 0.2,
    wlen_reference=4, layer='estimate'
)
mappings <- list(x='lag', y='estimate', color='predictor:repetition_index')
stopifnot(identical(series_axes(grid, mappings), 'repetition_index'))
shown <- discrete(grid, mappings)
stopifnot(
    is.factor(shown$repetition_index),
    identical(levels(shown$repetition_index), as.character(1:5))
)
definition <- list(
    query=validate(query, 'query'),
    render=list(geometry='line', mappings=mappings, interval='ribbon', theme='paper')
)
plot <- plot_effect(grid, definition)
built <- ggplot2::ggplot_build(plot)
stopifnot(length(unique(built$data[[length(built$data)]]$group)) == 5L)

# Axis values may be arithmetic over training summaries; lag values may also
# name a peak.
value <- getFromNamespace('.cdrgam_cli_axis_value', 'cdrgam.cli')
statistics <- list(mean=2, sd=0.5, median=1.5)
stopifnot(
    identical(value('mean + 2 * sd', 'x', statistics), 3),
    identical(value('-(median - mean) / sd', 'x', statistics), 1),
    identical(value(4, 'x', statistics), 4),
    identical(value('peak', 'lag', list(), peak=function(direction) {
        if (identical(direction, 'pos')) 0.3 else 0.9
    }), 0.3),
    identical(value('peak-neg + 0.1', 'lag', list(), peak=function(direction) {
        if (identical(direction, 'pos')) 0.3 else 0.9
    }), 1)
)
expect_error(value('mean + q', 'x', statistics), 'unknown name q')

# peak(LABEL) borrows the peak of another term; labels may contain balanced
# parentheses and may be quoted.
borrowed <- function(direction, label=NULL) {
    lags <- c(pos=0.3, neg=0.9)
    if (is.null(label)) return(lags[[direction]])
    if (identical(label, 's(lag, wlen)')) return(lags[[direction]] + 1)
    if (identical(label, 'irf(1)')) return(5)
    stop('unexpected label ', label)
}
stopifnot(
    identical(value('peak(s(lag, wlen))', 'lag', list(), peak=borrowed), 1.3),
    identical(value('peak-neg(s(lag, wlen))', 'lag', list(), peak=borrowed), 1.9),
    identical(value('peak-pos( "irf(1)" ) - peak', 'lag', list(), peak=borrowed), 4.7),
    identical(value("peak('irf(1)') / 5", 'lag', list(), peak=borrowed), 1)
)
expect_error(value('peak(irf(1))', 'x', statistics), 'only lag values')
expect_error(value('peak(irf(1)', 'lag', list(), peak=borrowed), 'unclosed parenthesis')
expect_error(value('peak()', 'lag', list(), peak=borrowed), 'empty term label')
parse_value <- getFromNamespace('.cdrgam_cli_value_parse', 'cdrgam.cli')
stopifnot(identical(
    deparse(parse_value('peak(s(lag, wlen)) + 1', 'lag', lag=TRUE)),
    'peak_pos("s(lag, wlen)") + 1'
))
expect_error(value('peak', 'x', statistics), 'only lag values')
expect_error(value('sd', 'x', list(mean=1, sd=NA)), 'no stored training sd')
expect_error(value('max(sd)', 'x', statistics), 'not a valid value expression')
expect_error(value('mean +', 'x', statistics), 'not a valid value expression')

expressions <- base
expressions$axes <- list(
    lag='peak-neg',
    time='mean',
    predictors=list(
        wlen='mean + sd',
        repetition_index=list(values=list(1, 'mean', 'mean + 2 * sd')),
        '*'=list(range=list('mean - sd', 'mean + sd'), n=7L)
    )
)
expressions$reference <- list('*'='mean', lag='peak')
expressions$uncertainty <- list(kind='simultaneous')
expressions$terms <- list(predictors='*', interacting=FALSE)
expressions <- validate(expressions, 'query')
stopifnot(
    identical(expressions$axes$lag, 'peak-neg'),
    identical(expressions$axes$predictors$repetition_index$values,
        c('1', 'mean', 'mean + 2 * sd')),
    identical(expressions$axes$predictors[['*']]$range, list('mean - sd', 'mean + sd')),
    identical(expressions$reference[['*']], 'mean'),
    identical(expressions$uncertainty$points, 40L),
    isFALSE(expressions$terms$interacting)
)
bad <- base
bad$axes$predictors$wlen <- 'peak'
expect_error(validate(bad, 'query'), 'only lag values')
# peak-pointwise is accepted only as the whole fixed lag of a query with
# pointwise intervals and no test.
pointwise <- base
pointwise$axes <- list(lag='peak-pointwise-neg')
stopifnot(identical(validate(pointwise, 'query')$axes$lag, 'peak-pointwise-neg'))
pointwise$axes <- list(lag=list(at='peak-pointwise'))
stopifnot(identical(validate(pointwise, 'query')$axes$lag, list(at='peak-pointwise')))
for (lag in list('peak-pointwise + 0.1', list(values=list('peak-pointwise')))) {
    bad <- base
    bad$axes <- list(lag=lag)
    expect_error(validate(bad, 'query'), 'may only be the whole value of a fixed lag')
}
bad <- base
bad$reference <- list(lag='peak-pointwise')
expect_error(validate(bad, 'query'), 'may only be the whole value of a fixed lag')
bad <- pointwise
bad$uncertainty <- list(kind='simultaneous')
expect_error(validate(bad, 'query'), 'needs uncertainty.kind pointwise')
bad <- pointwise
bad$test <- TRUE
expect_error(validate(bad, 'query'), 'cannot be combined with test')
bad <- base
bad$reference <- list(wlen=list(range=list('mean', 'mean + sd')))
expect_error(validate(bad, 'query'), 'must resolve to one value')
bad <- base
bad$uncertainty <- list(points=10L)
expect_error(validate(bad, 'query'), 'requires kind simultaneous')
bad$uncertainty <- list(kind='joint')
expect_error(validate(bad, 'query'), 'pointwise or simultaneous')

# Presets expand into an ordinary query and render.
visualization <- getFromNamespace('.cdrgam_cli_validate_visualization', 'cdrgam.cli')
lag_preset <- visualization(list(
    schema=1L, model='main', preset='irf-lag',
    options=list(irf1=TRUE, step='mean + 2 * sd', level=0.9),
    render=list(formats=c('png'), mappings=list(color='term'))
), 'lag.yml')
stopifnot(
    identical(lag_preset$query$terms, list(grouped=FALSE, interacting=FALSE)),
    identical(lag_preset$query$axes$predictors[['*']], 'mean + 2 * sd'),
    identical(lag_preset$query$reference[['*']], 'mean'),
    identical(lag_preset$query$uncertainty$kind, 'simultaneous'),
    identical(lag_preset$query$uncertainty$level, 0.9),
    identical(lag_preset$render$palette, 'rainbow'),
    identical(lag_preset$render$mappings$x, 'lag'),
    identical(lag_preset$render$formats, 'png')
)
predictor_preset <- visualization(list(
    schema=1L, model='main', preset='irf-predictor',
    options=list(hold=list(lag='peak'), interval='pointwise')
), 'predictor.yml')
stopifnot(
    identical(predictor_preset$query$terms$predictors, '*'),
    identical(predictor_preset$query$axes$lag, 'peak'),
    identical(predictor_preset$query$axes$time, 'mean'),
    identical(predictor_preset$query$axes$predictors[['*']]$grid, 'fitted'),
    identical(predictor_preset$query$uncertainty$kind, 'pointwise'),
    identical(predictor_preset$render$mappings$x, 'predictor_value'),
    identical(predictor_preset$render$scales, 'free_x')
)
expect_error(visualization(list(
    schema=1L, model='main', preset='irf-lag', options=list(hold=list(lag=0))
), 'bad.yml'), 'unknown field')
expect_error(visualization(list(
    schema=1L, model='main', preset='irf-lag', query=base
), 'bad.yml'), 'cannot be combined')
expect_error(visualization(list(
    schema=1L, model='main', preset='irf-surface'
), 'bad.yml'), 'must be one of')

# The simultaneous critical value lies between the pointwise and Bonferroni
# multipliers, reduces to the pointwise one for perfectly correlated points,
# and leaves the caller's random stream unchanged.
multiplier <- getFromNamespace('.cdrgam_cli_simultaneous_multiplier', 'cdrgam.cli')
set.seed(5)
before <- .Random.seed
independent <- multiplier(diag(20), 0.95)
stopifnot(identical(.Random.seed, before))
stopifnot(
    independent > stats::qnorm(0.975),
    independent < stats::qnorm(1 - 0.025 / 20),
    abs(independent - stats::qnorm((1 + 0.95^(1 / 20)) / 2)) < 0.05,
    abs(multiplier(matrix(4, 10, 10), 0.95) - stats::qnorm(0.975)) < 1e-6,
    identical(independent, multiplier(diag(20), 0.95))
)

# The rainbow palette draws irf(1) in black, and a predictor-value axis does
# not split each term's curve on its own predictor column.
rainbow <- getFromNamespace('.cdrgam_cli_rainbow', 'cdrgam.cli')
colors <- rainbow(c('irf(1)', 'x', 'y'))
stopifnot(identical(unname(colors[['irf(1)']]), 'black'), !anyDuplicated(colors))
curves <- rbind(
    data.frame(lag=0, x=seq(-1, 1, length.out=5), y=NA_real_, term='x',
        predictor='x', predictor_value=seq(-1, 1, length.out=5)),
    data.frame(lag=0, x=NA_real_, y=seq(0, 4, length.out=5), term='y',
        predictor='y', predictor_value=seq(0, 4, length.out=5))
)
curves <- transform(
    curves, term_id=paste0('irf-', term), group=NA_character_, composition='term',
    grouping='population', estimate=predictor_value, se=0.1,
    lower=predictor_value - 0.2, upper=predictor_value + 0.2, layer='estimate',
    critical_value=2
)
stopifnot(identical(series_axes(curves, predictor_preset$render$mappings), character()))
plot <- plot_effect(curves, predictor_preset)
built <- ggplot2::ggplot_build(plot)
lines <- built$data[[length(built$data)]]
stopifnot(
    length(unique(lines$group)) == 2L,
    identical(sort(unique(lines$colour)), sort(unname(rainbow(c('x', 'y')))))
)

# A predictor column that exists only for some terms does not split their
# lag curves.
lag_curves <- rbind(
    data.frame(lag=seq(0, 1, length.out=4), x=1, y=NA_real_, term='x'),
    data.frame(lag=seq(0, 1, length.out=4), x=NA_real_, y=2, term='y'),
    data.frame(lag=seq(0, 1, length.out=4), x=NA_real_, y=NA_real_, term='irf(1)')
)
lag_curves <- transform(
    lag_curves, term_id=paste0('irf-', term), group=NA_character_,
    composition='term', grouping='population', estimate=lag, se=0.1,
    lower=lag - 0.2, upper=lag + 0.2, layer='estimate'
)
stopifnot(identical(series_axes(lag_curves, lag_preset$render$mappings), character()))
built <- ggplot2::ggplot_build(plot_effect(lag_curves, lag_preset))
lines <- built$data[[length(built$data)]]
stopifnot(
    length(unique(lines$group)) == 3L,
    all(table(lines$group) == 4L),
    'black' %in% lines$colour
)

validate_render <- getFromNamespace('.cdrgam_cli_validate_render', 'cdrgam.cli')

# Point and bar geometries default to error bars; spacing, statistic, and
# palette are checked and stay absent unless given.
point_render <- validate_render(
    list(geometry='point', mappings=list(x='lag', y='estimate')), 'render'
)
stopifnot(
    identical(point_render$interval, 'errorbar'),
    is.null(point_render$spacing), is.null(point_render$statistic),
    is.null(point_render$palette)
)
line_render <- validate_render(
    list(geometry='line', mappings=list(x='lag', y='estimate'), statistic='estimate'),
    'render'
)
stopifnot(identical(line_render$interval, 'ribbon'), is.null(line_render$statistic))
expect_error(validate_render(list(
    geometry='line', mappings=list(x='lag', y='estimate'), interval='errorbar'
), 'render'), 'requires point or bar')
expect_error(validate_render(list(
    geometry='bar', mappings=list(x='term', y='estimate'), interval='ribbon'
), 'render'), 'requires line geometry')
expect_error(validate_render(list(
    geometry='line', mappings=list(x='lag', y='estimate'), statistic='peak-lag'
), 'render'), 'requires point or bar')
expect_error(validate_render(list(
    geometry='raster', mappings=list(x='lag', y='x', fill='estimate'),
    interval='none', spacing='uniform'
), 'render'), 'spacing requires')
stopifnot(
    identical(validate_render(list(
        geometry='line', mappings=list(x='lag', y='estimate'), palette='set1'
    ), 'render')$palette, 'Set1'),
    identical(validate_render(list(
        geometry='line', mappings=list(x='lag', y='estimate'), palette='Magma'
    ), 'render')$palette, 'magma'),
    identical(validate_render(list(
        geometry='line', mappings=list(x='lag', y='estimate'), palette='hue'
    ), 'render')$palette, 'default')
)
expect_error(validate_render(list(
    geometry='line', mappings=list(x='lag', y='estimate'), palette='nope'
), 'render'), 'ColorBrewer')

# A gradient lists two or more colors, normalized to hex, and colors the
# levels at equal steps from the first color to the last.
gradient <- validate_render(list(
    geometry='line', mappings=list(x='lag', y='estimate'),
    palette=list('#00f', 'rgb(255, 0, 0)', 'white')
), 'render')$palette
stopifnot(identical(gradient, c('#0000FF', '#FF0000', '#FFFFFF')))
expect_error(validate_render(list(
    geometry='line', mappings=list(x='lag', y='estimate'),
    palette=list('#0000FF', 'rgb(300, 0, 0)')
), 'render'), 'rgb\\(300, 0, 0\\)')
palette_colors <- getFromNamespace('.cdrgam_cli_palette_colors', 'cdrgam.cli')
stepped <- palette_colors(c('#000000', '#FFFFFF'), c('a', 'b', 'c'))
stopifnot(
    identical(unname(stepped), c('#000000', '#7F7F7F', '#FFFFFF')),
    identical(names(stepped), c('a', 'b', 'c'))
)

# Points over a few lag ticks: uniform spacing puts them at 1, 2, 3 with the
# lag values as labels; linear spacing at the lags themselves. Series sharing
# an x position are dodged.
ticks <- expand.grid(lag=c(0, 0.1, 1), term=c('x', 'y'), stringsAsFactors=FALSE)
ticks <- transform(
    ticks, term_id=paste0('irf-', term), group=NA_character_, composition='term',
    grouping='population', estimate=lag + (term == 'y'), se=0.1,
    lower=lag - 0.2, upper=lag + 0.2, layer='estimate'
)
point_definition <- list(
    query=validate(list(terms=c('x', 'y')), 'query'),
    render=validate_render(list(
        geometry='point', mappings=list(x='lag', y='estimate', color='term'),
        spacing='uniform', palette='Dark2'
    ), 'render')
)
built <- ggplot2::ggplot_build(plot_effect(ticks, point_definition))
points <- built$data[[length(built$data)]]
bars <- built$data[[length(built$data) - 1L]]
stopifnot(
    nrow(points) == 6L, all(abs(round(points$x) - points$x) > 0),
    identical(sort(unique(round(points$x))), c(1, 2, 3)),
    all(is.finite(bars$ymin)),
    identical(
        sort(unique(points$colour)),
        sort(unname(ggplot2::scale_colour_brewer(palette='Dark2')$palette(3)[1:2]))
    ),
    identical(built$layout$panel_params[[1]]$x$get_labels(), c('0', '0.1', '1'))
)
point_definition$render$spacing <- 'linear'
built <- ggplot2::ggplot_build(plot_effect(ticks, point_definition))
stopifnot(identical(built$layout$panel_params[[1]]$x$get_labels(), c('0', '0.1', '1')))
points <- built$data[[length(built$data)]]
stopifnot(all(vapply(points$x, function(x) min(abs(x - c(0, 0.1, 1))), numeric(1)) < 0.05))

# Bars over a categorical x: one bar per term with an error bar, filled by
# the color palette.
single <- ticks[ticks$lag == 0, ]
bar_definition <- list(
    query=validate(list(terms=c('x', 'y')), 'query'),
    render=validate_render(list(
        geometry='bar', mappings=list(x='term', y='estimate', color='term'),
        palette='viridis'
    ), 'render')
)
built <- ggplot2::ggplot_build(plot_effect(single, bar_definition))
stopifnot(
    nrow(built$data[[1]]) == 2L, 'fill' %in% names(built$data[[1]]),
    identical(
        built$data[[1]]$fill,
        unname(ggplot2::scale_colour_viridis_d()$palette(2))
    ),
    nrow(built$data[[2]]) == 2L
)
# A Brewer palette with too few colors fails clearly.
many <- transform(
    data.frame(term=sprintf('t%02d', 1:10), lag=0), term_id=term,
    group=NA_character_, layer='estimate', estimate=1, se=0.1, lower=0.8, upper=1.2
)
bar_definition$render$palette <- 'Dark2'
expect_error(
    ggplot2::ggplot_build(plot_effect(many, bar_definition)), 'has 8 colors'
)

# Peak lags as a summary statistic: one bar per peak, labeled as the strip
# labels it.
peak_data <- getFromNamespace('.cdrgam_cli_peak_data', 'cdrgam.cli')
peaks <- data.frame(
    term=c('x', 'y', 'y'), source=c('x', 'y', 'y'), direction=c('pos', 'pos', 'neg'),
    lag=c(0.2, 0.4, 0.9), lower=c(0.1, 0.3, 0.7), upper=c(0.3, 0.5, 1.1),
    stringsAsFactors=FALSE
)
summary <- peak_data(peaks)
stopifnot(
    identical(summary$estimate, peaks$lag),
    identical(summary$peak[[3]], 'y · peak-neg')
)
expect_error(peak_data(NULL), 'needs a query whose lag values use a peak')
peak_definition <- list(
    query=validate(list(terms=c('x', 'y')), 'query'),
    render=validate_render(list(
        geometry='bar', mappings=list(x='peak', y='estimate', color='term'),
        statistic='peak-lag'
    ), 'render')
)
peak_plot <- plot_effect(summary, peak_definition)
built <- ggplot2::ggplot_build(peak_plot)
stopifnot(nrow(built$data[[1]]) == 3L, identical(peak_plot$labels$y, 'Peak lag'))

cat('Effect query tests passed\n')
