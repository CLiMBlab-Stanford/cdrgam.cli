library(cdrgam.cli)
library(cdrgam)

internal <- function(name) getFromNamespace(name, 'cdrgam.cli')

temporary_parent <- tempfile('cdrgam-cli-viz-test-')
dir.create(temporary_parent)
on.exit(unlink(temporary_parent, recursive=TRUE), add=TRUE)
root <- file.path(temporary_parent, 'root')
cdrgam_cli_configure(root)
options(cdrgam.cli.root=root)

help <- paste(capture.output(
    stopifnot(identical(cli_main(c('viz', '--help')), 0L))
), collapse='\n')
root_help <- paste(capture.output(cli_main('--help')), collapse='\n')
stopifnot(
    grepl('Usage: cdrgam viz', help, fixed=TRUE),
    grepl('--stop', help, fixed=TRUE),
    grepl('--no-browser', help, fixed=TRUE),
    grepl('viz', root_help, fixed=TRUE),
    !grepl('viz-server', root_help, fixed=TRUE)
)

viewer <- internal('.cdrgam_cli_viewer_path')(
    list(cdrgam_root=root), user='some user'
)
stopifnot(
    identical(basename(viewer), 'some_user'),
    identical(basename(dirname(viewer)), 'viewers')
)
port <- internal('.cdrgam_cli_viz_preferred_port')('someone')
stopifnot(
    port >= 20000L, port < 60000L,
    identical(port, internal('.cdrgam_cli_viz_preferred_port')('someone'))
)

stopifnot(identical(cli_main(c('def', 'init', 'viz-test')), 0L))
project <- file.path(root, 'projects', 'viz-test')
simulation <- simulate_cdr(
    list(x=function(lag) exp(-2 * lag)), n_impulses=200, n_responses=200,
    duration=20, window=1.5, noise_sd=0.5, seed=17
)
set.seed(18)
simulation$impulses$z <- stats::rnorm(nrow(simulation$impulses))
dir.create(file.path(project, 'data'))
saveRDS(simulation$impulses, file.path(project, 'data', 'impulses.rds'))
saveRDS(simulation$responses, file.path(project, 'data', 'responses.rds'))
yaml::write_yaml(list(
    schema=1L,
    sources=list(
        impulses=list(path='data/impulses.rds', format='rds'),
        responses=list(path='data/responses.rds', format='rds')
    ),
    columns=list(impulse_time='time', response_time='time')
), file.path(project, 'definitions', 'datasets', 'training.yml'))
yaml::write_yaml(list(
    schema=1L, datasets=list(train='training'), window=c(0, 1.5), k_l=4L,
    formula='response ~ irf(x, z, k_p=list(3, 3))',
    fit=list(family='gaussian', link='identity', method='REML', backend='mgcv', engine='gam')
), file.path(project, 'definitions', 'models', 'tensor.yml'))

empty <- internal('.cdrgam_cli_viz_models')('viz-test')
stopifnot(identical(nrow(empty), 0L))
cdrgam_cli_run(projects='viz-test', models='tensor')
models <- internal('.cdrgam_cli_viz_models')('viz-test')
stopifnot(
    identical(models$id, 'viz-test/tensor'),
    file.exists(models$fit)
)

fit <- readRDS(models$fit)
terms <- internal('.cdrgam_cli_viz_terms')(fit)
labels <- vapply(terms, `[[`, character(1), 'label')

# peak-pointwise(LABEL) holds each point of x:z at the peak lag of x at
# that point's x value, so points that share x share a lag.
if (internal('.cdrgam_cli_core_has_points')()) {
    yaml::write_yaml(list(
        schema=1L, datasets=list(train='training'), window=c(0, 1.5), k_l=4L,
        formula='response ~ irf(x) + irf(x, z, k_p=list(3, 3))',
        fit=list(family='gaussian', link='identity', method='REML',
            backend='mgcv', engine='gam')
    ), file.path(project, 'definitions', 'models', 'nested.yml'))
    cdrgam_cli_run(projects='viz-test', models='nested')
    nested <- readRDS(file.path(project, 'results', 'models', 'nested', 'fit.rds'))
    nested_labels <- internal('.cdrgam_cli_core_effect_catalog')(nested)$label
    stopifnot(all(c('x', 'x:z') %in% nested_labels))
    fit_saved <- fit
    fit <- nested
    compute <- internal('.cdrgam_cli_compute_effect')
    training <- internal('.cdrgam_cli_effect_training')(fit, NULL)
    borrowed_query <- function(lag) list(
        terms='x:z', n=25L, composition='term', grouping='population',
        uncertainty=list(kind='pointwise', level=0.95, peak_draws=20L, unconditional=FALSE),
        axes=list(lag=lag, predictors=list(
            x=list(values=c('mean - sd', 'mean + sd')),
            z=list(values=c('mean - sd', 'mean', 'mean + sd'))
        ))
    )
    borrowed <- compute(fit, borrowed_query('peak-pointwise(x)'), training=training)
    peaks <- attr(borrowed, 'pointwise_peaks')
    by_x <- tapply(peaks$lag, peaks$x, function(value) length(unique(value)))
    own <- attr(compute(fit, list(
        terms='x', n=25L, composition='term', grouping='population',
        uncertainty=list(kind='pointwise', level=0.95, peak_draws=20L, unconditional=FALSE),
        axes=list(lag='peak-pointwise', predictors=list(
            x=list(values=c('mean - sd', 'mean + sd'))
        ))
    ), training=training), 'pointwise_peaks')
    stopifnot(
        nrow(borrowed) == 6L, all(peaks$source == 'x'), all(by_x == 1L),
        isTRUE(all.equal(
            # x is recomputed per term and can differ in the last bits.
            unname(peaks$lag[match(signif(own$x, 10), signif(peaks$x, 10))]),
            own$lag
        )),
        all(is.finite(borrowed$se))
    )
    unmatched <- tryCatch(
        compute(fit, list(
            terms='x', n=25L, composition='term', grouping='population',
            uncertainty=list(kind='pointwise', level=0.95, peak_draws=0L, unconditional=FALSE),
            axes=list(lag='peak-pointwise(x:z)')
        ), training=training),
        error=conditionMessage
    )
    stopifnot(is.character(unmatched), grepl('z is not a predictor of x', unmatched))
    # A simultaneous band re-finds x's peak lags in each draw and reads x:z
    # at them, with the same coefficient draws for both terms.
    if (internal('.cdrgam_cli_core_has_shared_draws')()) {
        joint_query <- borrowed_query('peak-pointwise(x)')
        joint_query$uncertainty$kind <- 'simultaneous'
        joint_query$uncertainty$peak_draws <- 50L
        joint <- compute(fit, joint_query, training=training)
        stopifnot(
            isTRUE(all.equal(joint$estimate, borrowed$estimate)),
            all(is.finite(joint$critical_value)),
            all(joint$lower <= joint$estimate & joint$estimate <= joint$upper),
            isTRUE(all.equal(
                attr(joint, 'pointwise_peaks')$lag, peaks$lag
            ))
        )
    }
    message('peak-pointwise(LABEL) checks ran')
    fit <- fit_saved
    unlink(file.path(project, 'definitions', 'models', 'nested.yml'))
}
tensor <- terms[[match('x:z', labels)]]

# A simultaneous band with a reference takes its critical value from the
# core: above the pointwise multiplier and below Bonferroni over the
# evaluated points. Without the core helper it fails instead of falling
# back.
if (internal('.cdrgam_cli_core_has_simultaneous')()) {
    band_query <- list(
        terms='x:z', n=15L, composition='term', grouping='population',
        uncertainty=list(kind='simultaneous', level=0.95, points=12L,
            unconditional=FALSE),
        axes=list(lag=list(grid='fitted', n=15L), predictors=list(
            x='mean + sd', z=list(values=c('mean - sd', 'mean + sd'))
        )),
        reference=list(x='mean')
    )
    band_training <- internal('.cdrgam_cli_effect_training')(fit, NULL)
    joint_band <- internal('.cdrgam_cli_compute_effect')(
        fit, band_query, training=band_training
    )
    stopifnot(
        all(joint_band$critical_value > stats::qnorm(0.975)),
        all(joint_band$critical_value < stats::qnorm(1 - 0.025 / 30)),
        all(joint_band$lower <= joint_band$estimate &
            joint_band$estimate <= joint_band$upper)
    )
    namespace <- asNamespace('cdrgam.cli')
    original <- get('.cdrgam_cli_core_has_simultaneous', namespace)
    unlockBinding('.cdrgam_cli_core_has_simultaneous', namespace)
    assign('.cdrgam_cli_core_has_simultaneous', function() FALSE, namespace)
    missing_core <- tryCatch(
        internal('.cdrgam_cli_compute_effect')(fit, band_query, training=band_training),
        error=conditionMessage,
        finally=assign('.cdrgam_cli_core_has_simultaneous', original, namespace)
    )
    lockBinding('.cdrgam_cli_core_has_simultaneous', namespace)
    stopifnot(is.character(missing_core), grepl('update cdrgam', missing_core))
}

# The viewer's X range sets where a line is evaluated: an expression end is
# resolved from the axis's training summaries, and an empty end is the
# fitted end.
ranged <- internal('.cdrgam_cli_viz_estimate')(fit, tensor, list(
    term='x:z', varying='x', fixed=list(), composition='term', grouping='population'
), list(n=5L, x_range=c('mean - sd', '')))
stopifnot(
    nrow(ranged) == 5L,
    isTRUE(all.equal(min(ranged$x), tensor$axes$x$mean - tensor$axes$x$sd)),
    isTRUE(all.equal(max(ranged$x), tensor$axes$x$max))
)
stopifnot(
    setequal(names(tensor$axes), c('lag', 'x', 'z')),
    all(vapply(tensor$axes, function(axis) {
        is.finite(axis$min) && is.finite(axis$max) && axis$min <= axis$median &&
            axis$median <= axis$max
    }, logical(1))),
    !tensor$grouped
)

line_query <- internal('.cdrgam_cli_viz_line_query')
axes <- line_query(
    tensor, list(varying='x', fixed=list(z=1, lag=NA), ranges=list(x=c(1, -1))),
    list(n=7L)
)$axes
stopifnot(
    identical(axes$predictors$z, 1),
    identical(axes$lag, 0),
    identical(axes$predictors$x$values, seq(-1, 1, length.out=7L))
)
# Unset axes default to lag 0 and training means; values may be expressions
# in the axis's training summaries, kept as a definition writes them.
axes <- line_query(tensor, list(varying='lag', fixed=list(x='mean + 2 * sd')))$axes
stopifnot(
    identical(axes$predictors$x, 'mean + 2 * sd'),
    identical(axes$predictors$z, 'mean'),
    identical(axes$lag, list(grid='fitted', n=200L))
)
# Lag may name the term's peak, which the evaluation resolves.
at_lag <- function(lag) unique(internal('.cdrgam_cli_viz_estimate')(
    fit, tensor, list(varying='x', fixed=list(lag=lag)), list(n=21L)
)$lag)
peak <- at_lag('peak')
stopifnot(
    peak >= tensor$axes$lag$min, peak <= tensor$axes$lag$max,
    identical(internal('.cdrgam_cli_viz_input_value')(' 2.5 ', 'x'), 2.5),
    identical(internal('.cdrgam_cli_viz_input_value')('', 'x'), NA_real_),
    identical(internal('.cdrgam_cli_viz_input_value')('peak-neg', 'lag'), 'peak-neg')
)
# peak(LABEL) borrows another term's peak; naming the term itself matches
# its own peak, and an unknown label lists the lagged terms.
lagged <- Filter(function(term) 'lag' %in% names(term$axes), terms)
other <- lagged[[which(vapply(lagged, `[[`, character(1), 'label') != 'x:z')[[1L]]]]
own <- at_lag('peak(x:z)')
borrowed <- at_lag(paste0('peak(', other$label, ')'))
expected <- internal('.cdrgam_cli_effect_peak')(fit, other$catalog, 'pos', n=21L)
unknown_peak <- tryCatch(at_lag('peak(nope)'), error=conditionMessage)
stopifnot(
    identical(own, peak), identical(borrowed, expected),
    grepl('the fit has no term nope; terms with a lag axis:', unknown_peak, fixed=TRUE)
)
# Under total composition the peak is that of the total response to a
# one-SD increase, the curve a total line shows, and a line records it.
total_request <- internal('.cdrgam_cli_peak_request')(tensor$catalog, 41L, 'total')
total_curve <- do.call(cdrgam::estimate_effect, c(list(fit, se=FALSE), total_request))
total_peak <- internal('.cdrgam_cli_effect_peak')(
    fit, tensor$catalog, 'pos', n=41L, composition='total'
)
total_line <- internal('.cdrgam_cli_viz_estimate')(fit, tensor, list(
    term='x:z', varying='x', fixed=list(lag='peak', z='mean'), composition='total'
), list(n=41L))
stopifnot(
    identical(total_request$composition, 'total'),
    identical(total_peak, total_curve$lag[[which.max(total_curve$estimate)]]),
    identical(attr(total_line, 'peaks')$composition, 'total'),
    isTRUE(all.equal(unique(total_line$lag), total_peak))
)
# peak-pointwise holds each point at the lag where the line's own contrast
# (here z = 1 against z = 0) peaks at that point's x. Estimates and standard
# errors are those at the chosen (x, lag) pairs, and each point's peak has a
# posterior interval.
pointwise_spec <- list(
    term='x:z', varying='x', fixed=list(lag='peak-pointwise', z=1),
    reference=list(z=0), composition='total'
)
pointwise_line <- internal('.cdrgam_cli_viz_estimate')(
    fit, tensor, pointwise_spec, list(n=15L)
)
pointwise <- attr(pointwise_line, 'pointwise_peaks')
search <- cdrgam::estimate_effect(
    fit, terms='x:z', composition='total', n=15L, se=FALSE,
    axes=list(lag=list(grid='fitted', n=15L), x=list(grid='fitted', n=15L), z=list(at=1)),
    reference=list(z=0)
)
expected_lags <- vapply(split(search, search$x), function(rows) {
    rows$lag[[which.max(rows$estimate)]]
}, numeric(1))
direct <- cdrgam::estimate_effect(
    fit, terms='x:z', composition='total', reference=list(z=0),
    points=data.frame(x=pointwise_line$x, lag=pointwise_line$lag, z=1)
)
stopifnot(
    nrow(pointwise_line) == 15L, nrow(pointwise) == 15L,
    isTRUE(all.equal(unname(expected_lags[as.character(pointwise_line$x)]),
        pointwise_line$lag)),
    isTRUE(all.equal(pointwise_line$estimate, direct$estimate)),
    isTRUE(all.equal(pointwise_line$se, direct$se)),
    is.null(attr(pointwise_line, 'peaks')),
    identical(pointwise$direction, rep('pos', 15L))
)
if ('simulate_effect' %in% getNamespaceExports('cdrgam')) stopifnot(
    all(pointwise$lower <= pointwise$lag & pointwise$lag <= pointwise$upper)
)
# peak-pointwise is a whole fixed-lag value only: not a reference and not
# part of an expression.
input_value <- internal('.cdrgam_cli_viz_input_value')
pointwise_errors <- c(
    tryCatch(input_value('peak-pointwise', 'lag'), error=conditionMessage),
    tryCatch(input_value('peak-pointwise + 1', 'lag', pointwise=TRUE),
        error=conditionMessage)
)
stopifnot(
    identical(input_value('peak-pointwise-neg', 'lag', pointwise=TRUE),
        'peak-pointwise-neg'),
    grepl('may only be the whole value of a fixed lag', pointwise_errors[[1L]], fixed=TRUE),
    grepl('may only be the whole value of a fixed lag', pointwise_errors[[2L]], fixed=TRUE)
)
# A line held at a peak carries that peak, with posterior draws whose
# central interval contains it. The static strip stacks under the main plot
# when peaks differ, and saves to every format.
peak_line <- internal('.cdrgam_cli_viz_estimate')(fit, tensor, list(
    term='x:z', varying='x', fixed=list(lag='peak', z='mean')
), list(n=21L))
line_peaks <- attr(peak_line, 'peaks')
stopifnot(
    identical(line_peaks$term, 'x:z'), identical(line_peaks$source, 'x:z'),
    isTRUE(all.equal(line_peaks$lag, unique(peak_line$lag)))
)
if ('simulate_effect' %in% getNamespaceExports('cdrgam')) stopifnot(
    line_peaks$lower <= line_peaks$lag, line_peaks$lag <= line_peaks$upper,
    nrow(attr(line_peaks, 'draws')) == 1000L,
    all(attr(line_peaks, 'draws')$lag >= tensor$axes$lag$min)
)
# Simultaneous bands at a peak come from draws that each re-find the peak,
# on the same coefficient draw as the line. They cover the line jointly, so
# they are at least as wide as the pointwise intervals, and they report
# their critical value. A peak-pointwise line takes each draw's effect at
# its own per-point peak.
if ('sample_effect_coefficients' %in% getNamespaceExports('cdrgam')) {
    peak_spec <- list(term='x:z', varying='x', fixed=list(lag='peak', z='mean'))
    at_peak <- internal('.cdrgam_cli_viz_estimate')(
        fit, tensor, peak_spec, list(n=21L, interval='simultaneous')
    )
    stopifnot(
        isTRUE(all.equal(at_peak$estimate, peak_line$estimate)),
        all(is.finite(at_peak$critical_value)),
        all(at_peak$critical_value > stats::qnorm(0.975)),
        all(at_peak$lower <= at_peak$estimate & at_peak$estimate <= at_peak$upper),
        !is.null(attr(at_peak, 'peaks'))
    )
    pointwise_band <- internal('.cdrgam_cli_viz_estimate')(
        fit, tensor, pointwise_spec, list(n=15L, interval='simultaneous')
    )
    stopifnot(
        isTRUE(all.equal(pointwise_band$estimate, pointwise_line$estimate)),
        all(is.finite(pointwise_band$critical_value)),
        all(pointwise_band$lower <= pointwise_band$upper)
    )
}
# The draw band: with draws centered on the estimate, its critical value
# exceeds the pointwise one and covers each group jointly.
set.seed(2)
band <- internal('.cdrgam_cli_draw_band')(
    rep(0, 6), matrix(stats::rnorm(6 * 4000), 6), rep(c('a', 'b'), each=3), 0.95
)
stopifnot(
    band$critical_value[[1L]] > 1.96, band$critical_value[[1L]] < 3,
    all(band$lower < 0 & band$upper > 0)
)
# Definition YAML: the viewer's lines become a lines definition whose
# validated queries are the queries the viewer evaluates. Shared settings
# move to the top-level query, and defaults are left out.
viewer_lines <- list(
    list(model_id='proj/main', term='x:z', varying='x', fixed=list(lag='peak', z='mean'),
        composition='total', grouping='population', label='at peak'),
    list(model_id='proj/main', term='x:z', varying='lag', fixed=list(x=1, z='mean'),
        reference=list(x=0), composition='total', grouping='population', label='x = 1')
)
viewer_options <- list(n=15L, level=0.9, interval='pointwise', unconditional=FALSE)
definition_text <- internal('.cdrgam_cli_viz_definition_yaml')(
    viewer_lines, function(id) terms, viewer_options, list(theme='classic')
)
definition <- yaml::yaml.load(definition_text)
validated <- internal('.cdrgam_cli_validate_visualization')(definition, 'viewer.yml')
canonical <- function(value) {
    if (!is.list(value)) return(value)
    if (!is.null(names(value))) value <- value[order(names(value))]
    lapply(value, canonical)
}
stopifnot(
    identical(definition$model, 'main'),
    identical(definition$query$composition, 'total'),
    identical(definition$query$n, 15L),
    identical(definition$query$axes$predictors$z, 'mean'),
    is.null(definition$lines[[1L]]$composition),
    identical(definition$lines[[2L]]$reference, list(x=0L)) ||
        identical(definition$lines[[2L]]$reference, list(x=0)),
    identical(vapply(validated$lines, `[[`, character(1), 'label'), c('at peak', 'x = 1')),
    identical(validated$render$theme, 'classic'),
    isTRUE(all.equal(
        canonical(lapply(validated$lines, `[[`, 'query')),
        canonical(lapply(viewer_lines, function(line) {
            internal('.cdrgam_cli_viz_line_query')(tensor, line, viewer_options)
        }))
    ))
)
mixed <- tryCatch(internal('.cdrgam_cli_viz_definition_yaml')(
    list(viewer_lines[[1L]], modifyList(viewer_lines[[1L]], list(model_id='other/main'))),
    function(id) terms, viewer_options, list()
), error=conditionMessage)
stopifnot(grepl('a visualization definition belongs to one project', mixed, fixed=TRUE))
# X tick values: parsed, sorted and unique; points and bars are evaluated
# at them.
ticks <- internal('.cdrgam_cli_viz_ticks')
stopifnot(
    is.null(ticks('  ')), identical(ticks('1, 0.5 0.5\t2'), c(0.5, 1, 2)),
    grepl('must be numbers', tryCatch(ticks('1, a'), error=conditionMessage))
)
ticked <- internal('.cdrgam_cli_viz_estimate')(fit, tensor, list(
    term='x:z', varying='x', fixed=list(lag=0, z='mean')
), list(n=21L, ticks=c(-0.5, 0, 0.5)))
stopifnot(identical(ticked$x, c(-0.5, 0, 0.5)))
# The palette menu's swatches: up to eight colors, all of a shorter
# palette, none for custom.
swatches <- internal('.cdrgam_cli_viz_palette_swatches')(c('okabe-ito', 'viridis', 'custom'))
stopifnot(
    length(swatches[['okabe-ito']]) == 8L, length(swatches$viridis) == 8L,
    length(swatches$custom) == 0L
)
show <- internal('.cdrgam_cli_show_peaks')
two <- rbind(line_peaks, transform(line_peaks, term='irf(1)', source='irf(1)', lag=lag + 0.5))
attr(two, 'draws') <- attr(line_peaks, 'draws')
stopifnot(
    !show(NULL), !show(line_peaks), show(line_peaks, 'always'),
    show(line_peaks, lines=TRUE), !show(line_peaks, 'never', lines=TRUE),
    show(two), !show(two, 'never'),
    identical(internal('.cdrgam_cli_peak_lanes')(transform(
        two, source=c('x:z', 'x:z'), direction=c('pos', 'neg')
    )), c('x:z', 'irf(1) \u00b7 peak-neg(x:z)'))
)
grid_data <- rbind(
    transform(as.data.frame(peak_line)[c('x', 'lag', 'estimate', 'lower', 'upper')],
        term='x:z'),
    transform(as.data.frame(peak_line)[c('x', 'lag', 'estimate', 'lower', 'upper')],
        term='irf(1)', estimate=estimate / 2)
)
render <- list(
    geometry='line', mappings=list(x='x', y='estimate', color='term'),
    palette='rainbow', theme='paper'
)
main <- ggplot2::ggplot(grid_data, ggplot2::aes(x, estimate, colour=term)) +
    ggplot2::geom_line()
figure <- internal('.cdrgam_cli_peak_figure')(main, grid_data, two, list(render=render))
stopifnot(inherits(figure$figure, 'gtable'), inherits(figure$strip, 'ggplot'))
figure_path <- tempfile(fileext='.pdf')
ggplot2::ggsave(figure_path, figure$figure, width=6, height=5)
stopifnot(file.size(figure_path) > 0)
render$peaks <- 'never'
stopifnot(is.null(internal('.cdrgam_cli_peak_figure')(main, grid_data, two, list(render=render))))
bad_input <- tryCatch(
    internal('.cdrgam_cli_viz_input_value')('peak', 'x'), error=conditionMessage
)
stopifnot(grepl('only lag values', bad_input, fixed=TRUE))
unknown_axis <- tryCatch(
    line_query(tensor, list(varying='w')), error=conditionMessage
)
stopifnot(grepl('has no axis w', unknown_axis, fixed=TRUE))

line <- internal('.cdrgam_cli_viz_estimate')(
    fit, tensor, list(varying='x', fixed=list(lag=0.2, z=1)), list(n=9L)
)
stopifnot(
    identical(nrow(line), 9L),
    all(line$lag == 0.2), all(line$z == 1),
    all(line$lower <= line$estimate & line$estimate <= line$upper)
)
joint <- internal('.cdrgam_cli_viz_estimate')(
    fit, tensor,
    list(varying='lag', fixed=list(x='mean + sd', z='mean'), reference=list(x='mean')),
    list(n=9L, interval='simultaneous')
)
stopifnot(
    identical(nrow(joint), 9L),
    all(joint$critical_value >= stats::qnorm(0.975)),
    isTRUE(all.equal(joint$upper - joint$estimate, joint$critical_value * joint$se)),
    isTRUE(all.equal(joint$x_reference, rep(tensor$axes$x$mean, 9L)))
)
reference_line <- internal('.cdrgam_cli_viz_estimate')(
    fit, tensor, list(varying='x', fixed=list(lag=0.2, z=0)), list(n=9L)
)
contrast_line <- internal('.cdrgam_cli_viz_estimate')(
    fit, tensor, list(varying='x', fixed=list(lag=0.2, z=1), reference=list(z=0)),
    list(n=9L)
)
stopifnot(
    all(contrast_line$z_reference == 0),
    isTRUE(all.equal(
        contrast_line$estimate, line$estimate - reference_line$estimate,
        tolerance=1e-10
    ))
)
# An empty reference field leaves that predictor at the model default, which
# removes the term from the reference entirely.
default_line <- internal('.cdrgam_cli_viz_estimate')(
    fit, tensor,
    list(varying='x', fixed=list(lag=0.2, z=1), reference=list(x=NA_real_, z=NA_real_)),
    list(n=9L)
)
stopifnot(
    all(is.na(default_line$x_reference)),
    isTRUE(all.equal(default_line$estimate, line$estimate, tolerance=1e-10))
)
# Only fixed axes take reference values; a varying axis keeps its plotted
# value. An empty lag field keeps lag at its fixed value.
stopifnot(
    identical(internal('.cdrgam_cli_viz_reference_axes')(tensor, 'x'), c('lag', 'z')),
    identical(internal('.cdrgam_cli_viz_reference_axes')(tensor, c('lag', 'x')), 'z'),
    identical(
        internal('.cdrgam_cli_viz_reference_values')(list(lag=NA_real_, z=NA_real_)),
        list(z=NA_real_)
    ),
    # An empty field (NULL) keeps the fixed value: the axis leaves the reference.
    is.null(internal('.cdrgam_cli_viz_reference_values')(list(lag=NULL, z=NULL))),
    identical(
        internal('.cdrgam_cli_viz_reference_values')(list(lag=NULL, z=0)), list(z=0)
    ),
    identical(
        internal('.cdrgam_cli_viz_reference_values')(list(lag='peak')), list(lag='peak')
    ),
    is.null(internal('.cdrgam_cli_viz_reference_values')(list(lag=NA_real_)))
)
contrast_spec <- list(
    model_id='viz-test/tensor', term='x:z', varying='x', fixed=list(lag=0.2, z=1),
    composition='total', grouping='population', reference=list(x=NA_real_, z=0)
)
stopifnot(grepl(
    'minus [x=default, z=0]', internal('.cdrgam_cli_viz_label')(contrast_spec, tensor),
    fixed=TRUE
))
surface <- internal('.cdrgam_cli_viz_estimate')(
    fit, tensor, list(varying=c('lag', 'z'), fixed=list(x=1)), list(n=5L)
)
matrix <- internal('.cdrgam_cli_viz_surface_matrix')(surface, 'lag', 'z')
stopifnot(
    identical(dim(matrix$z), c(5L, 5L)),
    !anyNA(matrix$z)
)

kept_lines <- list(
    list(id='a', model_id='viz-test/tensor', fit=models$fit, term='x:z', varying='x',
        fixed=list(lag=0.2, z=NA_real_), composition='term', grouping='population',
        label='first'),
    list(id='b', model_id='viz-test/tensor', fit=models$fit, term='x:z',
        varying=c('lag', 'z'), fixed=list(x=1), composition='total',
        grouping='conditional', group='g1', label='second'),
    list(id='c', model_id='viz-test/tensor', fit=models$fit, term='x:z', varying='x',
        fixed=list(lag=0.2, z=1), composition='total', grouping='population',
        reference=list(x=NA_real_, z=0), label='third'),
    list(id='d', model_id='viz-test/tensor', fit=models$fit, term='x:z', varying='lag',
        fixed=list(x='mean + sd', z='mean'), composition='term', grouping='population',
        reference=list(x='mean', z=NA_real_), label='fourth')
)
round_trip <- internal('.cdrgam_cli_viz_lines_parse')(
    internal('.cdrgam_cli_viz_lines_json')(kept_lines)
)
stopifnot(
    identical(round_trip[[1L]]$varying, 'x'),
    identical(round_trip[[2L]]$varying, c('lag', 'z')),
    identical(round_trip[[1L]]$fixed, list(lag=0.2, z=NA_real_)),
    identical(round_trip[[2L]][['group']], 'g1'),
    is.null(round_trip[[1L]][['group']]),
    is.null(round_trip[[1L]]$reference),
    identical(round_trip[[3L]]$reference, list(x=NA_real_, z=0)),
    identical(round_trip[[4L]]$fixed, list(x='mean + sd', z='mean')),
    identical(round_trip[[4L]]$reference, list(x='mean', z=NA_real_))
)

script_path <- file.path(temporary_parent, 'viz.sh')
internal('.cdrgam_cli_viz_script')(
    internal('.cdrgam_cli_site')(root), file.path(temporary_parent, 'state'),
    script_path, file.path(root, '.cdrgam', 'server.log'),
    list(cpus=1L, memory='1G', time='1:00:00'), 41741L
)
script <- readLines(script_path)
stopifnot(
    any(script == "export CDRGAM_VIZ_RESTARTABLE='1'"),
    any(script == 'while [ "$status" -eq 75 ]; do'),
    any(grepl('viz-server .* --port 41741 \\|\\| status=\\$\\?$', script))
)

# A plotly title, whether given as text or as a title object.
title_text <- function(title) if (is.list(title)) title$text else title

if (all(vapply(c('shiny', 'plotly', 'bslib'), requireNamespace, logical(1), quietly=TRUE))) {
    shifted <- peak_line
    attr(shifted, 'peaks') <- transform(line_peaks, lag=lag + 0.5)
    strip_entries <- list(
        list(spec=list(id='a', label='a', varying='x'), data=peak_line),
        list(spec=list(id='b', label='b', varying='x'), data=shifted)
    )
    stopifnot(
        identical(internal('.cdrgam_cli_viz_peak_strip')(strip_entries)$rows, c('a', 'b')),
        # One line at a peak still gets its strip, and peaks = never hides it.
        identical(internal('.cdrgam_cli_viz_peak_strip')(strip_entries[1L])$rows, 'a'),
        is.null(internal('.cdrgam_cli_lines_strip_spec')(
            strip_entries[1L],
            internal('.cdrgam_cli_viz_line_spec')(strip_entries[1L],
                settings=list(peaks='never'))
        ))
    )
    # Every lane is labeled, with the line's legend label.
    strip_axis <- plotly::plotly_build(
        internal('.cdrgam_cli_viz_line_plot')(strip_entries)
    )$x$layout$yaxis2
    stopifnot(
        identical(strip_axis$tickmode, 'array'),
        setequal(unlist(strip_axis$ticktext), c('a', 'b'))
    )
    invisible(plotly::plotly_build(internal('.cdrgam_cli_viz_line_plot')(strip_entries)))
    # A line at peak-pointwise adds a panel of its peak lag over x, which
    # shares the main x-axis; render.peaks never removes it.
    pointwise_entries <- list(
        list(spec=list(id='p', label='p', varying='x'), data=pointwise_line)
    )
    with_panel <- plotly::plotly_build(
        internal('.cdrgam_cli_viz_line_plot')(pointwise_entries)
    )$x$layout
    without_panel <- plotly::plotly_build(internal('.cdrgam_cli_viz_line_plot')(
        pointwise_entries, settings=list(peaks='never')
    ))$x$layout
    stopifnot(
        identical(title_text(with_panel$yaxis2$title), 'Peak lag'),
        identical(with_panel$xaxis$matches, 'x2'),
        # The main panel repeats the panel below's axis, so it drops its
        # title and tick labels, and the panels do not overlap.
        isFALSE(with_panel$xaxis$showticklabels),
        with_panel$yaxis2$domain[[2L]] < with_panel$yaxis$domain[[1L]],
        is.null(without_panel$yaxis2)
    )
    invisible(plotly::plotly_build(internal('.cdrgam_cli_viz_line_plot')(list(
        list(spec=list(id='a', label='a', varying='x'), data=line)
    ))))
    # Two lines at different peaks over x: the strip has its own lag axis,
    # so the main panel keeps its x title with a gap below it.
    stacked <- plotly::plotly_build(
        internal('.cdrgam_cli_viz_line_plot')(strip_entries)
    )$x$layout
    stopifnot(
        is.null(stacked$xaxis$matches),
        stacked$yaxis$domain[[1L]] - stacked$yaxis2$domain[[2L]] > 0.1
    )
    # y_zero keeps zero on the vertical axis; themes style the panel.
    zeroed <- plotly::plotly_build(internal('.cdrgam_cli_viz_line_plot')(list(
        list(spec=list(id='a', label='a', varying='x'), data=line)
    ), settings=list(y_zero=TRUE, theme='grey')))$x$layout
    stopifnot(
        identical(zeroed$yaxis$rangemode, 'tozero'),
        identical(zeroed$plot_bgcolor, '#EBEBEB')
    )
    # The default paper theme draws no outline around the panels.
    stopifnot(!isTRUE(plotly::plotly_build(internal('.cdrgam_cli_viz_line_plot')(list(
        list(spec=list(id='a', label='a', varying='x'), data=line)
    )))$x$layout$xaxis$mirror))
    for (display in c('surface', 'heatmap', 'contour')) {
        invisible(plotly::plotly_build(internal('.cdrgam_cli_viz_surface_plot')(
            surface, list(varying=c('lag', 'z')), display=display, show_interval=TRUE
        )))
    }
    stopifnot(inherits(internal('.cdrgam_cli_viz_ui')(), 'shiny.tag.list'))
    shiny::testServer(internal('.cdrgam_cli_viz_server')(root), {
        session$setInputs(mode='line', model='viz-test/tensor')
        session$setInputs(term='x:z', composition='term', n=11, level=0.9)
        session$setInputs(x_axis='x')
        session$setInputs(
            add_line=1, label='',
            !!internal('.cdrgam_cli_viz_input_id')('z') := 1
        )
        session$setInputs(x_axis='lag', add_line=2)
        stopifnot(identical(length(lines()), 2L), is.null(lines()[[2L]]$reference))
        value <- plotted()
        stopifnot(
            identical(value$mode, 'line'),
            identical(length(value$entries), 2L),
            identical(nrow(value$entries[[1L]]$data), 11L)
        )
        session$setInputs(
            use_reference=TRUE,
            !!internal('.cdrgam_cli_viz_reference_id')('z') := 0,
            !!internal('.cdrgam_cli_viz_reference_id')('x') := NA,
            add_line=3
        )
        stopifnot(
            identical(length(lines()), 3L),
            identical(lines()[[3L]]$reference, list(x=NA_real_, z=0)),
            'z_reference' %in% names(plotted()$entries[[3L]]$data)
        )
        # Editing a line replaces Add line with Update line and Cancel.
        stopifnot(grepl('add_line', output$line_buttons$html, fixed=TRUE))
        session$setInputs(edit_line=lines()[[3L]]$id)
        stopifnot(
            grepl('update_line', output$line_buttons$html, fixed=TRUE),
            !grepl('add_line', output$line_buttons$html, fixed=TRUE)
        )
        session$setInputs(cancel_edit=1)
        stopifnot(grepl('add_line', output$line_buttons$html, fixed=TRUE))
        session$setInputs(use_reference=FALSE, remove_line=lines()[[3L]]$id)
        # Appearance inputs reach the plot: bars at uniform ticks, a
        # palette that is too small for the lines is shown as an error, and
        # restored appearance values set the controls once.
        session$setInputs(render_geometry='bar', render_spacing='uniform',
            render_palette='Set1', render_title='Bars')
        drawn <- jsonlite::fromJSON(output$plot, simplifyVector=FALSE)$x
        stopifnot(
            any(vapply(drawn$data, function(trace) identical(trace$type, 'bar'), logical(1))),
            identical(title_text(drawn$layout$title), 'Bars')
        )
        session$setInputs(show_render_yaml=1)
        session$setInputs(render_palette='custom', render_palette_colors='red')
        stopifnot(grepl('two or more gradient colors', tryCatch(
            output$plot, error=conditionMessage
        )))
        session$setInputs(render_palette='okabe-ito', render_geometry='line',
            render_spacing='')
        session$setInputs(remove_line=lines()[[1L]]$id)
        stopifnot(identical(length(lines()), 1L))
        kept <- lines()
        session$setInputs(refresh=1)
        stopifnot(identical(data_version(), 1L), identical(lines(), kept))
        session$setInputs(mode='surface', x_axis='lag', y_axis='z', draw_surface=1)
        stopifnot(identical(plotted()$mode, 'surface'))
    })
    shiny::testServer(internal('.cdrgam_cli_viz_server')(root), {
        session$setInputs(mode='line', restore_appearance=
            '{"geometry":"point","palette":"Dark2","title":"Kept"}')
        panel <- paste(unlist(output$appearance_controls), collapse='')
        stopifnot(
            grepl('<option value="point" selected>', panel, fixed=TRUE),
            grepl('<option value="Dark2" selected>', panel, fixed=TRUE),
            grepl('value="Kept"', panel, fixed=TRUE)
        )
        session$setInputs(restore_lines=internal('.cdrgam_cli_viz_lines_json')(kept_lines))
        stopifnot(
            identical(length(lines()), 4L),
            identical(lines()[[2L]]$varying, c('lag', 'z')),
            identical(lines()[[3L]]$reference, list(x=NA_real_, z=0))
        )
    })
    ui_text <- function(...) {
        paste(format(internal('.cdrgam_cli_viz_ui')(...)), collapse='')
    }
    stopifnot(!grepl('Restart server', ui_text(), fixed=TRUE))
    stopifnot(grepl('Restart server', ui_text(TRUE), fixed=TRUE))
}

# A root without Slurm serves the viewer locally on every platform, where it
# has no job to stop or restart, and prints a loopback address.
local({
    configuration <- list(scheduler='local')
    viz_local <- internal('.cdrgam_cli_viz_local')
    stopifnot(
        viz_local(configuration),
        viz_local(list(scheduler='slurm'), local=TRUE),
        !viz_local(list(scheduler='slurm')),
        identical(
            internal('.cdrgam_cli_viz_url')('127.0.0.1', 41741L),
            'http://127.0.0.1:41741/'
        ),
        identical(
            internal('.cdrgam_cli_viz_url')('0.0.0.0', 8080),
            'http://127.0.0.1:8080/'
        )
    )
    if (!identical(internal('.cdrgam_cli_site')(root)$scheduler, 'slurm')) {
        for (arguments in list(list(stop=TRUE), list(restart=TRUE))) {
            failure <- tryCatch(
                do.call(cdrgam_cli_viz, c(list(cdrgam_root=root), arguments)),
                error=conditionMessage
            )
            stopifnot(grepl('local viewer runs in the foreground', failure,
                fixed=TRUE))
        }
    }
})

# The fake Slurm tools are shell scripts, and Slurm runs only on Unix.
if (.Platform$OS.type == 'unix') {
    fake_tool <- function(name, body) {
        path <- file.path(temporary_parent, name)
        writeLines(c('#!/bin/sh', body), path)
        Sys.chmod(path, mode='0755')
        path
    }
    expired_squeue <- fake_tool('squeue', c(
        'echo "slurm_load_jobs error: Invalid job id specified" >&2', 'exit 1'
    ))
    job_alive <- internal('.cdrgam_cli_viz_job_alive')
    local({
        old <- options(
            cdrgam.cli.squeue=expired_squeue,
            cdrgam.cli.sacct=fake_tool('sacct', 'echo "42|TIMEOUT"')
        )
        on.exit(options(old))
        stopifnot(!job_alive('42'))
        options(cdrgam.cli.sacct=fake_tool('sacct-running', 'echo "42|RUNNING"'))
        stopifnot(job_alive('42'))
        options(cdrgam.cli.sacct=fake_tool('sacct-empty', 'exit 0'))
        stopifnot(job_alive('42'))
        options(cdrgam.cli.squeue=fake_tool('squeue-listed', 'echo 42'))
        stopifnot(job_alive('42'))
        options(cdrgam.cli.squeue=fake_tool('squeue-unlisted', 'exit 0'))
        stopifnot(!job_alive('42'))
    })
}

instructions <- internal('.cdrgam_cli_viz_instructions')(
    list(host='node7.example.edu', port=41741L, job_id='42'), gateway='me@login.example.edu'
)
stopifnot(grepl(
    'ssh -N -L 41741:node7.example.edu:41741 me@login.example.edu', instructions, fixed=TRUE
))

# The viewer draws its lines from the shared plot specification.
if (requireNamespace('plotly', quietly=TRUE)) {
    synthetic <- function(axis, shift, lag=NULL) {
        data <- data.frame(v=seq(0, 1, length.out=5), term='x', group=NA_character_,
            estimate=seq(0, 1, length.out=5) + shift, se=0.1)
        names(data)[1L] <- axis
        data$lower <- data$estimate - 0.2
        data$upper <- data$estimate + 0.2
        if (!is.null(lag)) {
            peaks <- data.frame(term='x', source='x', direction='pos', lag=lag,
                lower=lag - 0.1, upper=lag + 0.1)
            attr(peaks, 'draws') <- data.frame(source='x', direction='pos', draw=1:3,
                lag=lag + c(-0.05, 0, 0.05))
            attr(data, 'peaks') <- peaks
        }
        data
    }
    entry <- function(id, label, data, axis='lag') {
        list(spec=list(id=id, label=label, varying=axis), data=data)
    }
    line_colors <- function(plot) {
        traces <- plotly::plotly_build(plot)$x$data
        lines <- Filter(function(trace) isTRUE(trace$showlegend), traces)
        stats::setNames(
            vapply(lines, function(trace) trace$line$color, character(1)),
            vapply(lines, function(trace) trace$name, character(1))
        )
    }
    # Up to eight lines use okabe-ito in line order; a ninth switches every
    # line to viridis. Legend entries are labeled by line, even when two
    # lines share a label.
    eight <- lapply(1:8, function(index) entry(
        paste0('l', index), if (index <= 2L) 'same' else paste0('line ', index),
        synthetic('lag', index)
    ))
    colors <- line_colors(internal('.cdrgam_cli_viz_line_plot')(eight))
    stopifnot(
        identical(unname(colors), internal('.cdrgam_cli_okabe_ito')),
        identical(names(colors)[1:3], c('same', 'same', 'line 3'))
    )
    nine <- c(eight, list(entry('l9', 'line 9', synthetic('lag', 9))))
    stopifnot(
        identical(
            unname(line_colors(internal('.cdrgam_cli_viz_line_plot')(nine))),
            ggplot2::scale_colour_viridis_d()$palette(9L)
        ),
        identical(
            unname(internal('.cdrgam_cli_viz_line_colors')(paste0('l', 1:9))),
            ggplot2::scale_colour_viridis_d()$palette(9L)
        )
    )
    # Two lines held at the same source term's peak keep their own draws.
    shared <- list(
        entry('a', 'first', synthetic('lag', 0, lag=0.2)),
        entry('b', 'second', synthetic('lag', 1, lag=0.7))
    )
    spec <- internal('.cdrgam_cli_viz_line_spec')(shared)
    strip <- internal('.cdrgam_cli_lines_strip_spec')(shared, spec)
    stopifnot(
        identical(strip$lanes, c('first', 'second')),
        nrow(strip$draws) == 6L,
        all(abs(strip$draws$lag[strip$draws$line == 'b'] - 0.7) <= 0.05 + 1e-12),
        !is.null(strip$xlim)
    )
    # Heatmaps use the static renderer's diverging colors, symmetric about 0.
    grid_surface <- expand.grid(lag=seq(0, 1, length.out=3), z=c(-1, 0, 2))
    grid_surface <- transform(grid_surface, term='s', group=NA_character_,
        estimate=lag * z, se=0.1, lower=lag * z - 0.1, upper=lag * z + 0.1)
    heatmap <- plotly::plotly_build(internal('.cdrgam_cli_viz_surface_plot')(
        grid_surface, list(varying=c('lag', 'z')), display='heatmap'
    ))$x$data[[1L]]
    stopifnot(
        identical(heatmap$type, 'heatmap'), identical(heatmap$zmin, -2),
        identical(heatmap$zmax, 2)
    )

    # Every render field with a viewer control has an input in the line
    # appearance panel, and setting it changes the viewer's render.
    # The peak timeline is the Show peak lags box under the plot instead.
    controls <- setdiff(internal('.cdrgam_cli_viz_control_fields')(), 'peaks')
    html <- as.character(internal('.cdrgam_cli_viz_appearance')('line'))
    missing <- controls[!vapply(controls, function(name) {
        grepl(paste0('id="render_', name, '"'), html, fixed=TRUE)
    }, logical(1))]
    if (length(missing)) stop('viewer has no control for render fields: ',
        paste(missing, collapse=', '))
    viz_render <- internal('.cdrgam_cli_viz_render')
    peaked <- list(entry('a', 'first', synthetic('lag', 0, lag=0.2)),
        entry('b', 'second', synthetic('lag', 1, lag=0.7)))
    chosen <- list(
        geometry='point', interval='none', theme='slides', title='T', xlab='X',
        ylab='Y', palette='Set1', scales=NULL, peaks='never', spacing='linear',
        statistic='peak-lag'
    )
    for (name in controls) {
        value <- chosen[[name]]
        settings <- list(value)
        names(settings) <- name
        if (identical(name, 'statistic')) settings$geometry <- 'bar'
        rendered <- viz_render(peaked, settings=settings)
        if (!identical(rendered[[name]], value)) stop(
            'viewer control ', name, ' does not reach the render'
        )
    }

    # Settings come from render_FIELD inputs; a custom palette is read from
    # its color list, and empty inputs are left out.
    settings <- internal('.cdrgam_cli_viz_settings')(list(
        render_geometry='bar', render_spacing='', render_palette='custom',
        render_palette_colors='#000000, rgb(255, 0, 0); white'
    ))
    stopifnot(
        identical(settings$geometry, 'bar'), is.null(settings$spacing),
        identical(settings$palette, c('#000000', 'rgb(255, 0, 0)', 'white'))
    )
    # Tick values label a line's axis and leave its grid alone; points and
    # bars are evaluated at them instead.
    line_ticks <- list(mode='line', render_geometry='line', x_ticks='0 5 10')
    stopifnot(
        identical(internal('.cdrgam_cli_viz_settings')(line_ticks)$breaks, c(0, 5, 10)),
        !internal('.cdrgam_cli_viz_ticks_evaluated')(line_ticks),
        internal('.cdrgam_cli_viz_ticks_evaluated')(
            modifyList(line_ticks, list(render_geometry='point'))
        ),
        is.null(internal('.cdrgam_cli_viz_settings')(
            modifyList(line_ticks, list(render_geometry='point'))
        )$breaks)
    )
    # Tick labels follow the typed tick values, which are sorted, and are
    # kept for points evaluated at the ticks.
    tick_labels <- internal('.cdrgam_cli_viz_tick_labels')
    stopifnot(
        identical(tick_labels('ten, zero, five', '10 0 5'), c('zero', 'five', 'ten')),
        identical(tick_labels('a,,c', '1 2 3'), c('a', '', 'c')),
        is.null(tick_labels('', '1 2')),
        grepl('2 labels for 3', tryCatch(tick_labels('a, b', '1 2 3'),
            error=conditionMessage)),
        identical(internal('.cdrgam_cli_viz_settings')(modifyList(line_ticks, list(
            x_tick_labels='start, middle, end'
        )))$tick_labels, c('start', 'middle', 'end')),
        identical(internal('.cdrgam_cli_viz_settings')(modifyList(line_ticks, list(
            render_geometry='point', x_tick_labels='start, middle, end'
        )))$tick_labels, c('start', 'middle', 'end'))
    )
    # Reversing the palette starts it from its last color: two lines under
    # okabe-ito take its eighth and seventh colors, in the plot and in the
    # line list. A spread palette such as viridis reverses its spread.
    okabe <- getFromNamespace('.cdrgam_cli_okabe_ito', 'cdrgam.cli')
    forward <- viz_render(peaked, settings=list())
    reversed <- viz_render(peaked, settings=list(reverse_palette=TRUE))
    reversed_colors <- internal('.cdrgam_cli_lines_spec')(peaked, reversed)$color$values
    palette_colors <- internal('.cdrgam_cli_palette_colors')
    stopifnot(
        isTRUE(reversed$reverse_palette), is.null(forward$reverse_palette),
        identical(unname(reversed_colors), okabe[c(8L, 7L)]),
        identical(unname(internal('.cdrgam_cli_viz_line_colors')(c('a', 'b'), reverse=TRUE)),
            okabe[c(8L, 7L)]),
        identical(unname(palette_colors('Set1', c('a', 'b', 'c'), TRUE)),
            rev(unname(palette_colors('Set1', paste0('l', 1:9))))[1:3]),
        identical(unname(palette_colors('viridis', c('a', 'b', 'c'), TRUE)),
            rev(unname(palette_colors('viridis', c('a', 'b', 'c'))))),
        identical(names(palette_colors('okabe-ito', c('a', 'b'), TRUE)), c('a', 'b'))
    )
    # Typed tick values and labels reach the viewer's plot.
    labelled_axis <- plotly::plotly_build(internal('.cdrgam_cli_viz_line_plot')(
        peaked, settings=internal('.cdrgam_cli_viz_settings')(list(
            mode='line', render_geometry='line', x_ticks='0 0.5 1',
            x_tick_labels='start, middle, end'
        ))
    ))$x$layout$xaxis
    stopifnot(identical(unlist(labelled_axis$ticktext), c('start', 'middle', 'end')))
    # The vertical range only zooms: rows are kept, and an empty end is
    # automatic. Both renderers apply it.
    zoom_settings <- internal('.cdrgam_cli_viz_settings')(list(
        mode='line', render_geometry='line', render_ylim_min=-0.5, render_ylim_max=NA
    ))
    stopifnot(identical(zoom_settings$ylim, c(-0.5, NA)))
    zoom_render <- viz_render(peaked, settings=zoom_settings)
    zoom_spec <- internal('.cdrgam_cli_lines_spec')(peaked, zoom_render)
    zoom_limits <- internal('.cdrgam_cli_spec_ylim')(zoom_spec)
    stopifnot(
        nrow(zoom_spec$rows) == nrow(internal('.cdrgam_cli_lines_spec')(
            peaked, viz_render(peaked, settings=list())
        )$rows),
        zoom_limits[[1L]] == -0.5, zoom_limits[[2L]] > max(zoom_spec$rows$estimate),
        isTRUE(all.equal(unlist(plotly::plotly_build(
            internal('.cdrgam_cli_viz_line_plot')(peaked, settings=zoom_settings)
        )$x$layout$yaxis$range), zoom_limits)),
        isTRUE(all.equal(
            internal('.cdrgam_cli_ggplot_plot')(zoom_spec)$coordinates$limits$y,
            zoom_limits
        )),
        is.null(internal('.cdrgam_cli_viz_limits')(2, 1)),
        grepl('- ~', internal('.cdrgam_cli_viz_render_yaml')(zoom_settings))
    )
    # The viewer's zero line follows the Line at zero box.
    zero_axis <- function(settings) plotly::plotly_build(
        internal('.cdrgam_cli_viz_line_plot')(peaked, settings=settings)
    )$x$layout$yaxis$zeroline
    stopifnot(isTRUE(zero_axis(list())), isFALSE(zero_axis(list(zero_line=FALSE))))
    # Duplicate inserts a copy after its line under a new id and label.
    duplicate <- internal('.cdrgam_cli_viz_duplicate_line')
    listed <- list(list(id='one', label='A', term='x'), list(id='two', label='B'))
    copied <- duplicate(listed, 'one')
    stopifnot(
        length(copied) == 3L,
        identical(vapply(copied, `[[`, character(1), 'label'), c('A', 'A (copy)', 'B')),
        !(copied[[2L]]$id %in% c('one', 'two')), identical(copied[[2L]]$term, 'x'),
        identical(duplicate(listed, 'missing'), listed)
    )
    # An interval that does not suit the geometry falls back to its default.
    stopifnot(identical(
        viz_render(peaked, settings=list(geometry='bar', interval='ribbon'))$interval,
        'errorbar'
    ))
    # Points over uniform ticks, dodged per line, in a custom gradient.
    points <- plotly::plotly_build(internal('.cdrgam_cli_viz_line_plot')(peaked, settings=list(
        geometry='point', spacing='uniform', palette=c('#000000', '#FF0000'), peaks='never'
    )))$x$data
    markers <- Filter(function(trace) identical(trace$mode, 'markers'), points)
    stopifnot(
        length(markers) == 2L,
        identical(vapply(markers, function(trace) trace$marker$color[[1L]], character(1)),
            c('#000000', '#FF0000')),
        # Two lines sit in adjacent columns one point wide around each tick.
        all(abs(markers[[1L]]$x - (1:5 - 9 / (0.85 * 96 * 7) * 4 / 2)) < 1e-9),
        identical(unlist(plotly::plotly_build(internal('.cdrgam_cli_viz_line_plot')(
            peaked, settings=list(geometry='point', spacing='uniform', peaks='never')
        ))$x$layout$xaxis$ticktext), c('0', '0.25', '0.5', '0.75', '1'))
    )
    # Connected points add one thin line per series through its dodged
    # points, in x order, with no band; the ggplot2 renderer draws the same.
    connected_settings <- list(
        geometry='connected-point', spacing='uniform', peaks='never'
    )
    connected <- plotly::plotly_build(internal('.cdrgam_cli_viz_line_plot')(
        peaked, settings=connected_settings
    ))$x$data
    # Error bars are also line traces, broken by NA between whiskers.
    joined <- Filter(function(trace) {
        identical(trace$mode, 'lines') && !anyNA(trace$x)
    }, connected)
    stopifnot(
        length(Filter(function(trace) identical(trace$mode, 'markers'), connected)) == 2L,
        length(joined) == 2L, !any(vapply(connected, function(trace) {
            identical(trace$fill, 'toself') || identical(trace$fill, 'tonexty')
        }, logical(1))),
        all(diff(joined[[1L]]$x) > 0),
        isTRUE(all.equal(joined[[1L]]$x, markers[[1L]]$x)),
        identical(viz_render(peaked, settings=connected_settings)$interval, 'errorbar')
    )
    connected_figure <- internal('.cdrgam_cli_lines_figure')(
        peaked, viz_render(peaked, settings=connected_settings)
    )
    connected_ggplot <- internal('.cdrgam_cli_ggplot_plot')(connected_figure$spec)
    geoms <- vapply(connected_ggplot$layers, function(layer) class(layer$geom)[[1L]],
        character(1))
    stopifnot('GeomLine' %in% geoms, 'GeomPoint' %in% geoms, !('GeomRibbon' %in% geoms))
    # Peak lags as bars: one per line, labeled by lane, with error bars.
    bars <- plotly::plotly_build(internal('.cdrgam_cli_viz_line_plot')(
        peaked, settings=list(geometry='bar', statistic='peak-lag')
    ))$x
    bar_traces <- Filter(function(trace) identical(trace$type, 'bar'), bars$data)
    stopifnot(
        length(bar_traces) == 2L,
        identical(vapply(bar_traces, function(trace) trace$y[[1L]], numeric(1)), c(0.2, 0.7)),
        identical(unlist(bars$layout$xaxis$ticktext), c('first', 'second')),
        identical(title_text(bars$layout$yaxis$title), 'Peak lag')
    )
    no_peaks <- tryCatch(internal('.cdrgam_cli_viz_line_plot')(
        list(entry('c', 'plain', synthetic('lag', 0))),
        settings=list(geometry='bar', statistic='peak-lag')
    ), error=conditionMessage)
    stopifnot(grepl('No line uses a peak', no_peaks))
    # The render YAML export pastes into a definition: with mappings added,
    # it validates to the settings that were chosen.
    to_yaml <- internal('.cdrgam_cli_viz_render_yaml')
    exported <- to_yaml(list(geometry='bar', spacing='uniform', title='T',
        palette=c('#00f', 'red')))
    parsed <- yaml::yaml.load(exported)$render
    parsed$mappings <- list(x='lag', y='estimate', color='term')
    checked <- internal('.cdrgam_cli_validate_render')(parsed, 'render')
    stopifnot(
        grepl('^# Render settings from cdrgam viz', exported),
        identical(names(yaml::yaml.load(exported)$render),
            c('geometry', 'title', 'palette', 'spacing')),
        identical(checked$palette, c('#0000FF', '#FF0000')),
        identical(checked$spacing, 'uniform'),
        identical(yaml::yaml.load(to_yaml(list()))$render$palette, 'okabe-ito')
    )
    flat <- to_yaml(list(geometry='surface', theme='slides'), mode='surface')
    stopifnot(
        grepl('3D surfaces are drawn only by the viewer', flat),
        identical(yaml::yaml.load(flat)$render, list(theme='slides')),
        identical(yaml::yaml.load(to_yaml(list(geometry='raster'), mode='surface'))$render,
            list(geometry='raster', interval='none'))
    )
    # A surface's standard-error companion stacks a second heatmap.
    stacked <- plotly::plotly_build(internal('.cdrgam_cli_viz_surface_plot')(
        grid_surface, list(varying=c('lag', 'z')),
        settings=list(geometry='raster', interval='companion', title='S')
    ))$x$data
    stopifnot(sum(vapply(stacked, function(trace) identical(trace$type, 'heatmap'),
        logical(1))) == 2L)
}

cat('viz: ok\n')
