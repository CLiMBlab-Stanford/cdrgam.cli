library(cdrgam.cli)
library(cdrgam)

internal <- function(name) getFromNamespace(name, 'cdrgam.cli')
expect_error <- function(expression, pattern) {
    message <- tryCatch({
        force(expression)
        NULL
    }, error=function(error) conditionMessage(error))
    if (is.null(message) || !grepl(pattern, message, fixed=TRUE)) stop(
        'Expected an error matching "', pattern, '", got: ',
        if (is.null(message)) 'no error' else message
    )
}

temporary_parent <- tempfile('cdrgam-cli-test-definitions-')
dir.create(temporary_parent)
on.exit(unlink(temporary_parent, recursive=TRUE), add=TRUE)
root <- file.path(temporary_parent, 'root')
cdrgam_cli_configure(root)
options(cdrgam.cli.root=root)

stopifnot(identical(cli_main(c('def', 'init', 'tests-project')), 0L))
project <- file.path(root, 'projects', 'tests-project')
simulation <- simulate_cdr(
    list(x=function(lag) exp(-2 * lag), z=function(lag) 0 * lag),
    n_impulses=250, n_responses=250, duration=25, window=1.5, noise_sd=0.5,
    seed=21
)
set.seed(22)
simulation$responses$g <- factor(sample(c('a', 'b', 'c'), nrow(simulation$responses), TRUE))
simulation$responses$w <- stats::rnorm(nrow(simulation$responses))
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
    schema=1L, datasets=list(train='training'), window=c(0, 1.5), k_l=5L,
    formula='response ~ g + w + irf(x) + irf(z)',
    fit=list(family='gaussian', link='identity', method='REML', backend='mgcv',
        engine='gam')
), file.path(project, 'definitions', 'models', 'main.yml'))

dir.create(file.path(project, 'definitions', 'tests'))
test_path <- file.path(project, 'definitions', 'tests', 'shape.yml')
lags <- c(0.1, 0.5)
native_path <- file.path(project, 'definitions', 'tests', 'native.yml')
yaml::write_yaml(list(schema=1L, model='main', terms=list('*')), native_path)
yaml::write_yaml(list(
    schema=1L, model='main', terms=list('z'),
    effects=list(
        x_curve=list(terms='x', axes=list(lag=list(grid='fitted', n=31L),
            predictors=list(x=1))),
        x_points=list(terms='x', axes=list(lag=list(values=lags),
            predictors=list(x=1))),
        z_curve=list(terms='z', axes=list(lag=list(grid='fitted', n=31L),
            predictors=list(z=1)))
    ),
    estimands=list(
        early_late=list(weights=list(x_points=c(1, -1))),
        x_area=list(area='x_curve'),
        z_mean=list(mean='z_curve', null=0),
        x_peak=list(peak_lag='x_curve'),
        x_height=list(peak_value='x_curve'),
        x_whole=list(wood='x_curve'),
        z_whole=list(wood='z_curve')
    ),
    uncertainty=list(draws=400L, seed=3L)
), test_path)

# The definition validates, lists, and plans with its fit as a dependency.
stopifnot(identical(cli_main(c('def', 'val', '-P', 'tests-project', '-t', 'shape')), 0L))
listed <- internal('.cdrgam_cli_list_definitions')('tests-project', list(test='shape'))
stopifnot(identical(listed$path, 'definitions/tests/shape.yml'))
plan <- cdrgam_cli_plan(projects='tests-project', tests='shape')
stopifnot(
    identical(sort(plan$kind), c('fit', 'test')),
    plan$target[plan$kind == 'test'],
    identical(basename(plan$path[plan$kind == 'test']), 'shape')
)

cdrgam_cli_run(projects='tests-project', tests=c('shape', 'native'))

# def terms prints each model's term labels as a YAML list, from its fit
# artifact, and reports models that have not been fitted.
yaml::write_yaml(list(
    schema=1L, datasets=list(train='training'), window=c(0, 1.5), k_l=5L,
    formula='response ~ irf(x)',
    fit=list(family='gaussian', link='identity', method='REML', backend='mgcv',
        engine='gam')
), file.path(project, 'definitions', 'models', 'unfitted.yml'))
listing <- capture.output(stopifnot(identical(
    cli_main(c('def', 'terms', '-P', 'tests-project')), 0L
)))
selected <- capture.output(stopifnot(identical(
    cli_main(c('def', 'terms', '-P', 'tests-project', '-m', 'main')), 0L
)))
fitted_terms <- c('g', 'w', utils::read.csv(file.path(
    project, 'results', 'models', 'main', 'smooths.csv'
))$term)
stopifnot(
    all(c('x', 'z') %in% fitted_terms),
    file.exists(file.path(project, 'results', 'models', 'main', 'parametric-terms.csv')),
    identical(selected, c('main:', paste0("  - '", fitted_terms, "'"))),
    identical(listing, c(selected, 'unfitted: []  # not fitted')),
    identical(yaml::yaml.load(paste(selected, collapse='\n'))$main, fitted_terms)
)
unlink(file.path(project, 'definitions', 'models', 'unfitted.yml'))

# Native terms get the tests of summary(), selected by label: parametric
# terms from its pTerms.table, then smooth terms from its smooth table.
native <- utils::read.delim(file.path(project, 'results', 'tests', 'native', 'tests.tsv'))
fitted_model <- readRDS(file.path(project, 'results', 'models', 'main', 'fit.rds'))
native_summary <- summary(fitted_model)
native_table <- native_summary$s.table
parametric_table <- native_summary$pTerms.table
smooth_rows <- native[-(1:2), ]
stopifnot(
    identical(native$estimand, c('g', 'w', rownames(native_table))),
    all(native$form == 'term'),
    identical(native$df[1:2], c(2, 1)),
    max(abs(native$p_value[1:2] - parametric_table[, 'p-value'])) < 1e-12,
    max(abs(smooth_rows$p_value - native_table[, 'p-value'])) < 1e-12,
    max(abs(smooth_rows$df - native_table[, 'Ref.df'])) < 1e-12,
    max(abs(smooth_rows$edf - native_table[, 'edf'])) < 1e-12
)
output <- file.path(project, 'results', 'tests', 'shape')
table <- utils::read.delim(file.path(output, 'tests.tsv'))
stopifnot(
    identical(table$estimand, c(
        'z', 'early_late', 'x_area', 'z_mean', 'x_peak', 'x_height', 'x_whole', 'z_whole'
    )),
    identical(table$form, c('term', 'weights', 'area', 'mean', 'peak_lag', 'peak_value',
        'wood', 'wood')),
    file.exists(file.path(output, 'effects-grid.tsv')),
    !file.exists(file.path(output, 'tests.csv')),
    file.exists(file.path(output, 'request.yml'))
)

# The numbers are the core's: linear estimands from one effect_inference()
# call, peaks from simulation, and whole-functional tests from test_effect(),
# which match the smooth table.
fit <- readRDS(file.path(project, 'results', 'models', 'main', 'fit.rds'))
row <- function(name) table[table$estimand == name, ]
evaluated <- utils::read.delim(file.path(output, 'effects-grid.tsv'))
grid <- sort(evaluated$lag[evaluated$effect == 'x_curve'])
stopifnot(length(grid) == 31L)
x_curve <- estimate_effect(fit, terms='x', points=data.frame(lag=grid, x=1))
points <- estimate_effect(fit, terms='x', points=data.frame(lag=lags, x=1))
smooths <- summary(fit)$s.table
stopifnot(
    abs(row('early_late')$estimate - diff(rev(points$estimate))) < 1e-8,
    abs(row('x_area')$estimate - sum(diff(grid) *
        (head(x_curve$estimate, -1L) + tail(x_curve$estimate, -1L)) / 2)) < 1e-8,
    abs(row('x_peak')$estimate - x_curve$lag[[which.max(x_curve$estimate)]]) < 1e-12,
    abs(row('x_height')$estimate - max(x_curve$estimate)) < 1e-10,
    row('x_peak')$lower <= row('x_peak')$upper,
    grepl('Wood', row('x_whole')$method),
    abs(row('x_whole')$p_value - smooths['x', 'p-value']) < 1e-10,
    abs(row('z_whole')$p_value - smooths['z', 'p-value']) < 1e-10,
    row('x_whole')$p_value < 1e-6
)

# Draws and seed are execution settings; changing them keeps the identity.
identity <- function(value) internal('.cdrgam_cli_test_identity')(value, list())$identity
validate <- internal('.cdrgam_cli_validate_test')
definition <- validate(yaml::read_yaml(test_path), test_path)
changed <- definition
changed$uncertainty$draws <- 50L
changed$uncertainty$seed <- 9L
scientific <- definition
scientific$uncertainty$level <- 0.9
stopifnot(
    identical(identity(definition), identity(changed)),
    !identical(identity(definition), identity(scientific))
)

# Invalid definitions fail with the field that is wrong.
base <- yaml::read_yaml(test_path)
invalid <- function(edit, pattern) {
    value <- base
    value <- edit(value)
    expect_error(validate(value, 'bad.yml'), pattern)
}
invalid(function(value) {
    value$estimands$x_area <- list(area='missing')
    value
}, 'names unknown effect missing')
invalid(function(value) {
    value$estimands$x_area <- list(area='x_curve', mean='x_curve')
    value
}, 'must use exactly one of')
invalid(function(value) {
    value$estimands$x_whole$null <- 1
    value
}, 'null does not apply to wood')
invalid(function(value) {
    value$estimands$x_area$direction <- 'neg'
    value
}, 'direction applies only to peak_lag and peak_value')
invalid(function(value) {
    value$effects$x_curve$uncertainty <- list(level=0.9)
    value
}, 'uncertainty is not an effect field')
invalid(function(value) {
    value$effects$x_curve$axes$lag <- 'peak-pointwise'
    value
}, 'cannot be a pointwise peak')
invalid(function(value) {
    value$test <- 'shape'
    value
}, 'test is derived from the file name')
invalid(function(value) {
    value$estimands <- NULL
    value
}, 'effects and estimands must be given together')
invalid(function(value) {
    list(schema=1L, model='main')
}, 'give terms, or effects and estimands')
native_definition <- validate(list(schema=1L, model='main', terms=list('nope')), 'n.yml')
expect_error(
    internal('.cdrgam_cli_test_term_rows')(fitted_model, native_definition$terms),
    'test term selector matched nothing: nope; available: '
)
# Renaming or removing a test is refused while it has results, and the
# message gives the purge command; after purging, it is renamed.
expect_error(
    cli_main(c('def', 'mv', '-P', 'tests-project', '-t', 'shape', 'renamed')),
    'cdrgam purge -P tests-project -t shape'
)
expect_error(
    cli_main(c('def', 'rm', '-P', 'tests-project', '-t', 'shape')),
    'cdrgam purge -P tests-project -t shape'
)
cdrgam_cli_purge(projects='tests-project', tests='shape', force=TRUE)
# def edit stages the definitions it creates; this one was written directly.
system2('git', c('-C', project, 'add', '--', 'definitions/tests/shape.yml'))
stopifnot(
    !dir.exists(file.path(project, 'results', 'tests', 'shape')),
    identical(cli_main(c('def', 'mv', '-P', 'tests-project', '-t', 'shape', 'renamed')), 0L),
    file.exists(file.path(project, 'definitions', 'tests', 'renamed.yml')),
    !file.exists(test_path)
)
# run -t MODEL, with no test of that name, creates and runs a test of every
# term of the model; plan and dry runs create nothing.
model_test <- file.path(project, 'definitions', 'tests', 'main.yml')
cdrgam_cli_run(projects='tests-project', tests='main', dry_run=TRUE) |>
    tryCatch(error=function(error) NULL)
stopifnot(!file.exists(model_test))
cdrgam_cli_run(projects='tests-project', tests='main')
created <- yaml::read_yaml(model_test)
model_rows <- utils::read.delim(file.path(project, 'results', 'tests', 'main', 'tests.tsv'))
stopifnot(
    identical(created$model, 'main'), identical(created$terms, '*'),
    identical(model_rows$estimand, native$estimand),
    any(grepl('definitions/tests/main.yml', system2(
        'git', c('-C', project, 'diff', '--cached', '--name-only'), stdout=TRUE
    ), fixed=TRUE))
)
expect_error(
    cdrgam_cli_run(projects='tests-project', tests='no-such-model'),
    'test selector matched nothing: no-such-model'
)
message('test definition checks ran')
