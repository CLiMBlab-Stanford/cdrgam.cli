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
write_template <- function(root, kind, file, text) {
    dir.create(file.path(root, kind), recursive=TRUE, showWarnings=FALSE)
    path <- file.path(root, kind, file)
    writeLines(text, path)
    path
}

temporary_parent <- tempfile('cdrgam-cli-methods-')
dir.create(temporary_parent)
on.exit(unlink(temporary_parent, recursive=TRUE), add=TRUE)

# Template engine ------------------------------------------------------------

templates <- file.path(temporary_parent, 'templates')
read_template <- internal('.cdrgam_cli_methods_read_template')
render <- internal('.cdrgam_cli_methods_render')

path <- write_template(templates, 'fit', '01_estimation.md', c(
    '---', 'section: Estimation', 'context_version: 1', 'when: described',
    'requires: [described, count, method, smoothed]', '---',
    'We fitted {{count}} models by {{method}} [@wood2011fast].',
    '{{#smoothed}}Smooth terms were penalized.{{/smoothed}}{{^smoothed}}No',
    'term was penalized.{{/smoothed}}', '', '', 'As @wood2017generalized describes.'
))
template <- read_template(path)
context <- list(described=TRUE, count=3L, method='REML & <PIRLS>', smoothed=FALSE)
paragraphs <- render(template, context)
stopifnot(
    identical(paragraphs, c(
        'We fitted 3 models by REML & <PIRLS> [@wood2011fast]. No term was penalized.',
        'As @wood2017generalized describes.'
    )),
    is.null(render(template, modifyList(context, list(described=FALSE)))),
    identical(
        internal('.cdrgam_cli_methods_citation_keys')(paragraphs),
        c('wood2011fast', 'wood2017generalized')
    )
)
expect_error(render(template, context[-2L]), 'undefined context field count')
expect_error(render(template, modifyList(context, list(count=3))),
    'must be one character string or integer')
expect_error(render(template, modifyList(context, list(smoothed='yes'))),
    'must be TRUE or FALSE')

expect_error(read_template(write_template(templates, 'bad', '01_a.md', c(
    '---', 'section: A', 'context_version: 1', 'requires: [x]', '---', '{{x}} {{y}}'
))), 'uses fields missing from requires: y')
expect_error(read_template(write_template(templates, 'bad', '02_b.md', c(
    '---', 'section: B', 'context_version: 1', 'requires: [x, y]', '---', '{{x}}'
))), 'lists fields it does not use: y')
expect_error(read_template(write_template(templates, 'bad', '03_c.md', c(
    '---', 'section: C', 'context_version: 1', 'requires: [x]', '---',
    '{{#x}}open'
))), 'leaves section x open')
expect_error(read_template(write_template(templates, 'bad', '04_d.md', c(
    '---', 'section: D', 'context_version: 1', 'requires: []', '---', '{{ x y }}'
))), 'malformed')
expect_error(read_template(write_template(templates, 'bad', '05_e.md', c(
    '---', 'section: E', 'context_version: 1', 'order: 2', '---', 'text'
))), 'unknown front matter fields: order')

# Project fixture ------------------------------------------------------------

root <- file.path(temporary_parent, 'root')
cdrgam_cli_configure(root)
options(cdrgam.cli.root=root)
stopifnot(identical(cli_main(c('def', 'init', 'methods-project')), 0L))
project <- file.path(root, 'projects', 'methods-project')
simulation <- simulate_cdr(
    list(x=function(lag) exp(-2 * lag)),
    n_impulses=150, n_responses=150, duration=15, window=1, noise_sd=0.5, seed=31
)
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
for (name in c('main', 'other')) yaml::write_yaml(list(
    schema=1L, datasets=list(train='training'), window=c(0, 1),
    k_l=if (name == 'main') 5L else 6L,
    formula='response ~ irf(x)',
    fit=list(family='gaussian', link='identity', method='REML', backend='mgcv',
        engine='gam')
), file.path(project, 'definitions', 'models', paste0(name, '.yml')))

collect <- function(preview, models=NULL) {
    graph <- internal('.cdrgam_cli_combined_graph')(
        'methods-project', models=models, checkout=root
    )
    internal('.cdrgam_cli_methods_collect')(graph, preview=preview)
}

# Preview mode describes resolved definitions without artifacts.
records <- collect(TRUE)
stopifnot(
    identical(vapply(records, `[[`, character(1), 'kind'), c('dataset', 'fit', 'fit')),
    identical(vapply(records, `[[`, character(1), 'name'), c('training', 'main', 'other')),
    all(vapply(records, `[[`, logical(1), 'included'))
)

# Realized mode requires a complete artifact for the current identity.
expect_error(
    cdrgam_cli_methods('methods-project', cdrgam_root=root),
    'No selected work item has a complete artifact'
)
stopifnot(identical(cli_main(c('run', '-P', 'methods-project', '-m', 'main')), 0L))
records <- collect(FALSE)
states <- stats::setNames(
    vapply(records, `[[`, character(1), 'state'),
    vapply(records, `[[`, character(1), 'name')
)
stopifnot(
    identical(unname(states), c('resolved', 'complete', 'missing')),
    identical(records[[2L]]$manifest$identity, records[[2L]]$identity)
)

# A changed definition makes the existing artifact stale.
main_path <- file.path(project, 'definitions', 'models', 'main.yml')
main_definition <- yaml::read_yaml(main_path)
yaml::write_yaml(modifyList(main_definition, list(k_l=4L)), main_path)
stale <- collect(FALSE, models='main')
# Datasets are reported only for included fits and predictions.
stopifnot(identical(vapply(stale, `[[`, character(1), 'state'), 'stale'))
yaml::write_yaml(main_definition, main_path)

# Bibliography ----------------------------------------------------------------

bib_path <- file.path(temporary_parent, 'references.bib')
writeLines(c(
    '@article{wood2013a, author = {Wood, Simon N.}, title = {On p-values},',
    '  journal = {Biometrika}, volume = {100}, number = {1}, pages = {221--228},',
    '  year = {2013}, doi = {10.1093/biomet/ass048}}',
    '@article{wood2013b, author = {Wood, Simon N.}, title = {A simple test},',
    '  journal = "Statistics and Computing", year = {2013}}',
    '@book{trio, author = {S{\\"a}fken, Benjamin and Pya, Natalya and Wood, Simon N.},',
    '  title = {Three authors}, edition = {2nd}, publisher = {CRC}, year = {2016}}',
    '@article{pair, author = {Shain, Cory and Schuler, William}, title = {Pair.},',
    '  journal = {Cognition}, year = {2021}}'
), bib_path)
bibliography <- internal('.cdrgam_cli_methods_read_bibliography')(bib_path)
cited <- internal('.cdrgam_cli_methods_cited_references')(
    bibliography, c('pair', 'trio', 'wood2013a', 'wood2013b')
)
entry_of <- internal('.cdrgam_cli_methods_reference_entry')
stopifnot(
    identical(unname(vapply(cited, `[[`, character(1), 'year_label')),
        c('2021', '2016', '2013b', '2013a')),
    identical(cited$pair$author_label, 'Shain & Schuler'),
    identical(cited$pair$narrative_label, 'Shain and Schuler'),
    identical(cited$trio$author_label, 'Säfken et al.'),
    identical(entry_of(cited$wood2013a, emphasis=function(text) paste0('*', text, '*')),
        paste('Wood, S. N. (2013b). On p-values. *Biometrika*, *100*(1), 221–228.',
            'https://doi.org/10.1093/biomet/ass048')),
    identical(entry_of(cited$trio), paste(
        'Säfken, B., Pya, N., & Wood, S. N. (2016). Three authors (2nd ed.). CRC.')),
    identical(entry_of(cited$pair), 'Shain, C., & Schuler, W. (2021). Pair. Cognition.')
)
expect_error(internal('.cdrgam_cli_methods_cited_references')(bibliography, 'absent'),
    'cites unknown references: absent')
shipped <- internal('.cdrgam_cli_methods_read_bibliography')()
stopifnot(length(shipped) > 0L)
for (key in names(shipped)) entry_of(
    internal('.cdrgam_cli_methods_cited_references')(shipped, key)[[1L]]
)

# Document and writers --------------------------------------------------------

builders <- list(fit=list(
    title='Model estimation', context_version=1L,
    build=function(records, preview) list(
        described=TRUE, count=length(records), method='REML', smoothed=TRUE
    )
))
fixture_bibliography <- list(
    wood2011fast=list(type='article', key='wood2011fast', title='Fast REML',
        year='2011', journal='JRSSB', authors=list(list(family='Wood', initials='S. N.'))),
    wood2017generalized=list(type='book', key='wood2017generalized', title='GAMs',
        year='2017', authors=list(list(family='Wood', initials='S. N.'))),
    unused=list(type='book', key='unused', title='Unused', year='2000',
        authors=list(list(family='Nobody', initials='N.')))
)
document_of <- internal('.cdrgam_cli_methods_document')
unlink(file.path(templates, 'bad'), recursive=TRUE)
document <- document_of(
    records, builders=builders, template_root=templates, bibliography=fixture_bibliography
)
stopifnot(
    identical(document$undescribed, 'dataset'),
    identical(names(document$references), c('wood2011fast', 'wood2017generalized'))
)
markdown <- internal('.cdrgam_cli_methods_write_markdown')(document)
stopifnot(
    grepl('## Model estimation\n\n### Estimation\n\n', markdown, fixed=TRUE),
    grepl('We fitted 1 models by REML (Wood, 2011). Smooth terms were penalized.',
        markdown, fixed=TRUE),
    grepl('As Wood (2017) describes.', markdown, fixed=TRUE),
    grepl('## References\n\n- Wood, S. N. (2011). Fast REML. *JRSSB*.\n', markdown,
        fixed=TRUE),
    !grepl('Nobody', markdown, fixed=TRUE)
)
expect_error(
    document_of(records, builders=builders, template_root=templates,
        bibliography=fixture_bibliography['wood2011fast']),
    'cites unknown references: wood2017generalized'
)
expect_error(
    document_of(records, builders=list(fit=modifyList(builders$fit,
        list(context_version=2L))), template_root=templates,
        bibliography=fixture_bibliography),
    'context version 1; the current version is 2'
)

# Coverage --------------------------------------------------------------------

# Every scientific field of a model definition is reported or silent.
model_fields <- c(
    setdiff(internal('.cdrgam_cli_model_keys'), c('fit', 'autosimplify')),
    paste0('fit.', internal('.cdrgam_cli_fit_keys')),
    paste0('autosimplify.', names(internal('.cdrgam_cli_autosimplify_defaults')()))
)
fit_builder <- internal('.cdrgam_cli_methods_fit_builder')()
check_coverage <- internal('.cdrgam_cli_methods_check_coverage')
fit_facts <- internal('.cdrgam_cli_methods_fit_facts')
check_coverage(fit_builder, model_fields, fit_facts)
expect_error(check_coverage(fit_builder, c(model_fields, 'fit.new_setting'), fit_facts),
    'neither reported nor silent: fit.new_setting')
expect_error(check_coverage(fit_builder, setdiff(model_fields, 'window'), fit_facts),
    'declared but not a field: window')
expect_error(check_coverage(fit_builder, model_fields, setdiff(fit_facts, 'rank')),
    'reported by unknown facts: rank')
# Every shipped template parses and agrees with its builder's context version.
for (kind in names(internal('.cdrgam_cli_methods_builders')())) {
    for (template in internal('.cdrgam_cli_methods_templates')(kind)) stopifnot(
        template$context_version ==
            internal('.cdrgam_cli_methods_builders')()[[kind]]$context_version
    )
}

# Fit sections ------------------------------------------------------------------

fit_document <- function(records, preview) {
    document_of(Filter(function(record) record$kind == 'fit', records), preview=preview)
}
render_text <- function(document) internal('.cdrgam_cli_methods_write_markdown')(document)

realized <- Filter(function(record) record$state == 'complete', collect(FALSE))
text <- render_text(fit_document(realized, FALSE))
stopifnot(
    grepl('CDR-GAM, an unpublished extension of continuous-time deconvolutional regression (Shain & Schuler, 2021) that estimates',
        text, fixed=TRUE),
    grepl(paste('IRFs spanned lags from 0 to 1 in the time units of the data. Unless a',
        'term specified otherwise, the lag dimension'), text, fixed=TRUE),
    grepl(paste('used a cubic regression spline basis of dimension 5 (Wood, 2017).'),
        text, fixed=TRUE),
    grepl('predictors entered IRFs linearly.', text, fixed=TRUE),
    grepl('divided by their standard deviations', text, fixed=TRUE),
    grepl('centered at their training means', text, fixed=TRUE),
    grepl('Models were estimated with the mgcv function gam() (Wood, 2017).',
        text, fixed=TRUE),
    grepl('modeled with a Gaussian distribution and an identity link.', text, fixed=TRUE),
    grepl('restricted maximum likelihood (Wood, 2011).', text, fixed=TRUE),
    grepl("The fit met the convergence criteria of cdrgam's optimizer diagnostics",
        text, fixed=TRUE),
    !grepl('Automatic simplification', text, fixed=TRUE),
    !grepl('This report describes', text, fixed=TRUE),
    !grepl('differed', text, fixed=TRUE),
    !grepl('{{', text, fixed=TRUE)
)

# Pooled preview: the models share everything but the lag basis.
preview_text <- render_text(fit_document(collect(TRUE), TRUE))
stopifnot(
    grepl('This report describes 2 models.', preview_text, fixed=TRUE),
    grepl('The models differed in their lag basis;', preview_text, fixed=TRUE),
    !grepl('the lag dimension of each IRF used', preview_text, fixed=TRUE),
    grepl('IRFs spanned lags from 0 to 1', preview_text, fixed=TRUE),
    !grepl('Convergence', preview_text, fixed=TRUE)
)

# Preview reads omitted settings from the installed core's defaults, except
# the smoothing criterion, which the core resolves from the backend.
defaults_record <- collect(TRUE)[[2L]]
defaults_record$item$model$fit <- list()
defaults_record$item$model$k_l <- NULL
settings <- internal('.cdrgam_cli_methods_fit_settings')(
    defaults_record, internal('.cdrgam_cli_core_fit_defaults')()
)
stopifnot(
    identical(settings$k_l, 10), identical(settings$bs_l, 'cr'),
    identical(settings$engine, 'bam'), identical(settings$backend, 'mgcv'),
    isTRUE(settings$rescale), is.null(settings$method)
)
default_text <- render_text(document_of(list(defaults_record), preview=TRUE))
stopifnot(grepl(paste('selected by fast restricted maximum likelihood',
    '(Wood et al., 2015).'), default_text, fixed=TRUE))

# Automatic simplification policy and realized edits.
policy_record <- realized[[1L]]
policy_record$item$model$autosimplify <- list(enabled=TRUE, max_steps=3L,
    conservatism=1, allow=c('drop_term', 'drop_random_effect'), protect='irf(x)')
policy_text <- render_text(document_of(list(policy_record), preview=FALSE))
stopifnot(
    grepl(paste('Simplification was limited to 3 fits per model with conservatism 1;',
        'the allowed edits were drop term or drop random effect; the terms irf(x)',
        'were protected from simplification.'), policy_text, fixed=TRUE),
    grepl('No model required simplification.', policy_text, fixed=TRUE)
)

table <- internal('.cdrgam_cli_methods_fit_table')(realized)
stopifnot(
    identical(table$model, 'main'),
    identical(table$formula, 'response ~ irf(x)'),
    identical(table$fitted_formula, 'response ~ irf(x)'),
    identical(table$lag_basis, 'cr, k=5'),
    identical(table$method, 'REML')
)

# Other kinds -------------------------------------------------------------------

stopifnot(identical(cli_main(c('def', 'init', 'kinds-project')), 0L))
kinds <- file.path(root, 'projects', 'kinds-project')
dir.create(file.path(kinds, 'data'))
saveRDS(simulation$impulses, file.path(kinds, 'data', 'impulses.rds'))
saveRDS(transform(simulation$responses, row=seq_len(nrow(simulation$responses))),
    file.path(kinds, 'data', 'responses.rds'))
dir.create(file.path(kinds, 'scripts'))
writeLines('prepare <- function(impulses, responses) list(impulses=impulses, responses=responses)',
    file.path(kinds, 'scripts', 'prepare.R'))
yaml::write_yaml(list(
    schema=1L,
    sources=list(
        impulses=list(path='data/impulses.rds', format='rds'),
        responses=list(path='data/responses.rds', format='rds')
    ),
    columns=list(impulse_time='time', response_time='time', row_id='row'),
    filters=list(list(column='response', fun='>', args=-100)),
    preprocess=list(script='scripts/prepare.R', `function`='prepare')
), file.path(kinds, 'definitions', 'datasets', 'training.yml'))
for (name in c('a', 'b')) yaml::write_yaml(list(
    schema=1L, datasets=list(train='training', val='training'), window=c(0, 1),
    k_l=5L, formula='response ~ irf(x)',
    fit=list(family='gaussian', link='identity', method='REML', backend='mgcv',
        engine=if (name == 'a') 'gam' else 'bam')
), file.path(kinds, 'definitions', 'models', paste0(name, '.yml')))
yaml::write_yaml(list(schema=1L, models=c('a', 'b'), evaluation=list(dataset='val', row_id='row'),
    methods=c('mse', 'mae')), file.path(kinds, 'definitions', 'comparisons', 'ab.yml'))
dir.create(file.path(kinds, 'definitions', 'tests'))
yaml::write_yaml(list(schema=1L, model='a', terms=list('*')),
    file.path(kinds, 'definitions', 'tests', 'terms.yml'))
yaml::write_yaml(list(
    schema=1L, model='a',
    effects=list(curve=list(terms='x', axes=list(lag=list(grid='fitted', n=11L),
        predictors=list(x=1)))),
    estimands=list(area=list(area='curve'), peak=list(peak_lag='curve', null=0.5)),
    uncertainty=list(level=0.9, unconditional=TRUE, draws=500L)
), file.path(kinds, 'definitions', 'tests', 'shape.yml'))
yaml::write_yaml(list(
    schema=1L, model='a',
    query=list(terms=list(predictors='*'), composition='total',
        axes=list(lag=list(grid='fitted', n=11L),
            predictors=list('*'=list(at=list(summary='mean', 'offset-sd'=1)))),
        uncertainty=list(level=0.9, kind='simultaneous', unconditional=TRUE)),
    render=list(geometry='line', mappings=list(x='lag', y='estimate', color='term'),
        interval='ribbon', theme='paper', formats='pdf')
), file.path(kinds, 'definitions', 'visualizations', 'curve.yml'))
yaml::write_yaml(list(schema=1L, model='a', kind='booklet', pages=1L),
    file.path(kinds, 'definitions', 'visualizations', 'pages.yml'))

kinds_records <- function(preview) {
    graph <- internal('.cdrgam_cli_combined_graph')('kinds-project', checkout=root)
    internal('.cdrgam_cli_methods_collect')(graph, preview=preview)
}
preview_records <- kinds_records(TRUE)
stopifnot(identical(
    unique(vapply(preview_records, `[[`, character(1), 'kind')),
    c('dataset', 'fit', 'prediction', 'comparison', 'test', 'visualization')
))
preview_text <- render_text(document_of(preview_records, preview=TRUE))
stopifnot(
    grepl('Responses were retained only when response > -100; impulses were not filtered.',
        preview_text, fixed=TRUE),
    grepl('transformed by the function prepare in the project script scripts/prepare.R.',
        preview_text, fixed=TRUE),
    grepl('The models differed in their estimation software;', preview_text, fixed=TRUE),
    grepl('Fitted models generated 2 prediction sets for 1 dataset.', preview_text,
        fixed=TRUE),
    grepl(paste('compared by the mean squared error and mean absolute error of their',
        'residuals, averaged over the responses with defined residuals in the training',
        'dataset.'), preview_text, fixed=TRUE),
    # APA orders same-author, same-year references by title.
    grepl(paste('Wald tests of parametric terms (Wood, 2017), tests of smooth terms',
        'based on a rank-truncated pseudo-inverse (Wood, 2013b), and',
        'tests of fully penalized terms, such as random effects (Wood, 2013a).'),
        preview_text, fixed=TRUE),
    !grepl('The tests covered', preview_text, fixed=TRUE),
    grepl('Linear estimands, namely trapezoidal areas under the effect over lag, received',
        preview_text, fixed=TRUE),
    grepl('namely peak lags, were computed from 500 draws', preview_text, fixed=TRUE),
    grepl('Intervals have 90% coverage.', preview_text, fixed=TRUE),
    grepl('corrected for smoothing-parameter uncertainty (Wood et al., 2016).',
        preview_text, fixed=TRUE),
    grepl('null value of 0, unless an estimand specified its own.', preview_text,
        fixed=TRUE),
    !grepl('The tests differed', preview_text, fixed=TRUE),
    grepl('## Data\n\nEach dataset pairs', preview_text, fixed=TRUE),
    grepl('Figures of fitted effects show 90% simultaneous intervals, computed',
        preview_text, fixed=TRUE),
    grepl('A simultaneous interval uses', preview_text, fixed=TRUE),
    grepl('Default plot booklets of fitted models were produced', preview_text, fixed=TRUE),
    grepl('- Wood, S. N. (2013b). On p-values for smooth components', preview_text,
        fixed=TRUE),
    !grepl('{{', preview_text, fixed=TRUE)
)
for (kind in c('dataset', 'prediction', 'comparison', 'test', 'visualization')) {
    stopifnot(grepl(paste0('## ', internal('.cdrgam_cli_methods_builders')()[[kind]]$title),
        preview_text, fixed=TRUE))
}

# Realized tests report the tests that were actually computed.
stopifnot(identical(cli_main(c('run', '-P', 'kinds-project', '-c', 'ab')), 0L))
stopifnot(identical(cli_main(c('run', '-P', 'kinds-project', '-t', 'terms')), 0L))
realized_kinds <- Filter(function(record) isTRUE(record$included), kinds_records(FALSE))
stopifnot(setequal(
    vapply(Filter(function(record) record$kind != 'dataset', realized_kinds),
        `[[`, character(1), 'name'),
    c('a', 'b', 'a_training', 'b_training', 'ab', 'terms')
))
realized_text <- render_text(document_of(realized_kinds, preview=FALSE))
stopifnot(
    grepl('The tests covered 2 smooth terms.', realized_text, fixed=TRUE),
    !grepl('Estimands of fitted effects', realized_text, fixed=TRUE),
    !grepl('## Figures', realized_text, fixed=TRUE)
)
tests_table <- internal('.cdrgam_cli_methods_test_table')(Filter(function(record) {
    record$kind == 'test'
}, preview_records))
stopifnot(
    identical(tests_table$entry, c('estimand', 'estimand', 'term')),
    identical(tests_table$null, c('0', '0.5', '')),
    grepl('"grid":"fitted"', tests_table$definition[[1L]], fixed=TRUE)
)

# Coverage of the other kinds.
dataset_fields <- c(
    setdiff(internal('.cdrgam_cli_dataset_keys'), c('sources', 'columns', 'preprocess')),
    paste0('sources.', internal('.cdrgam_cli_source_keys')),
    paste0('columns.', internal('.cdrgam_cli_dataset_column_keys')),
    'preprocess.script', 'preprocess.function'
)
test_fields <- c(
    setdiff(internal('.cdrgam_cli_test_keys'), 'uncertainty'),
    paste0('uncertainty.', internal('.cdrgam_cli_test_uncertainty_keys'))
)
comparison_fields <- c(
    setdiff(internal('.cdrgam_cli_comparison_keys'), 'evaluation'),
    'evaluation.dataset', 'evaluation.row_id'
)
all_builders <- internal('.cdrgam_cli_methods_builders')()
check_coverage(all_builders$dataset, dataset_fields)
check_coverage(all_builders$fit, model_fields)
check_coverage(all_builders$prediction, character())
check_coverage(all_builders$comparison, comparison_fields)
check_coverage(all_builders$test, test_fields)
check_coverage(all_builders$visualization, internal('.cdrgam_cli_visualization_keys'))

# Several projects are pooled into one report, which needs -o.
expect_error(cli_main(c('methods', '-P', 'methods-project', 'kinds-project')),
    'Give an output directory with -o')
pooled <- file.path(temporary_parent, 'pooled')
stopifnot(identical(cli_main(c('methods', '-P', 'methods-project', 'kinds-project',
    '-o', pooled)), 0L))
pooled_models <- utils::read.delim(file.path(pooled, 'supplementary_models.tsv'),
    stringsAsFactors=FALSE)
pooled_trace <- jsonlite::fromJSON(file.path(pooled, 'methods.json'),
    simplifyVector=FALSE)
stopifnot(
    identical(pooled_models$project, c('kinds-project', 'kinds-project', 'methods-project')),
    identical(unlist(pooled_trace$scope$projects), c('methods-project', 'kinds-project')),
    identical(pooled_trace$omitted$missing, 4L)
)

# Command ---------------------------------------------------------------------

stopifnot(identical(cli_main(c('methods', '-P', 'methods-project', '--format', 'md')), 0L))
output <- file.path(project, 'results', 'methods')
first <- lapply(c('methods.json', 'methods.md', 'supplementary_models.tsv'),
    function(name) readLines(file.path(output, name)))
trace <- jsonlite::fromJSON(file.path(output, 'methods.json'), simplifyVector=FALSE)
tsv <- utils::read.delim(file.path(output, 'supplementary_models.tsv'),
    stringsAsFactors=FALSE)
stopifnot(
    identical(trace$scope$mode, 'realized'),
    identical(trace$omitted$missing, 1L),
    identical(trace$omitted$stale, 0L),
    identical(vapply(trace$items, `[[`, character(1), 'state'),
        c('resolved', 'complete', 'missing')),
    !length(trace$undescribed),
    identical(unlist(trace$tables),
        c('supplementary_datasets.tsv', 'supplementary_models.tsv',
            'supplementary_software.tsv')),
    identical(tsv$model, 'main'),
    'wood2011fast' %in% unlist(lapply(trace$sections, function(section) section$citations)),
    any(grepl('^- Shain, C., & Schuler, W. \\(2021\\)', first[[2L]]))
)
software <- utils::read.delim(file.path(output, 'supplementary_software.tsv'),
    stringsAsFactors=FALSE)
stopifnot(
    identical(software$source[software$software == 'mgcv'], 'report'),
    any(software$software == 'cdrgam' & grepl('^artifacts \\(', software$source))
)
stopifnot(identical(cli_main(c('methods', '-P', 'methods-project', '--format', 'md')), 0L))
stopifnot(identical(lapply(c('methods.json', 'methods.md', 'supplementary_models.tsv'),
    function(name) readLines(file.path(output, name))), first))

elsewhere <- file.path(temporary_parent, 'elsewhere')
stopifnot(identical(cli_main(c(
    'methods', '-P', 'methods-project', '--preview', '-o', elsewhere
)), 0L))
trace <- jsonlite::fromJSON(file.path(elsewhere, 'methods.json'), simplifyVector=FALSE)
stopifnot(
    identical(trace$scope$mode, 'preview'),
    all(vapply(trace$items, `[[`, character(1), 'state') == 'preview'),
    identical(nrow(utils::read.delim(file.path(elsewhere, 'supplementary_models.tsv'))), 2L)
)
# RTF is the default format; tex adds a bibliography of the cited entries.
stopifnot(identical(cli_main(c('methods', '-P', 'methods-project')), 0L))
rtf <- readLines(file.path(output, 'methods.rtf'))
stopifnot(
    identical(rtf[[1L]], '{\\rtf1\\ansi\\ansicpg1252\\deff0'),
    any(grepl('(Shain & Schuler, 2021)', rtf, fixed=TRUE)),
    any(grepl('{\\i Cognition}, {\\i 215}', rtf, fixed=TRUE)),
    all(utf8ToInt(paste(rtf, collapse='')) < 128L)
)
stopifnot(identical(cli_main(c('methods', '-P', 'methods-project', '--format', 'tex')), 0L))
tex <- readLines(file.path(output, 'methods.tex'))
bib <- internal('.cdrgam_cli_methods_read_bibliography')(file.path(output, 'methods.bib'))
cited <- unique(unlist(regmatches(tex, gregexpr('(?<=\\\\cite[pt]\\{)[^}]+', tex,
    perl=TRUE))))
stopifnot(
    any(grepl('\\citep{shain2021cdr}', tex, fixed=TRUE)),
    setequal(names(bib), unlist(strsplit(cited, ','))),
    identical(bib$shain2021cdr$journal, 'Cognition')
)
stopifnot(identical(
    internal('.cdrgam_cli_methods_tex_escape')('a_b & 50% {x} \\ ~^'),
    'a\\_b \\& 50\\% \\{x\\} \\textbackslash{} \\textasciitilde{}\\textasciicircum{}'
))
stopifnot(identical(internal('.cdrgam_cli_methods_rtf_escape')('S\u00e4fken {\\}'),
    'S\\u228?fken \\{\\\\\\}'))

# Golden output: the preview report of both fixture projects in every format.
# Set CDRGAM_UPDATE_GOLDEN=1 to rewrite the files after an intended change.
golden <- file.path('tests', 'methods-golden')
golden_output <- file.path(temporary_parent, 'golden')
for (format in c('md', 'rtf', 'tex')) {
    stopifnot(identical(cli_main(c('methods', '-P', 'methods-project', 'kinds-project',
        '--preview', '--format', format, '-o', golden_output)), 0L))
}
golden_files <- c('methods.md', 'methods.rtf', 'methods.tex', 'methods.bib',
    'supplementary_datasets.tsv', 'supplementary_models.tsv',
    'supplementary_predictions.tsv', 'supplementary_comparisons.tsv',
    'supplementary_tests.tsv', 'supplementary_visualizations.tsv')
if (identical(Sys.getenv('CDRGAM_UPDATE_GOLDEN'), '1')) {
    dir.create(golden, showWarnings=FALSE)
    invisible(file.copy(file.path(golden_output, golden_files), golden, overwrite=TRUE))
}
for (name in golden_files) {
    if (!identical(readLines(file.path(golden_output, name)),
            readLines(file.path(golden, name)))) {
        stop('Methods output ', name, ' differs from tests/methods-golden/', name)
    }
}

expect_error(
    cli_main(c('methods', '-P', 'methods-project', '--format', 'docx')),
    "Unsupported methods format"
)

cat('methods: ok\n')
