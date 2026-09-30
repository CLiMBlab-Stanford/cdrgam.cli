library(cdrgam)
library(cdrgam.cli)

root_help <- paste(capture.output(
    stopifnot(identical(cli_main('--help'), 0L))
), collapse='\n')
short_help <- paste(capture.output(
    stopifnot(identical(cli_main('-h'), 0L))
), collapse='\n')
help_command <- paste(capture.output(
    stopifnot(identical(cli_main('help'), 0L))
), collapse='\n')
run_help <- paste(capture.output(
    stopifnot(identical(cli_main(c('run', '--help')), 0L))
), collapse='\n')
def_help <- paste(capture.output(
    stopifnot(identical(cli_main(c('def', '--help')), 0L))
), collapse='\n')
edit_help <- paste(capture.output(
    stopifnot(identical(cli_main(c('help', 'def', 'edit')), 0L))
), collapse='\n')
init_help <- paste(capture.output(
    stopifnot(identical(cli_main(c('help', 'def', 'init')), 0L))
), collapse='\n')
publish_help <- paste(capture.output(
    stopifnot(identical(cli_main(c('publish', '--help')), 0L))
), collapse='\n')
fetch_help <- paste(capture.output(
    stopifnot(identical(cli_main(c('fetch', '--help')), 0L))
), collapse='\n')
stopifnot(
    identical(root_help, short_help),
    identical(root_help, help_command),
    grepl('Usage: cdrgam <COMMAND>', root_help, fixed=TRUE),
    grepl('Commands:', root_help, fixed=TRUE),
    !grepl('scheduler', root_help, fixed=TRUE),
    !grepl('worker', root_help, fixed=TRUE),
    grepl('Usage: cdrgam run', run_help, fixed=TRUE),
    grepl('-P, --project PROJECT ...', run_help, fixed=TRUE),
    grepl('re: regex', run_help, fixed=TRUE),
    grepl('--dry-run', run_help, fixed=TRUE),
    grepl('edit', def_help, fixed=TRUE),
    grepl('init', def_help, fixed=TRUE),
    grepl('ls', def_help, fixed=TRUE),
    grepl('rm', def_help, fixed=TRUE),
    grepl('val', def_help, fixed=TRUE),
    grepl('Usage: cdrgam def edit TARGET', edit_help, fixed=TRUE),
    grepl('--source SOURCE', edit_help, fixed=TRUE),
    grepl('Usage: cdrgam def init PROJECT', init_help, fixed=TRUE),
    grepl('Usage: cdrgam publish PROJECT', publish_help, fixed=TRUE),
    grepl('Usage: cdrgam fetch LOCATION', fetch_help, fixed=TRUE)
)

temporary_parent <- tempfile('cdrgam-cli-test-')
dir.create(temporary_parent)
if (!nzchar(Sys.getenv('CDRGAM_TEST_KEEP', ''))) {
    on.exit(unlink(temporary_parent, recursive=TRUE), add=TRUE)
} else {
    message('Keeping test directory ', temporary_parent)
}
checkout <- file.path(temporary_parent, 'checkout')
root <- file.path(temporary_parent, 'root')
dir.create(checkout)
cdrgam_cli_configure(checkout, root, concurrency=2L)
options(cdrgam.cli.checkout=checkout)

internal <- function(name) getFromNamespace(name, 'cdrgam.cli')
same_path <- function(left, right) {
    identical(
        internal('.cdrgam_cli_normalize_path')(left, must_work=FALSE),
        internal('.cdrgam_cli_normalize_path')(right, must_work=FALSE)
    )
}
selector_choices <- c(
    'brown-linear', 'natstor-linear', 'natstor-nonlinear', 'unrelated'
)
stopifnot(
    identical(
        internal('.cdrgam_cli_match_names')(
            're:^(brown|natstor)-linear$', selector_choices, 'model'
        ),
        c('brown-linear', 'natstor-linear')
    ),
    identical(
        internal('.cdrgam_cli_match_names')(
            're:natstor', selector_choices, 'model'
        ),
        c('natstor-linear', 'natstor-nonlinear')
    ),
    identical(
        internal('.cdrgam_cli_match_names')(
            'natstor-*', selector_choices, 'model'
        ),
        c('natstor-linear', 'natstor-nonlinear')
    ),
    internal('.cdrgam_cli_log_selector_matches')(
        selector_choices, 're:^natstor-.*linear$', 'model'
    )
)
empty_regex_error <- tryCatch({
    internal('.cdrgam_cli_match_names')('re:', selector_choices, 'model')
    NA_character_
}, error=function(error) conditionMessage(error))
invalid_regex_error <- tryCatch({
    internal('.cdrgam_cli_match_names')('re:[', selector_choices, 'model')
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(
    grepl('empty regular expression', empty_regex_error, fixed=TRUE),
    grepl('invalid regular expression', invalid_regex_error, fixed=TRUE)
)
nonconverged_distributional_fit <- structure(list(
    converged=FALSE,
    optimizer=list(
        convergence=1L,
        message='test distributional nonconvergence',
        gradient=c(0.25, -0.5)
    ),
    distributional=list(evaluations=12L, gradient_norm=0.01)
), class=c('cdrgam_distributional_sparse', 'cdrgam_sparse', 'cdrgam'))
nonconverged_distributional_diagnostics <- internal(
    '.cdrgam_cli_core_diagnostics'
)(nonconverged_distributional_fit)
idle_now <- as.POSIXct('2026-01-01 00:05:00', tz='UTC')
idle_started <- as.POSIXct('2026-01-01 00:00:00', tz='UTC')
idle_fresh <- internal('.cdrgam_cli_worker_idle_state')(
    now=idle_now, timeout=300
)
idle_waiting <- internal('.cdrgam_cli_worker_idle_state')(
    idle_started, idle_now - 1, timeout=300
)
idle_expired <- internal('.cdrgam_cli_worker_idle_state')(
    idle_started, idle_now, timeout=300
)
stopifnot(
    identical(nonconverged_distributional_diagnostics$converged, FALSE),
    identical(nonconverged_distributional_diagnostics$code, 1L),
    identical(nonconverged_distributional_diagnostics$gradient_norm, 0.5),
    identical(
        nonconverged_distributional_diagnostics$message,
        'test distributional nonconvergence'
    ),
    identical(idle_fresh$started, idle_now),
    !idle_fresh$expired,
    identical(idle_fresh$remaining, 300),
    !idle_waiting$expired,
    identical(idle_waiting$remaining, 1),
    idle_expired$expired,
    identical(idle_expired$remaining, 0),
    internal('.cdrgam_cli_absolute_path')('/tmp/example'),
    internal('.cdrgam_cli_absolute_path')('C:\\example'),
    internal('.cdrgam_cli_absolute_path')('\\\\server\\share'),
    !internal('.cdrgam_cli_absolute_path')('relative/example'),
    isTRUE(internal('.cdrgam_cli_process_alive')(Sys.getpid())),
    isTRUE(internal('.cdrgam_cli_process_alive')(
        Sys.getpid(), internal('.cdrgam_cli_process_started')()
    )),
    identical(internal('.cdrgam_cli_process_alive')(-1L), FALSE)
)
process_result <- internal('.cdrgam_cli_process_run')(
    file.path(R.home('bin'), 'Rscript'),
    c('--vanilla', '-e', 'cat("portable-process")')
)
stopifnot(
    identical(process_result$status, 0L),
    identical(process_result$stdout, 'portable-process')
)
pager_call <- NULL
pager_result <- internal('.cdrgam_cli_process_pager')(
    'pager', c('-R', 'example file.txt'),
    runner=function(command, arguments) {
        pager_call <<- list(command=command, arguments=arguments)
        0L
    }
)
stopifnot(
    identical(pager_result$status, 0L),
    identical(pager_call$command, 'pager'),
    identical(
        pager_call$arguments,
        vapply(c('-R', 'example file.txt'), shQuote, character(1))
    )
)
lock_test <- file.path(temporary_parent, 'locks', 'test.lock')
locked_value <- internal('.cdrgam_cli_with_lock')(lock_test, 42L)
stopifnot(identical(locked_value, 42L), file.exists(lock_test))
replacement <- file.path(temporary_parent, 'replace.txt')
writeLines('old', replacement)
replacement_source <- tempfile('.replace-', tmpdir=temporary_parent)
writeLines('new', replacement_source)
internal('.cdrgam_cli_replace_path')(replacement_source, replacement)
stopifnot(identical(readLines(replacement), 'new'), !file.exists(replacement_source))
replacement_directory <- file.path(temporary_parent, 'replace-directory')
dir.create(replacement_directory)
writeLines('old', file.path(replacement_directory, 'old.txt'))
replacement_directory_source <- tempfile('.replace-directory-', tmpdir=temporary_parent)
dir.create(replacement_directory_source)
writeLines('new', file.path(replacement_directory_source, 'new.txt'))
internal('.cdrgam_cli_replace_path')(
    replacement_directory_source, replacement_directory
)
stopifnot(
    file.exists(file.path(replacement_directory, 'new.txt')),
    !file.exists(file.path(replacement_directory, 'old.txt')),
    !dir.exists(replacement_directory_source),
    internal('.cdrgam_cli_within')(
        file.path(temporary_parent, 'child'), temporary_parent
    ),
    !internal('.cdrgam_cli_within')(
        paste0(temporary_parent, '-sibling'), temporary_parent
    )
)
if (.Platform$OS.type != 'windows') {
    symlink_target <- file.path(temporary_parent, 'outside')
    symlink_path <- file.path(temporary_parent, 'container', 'link')
    dir.create(symlink_target)
    dir.create(dirname(symlink_path))
    fs::link_create(symlink_target, symlink_path)
    stopifnot(!internal('.cdrgam_cli_within')(
        file.path(symlink_path, 'missing'), dirname(symlink_path)
    ))
}
old_os_type <- getOption('cdrgam.cli.os_type')
options(cdrgam.cli.os_type='windows')
windows_launcher <- internal('.cdrgam_cli_launcher_lines')(
    'C:/cdrgam instance', 'C:/R/library', 'C:/R/bin/Rscript.exe'
)
windows_process_probe <- internal('.cdrgam_cli_process_alive')(Sys.getpid())
windows_slurm <- tryCatch({
    internal('.cdrgam_cli_require_slurm_platform')()
    NULL
}, error=identity)
options(cdrgam.cli.os_type=old_os_type)
stopifnot(
    identical(windows_launcher[[1L]], '@echo off'),
    any(grepl('CDRGAM_CHECKOUT', windows_launcher, fixed=TRUE)),
    any(grepl('--args %*', windows_launcher, fixed=TRUE)),
    isTRUE(windows_process_probe),
    inherits(windows_slurm, 'error')
)
reserved_name <- tryCatch({
    internal('.cdrgam_cli_name')('nul')
    NULL
}, error=identity)
stopifnot(inherits(reserved_name, 'error'))

invalid_name <- tryCatch(
    { cdrgam_cli_def('invalid_name'); NA_character_ },
    error=function(error) conditionMessage(error)
)
stopifnot(grepl('underscores are reserved', invalid_name, fixed=TRUE))

stopifnot(identical(cli_main(c('def', 'edit', 'test-project')), 0L))
project <- file.path(root, 'projects', 'test-project')
initial_git_status <- internal('.cdrgam_cli_git')(
    project, c('status', '--short')
)$stdout
stopifnot(
    dir.exists(project),
    dir.exists(file.path(project, '.git')),
    dir.exists(file.path(project, 'results')),
    dir.exists(file.path(project, 'code')),
    !any(grepl(
        '^/code/',
        readLines(file.path(project, '.gitignore'), warn=FALSE)
    )),
    grepl('definitions/project.yml', initial_git_status, fixed=TRUE),
    grepl('.gitignore', initial_git_status, fixed=TRUE),
    same_path(find_cdrgam_project('test-project'), project)
)
test_configuration <- internal('.cdrgam_cli_checkout')(checkout)
test_project_roots <- internal('.cdrgam_cli_project_roots')(test_configuration)
test_project_id <- yaml::read_yaml(
    file.path(project, 'definitions', 'project.yml')
)$project$id
registry_references <- c(
    file.path('projects', 'test-project', 'results', 'models', 'main'),
    paste0(
        'cdrgam-project://', test_project_id,
        '/results/models/main'
    )
)
registry_resolved <- internal('.cdrgam_cli_registry_resolve')(
    test_configuration, registry_references,
    project_roots=test_project_roots
)
strict_resolved <- vapply(registry_references, function(path) {
    internal('.cdrgam_cli_managed_resolve')(
        test_configuration, path, project_roots=test_project_roots
    )
}, character(1), USE.NAMES=FALSE)
unsafe_registry_path <- tryCatch({
    internal('.cdrgam_cli_registry_resolve')(
        test_configuration, '../outside', project_roots=test_project_roots
    )
    NA_character_
}, error=function(error) conditionMessage(error))
unknown_registry_project <- tryCatch({
    internal('.cdrgam_cli_registry_resolve')(
        test_configuration,
        'cdrgam-project://unknown/results/models/main',
        project_roots=test_project_roots
    )
    NA_character_
}, error=function(error) conditionMessage(error))
registry_log <- internal('.cdrgam_cli_registry_work_log_paths')(
    test_configuration, test_project_id, 'fit', 'main',
    project_roots=test_project_roots
)
legacy_registry_log <- internal('.cdrgam_cli_registry_work_log_paths')(
    test_configuration, 'test-project', 'fit', 'main',
    project_roots=test_project_roots
)
stopifnot(
    identical(unname(registry_resolved), unname(strict_resolved)),
    grepl('not a valid relative path', unsafe_registry_path, fixed=TRUE),
    grepl('unknown project.id', unknown_registry_project, fixed=TRUE),
    same_path(
        unname(registry_log),
        internal('.cdrgam_cli_work_log_path')(
            list(root=project), 'fit', 'main'
        )
    ),
    is.na(legacy_registry_log)
)

editor_script <- file.path(temporary_parent, 'editor')
writeLines(c('#!/bin/sh', 'touch "$1"', 'exit 0'), editor_script)
Sys.chmod(editor_script, mode='0755')
old_visual <- Sys.getenv('VISUAL', unset=NA_character_)
Sys.setenv(VISUAL=editor_script)
stopifnot(identical(cli_main(c('def', 'edit', 'site')), 0L))
stopifnot(identical(cli_main(c('def', 'init', 'scaffold')), 0L))
stopifnot(identical(cli_main(c(
    'def', 'edit', 'scaffold', '--dataset', 'training'
)), 0L))
stopifnot(identical(cli_main(c(
    'def', 'edit', 'scaffold', '--model', 'main'
)), 0L))
stopifnot(file.exists(file.path(
    root, 'projects', 'scaffold', 'definitions', 'models', 'main.yml'
)))
stopifnot(identical(cli_main(c(
    'def', 'edit', 'scaffold', '--model', 'main'
)), 0L))
unlink(file.path(root, 'projects', 'scaffold'), recursive=TRUE)
if (is.na(old_visual)) Sys.unsetenv('VISUAL') else Sys.setenv(VISUAL=old_visual)

typed_csv <- file.path(temporary_parent, 'typed.csv')
writeLines(c('id,value', '001,1.5', '002,2.5'), typed_csv)
typed_data <- getFromNamespace('.cdrgam_cli_read_source', 'cdrgam.cli')(list(
    resolved_path=typed_csv, format='csv', separator=',',
    types=list(id='character')
))
stopifnot(
    identical(typed_data$id, c('001', '002')),
    is.double(typed_data$value)
)
inferred_data <- getFromNamespace('.cdrgam_cli_read_source', 'cdrgam.cli')(list(
    resolved_path=typed_csv, format='csv', separator=','
))
stopifnot(is.integer(inferred_data$id), is.double(inferred_data$value))

simulation <- simulate_cdr(
    list(x=function(lag) exp(-2 * lag)), n_impulses=80, n_responses=100,
    duration=12, window=1.5, noise_sd=0.5, seed=731
)
simulation$responses$row_id <- seq_len(nrow(simulation$responses))
simulation$responses$filter_group <- 'included'
simulation$responses$filter_factor <- ifelse(
    simulation$responses$row_id <= 60L, 'first', 'second'
)
simulation$impulses$document <- rep(c('a', 'b'), length.out=nrow(simulation$impulses))
simulation$impulses$sentence <- rep(c(1L, 2L), each=2L,
    length.out=nrow(simulation$impulses))
simulation$impulses$position <- rep(c(1L, 2L, 3L), length.out=nrow(simulation$impulses))
simulation$responses$document <- rep(c('a', 'b'), length.out=nrow(simulation$responses))
simulation$responses$sentence <- rep(c(1L, 2L), each=2L,
    length.out=nrow(simulation$responses))
simulation$responses$position <- rep(c(1L, 2L, 3L), length.out=nrow(simulation$responses))
dir.create(file.path(project, 'data'))
saveRDS(simulation$impulses, file.path(project, 'data', 'impulses.rds'))
saveRDS(simulation$responses, file.path(project, 'data', 'responses.rds'))
validation_responses <- simulation$responses
validation_responses$document <- factor(
    validation_responses$document,
    levels=rev(unique(validation_responses$document))
)
validation_responses$document[
    validation_responses$row_id == 75L
] <- NA_character_
levels(validation_responses$document) <- c(
    levels(validation_responses$document), 'heldout'
)
validation_responses$document[
    validation_responses$row_id == 75L
] <- 'heldout'
saveRDS(
    validation_responses,
    file.path(project, 'data', 'validation-responses.rds')
)

dataset_definition <- list(
    schema=1L,
    sources=list(
        impulses=list(path='data/impulses.rds', format='rds'),
        responses=list(path='data/responses.rds', format='rds')
    ),
    columns=list(
        impulse_time='time', response_time='time', row_id='row_id',
        factor_interactions=list(
            item_id=c('document', 'sentence', 'position')
        )
    ),
    filters=list(
        list(column='row_id', fun='>', args=30L),
        list(factor='filter_factor', min=35L),
        list(column='row_id', fun='<=', args=90L),
        list(column='filter_group', fun='==', args='included'),
        list(column='filter_group', fun='!=', args='excluded')
    )
)
yaml::write_yaml(
    dataset_definition,
    file.path(project, 'definitions', 'datasets', 'training.yml')
)
validation_definition <- dataset_definition
validation_definition$sources$responses$path <-
    'data/validation-responses.rds'
yaml::write_yaml(
    validation_definition,
    file.path(project, 'definitions', 'datasets', 'validation.yml')
)
model_definition <- list(
    schema=1L,
    datasets=list(train='training', val='validation'),
    window=c(0, 1.5),
    knots_l=c(0, 0.1, 0.3, 0.8, 1.5),
    k_l=5L,
    bs_l='cr',
    formula=paste(
        "response ~ s(item_id, bs='re') +",
        'irf(x) - irf(1)'
    ),
    fit=list(
        family='gaussian', link='identity', method='REML',
        backend='mgcv', engine='gam'
    )
)
multiline_model <- model_definition
multiline_model$formula <- paste(
    'response ~',
    "  s(item_id, bs='re') +",
    '  irf(x) -',
    '  irf(1)',
    sep='\n'
)
single_line_model <- multiline_model
single_line_model$formula <- "response ~ s(item_id, bs='re') + irf(x) - irf(1)"
validated_multiline <- getFromNamespace(
    '.cdrgam_cli_validate_model', 'cdrgam.cli'
)(multiline_model, 'multiline.yml')
validated_single_line <- getFromNamespace(
    '.cdrgam_cli_validate_model', 'cdrgam.cli'
)(single_line_model, 'single-line.yml')
stopifnot(
    identical(validated_multiline$formula, validated_single_line$formula),
    !grepl('\n', validated_multiline$formula, fixed=TRUE)
)
legacy_history_model <- model_definition
legacy_history_model$fit$history_length <- 8L
legacy_history_error <- tryCatch({
    getFromNamespace('.cdrgam_cli_validate_model', 'cdrgam.cli')(
        legacy_history_model, 'legacy-history.yml'
    )
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(
    grepl('history_length', legacy_history_error, fixed=TRUE),
    grepl('unknown field', legacy_history_error, fixed=TRUE)
)
poisson_block_model <- model_definition
poisson_block_model$fit <- list(
    family='poisson', link='log', backend='block', method='REML'
)
stopifnot(inherits(
    getFromNamespace('.cdrgam_cli_validate_model', 'cdrgam.cli')(
        poisson_block_model, 'poisson-block.yml'
    ),
    'list'
))
poisson_sparse_model <- poisson_block_model
poisson_sparse_model$fit$backend <- 'sparse'
stopifnot(inherits(
    getFromNamespace('.cdrgam_cli_validate_model', 'cdrgam.cli')(
        poisson_sparse_model, 'poisson-sparse.yml'
    ),
    'list'
))
gamma_sparse_model <- poisson_sparse_model
gamma_sparse_model$fit$family <- 'Gamma'
stopifnot(inherits(
    getFromNamespace('.cdrgam_cli_validate_model', 'cdrgam.cli')(
        gamma_sparse_model, 'gamma-sparse.yml'
    ),
    'list'
))
probit_sparse_model <- poisson_sparse_model
probit_sparse_model$fit <- list(
    family='binomial', link='probit', backend='sparse', method='REML'
)
probit_sparse_error <- tryCatch({
    getFromNamespace('.cdrgam_cli_validate_model', 'cdrgam.cli')(
        probit_sparse_model, 'probit-sparse.yml'
    )
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(grepl('sparse currently supports', probit_sparse_error, fixed=TRUE))
distributional_model <- model_definition
distributional_model$formula <- list(
    location=model_definition$formula,
    scale='~ irf(x) - irf(1)'
)
distributional_model$fit <- list(
    family='gaulss', backend='block', engine='gam', method='REML'
)
validated_distributional <- getFromNamespace(
    '.cdrgam_cli_validate_model', 'cdrgam.cli'
)(distributional_model, 'location-scale.yml')
distributional_sparse <- distributional_model
distributional_sparse$fit$backend <- 'sparse'
validated_distributional_sparse <- getFromNamespace(
    '.cdrgam_cli_validate_model', 'cdrgam.cli'
)(distributional_sparse, 'location-scale-sparse.yml')
stopifnot(
    identical(names(validated_distributional$formula), c('location', 'scale')),
    identical(validated_distributional$fit$family, 'gaulss'),
    identical(validated_distributional_sparse$fit$backend, 'sparse')
)
invalid_distributional <- distributional_model
invalid_distributional$fit$family <- 'gaussian'
invalid_distributional_error <- tryCatch({
    getFromNamespace('.cdrgam_cli_validate_model', 'cdrgam.cli')(
        invalid_distributional, 'invalid-distributional.yml'
    )
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(grepl(
    'distributional formula requires fit.family gaulss',
    invalid_distributional_error,
    fixed=TRUE
))
yaml::write_yaml(
    model_definition, file.path(project, 'definitions', 'models', 'decay.yml')
)
yaml::write_yaml(
    distributional_model,
    file.path(project, 'definitions', 'models', 'location-scale.yml')
)
second_model <- model_definition
yaml::write_yaml(
    second_model, file.path(project, 'definitions', 'models', 'decay-two.yml')
)
yaml::write_yaml(list(
    schema=1L, model='decay',
    kind='booklet', pages=1L
), file.path(project, 'definitions', 'visualizations', 'diagnostics.yml'))
yaml::write_yaml(list(
    schema=1L, models=c('decay', 'decay-two'),
    evaluation=list(dataset='val', row_id='row_id'), methods=c('mse', 'mae')
), file.path(project, 'definitions', 'comparisons', 'alternatives.yml'))

# Filename stems are authoritative, so redundant identity fields are invalid
# even when their value happens to match the stem.
expect_redundant_definition_name <- function(path, field) {
    original <- yaml::read_yaml(path)
    on.exit(yaml::write_yaml(original, path), add=TRUE)
    changed <- original
    changed[[field]] <- sub('\\.ya?ml$', '', basename(path))
    yaml::write_yaml(changed, path)
    message <- tryCatch({
        getFromNamespace('.cdrgam_cli_read_definitions', 'cdrgam.cli')(
            'test-project', checkout=checkout
        )
        NA_character_
    }, error=function(error) conditionMessage(error))
    stopifnot(
        grepl(field, message, fixed=TRUE),
        grepl('derived from the file name', message, fixed=TRUE),
        grepl('must be omitted', message, fixed=TRUE)
    )
}
expect_redundant_definition_name(
    file.path(project, 'definitions', 'datasets', 'training.yml'),
    'dataset'
)
expect_redundant_definition_name(
    file.path(project, 'definitions', 'models', 'decay.yml'),
    'model'
)
expect_redundant_definition_name(
    file.path(project, 'definitions', 'visualizations', 'diagnostics.yml'),
    'visualization'
)
expect_redundant_definition_name(
    file.path(project, 'definitions', 'comparisons', 'alternatives.yml'),
    'comparison'
)

# An externally invalid definition remains editable. Only the staged result is
# validated before it replaces the file.
training_path <- file.path(project, 'definitions', 'datasets', 'training.yml')
invalid_training <- yaml::read_yaml(training_path)
invalid_training$dataset <- 'training'
yaml::write_yaml(invalid_training, training_path)
cdrgam_cli_def(
    'test-project', type='dataset', name='training',
    editor=function(path) {
        value <- yaml::read_yaml(path)
        stopifnot(identical(value$dataset, 'training'))
        value$dataset <- NULL
        yaml::write_yaml(value, path)
    }
)
stopifnot(
    !('dataset' %in% names(yaml::read_yaml(training_path))),
    identical(
        getFromNamespace('.cdrgam_cli_read_definitions', 'cdrgam.cli')(
            'test-project', checkout=checkout
        )$datasets$training$dataset,
        'training'
    )
)
cancelled_path <- file.path(
    project, 'definitions', 'models', 'cancelled-invalid.yml'
)
cancelled_draft <- file.path(
    project, '.cdrgam', 'drafts', 'model', 'cancelled-invalid.yml'
)
writeLines(c('schema: 1', 'model: cancelled-invalid'), cancelled_path)
cancelled_hash <- unname(tools::md5sum(cancelled_path))
cdrgam_cli_def(
    'test-project', type='model', name='cancelled-invalid',
    editor=function(path) invisible(path)
)
stopifnot(
    identical(unname(tools::md5sum(cancelled_path)), cancelled_hash),
    !file.exists(cancelled_draft)
)
unlink(cancelled_path)

# Invalid edits do not replace the published definition. Their draft is
# reopened by the next edit and removed after successful publication.
model_path <- file.path(project, 'definitions', 'models', 'decay.yml')
model_draft <- file.path(project, '.cdrgam', 'drafts', 'model', 'decay.yml')
model_before <- unname(tools::md5sum(model_path))
cdrgam_cli_def(
    'test-project', type='model', name='decay',
    editor=function(path) invisible(path)
)
stopifnot(identical(unname(tools::md5sum(model_path)), model_before))
invalid_edit <- tryCatch({
    cdrgam_cli_def(
        'test-project', type='model', name='decay',
        editor=function(path) {
            value <- yaml::read_yaml(path)
            value$model <- 'renamed'
            yaml::write_yaml(value, path)
        }
    )
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(
    grepl('derived from the file name', invalid_edit, fixed=TRUE),
    grepl('must be omitted', invalid_edit, fixed=TRUE),
    grepl(model_draft, invalid_edit, fixed=TRUE),
    identical(unname(tools::md5sum(model_path)), model_before),
    identical(yaml::read_yaml(model_draft)$model, 'renamed')
)
cdrgam_cli_def(
    'test-project', type='model', name='decay',
    editor=function(path) {
        value <- yaml::read_yaml(path)
        stopifnot(identical(value$model, 'renamed'))
        value$model <- NULL
        yaml::write_yaml(value, path)
    }
)
stopifnot(!file.exists(model_draft))
ambiguous_path <- file.path(
    project, 'definitions', 'models', 'ambiguous.yml'
)
ambiguous_draft <- file.path(
    project, '.cdrgam', 'drafts', 'model', 'ambiguous.yml'
)
ambiguous_error <- tryCatch({
    cdrgam_cli_def(
        'test-project', type='model', name='ambiguous',
        editor=function(path) {
            Sys.setFileTime(path, Sys.time())
            invisible(path)
        }
    )
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(
    grepl('missing datasets', ambiguous_error, fixed=TRUE),
    !file.exists(ambiguous_path),
    file.exists(ambiguous_draft)
)
cdrgam_cli_def(
    'test-project', type='model', name='ambiguous',
    editor=function(path) {
        value <- yaml::read_yaml(path)
        value$datasets$train <- 'training'
        yaml::write_yaml(value, path)
    }
)
stopifnot(file.exists(ambiguous_path), !file.exists(ambiguous_draft))
unlink(ambiguous_path)

# Project copying republishes user-owned definitions and code with a fresh
# project identity and no generated artifacts.
old_source_visual <- Sys.getenv('VISUAL', unset=NA_character_)
Sys.setenv(VISUAL=editor_script)
writeLines('not a definition', file.path(project, 'results', 'analyses', 'sentinel.txt'))
writeLines('project source', file.path(project, 'code', 'sentinel.R'))
stopifnot(identical(cli_main(c(
    'def', 'edit', 'test-copy', '--source', 'test-project'
)), 0L))
copied_project <- file.path(root, 'projects', 'test-copy')
source_project_definition <- yaml::read_yaml(
    file.path(project, 'definitions', 'project.yml')
)
copied_project_definition <- yaml::read_yaml(
    file.path(copied_project, 'definitions', 'project.yml')
)
stopifnot(
    identical(copied_project_definition$project$name, 'test-copy'),
    !identical(
        copied_project_definition$project$id,
        source_project_definition$project$id
    ),
    file.exists(file.path(
        copied_project, 'definitions', 'models', 'decay.yml'
    )),
    identical(
        readLines(file.path(copied_project, 'code', 'sentinel.R'), warn=FALSE),
        'project source'
    ),
    !file.exists(file.path(copied_project, 'results', 'analyses', 'sentinel.txt')),
    !file.exists(file.path(copied_project, '.cdrgam'))
)
unlink(file.path(project, 'results', 'analyses', 'sentinel.txt'))
unlink(file.path(project, 'code', 'sentinel.R'))
existing_copy_error <- tryCatch({
    cli_main(c('def', 'edit', 'test-copy', '--source', 'test-project'))
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(grepl('already exists', existing_copy_error, fixed=TRUE))

# A source combined with a subordinate selector initializes an editable target
# without rewriting the source YAML. The target filename supplies its identity.
source_model_lines <- readLines(model_path, warn=FALSE)
writeLines(c(
    '# Formatting and comments survive source initialization.',
    source_model_lines
), model_path, useBytes=TRUE)
source_model_bytes <- readBin(
    model_path, what='raw', n=file.info(model_path)$size
)
cancelled_source_path <- file.path(
    project, 'definitions', 'models', 'decay-copy-cancelled.yml'
)
cancelled_source_draft <- file.path(
    project, '.cdrgam', 'drafts', 'model', 'decay-copy-cancelled.yml'
)
cdrgam_cli_def(
    'test-project', type='model', name='decay-copy-cancelled', source='decay',
    editor=function(path) invisible(path)
)
stopifnot(
    !file.exists(cancelled_source_path),
    !file.exists(cancelled_source_draft)
)
stopifnot(identical(cli_main(c(
    'def', 'edit', 'test-project', '--model',
    'decay-copy-a', 'decay-copy-b',
    '--source', 'decay'
)), 0L))
copied_model_paths <- file.path(
    project, 'definitions', 'models',
    paste0(c('decay-copy-a', 'decay-copy-b'), '.yml')
)
source_model <- yaml::read_yaml(model_path)
for (index in seq_along(copied_model_paths)) {
    copied_model <- yaml::read_yaml(copied_model_paths[[index]])
    copied_model_bytes <- readBin(
        copied_model_paths[[index]], what='raw',
        n=file.info(copied_model_paths[[index]])$size
    )
    stopifnot(
        identical(copied_model, source_model),
        identical(copied_model_bytes, source_model_bytes)
    )
}
source_editor_ran <- FALSE
cdrgam_cli_def(
    'test-project', type='model', name='decay-copy-edited', source='decay',
    editor=function(path) {
        source_editor_ran <<- TRUE
        value <- yaml::read_yaml(path)
        value$formula <- paste(value$formula, '+ 0')
        yaml::write_yaml(value, path)
    }
)
edited_copy <- yaml::read_yaml(file.path(
    project, 'definitions', 'models', 'decay-copy-edited.yml'
))
stopifnot(
    source_editor_ran,
    identical(edited_copy$formula, paste(source_model$formula, '+ 0'))
)
missing_source_error <- tryCatch({
    cli_main(c(
        'def', 'edit', 'test-project', '--model', 'missing-copy',
        '--source', 'missing'
    ))
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(
    grepl('does not exist', missing_source_error, fixed=TRUE),
    !file.exists(file.path(
        project, 'definitions', 'models', 'missing-copy.yml'
    ))
)
existing_definition_error <- tryCatch({
    cli_main(c(
        'def', 'edit', 'test-project', '--model', 'decay-copy-a',
        '--source', 'decay'
    ))
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(grepl(
    'already exists', existing_definition_error, fixed=TRUE
))
copied_model_artifact <- file.path(project, 'results', 'models', 'decay-copy-b')
dir.create(copied_model_artifact)
writeLines('generated', file.path(copied_model_artifact, 'result.txt'))
result_guard_error <- tryCatch({
    cli_main(c(
        'def', 'rm', 'test-project', '--model', 'decay-copy-*'
    ))
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(grepl(
    'cdrgam purge -P test-project -m decay-copy-b --yes',
    result_guard_error, fixed=TRUE
))
stopifnot(all(file.exists(copied_model_paths)))
purge_definitions <- internal('.cdrgam_cli_read_definitions')(
    'test-project', check_sources=FALSE, checkout=checkout
)
orphan_output <- file.path(project, 'results', 'models', 'decay-copy-a')
orphan_key <- paste(
    'fit', test_project_id, 'decay-copy-a', 'orphan-identity', sep=':'
)
orphan_item <- list(
    key=orphan_key, project='test-project', kind='fit', name='decay-copy-a',
    identity='orphan-identity', output=orphan_output,
    dependencies=character()
)
orphan_graph <- list(
    items=stats::setNames(list(orphan_item), orphan_key),
    targets=orphan_key,
    definitions=list(`test-project`=purge_definitions)
)
internal('.cdrgam_cli_registry_record_graph')(
    test_configuration, orphan_graph, 'purge-registry-only-test'
)
orphan_attempt <- file.path(
    project, '.cdrgam', 'work', 'fit_decay-copy-a',
    'orphan-identity', 'attempt'
)
dir.create(orphan_attempt, recursive=TRUE)
internal('.cdrgam_cli_registry_attempt')(
    test_configuration, orphan_item,
    list(
        status='failed', job_id=NULL, path=orphan_attempt,
        definitions=purge_definitions
    )
)
stopifnot(!file.exists(orphan_output), dir.exists(orphan_attempt))
cdrgam_cli_purge(
    projects='test-project', models='decay-copy-a', yes=TRUE
)
orphan_count <- internal('.cdrgam_cli_registry_exec')(
    test_configuration,
    paste0(
        'SELECT COUNT(*) AS n FROM work_items WHERE work_key=',
        internal('.cdrgam_cli_sql_quote')(orphan_key)
    ),
    query=TRUE
)$n[[1L]]
stopifnot(orphan_count == 0L, !dir.exists(orphan_attempt))
cdrgam_cli_purge(
    projects='test-project', models='decay-copy-b', yes=TRUE
)
stopifnot(identical(cli_main(c(
    'def', 'rm', 'test-project', '--model', 'decay-copy-*'
)), 0L))
stopifnot(!any(file.exists(copied_model_paths)))
referenced_definition_error <- tryCatch({
    cli_main(c(
        'def', 'rm', 'test-project', '--dataset', 'training'
    ))
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(
    grepl('referenced by: model', referenced_definition_error, fixed=TRUE),
    file.exists(file.path(
        project, 'definitions', 'datasets', 'training.yml'
    ))
)
stopifnot(identical(cli_main(c(
    'def', 'edit', 'test-project', '--dataset',
    'validation-copy-a', 'validation-copy-b',
    '--source', 'validation'
)), 0L))
validation_copy_paths <- file.path(
    project, 'definitions', 'datasets',
    paste0(c('validation-copy-a', 'validation-copy-b'), '.yml')
)
stopifnot(all(file.exists(validation_copy_paths)))
stopifnot(identical(cli_main(c(
    'def', 'rm', 'test-project', '--dataset', 'validation-copy-*'
)), 0L))
stopifnot(!any(file.exists(validation_copy_paths)))
if (is.na(old_source_visual)) {
    Sys.unsetenv('VISUAL')
} else {
    Sys.setenv(VISUAL=old_source_visual)
}

listed <- cdrgam_cli_list('test-project')$`test-project`
stopifnot(
    identical(listed$datasets, c('training', 'validation')),
    setequal(listed$models, c('decay', 'decay-two', 'location-scale')),
    identical(listed$visualizations, 'diagnostics'),
    identical(listed$comparisons, 'alternatives')
)

list_definitions <- getFromNamespace(
    '.cdrgam_cli_list_definitions', 'cdrgam.cli'
)
project_list_output <- capture.output(stopifnot(identical(
    cli_main(c('def', 'ls')), 0L
)))
stopifnot(
    any(grepl('test-project', project_list_output, fixed=TRUE)),
    !any(grepl('training', project_list_output, fixed=TRUE))
)
all_definitions_output <- capture.output(
    all_definitions <- list_definitions('test-project', checkout=checkout)
)
selected_definitions_output <- capture.output(
    selected_definitions <- list_definitions(
        'test-project',
        list(dataset='train*', model=c('decay', 'location-*')),
        checkout=checkout
    )
)
stopifnot(
    identical(
        all_definitions$type,
        c(
            'project', 'dataset', 'dataset', 'model', 'model', 'model',
            'visualization', 'comparison'
        )
    ),
    identical(
        paste(selected_definitions$type, selected_definitions$name),
        c('dataset training', 'model decay', 'model location-scale')
    ),
    any(grepl('definitions/models/decay.yml', selected_definitions_output,
        fixed=TRUE)),
    length(all_definitions_output) > 1L,
    identical(cli_main(c(
        'def', 'ls', 'test-project', '--dataset', 'train*',
        '--model', 'decay', 'location-*'
    )), 0L)
)
models_only_output <- capture.output(stopifnot(identical(
    cli_main(c('def', 'ls', 'test-project', '--model')), 0L
)))
stopifnot(
    any(grepl('decay', models_only_output, fixed=TRUE)),
    any(grepl('location-scale', models_only_output, fixed=TRUE)),
    !any(grepl('training', models_only_output, fixed=TRUE))
)
old_del_error <- tryCatch({
    cli_main(c('def', 'del', 'test-project', '--model', 'decay'))
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(grepl(
    'Unknown command: cdrgam def del', old_del_error, fixed=TRUE
))

stopifnot(identical(cli_main(c('def', 'val', 'site')), 0L))
stopifnot(identical(cli_main(c(
    'def', 'val', 'test-project', '--model', 'decay', '--deep'
)), 0L))
broken_definition_path <- file.path(
    project, 'definitions', 'models',
    paste0(c('externally-broken-a', 'externally-broken-b'), '.yml')
)
for (index in seq_along(broken_definition_path)) yaml::write_yaml(
    list(schema=1L),
    broken_definition_path[[index]]
)
stopifnot(identical(cli_main(c(
    'def', 'val', 'test-project', '--dataset', 'training', 'validation'
)), 0L))
broken_definitions_error <- tryCatch({
    cli_main(c(
        'def', 'val', 'test-project', '--model', 'externally-*'
    ))
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(all(vapply(
    c('externally-broken-a', 'externally-broken-b'),
    grepl, logical(1), x=broken_definitions_error, fixed=TRUE
)))
broken_project_error <- tryCatch({
    cli_main(c('def', 'val', 'test-project'))
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(grepl(
    'missing required fields', broken_project_error, fixed=TRUE
))
unlink(broken_definition_path)

validation <- cdrgam_cli_validate('test-project', deep=TRUE)$`test-project`
stopifnot(
    validation$valid, validation$datasets == 2L, validation$models == 3L,
    validation$visualizations == 1L, validation$comparisons == 1L
)
training_definition_path <- file.path(
    project, 'definitions', 'datasets', 'training.yml'
)
relative_training_definition <- yaml::read_yaml(training_definition_path)
absolute_training_definition <- relative_training_definition
absolute_training_definition$sources$impulses$path <- file.path(
    project, 'data', 'impulses.rds'
)
yaml::write_yaml(absolute_training_definition, training_definition_path)
internal_absolute_error <- tryCatch({
    cdrgam_cli_validate('test-project', deep=FALSE)
    NA_character_
}, error=function(error) conditionMessage(error))
yaml::write_yaml(relative_training_definition, training_definition_path)
stopifnot(grepl(
    'must be relative to the project', internal_absolute_error, fixed=TRUE
))
loaded_definition <- getFromNamespace(
    '.cdrgam_cli_read_definitions', 'cdrgam.cli'
)('test-project')$datasets$training
loaded_definitions <- getFromNamespace(
    '.cdrgam_cli_read_definitions', 'cdrgam.cli'
)('test-project')
stopifnot(
    identical(loaded_definitions$datasets$training$dataset, 'training'),
    identical(loaded_definitions$models$decay$model, 'decay'),
    identical(
        loaded_definitions$visualizations$diagnostics$visualization,
        'diagnostics'
    ),
    identical(
        loaded_definitions$comparisons$alternatives$comparison,
        'alternatives'
    ),
    !('dataset' %in% names(yaml::read_yaml(training_definition_path))),
    !('model' %in% names(yaml::read_yaml(model_path)))
)
loaded_data <- getFromNamespace(
    '.cdrgam_cli_load_dataset', 'cdrgam.cli'
)(loaded_definition)
stopifnot(
    is.factor(loaded_data$impulses$item_id),
    is.factor(loaded_data$responses$item_id),
    nlevels(loaded_data$responses$item_id) == nrow(unique(
        loaded_data$responses[c('document', 'sentence', 'position')]
    ))
)

fit_plan <- cdrgam_cli_plan(projects='test-project', models='decay')
stopifnot(
    nrow(fit_plan) == 1L, fit_plan$kind == 'fit', fit_plan$state == 'missing',
    fit_plan$path == file.path(project, 'results', 'models', 'decay')
)
regex_fit_plan <- cdrgam_cli_plan(
    projects='re:^test-project$', models='re:^decay(-two)?$'
)
stopifnot(
    nrow(regex_fit_plan) == 2L,
    identical(regex_fit_plan$kind, c('fit', 'fit')),
    setequal(regex_fit_plan$name, c('decay', 'decay-two'))
)
fit_identity <- fit_plan$identity
cdrgam_cli_run(projects='test-project', models='decay')
fit_manifest <- yaml::read_yaml(file.path(project, 'results', 'models', 'decay', 'manifest.yml'))
work_root <- file.path(project, '.cdrgam', 'work')
coefficient_table <- utils::read.csv(
    file.path(project, 'results', 'models', 'decay', 'coefficients.csv'),
    check.names=FALSE
)
smooth_table <- utils::read.csv(
    file.path(project, 'results', 'models', 'decay', 'smooths.csv'),
    check.names=FALSE
)
stopifnot(
    fit_manifest$identity == fit_identity,
    identical(fit_manifest$contract$kind, 'fit'),
    is.null(fit_manifest$contract$schema),
    is.null(fit_manifest$contract$definition$schema),
    is.null(fit_manifest$contract$inputs$training_data$definition$schema),
    is.null(fit_manifest$contract$cdrgam_implementation),
    is.null(fit_manifest$contract$inputs$training_data$cdrgam_implementation),
    !is.null(fit_manifest$execution$cdrgam$code_md5),
    !is.null(fit_manifest$execution$cdrgam_cli$code_md5),
    identical(
        unlist(fit_manifest$configuration$effective$preparation$window),
        c(0, 1.5)
    ),
    fit_manifest$configuration$effective$preparation$history == 'auto',
    isTRUE(fit_manifest$configuration$effective$preparation$drop.unused.levels),
    fit_manifest$configuration$effective$fitting$backend == 'mgcv',
    is.character(fit_manifest$result$formulas$user),
    grepl('response ~', fit_manifest$result$formulas$user, fixed=TRUE),
    file.exists(file.path(project, 'results', 'models', 'decay', 'booklet.pdf')),
    identical(fit_manifest$result$booklet$scope, 'population'),
    identical(names(coefficient_table),
        c('term', 'Estimate', 'Std. Error', 't value', 'Pr(>|t|)')),
    nrow(coefficient_table) < length(stats::coef(readRDS(
        file.path(project, 'results', 'models', 'decay', 'fit.rds')
    ))),
    identical(
        names(smooth_table),
        c('term', 'edf', 'Ref.df', 'F', 'p-value', 'test_status')
    ),
    all(nzchar(smooth_table$test_status)),
    dir.exists(file.path(work_root, 'fit_decay', fit_identity)),
    !dir.exists(file.path(root, '.cdrgam', 'works', 'test-project')),
    !dir.exists(file.path(project, 'results', 'models', 'decay', 'predictions'))
)

autosimplify_item <- internal('.cdrgam_cli_fit_item')(
    loaded_definitions, 'decay'
)
autosimplify_item$model$autosimplify <- internal(
    '.cdrgam_cli_validate_autosimplify'
)(list(max_steps=2L, conservatism=1), 'test autosimplify')
autosimplify_stage <- file.path(temporary_parent, 'autosimplify-fit')
dir.create(autosimplify_stage)
autosimplify_result <- internal('.cdrgam_cli_execute_fit')(
    autosimplify_item, autosimplify_stage
)
stopifnot(
    isTRUE(autosimplify_result$diagnostics$converged),
    identical(autosimplify_result$derivation$steps, 1L),
    file.exists(file.path(autosimplify_stage, 'fit.rds')),
    file.exists(file.path(autosimplify_stage, 'derived-config.yml')),
    file.exists(file.path(
        autosimplify_stage, '.cdrgam', 'autosimplify', 'chain.yml'
    )),
    internal('.cdrgam_cli_valid_derivation')(
        autosimplify_stage, autosimplify_result$derivation
    )
)
autosimplify_derived_path <- file.path(
    autosimplify_stage, 'derived-config.yml'
)
autosimplify_derived <- yaml::read_yaml(autosimplify_derived_path)
autosimplify_tampered <- autosimplify_derived
autosimplify_tampered$formula <- 'response ~ 1'
yaml::write_yaml(autosimplify_tampered, autosimplify_derived_path)
stopifnot(!internal('.cdrgam_cli_valid_derivation')(
    autosimplify_stage, autosimplify_result$derivation
))
yaml::write_yaml(autosimplify_derived, autosimplify_derived_path)
stopifnot(internal('.cdrgam_cli_valid_derivation')(
    autosimplify_stage, autosimplify_result$derivation
))

prediction_plan <- cdrgam_cli_plan(
    projects='test-project', models='decay', predictions='val'
)
stopifnot(
    nrow(prediction_plan) == 2L,
    identical(prediction_plan$kind, c('fit', 'prediction')),
    prediction_plan$target[prediction_plan$kind == 'prediction'],
    endsWith(
        prediction_plan$path[prediction_plan$kind == 'prediction'],
        'models/decay/predictions/validation'
    )
)
cdrgam_cli_run(projects='test-project', models='decay', predictions='val')
prediction_path <- file.path(project, 'results', 'models', 'decay', 'predictions', 'validation')
predictions <- utils::read.csv(file.path(prediction_path, 'predictions.csv'))
expected_responses <- simulation$responses[
    simulation$responses$row_id > 60L & simulation$responses$row_id <= 90L,
    , drop=FALSE
]
stopifnot(
    nrow(predictions) == nrow(expected_responses),
    identical(predictions$row_id, expected_responses$row_id),
    all(is.finite(predictions$prediction)),
    all(is.finite(predictions$link_prediction)),
    all(is.finite(predictions$link_prediction_se)),
    max(abs(predictions$prediction - predictions$link_prediction)) < 1e-10,
    predictions$source_row[predictions$row_id == 75L] > 0L
)

cdrgam_cli_run(
    projects='test-project', models='location-scale', predictions='val'
)
distributional_path <- file.path(
    project, 'results', 'models', 'location-scale', 'predictions', 'validation'
)
distributional_predictions <- utils::read.csv(
    file.path(distributional_path, 'predictions.csv'),
    check.names=FALSE
)
distributional_manifest <- yaml::read_yaml(file.path(
    project, 'results', 'models', 'location-scale', 'manifest.yml'
))
stopifnot(
    identical(
        names(distributional_manifest$result$formulas$user),
        c('location', 'scale')
    ),
    all(c(
        'location_prediction', 'location_prediction_se',
        'location_link_prediction', 'location_link_prediction_se',
        'scale_prediction', 'scale_prediction_se',
        'scale_link_prediction', 'scale_link_prediction_se',
        'standard_deviation_prediction',
        'standard_deviation_prediction_se'
    ) %in% names(distributional_predictions)),
    all(is.finite(distributional_predictions$location_prediction)),
    all(distributional_predictions$scale_prediction > 0),
    max(abs(
        distributional_predictions$standard_deviation_prediction -
            1 / distributional_predictions$scale_prediction
    )) < 1e-10,
    max(abs(
        distributional_predictions$prediction -
            distributional_predictions$location_prediction
    )) < 1e-10
)

fallback <- cdrgam_cli_plan(
    projects='test-project', models='decay', predictions='validation'
)
stopifnot(
    fallback$identity[fallback$kind == 'prediction'] ==
        prediction_plan$identity[prediction_plan$kind == 'prediction']
)

visualization_plan <- cdrgam_cli_plan(
    projects='test-project', models='decay', visualizations='diagnostics'
)
stopifnot(
    nrow(visualization_plan) == 2L,
    sum(visualization_plan$target) == 1L,
    visualization_plan$kind[visualization_plan$target] == 'visualization'
)
cdrgam_cli_run(
    projects='test-project', models='decay', visualizations='diagnostics'
)
stopifnot(file.exists(file.path(
    project, 'results', 'models', 'decay', 'visualizations',
    'diagnostics', 'booklet.pdf'
)))

effect_definition <- list(
    schema=1L, model='decay',
    query=list(
        terms=list(predictors='*'), composition='total',
        axes=list(
            lag=list(grid='fitted', n=31L),
            predictors=list(
                '*'=list(at=list(summary='mean', 'offset-sd'=1))
            )
        ),
        uncertainty=list(level=0.9, kind='pointwise')
    ),
    render=list(
        geometry='line',
        mappings=list(x='lag', y='estimate', color='term'),
        interval='ribbon', theme='paper', formats='pdf'
    )
)
effect_definition_path <- file.path(
    project, 'definitions', 'visualizations', 'effects.yml'
)
yaml::write_yaml(effect_definition, effect_definition_path)
effect_plan <- cdrgam_cli_plan(
    projects='test-project', visualizations='effects'
)
stopifnot(
    identical(effect_plan$kind, c('fit', 'effect', 'visualization')),
    effect_plan$target[effect_plan$kind == 'visualization']
)
cdrgam_cli_run(projects='test-project', visualizations='effects')
effect_artifact <- effect_plan$path[effect_plan$kind == 'effect']
visualization_artifact <- file.path(
    project, 'results', 'models', 'decay', 'visualizations', 'effects'
)
effect_grid <- readRDS(file.path(effect_artifact, 'effect-grid.rds'))
stopifnot(
    nrow(effect_grid) == 31L,
    all(c('lag', 'x', 'estimate', 'se', 'lower', 'upper') %in% names(effect_grid)),
    file.exists(file.path(effect_artifact, 'effect-grid.csv')),
    file.exists(file.path(visualization_artifact, 'visualization.pdf')),
    file.exists(file.path(visualization_artifact, 'plot.rds'))
)

# Presentation changes create a new visualization while retaining the same
# deduplicated statistical effect-grid dependency.
alternate_effect <- effect_definition
alternate_effect$render$theme <- 'minimal'
alternate_effect_path <- file.path(
    project, 'definitions', 'visualizations', 'effects-minimal.yml'
)
yaml::write_yaml(alternate_effect, alternate_effect_path)
shared_graph <- getFromNamespace(
    '.cdrgam_cli_combined_graph', 'cdrgam.cli'
)(projects='test-project', visualizations=c('effects', 'effects-minimal'))
stopifnot(
    sum(vapply(shared_graph$items, `[[`, character(1), 'kind') == 'effect') == 1L,
    sum(vapply(shared_graph$items, `[[`, character(1), 'kind') == 'visualization') == 2L
)
unlink(alternate_effect_path)

surface_definition <- effect_definition
surface_definition$query$axes$lag$n <- 17L
surface_definition$query$axes$predictors <- list(
    x=list(quantiles=c(0.1, 0.3, 0.5, 0.7, 0.9))
)
surface_definition$render <- list(
    geometry='raster-contour',
    mappings=list(x='lag', y='predictor:x', fill='estimate'),
    interval='companion', theme='minimal', formats='pdf'
)
yaml::write_yaml(surface_definition, file.path(
    project, 'definitions', 'visualizations', 'effect-surface.yml'
))
cdrgam_cli_run(projects='test-project', visualizations='effect-surface')
surface_artifact <- file.path(
    project, 'results', 'models', 'decay', 'visualizations', 'effect-surface'
)
stopifnot(
    file.exists(file.path(surface_artifact, 'visualization.pdf')),
    file.exists(file.path(
        surface_artifact, 'visualization-uncertainty.pdf'
    ))
)

comparison_plan <- cdrgam_cli_plan(
    projects='test-project', comparisons='alternatives'
)
stopifnot(
    comparison_plan$kind[nrow(comparison_plan)] == 'comparison',
    sum(comparison_plan$kind == 'prediction') == 2L,
    sum(comparison_plan$kind == 'fit') == 2L
)
cdrgam_cli_run(projects='test-project', comparisons='alternatives')
comparison_metrics <- utils::read.csv(file.path(
    project, 'results', 'comparisons', 'alternatives', 'metrics.csv'
))
stopifnot(
    nrow(comparison_metrics) == 2L,
    setequal(comparison_metrics$model, c('decay', 'decay-two')),
    all(c('mse', 'mae') %in% names(comparison_metrics)),
    all(is.finite(comparison_metrics$mse)),
    all(is.finite(comparison_metrics$mae))
)

# Publication archives contain only verified, dependency-closed artifacts.
# Fetch installs them atomically into a second root and reconstructs status
# from their manifests rather than copying the source checkout's registry.
invisible(internal('.cdrgam_cli_git')(
    project, c('config', 'user.name', 'CDR-GAM test')
))
invisible(internal('.cdrgam_cli_git')(
    project, c('config', 'user.email', 'cdrgam-test@example.invalid')
))
invisible(internal('.cdrgam_cli_git')(
    project, c('add', '--all', '--', '.')
))
archive_path <- file.path(temporary_parent, 'test-project-results.tar.gz')
publication <- cdrgam_cli_publish(
    'test-project', results='archive', archive=archive_path,
    models='decay', predictions='val', commit='Test publication',
    checkout=checkout
)
stopifnot(
    identical(publication$results$mode, 'archive'),
    grepl('^[0-9a-f]{64}$', publication$results$sha256),
    file.exists(archive_path),
    identical(
        internal('.cdrgam_cli_sha256')(archive_path),
        publication$results$sha256
    ),
    !internal('.cdrgam_cli_git_dirty')(project)
)
published_checkout <- file.path(temporary_parent, 'published-checkout')
published_root <- file.path(temporary_parent, 'published-root')
dir.create(published_checkout)
cdrgam_cli_configure(published_checkout, published_root, concurrency=1L)
fetched <- cdrgam_cli_fetch(
    project, project='fetched-project', results=archive_path,
    checkout=published_checkout
)
fetched_plan <- cdrgam_cli_plan(
    projects='fetched-project', models='decay',
    checkout=published_checkout
)
fetched_status <- cdrgam_cli_status(
    'fetched-project', checkout=published_checkout, use_pager=FALSE
)
stopifnot(
    same_path(fetched, file.path(
        published_root, 'projects', 'fetched-project'
    )),
    all(fetched_plan$state == 'complete'),
    any(
        fetched_status$kind == 'fit' & fetched_status$state == 'complete'
    )
)
options(cdrgam.cli.checkout=checkout)

status_text <- NULL
status <- cdrgam_cli_status(
    'test-project', pager=function(text) status_text <<- text
)
registry_indexes <- internal('.cdrgam_cli_registry_exec')(
    test_configuration, 'PRAGMA index_list(attempts)', query=TRUE
)$name
stopifnot(
    any(status$kind == 'fit' & status$name == 'decay' & status$state == 'complete'),
    any(status$kind == 'prediction' & status$state == 'complete'),
    'attempts_by_work_key' %in% registry_indexes,
    grepl('PROJECT', status_text, fixed=TRUE),
    grepl('Success', status_text, fixed=TRUE),
    grepl('Summary:', status_text, fixed=TRUE)
)
status_model_source <- readLines(model_path, warn=FALSE)
writeLines('invalid: [', model_path)
marker_only_status <- cdrgam_cli_status(
    'test-project', pager=function(text) invisible(text)
)
writeLines(status_model_source, model_path, useBytes=TRUE)
stopifnot(
    nrow(marker_only_status) == nrow(status),
    identical(marker_only_status$work_key, status$work_key)
)
fit_manifest_path <- file.path(project, 'results', 'models', 'decay', 'manifest.yml')
fit_manifest <- yaml::read_yaml(fit_manifest_path)
converged_manifest <- fit_manifest
fit_manifest$result$diagnostics$converged <- FALSE
fit_manifest$result$diagnostics$message <- 'test convergence diagnostic'
yaml::write_yaml(fit_manifest, fit_manifest_path)
stopifnot(!internal('.cdrgam_cli_complete_artifact')(
    dirname(fit_manifest_path), fit_manifest$identity
))
checkpoint_value <- list(stage='optimization', marker='resume-me')
checkpoint_path <- file.path(dirname(fit_manifest_path), 'optimizer-checkpoint.rds')
saveRDS(checkpoint_value, checkpoint_path)
seed_graph <- internal('.cdrgam_cli_combined_graph')(
    projects='test-project', models='decay'
)
seed_item <- seed_graph$items[[1L]]
seeded_attempt <- internal('.cdrgam_cli_attempt_directory')(
    seed_graph$definitions[['test-project']], seed_item
)$path
stopifnot(identical(
    readRDS(file.path(seeded_attempt, 'optimizer-checkpoint.rds')),
    checkpoint_value
))
unlink(seeded_attempt, recursive=TRUE)
unlink(checkpoint_path)
internal('.cdrgam_cli_registry_state')(
    seed_graph$definitions[['test-project']]$checkout, seed_item, 'failed'
)
nonconverged_text <- NULL
nonconverged_status <- cdrgam_cli_status(
    'test-project', pager=function(text) nonconverged_text <<- text
)
stopifnot(
    nonconverged_status$display_state[
        nonconverged_status$kind == 'fit' & nonconverged_status$name == 'decay'
    ] == 'Nonconverged',
    grepl('Nonconverged fits', nonconverged_text, fixed=TRUE),
    grepl('test convergence diagnostic', nonconverged_text, fixed=TRUE)
)
internal('.cdrgam_cli_registry_state')(
    seed_graph$definitions[['test-project']]$checkout, seed_item, 'running'
)
retry_status <- cdrgam_cli_status(
    'test-project', pager=function(text) invisible(text)
)
stopifnot(
    retry_status$display_state[
        retry_status$kind == 'fit' & retry_status$name == 'decay'
    ] == 'Running'
)
yaml::write_yaml(converged_manifest, fit_manifest_path)
lightweight_directory <- file.path(root, '.cdrgam', 'lightweight-artifact-test')
dir.create(lightweight_directory, recursive=TRUE)
writeLines('present but changed', file.path(lightweight_directory, 'output.txt'))
yaml::write_yaml(list(
    status='complete', kind='comparison', identity='lightweight-test',
    outputs=list(list(path='output.txt', md5='deliberately-wrong'))
), file.path(lightweight_directory, 'manifest.yml'))
stopifnot(
    !internal('.cdrgam_cli_complete_artifact')(
        lightweight_directory, 'lightweight-test'
    ),
    internal('.cdrgam_cli_complete_artifact')(
        lightweight_directory, 'lightweight-test', verify_hashes=FALSE
    )
)
unlink(lightweight_directory, recursive=TRUE)
internal('.cdrgam_cli_registry_state')(
    seed_graph$definitions[['test-project']]$checkout, seed_item, 'complete'
)
log_paths <- cdrgam_cli_log('test-project', models='decay', lines=5L)
stopifnot(length(log_paths) >= 3L, all(file.exists(log_paths)))
regex_log_paths <- cdrgam_cli_log(
    'test-project', models='re:^decay$', lines=5L
)
stopifnot(setequal(regex_log_paths, log_paths))
current_definitions <- internal('.cdrgam_cli_read_definitions')(
    'test-project', check_sources=FALSE, checkout=checkout
)
fit_log <- internal('.cdrgam_cli_work_log_path')(
    current_definitions, 'fit', 'decay'
)
fit_log_text <- paste(readLines(fit_log, warn=FALSE), collapse='\n')
stopifnot(
    fit_log %in% log_paths,
    grepl(' START fit decay ', fit_log_text, fixed=TRUE),
    grepl(' COMPLETE fit decay', fit_log_text, fixed=TRUE),
    !file.exists(file.path(project, 'results', 'models', 'decay', 'run.log'))
)
overwrite_item <- list(
    kind='fit', name='overwrite-test', identity='new-identity'
)
overwrite_log <- internal('.cdrgam_cli_work_log_path')(
    current_definitions, overwrite_item$kind, overwrite_item$name
)
dir.create(dirname(overwrite_log), recursive=TRUE, showWarnings=FALSE)
writeLines('stale attempt output', overwrite_log)
internal('.cdrgam_cli_start_work_log')(
    current_definitions, overwrite_item, '2000-01-01T00:00:00Z'
)
stopifnot(identical(
    readLines(overwrite_log, warn=FALSE),
    '2000-01-01T00:00:00Z START fit overwrite-test [new-identity]'
))
unlink(overwrite_log)
seen_logs <- NULL
paged_paths <- cdrgam_cli_log(
    'test-project', models='decay', predictions='val',
    pager=function(paths, labels) seen_logs <<- list(paths=paths, labels=labels)
)
stopifnot(
    identical(paged_paths, seen_logs$paths), length(paged_paths) == 1L,
    grepl('/prediction/', seen_logs$labels, fixed=TRUE)
)

launcher <- file.path(temporary_parent, 'bin', 'cdrgam')
install_cli(launcher, checkout=checkout)
stopifnot(
    file.exists(launcher), file.access(launcher, mode=1L) == 0L,
    any(grepl('CDRGAM_CHECKOUT', readLines(launcher), fixed=TRUE)),
    any(grepl('R_LIBS_USER', readLines(launcher), fixed=TRUE))
)

protected_code <- file.path(project, 'code', 'protected.R')
writeLines('project source', protected_code)
project_preview <- cdrgam_cli_purge(projects='test-project', yes=FALSE)
stopifnot(
    !(protected_code %in% project_preview),
    !any(startsWith(project_preview, file.path(project, 'code'))),
    file.exists(protected_code)
)
unlink(protected_code)

preview <- cdrgam_cli_purge(
    projects='test-project', models='decay', yes=FALSE
)
stopifnot(same_path(preview, file.path(project, 'results', 'models', 'decay')))
prediction_preview <- cdrgam_cli_purge(
    projects='test-project', models='decay', predictions='val', yes=FALSE
)
stopifnot(same_path(
    prediction_preview,
    file.path(project, 'results', 'models', 'decay', 'predictions', 'validation')
))
visualization_preview <- cdrgam_cli_purge(
    projects='test-project', visualizations='diagnostics', yes=FALSE
)
stopifnot(same_path(
    visualization_preview,
    file.path(project, 'results', 'models', 'decay', 'visualizations', 'diagnostics')
))
comparison_preview <- cdrgam_cli_purge(
    projects='test-project', comparisons='alternatives', yes=FALSE
)
stopifnot(same_path(
    comparison_preview,
    file.path(project, 'results', 'comparisons', 'alternatives')
))
work_preview <- cdrgam_cli_purge(
    projects='test-project', work=TRUE, yes=FALSE
)
stopifnot(
    work_root %in% work_preview,
    any(startsWith(work_preview, file.path(project, 'results')))
)

model_definition$formula <- paste0(model_definition$formula, ' + 0')
yaml::write_yaml(
    model_definition, file.path(project, 'definitions', 'models', 'decay.yml')
)
changed <- cdrgam_cli_plan(projects='test-project', models='decay')
stopifnot(changed$identity != fit_identity, changed$state == 'stale')
changed_graph <- getFromNamespace(
    '.cdrgam_cli_combined_graph', 'cdrgam.cli'
)(projects='test-project', models='decay')
configuration <- getFromNamespace(
    '.cdrgam_cli_checkout', 'cdrgam.cli'
)(checkout)
getFromNamespace('.cdrgam_cli_registry_record_graph', 'cdrgam.cli')(
    configuration, changed_graph, 'lifecycle-test'
)
registry_rows <- getFromNamespace(
    '.cdrgam_cli_registry_exec', 'cdrgam.cli'
)(configuration, paste(
    "SELECT work_key,project,kind,name FROM work_items",
    "WHERE project='test-project' AND kind='fit' AND name='decay'"
), query=TRUE)
stopifnot(
    nrow(registry_rows) == 1L,
    identical(registry_rows$work_key, names(changed_graph$items))
)
changed_item <- changed_graph$items[[1L]]
attempt_parent <- file.path(
    work_root, 'fit_decay', changed_item$identity
)
first_attempt <- file.path(attempt_parent, 'first')
second_attempt <- file.path(attempt_parent, 'second')
dir.create(first_attempt, recursive=TRUE)
yaml::write_yaml(list(
    schema=1L, status='failed', project='test-project', kind='fit',
    name='decay', identity=changed_item$identity
), file.path(first_attempt, 'attempt.yml'))
writeLines('superseded log', file.path(first_attempt, 'run.log'))
getFromNamespace('.cdrgam_cli_registry_attempt', 'cdrgam.cli')(
    configuration, changed_item,
    list(
        status='failed', job_id=NULL, path=first_attempt,
        definitions=changed_graph$definitions[['test-project']]
    )
)
dir.create(second_attempt)
yaml::write_yaml(list(
    schema=1L, status='failed', project='test-project', kind='fit',
    name='decay', identity=changed_item$identity
), file.path(second_attempt, 'attempt.yml'))
getFromNamespace('.cdrgam_cli_registry_attempt', 'cdrgam.cli')(
    configuration, changed_item,
    list(
        status='failed', job_id=NULL, path=second_attempt,
        definitions=changed_graph$definitions[['test-project']]
    )
)
attempt_rows <- getFromNamespace(
    '.cdrgam_cli_registry_exec', 'cdrgam.cli'
)(configuration, paste0(
    'SELECT attempt_id,path FROM attempts WHERE work_key=',
    getFromNamespace('.cdrgam_cli_sql_quote', 'cdrgam.cli')(changed_item$key)
), query=TRUE)
stopifnot(
    !dir.exists(first_attempt), dir.exists(second_attempt),
    nrow(attempt_rows) == 1L,
    identical(
        attempt_rows$path,
        getFromNamespace('.cdrgam_cli_project_relative', 'cdrgam.cli')(
            changed_graph$definitions[['test-project']], second_attempt
        )
    )
)

# Exercise the Slurm-hosted scheduler and its generic worker pool with local
# scheduler shims. The fit is reusable, so a worker claims only its prediction.
probe_socket <- tryCatch(serverSocket(sample.int(10000L, 1L) + 39999L),
    error=function(error) NULL)
can_bind_controller <- !is.null(probe_socket)
if (can_bind_controller) close(probe_socket)
if (can_bind_controller) {
restored_model <- yaml::read_yaml(
    file.path(project, 'definitions', 'models', 'decay-two.yml')
)
yaml::write_yaml(restored_model, file.path(project, 'definitions', 'models', 'decay.yml'))
cdrgam_cli_run(projects='test-project', models='decay')
unlink(prediction_path, recursive=TRUE)
fake_bin <- file.path(temporary_parent, 'fake-bin')
dir.create(fake_bin)
fake_worker_log <- file.path(temporary_parent, 'fake-worker.log')
fake_submission_log <- file.path(temporary_parent, 'fake-submissions.log')
fake_exit_log <- file.path(temporary_parent, 'fake-exits.log')
fail_workers <- file.path(temporary_parent, 'fail-workers')
slow_squeue <- file.path(temporary_parent, 'slow-squeue')
writeLines(c(
    '#!/bin/sh',
    'script=',
    'for argument in "$@"; do script=$argument; done',
    paste('grep "^#SBATCH --job-name=" "$script" >>', shQuote(fake_submission_log)),
    paste0(
        'if grep -q "^#SBATCH --job-name=cdrgam-worker$" "$script" && [ -f ',
        shQuote(fail_workers), ' ]; then ( exit 0 ) & printf "%s\\n" "$!"; exit 0; fi'
    ),
    paste0(
        '( export SLURM_JOB_ID=$$ CDRGAM_CONTROLLER_HOST=127.0.0.1; ',
        'sh "$script"; status=$?; ',
        'if grep -q "^#SBATCH --job-name=cdrgam-worker$" "$script"; then ',
        'printf "%s\\n" "$status" >>', shQuote(fake_exit_log),
        '; fi; exit "$status" ) >>', shQuote(fake_worker_log), ' 2>&1 &'
    ),
    'printf "%s\\n" "$!"'
), file.path(fake_bin, 'sbatch'))
writeLines(c(
    '#!/bin/sh',
    'job=',
    'while [ "$#" -gt 0 ]; do',
    '  if [ "$1" = "-j" ]; then shift; job=$1; fi',
    '  shift',
    'done',
    paste0('if [ -f ', shQuote(slow_squeue), ' ]; then sleep 20; fi'),
    'old_ifs=$IFS',
    'IFS=,',
    'for candidate in $job; do',
    '  if kill -0 "$candidate" 2>/dev/null; then printf "%s\\n" "$candidate"; fi',
    'done',
    'IFS=$old_ifs'
), file.path(fake_bin, 'squeue'))
Sys.chmod(file.path(fake_bin, c('sbatch', 'squeue')), mode='0755')
old_path <- Sys.getenv('PATH')
Sys.setenv(PATH=paste(fake_bin, old_path, sep=.Platform$path.sep))
on.exit(Sys.setenv(PATH=old_path), add=TRUE)
cdrgam_cli_configure(
    checkout, root, concurrency=2L,
    slurm_partition='test', slurm_account='test', slurm_cpus=1L
)
writeLines('', slow_squeue)
accepted <- cdrgam_cli_run(
    projects='test-project', models='decay', predictions='val'
)
stopifnot(identical(accepted$status, 'accepted'))
for (attempt in seq_len(240L)) {
    if (file.exists(file.path(prediction_path, 'manifest.yml'))) break
    Sys.sleep(0.25)
}
if (!file.exists(file.path(prediction_path, 'manifest.yml'))) {
    stop(paste(
        'Fake Slurm worker did not publish:',
        paste(readLines(fake_worker_log, warn=FALSE), collapse='\n')
    ))
}
controller_path <- file.path(root, '.cdrgam', 'controller.yml')
for (attempt in seq_len(240L)) {
    if (!file.exists(controller_path)) break
    Sys.sleep(0.25)
}
stopifnot(!file.exists(controller_path))
unlink(slow_squeue)
for (attempt in seq_len(40L)) {
    if (file.exists(fake_exit_log)) break
    Sys.sleep(0.1)
}
slurm_status <- cdrgam_cli_status('test-project')
stopifnot(any(
    slurm_status$kind == 'prediction' & slurm_status$state == 'complete'
))
submissions <- readLines(fake_submission_log, warn=FALSE)
stopifnot(
    any(submissions == '#SBATCH --job-name=cdrgam-scheduler'),
    any(submissions == '#SBATCH --job-name=cdrgam-worker'),
    !any(grepl('decay|prediction|test-project', submissions)),
    identical(readLines(fake_exit_log, warn=FALSE), '0')
)
worker_rows <- internal('.cdrgam_cli_registry_exec')(
    configuration, paste(
        'SELECT worker_id,scheduler_id,state FROM workers',
        'ORDER BY updated_at DESC LIMIT 1'
    ), query=TRUE
)
stopifnot(nrow(worker_rows) == 1L)
worker_log <- internal('.cdrgam_cli_worker_log_path')(
    configuration, worker_rows$worker_id[[1L]]
)
dir.create(dirname(worker_log), recursive=TRUE, showWarnings=FALSE)
writeLines(c(
    paste('2000-01-01T00:00:00Z WORKER', worker_rows$worker_id[[1L]], 'START'),
    paste('2000-01-01T00:00:01Z WORKER', worker_rows$worker_id[[1L]],
          'CLAIM prediction decay_validation'),
    paste('2000-01-01T00:00:02Z WORKER', worker_rows$worker_id[[1L]],
          'COMPLETE prediction decay_validation')
), worker_log)
seen_worker_logs <- NULL
worker_paths <- cdrgam_cli_log(
    worker=TRUE, checkout=checkout,
    pager=function(paths, labels) {
        seen_worker_logs <<- list(paths=paths, labels=labels)
    }
)
stopifnot(
    worker_log %in% worker_paths,
    any(grepl('worker/', seen_worker_logs$labels, fixed=TRUE))
)
worker_selector_error <- tryCatch(
    cdrgam_cli_log(
        projects='test-project', worker=TRUE, checkout=checkout,
        pager=function(paths, labels) invisible(paths)
    ),
    error=function(error) conditionMessage(error)
)
stopifnot(grepl('cannot be combined', worker_selector_error, fixed=TRUE))
available_worker_keys <- getFromNamespace(
    '.cdrgam_cli_available_worker_resource_keys', 'cdrgam.cli'
)(list(
    list(resource_key='shared', status='running', current_work_key='fit:a'),
    list(resource_key='shared', status='idle', current_work_key=NULL),
    list(resource_key='other', status='stopping', current_work_key=NULL)
))
stopifnot(identical(available_worker_keys, 'shared'))
} else {
    message('Skipping TCP controller test because local socket binding is unavailable')
}

# Managed references remain valid after moving the complete store and updating
# only the checkout's root pointer.
controller_path <- file.path(root, '.cdrgam', 'controller.yml')
for (attempt in seq_len(240L)) {
    if (!file.exists(controller_path)) break
    Sys.sleep(0.25)
}
stopifnot(!file.exists(controller_path))
relocation_parent <- file.path(temporary_parent, 'relocated')
dir.create(relocation_parent)
moved_root <- file.path(relocation_parent, basename(root))
stopifnot(file.rename(root, moved_root))
cdrgam_cli_configure(checkout, moved_root, concurrency=2L)
moved_configuration <- getFromNamespace(
    '.cdrgam_cli_checkout', 'cdrgam.cli'
)(checkout)
registry_paths <- getFromNamespace(
    '.cdrgam_cli_registry_exec', 'cdrgam.cli'
)(moved_configuration, paste(
    'SELECT artifact_path AS path FROM work_items',
    'UNION ALL SELECT path FROM attempts',
    'UNION ALL SELECT path FROM workers'
), query=TRUE)$path
if (!length(registry_paths)) stop(paste0(
    'Relocated registry was empty at ',
    getFromNamespace('.cdrgam_cli_registry_path', 'cdrgam.cli')(moved_configuration),
    '; files: ', paste(list.files(
        file.path(moved_root, '.cdrgam'), all.files=TRUE
    ), collapse=', ')
))
stopifnot(
    all(!grepl('^/', registry_paths)),
    all(!startsWith(registry_paths, root))
)
moved_status <- cdrgam_cli_status(
    'test-project', checkout=checkout, use_pager=FALSE
)
stopifnot(
    nrow(moved_status) > 0L,
    all(startsWith(moved_status$artifact_path, moved_root)),
    all(
        is.na(moved_status$attempt_path) |
        startsWith(moved_status$attempt_path, moved_root)
    )
)
moved_plan <- cdrgam_cli_plan(
    projects='test-project', models='decay', checkout=checkout
)
stopifnot(nrow(moved_plan) > 0L, all(startsWith(moved_plan$path, moved_root)))

# Project-owned references follow the stable project ID, so renaming the
# directory does not invalidate artifacts, registry paths, or queued payloads.
moved_project <- file.path(moved_root, 'projects', 'test-project')
moved_definitions <- getFromNamespace(
    '.cdrgam_cli_read_definitions', 'cdrgam.cli'
)('test-project', check_sources=FALSE, checkout=checkout)
packed_project_path <- getFromNamespace(
    '.cdrgam_cli_pack_store_paths', 'cdrgam.cli'
)(list(path=file.path(moved_project, 'results', 'models', 'decay')), moved_configuration)
renamed_project <- file.path(moved_root, 'projects', 'renamed-project')
stopifnot(file.rename(moved_project, renamed_project))
unpacked_project_path <- getFromNamespace(
    '.cdrgam_cli_unpack_store_paths', 'cdrgam.cli'
)(packed_project_path, moved_configuration)$path
stopifnot(same_path(
    unpacked_project_path, file.path(renamed_project, 'results', 'models', 'decay')
))
renamed_plan <- cdrgam_cli_plan(
    projects='renamed-project', models='decay', checkout=checkout
)
stopifnot(
    nrow(renamed_plan) == 1L,
    renamed_plan$state == 'complete',
    startsWith(renamed_plan$path, renamed_project)
)
renamed_status <- cdrgam_cli_status(
    'renamed-project', checkout=checkout, use_pager=FALSE
)
stopifnot(
    nrow(renamed_status) > 0L,
    all(renamed_status$project == 'renamed-project'),
    all(startsWith(renamed_status$artifact_path, renamed_project))
)

cat('phase1: ok\n')
