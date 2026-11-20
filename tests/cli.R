library(cdrgam.cli)

capture <- function(arguments) paste(capture.output(
    stopifnot(identical(cli_main(arguments), 0L))
), collapse='\n')

root_help <- capture('--help')
version <- capture('--version')
stopifnot(
    identical(root_help, capture('-h')),
    identical(root_help, capture('help')),
    grepl('Usage: cdrgam <COMMAND>', root_help, fixed=TRUE),
    grepl('Commands:', root_help, fixed=TRUE),
    grepl('-V, --version', root_help, fixed=TRUE),
    identical(version, capture('-V')),
    grepl('cdrgam.cli ', version, fixed=TRUE),
    grepl(' (cdrgam ', version, fixed=TRUE),
    !grepl('scheduler', root_help, fixed=TRUE),
    !grepl('worker', root_help, fixed=TRUE)
)

public_help_paths <- local({
    visit <- function(command, path=character()) {
        paths <- list(path)
        children <- Filter(function(value) !isTRUE(value$hidden), command$commands)
        for (child in children) {
            paths <- c(paths, visit(child, c(path, child$name)))
        }
        paths
    }
    visit(getFromNamespace('.cdrgam_cli_command_spec', 'cdrgam.cli')())
})
stopifnot(all(vapply(public_help_paths, function(path) {
    identical(capture(c(path, '-h')), capture(c(path, '--help')))
}, logical(1))))

selector_shorthands <- c(
    project='-P', dataset='-d', model='-m', prediction='-p',
    visualization='-v', comparison='-c'
)
public_commands <- local({
    visit <- function(command) {
        children <- Filter(function(value) !isTRUE(value$hidden), command$commands)
        c(list(command), unlist(lapply(children, visit), recursive=FALSE))
    }
    visit(getFromNamespace('.cdrgam_cli_command_spec', 'cdrgam.cli')())
})
selector_options <- unlist(lapply(public_commands, function(command) {
    Filter(function(option) option$name %in% names(selector_shorthands),
        command$options)
}), recursive=FALSE)
stopifnot(all(vapply(selector_options, function(option) {
    identical(option$short, unname(selector_shorthands[[option$name]]))
}, logical(1))))

run_help <- capture(c('run', '--help'))
stop_help <- capture(c('stop', '--help'))
def_help <- capture(c('def', '--help'))
def_edit_help <- capture(c('def', 'edit', '--help'))
log_help <- capture(c('log', '--help'))
purge_help <- capture(c('purge', '--help'))
project_rm_help <- capture(c('rm', '--help'))
publish_help <- capture(c('publish', '--help'))
stopifnot(
    grepl('Usage: cdrgam run', run_help, fixed=TRUE),
    grepl('-P, --project PROJECT ...', run_help, fixed=TRUE),
    grepl('re: regex', run_help, fixed=TRUE),
    grepl('--dry-run', run_help, fixed=TRUE),
    grepl('--partition', run_help, fixed=TRUE),
    grepl('--scheduler-time', run_help, fixed=TRUE),
    !grepl('--slurm-time', run_help, fixed=TRUE),
    grepl('Usage: cdrgam stop', stop_help, fixed=TRUE),
    grepl('Slurm-managed', stop_help, fixed=TRUE),
    grepl('--force', stop_help, fixed=TRUE),
    grepl('--scheduler', stop_help, fixed=TRUE),
    all(vapply(
        c('edit', 'init', 'ls', 'rm', 'mv', 'val'),
        grepl,
        logical(1),
        x=def_help,
        fixed=TRUE
    )),
    grepl('Definitions are never selected', purge_help, fixed=TRUE),
    grepl('-d, --dataset DATASET ...', purge_help, fixed=TRUE),
    grepl('pager', purge_help, fixed=TRUE),
    grepl('-f, --force', purge_help, fixed=TRUE),
    !grepl('--yes', purge_help, fixed=TRUE),
    grepl('Usage: cdrgam rm PROJECT', project_rm_help, fixed=TRUE),
    grepl('entire project', project_rm_help, fixed=TRUE),
    grepl('-f, --force', project_rm_help, fixed=TRUE),
    !grepl('--yes', project_rm_help, fixed=TRUE),
    grepl('--running', log_help, fixed=TRUE),
    grepl('-d, --dataset DATASET ...', def_edit_help, fixed=TRUE),
    grepl('-m, --model MODEL ...', def_edit_help, fixed=TRUE),
    grepl('-v, --visualization VISUALIZATION ...', def_edit_help, fixed=TRUE),
    grepl('-c, --comparison COMPARISON ...', def_edit_help, fixed=TRUE),
    grepl('-P, --project PROJECT', def_edit_help, fixed=TRUE),
    grepl('[TARGET]', def_edit_help, fixed=TRUE),
    grepl('-P, --project PROJECT', publish_help, fixed=TRUE),
    grepl('[PROJECT]', publish_help, fixed=TRUE)
)

internal <- function(name) getFromNamespace(name, 'cdrgam.cli')
yaml_axis <- yaml::yaml.load('grid: fitted\nn: 300\n', eval.expr=FALSE)
stopifnot('FALSE' %in% names(yaml_axis))
validated_yaml_axis <- internal('.cdrgam_cli_validate_axis_request')(
    yaml_axis, 'query.axes.lag'
)
stopifnot(
    identical(validated_yaml_axis$grid, 'fitted'),
    identical(validated_yaml_axis$n, 300L),
    !('FALSE' %in% names(validated_yaml_axis))
)
yaml_query <- yaml::yaml.load(
    'n: 125\naxes:\n  lag:\n    grid: fitted\n    n: 50\n',
    eval.expr=FALSE
)
validated_yaml_query <- internal('.cdrgam_cli_validate_effect_query')(
    yaml_query, 'query'
)
stopifnot(
    identical(validated_yaml_query$n, 125L),
    identical(validated_yaml_query$axes$lag$n, 50L)
)
yaml_render <- yaml::yaml.load(
    paste(
        'geometry: line', 'mappings:', '  x: lag', '  y: estimate',
        sep='\n'
    ),
    eval.expr=FALSE
)
stopifnot('TRUE' %in% names(yaml_render$mappings))
validated_yaml_render <- internal('.cdrgam_cli_validate_render')(
    yaml_render, 'render'
)
stopifnot(
    identical(validated_yaml_render$mappings$x, 'lag'),
    identical(validated_yaml_render$mappings$y, 'estimate'),
    !('TRUE' %in% names(validated_yaml_render$mappings))
)
stopifnot(
    !internal('.cdrgam_cli_needs_simplification_report')(
        list(outcome='success', converged=TRUE)
    ),
    internal('.cdrgam_cli_needs_simplification_report')(
        list(outcome='warning', converged=TRUE)
    ),
    internal('.cdrgam_cli_needs_simplification_report')(
        list(outcome='nonconverged', converged=FALSE)
    )
)
report_directory <- tempfile('cdrgam-simplification-report-')
dir.create(report_directory)
invisible(loadNamespace('cdrgam'))
fake_report <- structure(list(
    candidates=data.frame(
        rank=1L, priority='high', parameter=NA_character_, term='s(time)',
        action='replace_with_null_space', recommendation='use a linear term',
        evidence='edf equals the null-space dimension', automatable=FALSE,
        patch_id=NA_character_, edf=1, coefficient_count=4L,
        null_space_dimension=1, smoothing_parameter=10, outer_score=NA_real_,
        evidence_score=80, df_loss=0, coefficient_loss=3L,
        terms_touched=1L, impact_fraction=0, score=80,
        stringsAsFactors=FALSE
    ),
    patches=list(), converged=TRUE, gradient_tolerance=1e-6,
    conservatism=1
), class='cdrgam_simplification_report')
report_metadata <- internal('.cdrgam_cli_write_simplification_report')(
    fake_report, list(outcome='warning', converged=TRUE), report_directory
)
report_yaml <- yaml::read_yaml(file.path(
    report_directory, 'simplification-report.yml'
))
stopifnot(
    identical(report_metadata$candidates, 1L),
    identical(report_yaml$outcome, 'warning'),
    identical(report_yaml$candidates[[1L]]$rank, 1L),
    file.exists(file.path(report_directory, 'simplification-report.txt'))
)
unlink(report_directory, recursive=TRUE)
warning_status <- data.frame(
    project='example', kind='fit', name='model', identity='1234',
    display_state='Warning', job=NA_character_, display_updated='2026-10-09',
    diagnostic='test warning', log=NA_character_, stringsAsFactors=FALSE
)
warning_status_text <- internal('.cdrgam_cli_status_report')(
    warning_status, color=TRUE
)
stopifnot(
    grepl('Fits requiring review', warning_status_text, fixed=TRUE),
    grepl('test warning', warning_status_text, fixed=TRUE)
)
confirmation_events <- character()
confirmation_input <- textConnection('yes\n')
confirmation_output <- textConnection(
    'confirmation_prompt', open='w', local=TRUE
)
confirmation_response <- internal('.cdrgam_cli_read_terminal_line')(
    'Confirm? ', input=confirmation_input, output=confirmation_output
)
close(confirmation_input)
close(confirmation_output)
stopifnot(
    identical(confirmation_response, 'yes'),
    identical(paste(confirmation_prompt, collapse=''), 'Confirm? ')
)
confirmed <- internal('.cdrgam_cli_confirm_destructive')(
    'preview\n', prompt='Confirm? ', confirm=TRUE,
    pager=function(text) {
        stopifnot(identical(text, 'preview\n'))
        confirmation_events <<- c(confirmation_events, 'preview')
    },
    reader=function(prompt) {
        stopifnot(identical(prompt, 'Confirm? '))
        confirmation_events <<- c(confirmation_events, 'prompt')
        'yes'
    }
)
stopifnot(isTRUE(confirmed), identical(
    confirmation_events, c('preview', 'prompt')
))
forced <- internal('.cdrgam_cli_confirm_destructive')(
    'preview\n', prompt='Confirm? ', force=TRUE,
    pager=function(text) stop('force must skip the preview'),
    reader=function(prompt) stop('force must skip the prompt')
)
stopifnot(isTRUE(forced))
scheduler_resources <- internal('.cdrgam_cli_scheduler_resources')(
    list(
        slurm_partition='sphinx', slurm_account='nlp', slurm_cpus=8L,
        slurm_memory='64G', slurm_time='72:00:00', slurm_qos=NULL
    ),
    list(partition='john', time='12:00:00')
)
stopifnot(identical(
    scheduler_resources,
    list(
        partition='john', account='nlp', cpus=1L, memory='1G',
        time='12:00:00', qos=NULL
    )
))
scheduler_root <- tempfile('cdrgam-scheduler-script-')
dir.create(scheduler_root)
scheduler_configuration <- list(cdrgam_root=scheduler_root)
scheduler_script <- file.path(scheduler_root, 'scheduler.sh')
internal('.cdrgam_cli_scheduler_script')(
    scheduler_configuration, scheduler_script,
    file.path(scheduler_root, 'scheduler-%j.log'), scheduler_resources
)
scheduler_lines <- readLines(scheduler_script, warn=FALSE)
stopifnot(
    '#SBATCH --requeue' %in% scheduler_lines,
    '#SBATCH --signal=B:USR1@300' %in% scheduler_lines,
    any(grepl('scontrol requeue', scheduler_lines, fixed=TRUE)),
    any(grepl('trap requeue_scheduler USR1', scheduler_lines, fixed=TRUE))
)
unlink(scheduler_root, recursive=TRUE)

cat('cli: ok\n')
