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
tensor <- terms[[match('x:z', labels)]]
stopifnot(
    setequal(names(tensor$axes), c('lag', 'x', 'z')),
    all(vapply(tensor$axes, function(axis) {
        is.finite(axis$min) && is.finite(axis$max) && axis$min <= axis$median &&
            axis$median <= axis$max
    }, logical(1))),
    !tensor$grouped
)

axes <- internal('.cdrgam_cli_viz_axes')(
    tensor, 'x', list(z=1, lag=NA), n=7L, ranges=list(x=c(1, -1))
)
stopifnot(
    identical(axes$z, list(at=1)),
    identical(axes$lag, list(at=tensor$axes$lag$median)),
    identical(axes$x$values, seq(-1, 1, length.out=7L))
)
unknown_axis <- tryCatch(
    internal('.cdrgam_cli_viz_axes')(tensor, 'w'), error=conditionMessage
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
stopifnot(identical(
    internal('.cdrgam_cli_viz_reference_axes')(tensor), c('x', 'z')
))
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
        reference=list(x=NA_real_, z=0), label='third')
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
    identical(round_trip[[3L]]$reference, list(x=NA_real_, z=0))
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

if (all(vapply(c('shiny', 'plotly', 'bslib'), requireNamespace, logical(1), quietly=TRUE))) {
    invisible(plotly::plotly_build(internal('.cdrgam_cli_viz_line_plot')(list(
        list(spec=list(id='a', label='a', varying='x'), data=line)
    ))))
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
        session$setInputs(use_reference=FALSE, remove_line=lines()[[3L]]$id)
        session$setInputs(remove_line=lines()[[1L]]$id)
        stopifnot(identical(length(lines()), 1L))
        kept <- lines()
        session$setInputs(refresh=1)
        stopifnot(identical(data_version(), 1L), identical(lines(), kept))
        session$setInputs(mode='surface', x_axis='lag', y_axis='z', draw_surface=1)
        stopifnot(identical(plotted()$mode, 'surface'))
    })
    shiny::testServer(internal('.cdrgam_cli_viz_server')(root), {
        session$setInputs(restore_lines=internal('.cdrgam_cli_viz_lines_json')(kept_lines))
        stopifnot(
            identical(length(lines()), 3L),
            identical(lines()[[2L]]$varying, c('lag', 'z')),
            identical(lines()[[3L]]$reference, list(x=NA_real_, z=0))
        )
    })
    stopifnot(!grepl('Restart server', paste(format(internal('.cdrgam_cli_viz_ui')()), collapse=''), fixed=TRUE))
    stopifnot(grepl('Restart server', paste(format(internal('.cdrgam_cli_viz_ui')(TRUE)), collapse=''), fixed=TRUE))
}

fake_tool <- function(name, body) {
    path <- file.path(temporary_parent, name)
    writeLines(c('#!/bin/sh', body), path)
    Sys.chmod(path, mode='0755')
    path
}
expired_squeue <- fake_tool('squeue', c('echo "slurm_load_jobs error: Invalid job id specified" >&2', 'exit 1'))
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

instructions <- internal('.cdrgam_cli_viz_instructions')(
    list(host='node7.example.edu', port=41741L, job_id='42'), gateway='me@login.example.edu'
)
stopifnot(grepl(
    'ssh -N -L 41741:node7.example.edu:41741 me@login.example.edu', instructions, fixed=TRUE
))

cat('viz: ok\n')
