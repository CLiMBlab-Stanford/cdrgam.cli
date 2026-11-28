.cdrgam_cli_viz_packages <- c('shiny', 'bslib', 'plotly', 'jsonlite', 'later')

.cdrgam_cli_viz_require <- function() {
    missing <- .cdrgam_cli_viz_packages[!vapply(
        .cdrgam_cli_viz_packages, requireNamespace, logical(1), quietly=TRUE
    )]
    if (length(missing)) {
        .cdrgam_cli_abort(paste0(
            'cdrgam viz requires the R packages ', paste(missing, collapse=', '),
            '. Install them, for example with install.packages(c(',
            paste(shQuote(missing, type='cmd'), collapse=', '), ')).'
        ))
    }
    invisible(TRUE)
}

# Completed fits for the current definitions of each selected project. A fit
# is listed when its published artifact has a complete manifest and fit.rds.
.cdrgam_cli_viz_models <- function(projects=NULL, cdrgam_root=NULL) {
    selected <- .cdrgam_cli_select_projects(projects, cdrgam_root)
    rows <- lapply(selected, function(project) {
        definitions <- tryCatch(
            .cdrgam_cli_read_definitions(
                project, check_sources=FALSE, checkout=cdrgam_root
            ),
            error=function(error) NULL
        )
        if (is.null(definitions)) return(NULL)
        lapply(names(definitions$models), function(name) {
            output <- .cdrgam_cli_path(definitions, 'model', name)
            fit <- file.path(output, 'fit.rds')
            manifest <- file.path(output, 'manifest.yml')
            if (!file.exists(fit) || !file.exists(manifest)) return(NULL)
            status <- tryCatch(
                .cdrgam_cli_read_yaml(manifest)$status, error=function(error) NULL
            )
            if (!identical(status, 'complete')) return(NULL)
            data.frame(
                id=paste(project, name, sep='/'), project=project, model=name,
                fit=fit, stringsAsFactors=FALSE
            )
        })
    })
    rows <- Filter(Negate(is.null), unlist(rows, recursive=FALSE))
    if (!length(rows)) return(data.frame(
        id=character(), project=character(), model=character(),
        fit=character(), stringsAsFactors=FALSE
    ))
    do.call(rbind, rows)
}

.cdrgam_cli_viz_number <- function(value) {
    value <- suppressWarnings(as.numeric(value))
    if (length(value) != 1L || !is.finite(value)) NA_real_ else value
}

.cdrgam_cli_viz_axis <- function(name, summary) {
    if (is.null(summary)) return(list(
        name=name, min=NA_real_, max=NA_real_, median=NA_real_,
        mean=NA_real_, sd=NA_real_
    ))
    quantiles <- as.numeric(summary$quantiles)
    probabilities <- suppressWarnings(as.numeric(names(summary$quantiles)))
    median <- if (length(quantiles) && all(is.finite(probabilities))) {
        stats::approx(probabilities, quantiles, 0.5, rule=2, ties='ordered')$y
    } else NA_real_
    list(
        name=name,
        min=.cdrgam_cli_viz_number(min(quantiles)),
        max=.cdrgam_cli_viz_number(max(quantiles)),
        median=.cdrgam_cli_viz_number(median),
        mean=.cdrgam_cli_viz_number(summary$mean),
        sd=.cdrgam_cli_viz_number(summary$sd)
    )
}

# Describe each effect term for the viewer controls: its axes with ranges and
# typical values, and its group levels when the term is grouped. Axes without
# stored summaries take their range from the fitted basis domain.
.cdrgam_cli_viz_terms <- function(fit) {
    catalog <- .cdrgam_cli_core_effect_catalog(fit)
    lapply(seq_len(nrow(catalog)), function(index) {
        label <- catalog$label[[index]]
        summaries <- catalog$axis_summaries[[index]]
        axes <- lapply(catalog$axes[[index]], function(name) {
            axis <- .cdrgam_cli_viz_axis(name, summaries[[name]])
            if (is.finite(axis$min) && is.finite(axis$max)) return(axis)
            request <- stats::setNames(list(list(grid='fitted', n=2L)), name)
            probe <- tryCatch(.cdrgam_cli_core_estimate_effect(
                fit, terms=label, axes=request, se=FALSE, n=2L
            ), error=function(error) NULL)
            if (!is.null(probe) && name %in% names(probe)) {
                axis$min <- min(probe[[name]])
                axis$max <- max(probe[[name]])
                axis$median <- (axis$min + axis$max) / 2
            }
            axis
        })
        names(axes) <- catalog$axes[[index]]
        grouped <- isTRUE(catalog$grouped[[index]])
        groups <- if (grouped) {
            probe <- tryCatch(.cdrgam_cli_core_estimate_effect(
                fit, terms=label, grouping='deviation', se=FALSE, n=2L
            ), error=function(error) NULL)
            if (is.null(probe)) character() else unique(as.character(probe$group))
        } else character()
        list(
            label=label, type=catalog$type[[index]], grouped=grouped,
            group=catalog$group[[index]], axes=axes, groups=groups,
            catalog=.cdrgam_cli_catalog_row(catalog, index)
        )
    })
}

# The value an axis is held at when the viewer is not told otherwise: lag 0
# and the training mean of every other axis.
.cdrgam_cli_viz_default <- function(name) if (identical(name, 'lag')) 0 else 'mean'

# Resolve a fixed or reference value: a number, or an expression such as
# "mean + 2 * sd" or, for lag, "peak". NA stays NA.
.cdrgam_cli_viz_resolve <- function(term, name, value, peak=NULL) {
    if (is.null(value) || (length(value) == 1L && is.na(value))) return(NA_real_)
    axis <- term$axes[[name]]
    .cdrgam_cli_axis_value(
        value, name, list(mean=axis$mean, sd=axis$sd, median=axis$median),
        peak=peak, field=name
    )
}

# A viewer line or surface as an effect query, in the form a visualization
# definition writes. `spec` holds term, varying, fixed, composition,
# grouping, group, and optional reference values and `ranges`; `options`
# holds n, level, unconditional, interval, and optional `ticks`, the x
# values of a line, and optional `x_range`, the low and high ends of a
# line's x axis as numbers or value expressions, "" for the fitted end.
# Varying axes span the fitted domain, or `ranges` gives their limits, or
# `ticks` their values, or `x_range` the range of a line's one varying
# axis. Every other axis is fixed at its
# value in `fixed`, which may be an expression, or at lag 0 and the training
# mean. An axis with no stored training summary is held at the midpoint of
# its fitted domain instead, and its expressions are resolved here, since a
# definition's query could not resolve them. A NA reference is the model
# default. Returns the validated query.
.cdrgam_cli_viz_line_query <- function(term, spec, options=list()) {
    varying <- as.character(spec$varying)
    unknown <- setdiff(varying, names(term$axes))
    if (length(unknown)) .cdrgam_cli_abort(paste0(
        'Term ', term$label, ' has no axis ', paste(unknown, collapse=', ')
    ))
    n <- as.integer(.cdrgam_cli_null(options$n, 200L))
    ticks <- options$ticks
    fixed <- .cdrgam_cli_null(spec$fixed, list())
    ranges <- .cdrgam_cli_null(spec$ranges, list())
    roles <- .cdrgam_cli_term_axes(.cdrgam_cli_null(term$catalog, list(
        label=term$label, axes=names(term$axes), predictors=setdiff(names(term$axes),
            c('lag', 'time'))
    )))
    summarized <- function(name) is.finite(term$axes[[name]]$mean)
    value_of <- function(name, value) {
        if (is.null(value) || (length(value) == 1L && !is.character(value) && is.na(value))) {
            value <- .cdrgam_cli_viz_default(name)
        }
        if (identical(name, 'lag') || summarized(name) || is.numeric(value)) return(value)
        if (identical(value, 'mean')) value <- 'median'
        .cdrgam_cli_viz_resolve(term, name, value)
    }
    axes <- list()
    place <- function(name, request) {
        if (identical(name, 'lag')) axes$lag <<- request
        else if (name %in% roles$time) axes$time <<- request
        else axes$predictors[[name]] <<- request
    }
    for (name in names(term$axes)) {
        request <- if (name %in% varying) {
            limits <- ranges[[name]]
            if (length(ticks) && length(varying) == 1L) {
                list(values=ticks)
            } else if (length(limits) == 2L && all(is.finite(as.numeric(limits)))) {
                limits <- sort(as.numeric(limits))
                list(values=seq(limits[[1L]], limits[[2L]], length.out=n))
            } else if (length(options$x_range) == 2L && length(varying) == 1L &&
                    any(nzchar(options$x_range))) {
                # An empty end is the fitted end; expressions are resolved by
                # the query, or here for an axis without training summaries.
                ends <- lapply(1:2, function(end) {
                    text <- options$x_range[[end]]
                    axis <- term$axes[[name]]
                    if (!nzchar(text)) return(c(axis$min, axis$max)[[end]])
                    number <- suppressWarnings(as.numeric(text))
                    if (is.finite(number)) return(number)
                    if (summarized(name)) text else .cdrgam_cli_viz_resolve(term, name, text)
                })
                list(range=ends, n=n)
            } else list(grid='fitted', n=n)
        } else value_of(name, fixed[[name]])
        place(name, request)
    }
    query <- list(
        terms=list(names=term$label, match='exact'),
        composition=.cdrgam_cli_null(spec$composition, 'term'),
        grouping=.cdrgam_cli_null(spec$grouping, 'population'),
        axes=axes,
        uncertainty=list(
            level=.cdrgam_cli_null(options$level, 0.95),
            kind=.cdrgam_cli_null(options$interval, 'pointwise'),
            unconditional=isTRUE(options$unconditional)
        ),
        n=n
    )
    if (!identical(query$grouping, 'population')) {
        if (isTRUE(term$grouped)) query$groups <- spec[['group']] else {
            query$grouping <- 'population'
        }
    }
    if (length(spec$reference)) {
        query$reference <- stats::setNames(lapply(names(spec$reference), function(name) {
            value <- spec$reference[[name]]
            if (length(value) == 1L && !is.character(value) && is.na(value)) return('default')
            if (identical(name, 'lag') || summarized(name) || is.numeric(value)) return(value)
            .cdrgam_cli_viz_resolve(term, name, value)
        }), names(spec$reference))
    }
    .cdrgam_cli_validate_effect_query(query, paste0(term$label, ' line'))
}

# Evaluate one viewer line or surface (see .cdrgam_cli_viz_line_query()) as
# `cdrgam run` evaluates a query. The grid carries attributes `peaks` and
# `pointwise_peaks` as from .cdrgam_cli_compute_effect(). `catalog` is the
# fit's effect catalog, computed when not given.
.cdrgam_cli_viz_estimate <- function(fit, term, spec, options=list(), catalog=NULL) {
    query <- .cdrgam_cli_viz_line_query(term, spec, options)
    training <- list(
        impulses=NULL, responses=NULL,
        catalog=.cdrgam_cli_null(catalog, .cdrgam_cli_core_effect_catalog(fit))
    )
    value <- .cdrgam_cli_compute_effect(fit, query, NULL, training)
    attr(value, 'tests') <- NULL
    attr(value, 'query') <- query
    value
}

.cdrgam_cli_viz_preferred_port <- function(user=Sys.info()[['effective_user']]) {
    offset <- strtoi(substr(digest::digest(user, algo='md5'), 1L, 6L), 16L)
    20000L + offset %% 40000L
}

.cdrgam_cli_viz_port_free <- function(port) {
    tryCatch({
        socket <- serverSocket(port)
        close(socket)
        TRUE
    }, error=function(error) FALSE, warning=function(warning) FALSE)
}

.cdrgam_cli_viz_choose_port <- function(port=NULL) {
    if (!is.null(port)) return(as.integer(port))
    first <- .cdrgam_cli_viz_preferred_port()
    for (candidate in first + 0:49) {
        if (.cdrgam_cli_viz_port_free(candidate)) return(candidate)
    }
    .cdrgam_cli_abort('No free viewer port was found; pass --port')
}

.cdrgam_cli_viz_read <- function(path) {
    if (!file.exists(path)) return(NULL)
    tryCatch(.cdrgam_cli_read_yaml(path), error=function(error) NULL)
}

.cdrgam_cli_viz_state <- function(configuration) {
    state <- .cdrgam_cli_viewer_path(configuration)
    if (!dir.exists(state)) dir.create(state, recursive=TRUE)
    Sys.chmod(state, mode='0700')
    state
}

# Exit status that asks the Slurm job script to start the server again.
.cdrgam_cli_viz_restart_status <- 75L

# Run the viewer in this process until it is interrupted. With `record`, the
# server publishes its address in active.yml for `cdrgam viz` to report. In a
# restartable job, a restart requested from the browser records the port and
# exits with the restart status, so the job script relaunches the server with
# freshly loaded code on the same port.
.cdrgam_cli_viz_serve <- function(
        configuration, state, port=NULL, host='127.0.0.1', record=TRUE,
        browse=FALSE
) {
    .cdrgam_cli_viz_require()
    restart_path <- file.path(state, 'restart.yml')
    job_id <- Sys.getenv('SLURM_JOB_ID')
    restartable <- identical(Sys.getenv('CDRGAM_VIZ_RESTARTABLE'), '1')
    if (is.null(port) && restartable) {
        previous <- .cdrgam_cli_viz_read(restart_path)
        if (identical(as.character(previous$job_id), job_id)) port <- previous$port
    }
    unlink(restart_path)
    port <- .cdrgam_cli_viz_choose_port(port)
    active_path <- file.path(state, 'active.yml')
    control <- new.env(parent=emptyenv())
    control$restartable <- restartable
    control$restart <- FALSE
    if (record) {
        active <- list(
            host=unname(Sys.info()[['nodename']]), port=port, pid=Sys.getpid(),
            job_id=job_id, started_at=.cdrgam_cli_timestamp()
        )
        .cdrgam_cli_write_yaml(active, active_path)
        on.exit({
            current <- .cdrgam_cli_viz_read(active_path)
            if (!isTRUE(control$restart) &&
                    identical(as.integer(current$pid), Sys.getpid())) {
                unlink(active_path)
            }
        }, add=TRUE)
    }
    if (record) {
        message('cdrgam viz listening on ', Sys.info()[['nodename']], ':', port)
    } else {
        message(
            'cdrgam viz serving at ', .cdrgam_cli_viz_url(host, port),
            '; press Ctrl+C to stop'
        )
    }
    shiny::runApp(
        .cdrgam_cli_viz_app(configuration$cdrgam_root, control),
        host=host, port=port, launch.browser=isTRUE(browse)
    )
    if (isTRUE(control$restart)) {
        .cdrgam_cli_write_yaml(list(job_id=job_id, port=port), restart_path)
        message('cdrgam viz restarting')
        quit(save='no', status=.cdrgam_cli_viz_restart_status)
    }
    invisible(NULL)
}

.cdrgam_cli_viz_script <- function(
        configuration, state, script_path, log_path, resources, port=NULL
) {
    rscript <- file.path(R.home('bin'), 'Rscript')
    invocation <- paste(
        shQuote(rscript),
        '-e', shQuote('cdrgam.cli::cli_main(commandArgs(trailingOnly=TRUE))'),
        '--args viz-server --root', shQuote(configuration$cdrgam_root),
        '--state', shQuote(state),
        if (is.null(port)) '' else paste('--port', as.integer(port))
    )
    restart <- .cdrgam_cli_viz_restart_status
    .cdrgam_cli_atomic_write(script_path, function(path) writeLines(c(
        '#!/bin/sh',
        .cdrgam_cli_slurm_directives(
            'cdrgam-viz', .cdrgam_cli_store_relative(configuration, log_path),
            resources, open_mode='truncate'
        ),
        '', 'set -eu', '',
        .cdrgam_cli_slurm_exports(configuration, resources$cpus),
        "export CDRGAM_VIZ_RESTARTABLE='1'",
        '',
        paste0('status=', restart),
        paste0('while [ "$status" -eq ', restart, ' ]; do'),
        '    status=0',
        paste0('    ', invocation, ' || status=$?'),
        'done',
        'exit "$status"'
    ), path, useBytes=TRUE))
    Sys.chmod(script_path, mode='0755')
    invisible(script_path)
}

.cdrgam_cli_viz_resources <- function(configuration, overrides=list()) {
    defaults <- list(cpus=1L, memory='16G', time='12:00:00')
    values <- defaults
    for (name in names(overrides)) values[[name]] <- overrides[[name]]
    .cdrgam_cli_resource_configuration(configuration, values)
}

# The gateway is the SSH host a workstation can reach: CDRGAM_VIZ_GATEWAY when
# set (for example user@login.example.edu), otherwise the invoking host.
.cdrgam_cli_viz_instructions <- function(active, gateway=Sys.getenv('CDRGAM_VIZ_GATEWAY')) {
    if (!nzchar(gateway)) gateway <- paste0(
        Sys.info()[['effective_user']], '@', Sys.info()[['nodename']]
    )
    port <- active$port
    paste0(
        'Viewer running on ', active$host,
        if (nzchar(.cdrgam_cli_null(active$job_id, ''))) paste0(
            ' (Slurm job ', active$job_id, ')'
        ) else '',
        ', port ', port, '.\n',
        'Forward the port from your workstation:\n',
        '  ssh -N -L ', port, ':', active$host, ':', port, ' ', gateway, '\n',
        'Then open http://localhost:', port, '\n'
    )
}

# The address a browser on this machine uses to open a viewer.
.cdrgam_cli_viz_url <- function(host, port) {
    if (host %in% c('0.0.0.0', '::')) host <- '127.0.0.1'
    paste0('http://', host, ':', as.integer(port), '/')
}

# Whether the viewer serves from this process rather than a Slurm job.
.cdrgam_cli_viz_local <- function(configuration, local=FALSE) {
    isTRUE(local) || !identical(configuration$scheduler, 'slurm')
}

.cdrgam_cli_viz_stop <- function(configuration, state) {
    jobs <- unique(Filter(nzchar, vapply(c('active.yml', 'job.yml'), function(name) {
        as.character(.cdrgam_cli_null(
            .cdrgam_cli_viz_read(file.path(state, name))$job_id, ''
        ))
    }, character(1))))
    if (length(jobs)) .cdrgam_cli_scancel(jobs)
    unlink(file.path(state, c('active.yml', 'job.yml')))
    if (length(jobs)) {
        cat('Cancelled viewer job ', paste(jobs, collapse=', '), '\n', sep='')
    } else cat('No viewer job was recorded\n')
    invisible(jobs)
}

# Whether a viewer job may still be queued or running. squeue rejects job IDs
# it no longer lists, so an unanswered query falls back to accounting, and
# only a job with no known state is presumed alive.
.cdrgam_cli_viz_job_alive <- function(job_id) {
    active <- .cdrgam_cli_slurm_active(job_id)
    if (!is.null(active)) return(isTRUE(active))
    states <- .cdrgam_cli_slurm_exit_states(job_id)
    state <- if (is.null(states)) NA_character_ else unname(states[[1L]])
    is.na(state) || state %in% c(
        'PENDING', 'CONFIGURING', 'RUNNING', 'COMPLETING', 'REQUEUED',
        'RESIZING', 'SUSPENDED'
    )
}

#' Serve the model viewer
#'
#' Starts, reports, or stops the current user's browser viewer for completed
#' fits under the configured root. On a root whose scheduler is Slurm, the
#' viewer runs in a long-lived Slurm job and later calls reuse the running
#' job. Otherwise, or with `local=TRUE`, it serves from this process until
#' interrupted, on any platform, and prints its address. A local viewer has no
#' job, so `stop` and `restart` are errors; interrupt it and start it again
#' instead. A Slurm viewer listens on all interfaces of its node, without
#' authentication, so other users who can reach the node can also open it. The
#' printed `ssh` command forwards the port through the login host named by the
#' `CDRGAM_VIZ_GATEWAY` environment variable, or through the invoking host when
#' it is unset. A local viewer listens only on the loopback interface.
#'
#' @param cdrgam_root Configured CDR-GAM root.
#' @param stop Cancel the recorded viewer job instead of starting one.
#' @param restart Cancel the recorded viewer job, then start a new one.
#' @param local Serve from this process even on a Slurm-configured root.
#' @param port Port to listen on. The default is a stable per-user port, or
#'   the next free port after it.
#' @param cpus,memory,time,partition,qos Slurm resource overrides for the
#'   viewer job. The defaults are one CPU, 16G, and 12 hours, with the
#'   partition, account, and QOS taken from the root configuration.
#' @param wait Seconds to wait for a submitted job to start serving.
#' @param browse Whether a local viewer opens the default browser. The default
#'   opens it when the session is interactive or standard output is a
#'   terminal. A Slurm viewer never opens one.
#' @return The active server record, invisibly, or `NULL` when the job has not
#'   started within `wait` seconds.
#' @export
cdrgam_cli_viz <- function(
        cdrgam_root=NULL, stop=FALSE, restart=FALSE, local=FALSE, port=NULL,
        cpus=NULL, memory=NULL, time=NULL, partition=NULL, qos=NULL, wait=120,
        browse=NULL
) {
    configuration <- .cdrgam_cli_site(cdrgam_root, create_root=TRUE)
    state <- .cdrgam_cli_viz_state(configuration)
    serve_local <- .cdrgam_cli_viz_local(configuration, local)
    if (serve_local && (isTRUE(stop) || isTRUE(restart))) {
        .cdrgam_cli_abort(paste(
            'A local viewer runs in the foreground and has no job to stop or',
            'restart; press Ctrl+C where it is serving and run cdrgam viz again'
        ))
    }
    if (isTRUE(stop)) return(invisible(.cdrgam_cli_viz_stop(configuration, state)))
    .cdrgam_cli_viz_require()
    overrides <- list(
        cpus=cpus, memory=memory, time=time, partition=partition, qos=qos
    )
    overrides <- overrides[!vapply(overrides, is.null, logical(1))]
    if (serve_local) {
        if (length(overrides)) {
            .cdrgam_cli_abort('Slurm resource options require a Slurm viewer job')
        }
        if (is.null(browse)) browse <- interactive() || isatty(stdout())
        .cdrgam_cli_viz_serve(
            configuration, state, port=port, record=FALSE, browse=browse
        )
        return(invisible(NULL))
    }
    .cdrgam_cli_require_slurm_platform()
    if (isTRUE(restart)) .cdrgam_cli_viz_stop(configuration, state)
    job_path <- file.path(state, 'job.yml')
    active_path <- file.path(state, 'active.yml')
    job <- .cdrgam_cli_with_lock(file.path(state, 'launch.lock'), {
        job <- .cdrgam_cli_viz_read(job_path)
        running <- !is.null(job$job_id) && .cdrgam_cli_viz_job_alive(job$job_id)
        if (running) {
            if (length(overrides)) message(
                'A viewer job is already active; resource options were ignored. ',
                'Use --restart to apply them.'
            )
        } else {
            unlink(active_path)
            resources <- .cdrgam_cli_viz_resources(configuration, overrides)
            script_path <- file.path(state, 'slurm.sh')
            log_path <- file.path(state, 'server.log')
            .cdrgam_cli_viz_script(
                configuration, state, script_path, log_path, resources, port
            )
            job <- list(
                job_id=.cdrgam_cli_sbatch(script_path, configuration),
                resources=resources, log=log_path,
                submitted_at=.cdrgam_cli_timestamp()
            )
            .cdrgam_cli_write_yaml(job, job_path)
            cat('Submitted viewer job ', job$job_id, '\n', sep='')
        }
        job
    })
    deadline <- Sys.time() + as.numeric(wait)
    repeat {
        active <- .cdrgam_cli_viz_read(active_path)
        if (!is.null(active) && identical(as.character(active$job_id), as.character(job$job_id))) {
            cat(.cdrgam_cli_viz_instructions(active))
            return(invisible(active))
        }
        if (!.cdrgam_cli_viz_job_alive(job$job_id)) {
            .cdrgam_cli_abort(paste0(
                'Viewer job ', job$job_id, ' ended before serving; see ', job$log
            ))
        }
        if (Sys.time() >= deadline) break
        Sys.sleep(2)
    }
    cat(
        'Viewer job ', job$job_id, ' has not started serving yet. ',
        'Run cdrgam viz again to get connection details.\n', sep=''
    )
    invisible(NULL)
}
