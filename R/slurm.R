.cdrgam_cli_resource_configuration <- function(configuration, overrides=list()) {
    value <- list(
        partition=configuration$slurm_partition,
        account=configuration$slurm_account,
        cpus=.cdrgam_cli_null(configuration$slurm_cpus, 1L),
        memory=configuration$slurm_memory,
        time=configuration$slurm_time,
        qos=configuration$slurm_qos
    )
    for (name in names(overrides)) value[[name]] <- overrides[[name]]
    value$cpus <- .cdrgam_cli_positive_integer(value$cpus, 'Slurm cpus')
    for (field in c('partition', 'account', 'memory', 'time', 'qos')) {
        if (!is.null(value[[field]])) {
            value[[field]] <- .cdrgam_cli_scalar_character(
                value[[field]], paste('Slurm', field)
            )
            if (grepl('[[:space:]]', value[[field]])) {
                .cdrgam_cli_abort(paste('Slurm', field, 'must not contain whitespace'))
            }
        }
    }
    value
}

.cdrgam_cli_require_slurm_platform <- function() {
    if (.cdrgam_cli_is_windows()) {
        .cdrgam_cli_abort(
            paste(
                'Direct Slurm execution is not supported on Windows;',
                'remove the Slurm fields to use local execution'
            )
        )
    }
    shell <- Sys.which('sh')
    if (!length(shell) || !nzchar(shell[[1L]])) {
        .cdrgam_cli_abort('Direct Slurm execution requires a POSIX sh executable')
    }
    invisible(TRUE)
}

.cdrgam_cli_resource_key <- function(resources) {
    .cdrgam_cli_short_hash(resources, length=20L)
}

.cdrgam_cli_scheduler_resources <- function(configuration) {
    list(
        partition=configuration$slurm_partition,
        account=configuration$slurm_account,
        cpus=1L,
        memory='1G',
        time=configuration$slurm_time,
        qos=configuration$slurm_qos
    )
}

.cdrgam_cli_slurm_directives <- function(name, output, resources) {
    directives <- c(
        paste0('#SBATCH --job-name=', name),
        paste0('#SBATCH --output=', output),
        paste0('#SBATCH --cpus-per-task=', resources$cpus)
    )
    for (field in c('memory', 'time', 'partition', 'account', 'qos')) {
        if (!is.null(resources[[field]])) {
            option <- switch(field, memory='mem', field)
            directives <- c(directives, paste0('#SBATCH --', option, '=', resources[[field]]))
        }
    }
    directives
}

.cdrgam_cli_slurm_exports <- function(configuration, cpus) {
    environment <- list(
        R_LIBS_USER=paste(.libPaths(), collapse=.Platform$path.sep),
        CDRGAM_CHECKOUT=configuration$checkout,
        OMP_NUM_THREADS=as.character(cpus),
        OPENBLAS_NUM_THREADS=as.character(cpus),
        MKL_NUM_THREADS=as.character(cpus),
        R_ENVIRON_USER='/dev/null'
    )
    vapply(names(environment), function(name) {
        paste('export', paste0(name, '=', shQuote(environment[[name]])))
    }, character(1))
}

.cdrgam_cli_sbatch <- function(script_path, configuration) {
    sbatch <- getOption('cdrgam.cli.sbatch', Sys.which('sbatch'))
    if (!length(sbatch) || !nzchar(sbatch)) {
        .cdrgam_cli_abort('sbatch was not found on PATH')
    }
    result <- .cdrgam_cli_process_run(sbatch, c(
        '--parsable', '--chdir', configuration$cdrgam_root, script_path
    ))
    status <- if (is.null(result)) NA_integer_ else result$status
    output <- if (is.null(result)) character() else c(result$stdout, result$stderr)
    output <- output[nzchar(output)]
    submitted <- if (is.null(result)) '' else trimws(result$stdout)
    submitted <- strsplit(submitted, '\n', fixed=TRUE)[[1L]][[1L]]
    if (status != 0L || !grepl('^[0-9]+(;.*)?$', submitted)) {
        .cdrgam_cli_abort(paste0(
            'Slurm submission failed: ',
            paste(c(output, paste0('sbatch exit status ', status)), collapse='; ')
        ))
    }
    sub(';.*$', '', submitted)
}

.cdrgam_cli_scheduler_script <- function(configuration, script_path, log_path) {
    resources <- .cdrgam_cli_scheduler_resources(configuration)
    rscript <- file.path(R.home('bin'), 'Rscript')
    invocation <- paste(
        'exec', shQuote(rscript),
        '-e', shQuote('cdrgam.cli::cli_main(commandArgs(trailingOnly=TRUE))'),
        '--args scheduler --checkout', shQuote(configuration$checkout)
    )
    .cdrgam_cli_atomic_write(script_path, function(path) writeLines(c(
        '#!/bin/sh',
        .cdrgam_cli_slurm_directives(
            'cdrgam-scheduler',
            .cdrgam_cli_store_relative(configuration, log_path), resources
        ),
        '', 'set -eu', '',
        .cdrgam_cli_slurm_exports(configuration, resources$cpus),
        '', invocation
    ), path, useBytes=TRUE))
    Sys.chmod(script_path, mode='0755')
    invisible(script_path)
}

.cdrgam_cli_submit_scheduler <- function(configuration) {
    .cdrgam_cli_require_slurm_platform()
    private <- file.path(configuration$cdrgam_root, '.cdrgam')
    lock_path <- file.path(private, 'locks', 'scheduler.lock')
    .cdrgam_cli_with_lock(lock_path, {
        .cdrgam_cli_submit_scheduler_unlocked(configuration, private)
    })
}

.cdrgam_cli_submit_scheduler_unlocked <- function(configuration, private) {
    scheduler_directory <- file.path(private, 'scheduler')
    if (!dir.exists(scheduler_directory)) dir.create(scheduler_directory, recursive=TRUE)
    metadata_path <- file.path(private, 'scheduler.yml')
    existing <- if (file.exists(metadata_path)) {
        tryCatch(.cdrgam_cli_read_yaml(metadata_path), error=function(error) NULL)
    } else NULL
    if (!is.null(existing) && !is.null(existing$job_id)) {
        active <- .cdrgam_cli_slurm_active(existing$job_id)
        if (!identical(active, FALSE)) return(existing)
    }
    scheduler_id <- .cdrgam_cli_random_id('scheduler')
    script_path <- file.path(scheduler_directory, paste0(scheduler_id, '.sh'))
    log_path <- file.path(scheduler_directory, paste0(scheduler_id, '-%j.log'))
    .cdrgam_cli_scheduler_script(configuration, script_path, log_path)
    metadata <- list(
        schema=1L, state='submitting', scheduler_id=scheduler_id,
        script=.cdrgam_cli_store_relative(configuration, script_path),
        log_path=.cdrgam_cli_store_relative(configuration, log_path),
        submitted_at=.cdrgam_cli_timestamp()
    )
    .cdrgam_cli_write_yaml(metadata, metadata_path)
    job_id <- tryCatch(
        .cdrgam_cli_sbatch(script_path, configuration), error=function(error) error
    )
    if (inherits(job_id, 'error')) {
        metadata$state <- 'submission-failed'
        metadata$error <- conditionMessage(job_id)
        .cdrgam_cli_write_yaml(metadata, metadata_path)
        stop(job_id)
    }
    metadata$state <- 'submitted'
    metadata$job_id <- job_id
    metadata$log <- sub('%j', job_id, metadata$log_path, fixed=TRUE)
    .cdrgam_cli_write_yaml(metadata, metadata_path)
    metadata
}

.cdrgam_cli_worker_script <- function(
        configuration, worker_id, resource_key, endpoint, resources, script_path,
        log_path
) {
    rscript <- file.path(R.home('bin'), 'Rscript')
    invocation <- paste(
        'exec', shQuote(rscript),
        '-e', shQuote('cdrgam.cli::cli_main(commandArgs(trailingOnly=TRUE))'),
        '--args worker',
        '--worker-id', shQuote(worker_id),
        '--resource-key', shQuote(resource_key),
        '--controller-host', shQuote(endpoint$host),
        '--controller-port', shQuote(as.character(endpoint$port)),
        '--controller-token', shQuote(endpoint$token)
    )
    .cdrgam_cli_atomic_write(script_path, function(path) writeLines(c(
        '#!/bin/sh',
        .cdrgam_cli_slurm_directives(
            'cdrgam-worker',
            .cdrgam_cli_store_relative(configuration, log_path), resources
        ),
        '', 'set -eu', '',
        .cdrgam_cli_slurm_exports(configuration, resources$cpus),
        '', invocation
    ), path, useBytes=TRUE))
    Sys.chmod(script_path, mode='0755')
    invisible(script_path)
}

.cdrgam_cli_submit_worker <- function(configuration, endpoint, resource_key, resources) {
    .cdrgam_cli_require_slurm_platform()
    worker_id <- .cdrgam_cli_random_id('worker')
    directory <- file.path(configuration$cdrgam_root, '.cdrgam', 'workers', worker_id)
    if (!dir.exists(directory)) dir.create(directory, recursive=TRUE)
    script_path <- file.path(directory, 'slurm.sh')
    log_path <- file.path(directory, '%j.log')
    .cdrgam_cli_worker_script(
        configuration, worker_id, resource_key, endpoint, resources, script_path,
        log_path
    )
    job_id <- .cdrgam_cli_sbatch(script_path, configuration)
    list(
        worker_id=worker_id, resource_key=resource_key, resources=resources,
        status='submitted', job_id=job_id, path=directory,
        log=sub('%j', job_id, log_path, fixed=TRUE),
        submitted_at=.cdrgam_cli_timestamp()
    )
}

.cdrgam_cli_worker <- function(
        worker_id, resource_key, controller_host, controller_port,
        controller_token
) {
    endpoint <- list(
        host=controller_host, port=as.integer(controller_port), token=controller_token
    )
    repeat {
        assignment <- .cdrgam_cli_controller_call(endpoint, list(
            type='claim', worker_id=worker_id, resource_key=resource_key
        ))
        if (identical(assignment$action, 'stop')) break
        if (identical(assignment$action, 'wait')) {
            Sys.sleep(.cdrgam_cli_null(assignment$seconds, 1))
            next
        }
        if (!identical(assignment$action, 'run')) {
            .cdrgam_cli_abort('Scheduler returned an invalid worker action')
        }
        configuration <- .cdrgam_cli_checkout(create_root=TRUE)
        submitted <- .cdrgam_cli_unpack_store_paths(
            readRDS(assignment$request_file), configuration
        )
        item <- submitted$graph$items[[assignment$work_key]]
        if (is.null(item)) .cdrgam_cli_abort('Assignment does not contain its work item')
        definitions <- submitted$graph$definitions[[item$project]]
        current <- .cdrgam_cli_execution_envelope(definitions)
        expected <- submitted$execution
        compatible <- identical(current$R, expected$R) &&
            identical(current$cdrgam$code_md5, expected$cdrgam$code_md5) &&
            identical(current$cdrgam_cli$code_md5, expected$cdrgam_cli$code_md5)
        if (!compatible) {
            message <- 'Worker execution environment differs from the submitted request'
            .cdrgam_cli_controller_call(endpoint, list(
                type='failed', worker_id=worker_id,
                work_key=assignment$work_key, error=message
            ))
            next
        }
        run_error <- tryCatch({
            .cdrgam_cli_run_item(
                definitions, item, attempt_path=assignment$attempt
            )
            NULL
        }, error=identity)
        if (inherits(run_error, 'error')) {
            reported <- tryCatch({
                .cdrgam_cli_controller_call(endpoint, list(
                    type='failed', worker_id=worker_id,
                    work_key=assignment$work_key, error=conditionMessage(run_error)
                ))
                TRUE
            }, error=function(error) FALSE)
            if (!reported) stop(run_error)
            next
        }
        reported <- tryCatch({
            .cdrgam_cli_controller_call(endpoint, list(
                type='complete', worker_id=worker_id,
                work_key=assignment$work_key
            ))
            TRUE
        }, error=function(error) {
            warning(
                'The artifact was published, but the scheduler did not acknowledge ',
                'completion; stopping this worker', call.=FALSE
            )
            FALSE
        })
        if (!reported) break
    }
    try(.cdrgam_cli_controller_call(endpoint, list(
        type='stopped', worker_id=worker_id
    )), silent=TRUE)
    invisible(TRUE)
}
