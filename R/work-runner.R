.cdrgam_cli_attempt_active <- function(metadata) {
    if (is.null(metadata) || !identical(metadata$status, 'running')) {
        return(FALSE)
    }
    # A PID is meaningful only on the host that created it.  Require its
    # creation time as well: Slurm nodes routinely recycle PID values after a
    # cancelled worker exits.
    if (!.cdrgam_cli_same_host(.cdrgam_cli_null(metadata$host, '')) ||
            is.null(metadata$process_started)) {
        return(FALSE)
    }
    !identical(.cdrgam_cli_process_alive(
        metadata$pid, metadata$process_started
    ), FALSE)
}

.cdrgam_cli_prune_attempt_directories <- function(paths, keep=character()) {
    keep <- .cdrgam_cli_normalize_path(keep, must_work=FALSE)
    for (path in setdiff(paths, keep)) {
        metadata_path <- file.path(path, 'attempt.yml')
        metadata <- if (file.exists(metadata_path)) tryCatch(
            .cdrgam_cli_read_yaml(metadata_path), error=function(error) NULL
        ) else NULL
        active <- .cdrgam_cli_attempt_active(metadata)
        if (active) .cdrgam_cli_abort(paste0(
            'A superseded attempt still appears active: ', path
        ))
        unlink(path, recursive=TRUE, force=TRUE)
        if (file.exists(path)) .cdrgam_cli_abort(paste0(
            'Could not remove superseded attempt ', sQuote(path)
        ))
    }
    invisible(keep)
}

.cdrgam_cli_attempt_directory <- function(definitions, item, explicit=NULL) {
    parent <- .cdrgam_cli_path(
        definitions, 'work', name=paste(item$kind, item$name, sep='_'),
        identity=item$identity
    )
    if (!dir.exists(parent) && !dir.create(parent, recursive=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not create work directory ', parent))
    }
    existing <- sort(list.dirs(parent, recursive=FALSE, full.names=TRUE), decreasing=TRUE)
    if (!is.null(explicit)) {
        explicit <- .cdrgam_cli_normalize_path(explicit, must_work=TRUE)
        if (!.cdrgam_cli_within(explicit, parent)) {
            .cdrgam_cli_abort('Worker attempt path does not match its work item')
        }
        .cdrgam_cli_prune_attempt_directories(existing, keep=explicit)
        return(list(path=explicit, resumed=FALSE))
    }
    resume <- NULL
    for (path in existing) {
        metadata_path <- file.path(path, 'attempt.yml')
        if (!file.exists(metadata_path)) next
        metadata <- tryCatch(.cdrgam_cli_read_yaml(metadata_path), error=function(error) NULL)
        if (is.null(metadata) || !(metadata$status %in% c('running', 'failed'))) next
        active <- .cdrgam_cli_attempt_active(metadata)
        if (active) .cdrgam_cli_abort(paste0('Work item appears active: ', item$key))
        if (is.null(resume)) resume <- path
    }
    if (!is.null(resume)) {
        .cdrgam_cli_prune_attempt_directories(existing, keep=resume)
        return(list(path=resume, resumed=TRUE))
    }
    .cdrgam_cli_prune_attempt_directories(existing)
    label <- paste0(format(Sys.time(), '%Y%m%dT%H%M%S'), '-', Sys.getpid())
    path <- file.path(parent, label)
    if (!dir.create(path)) .cdrgam_cli_abort(paste0('Could not create ', path))
    if (identical(item$kind, 'fit')) {
        manifest_path <- .cdrgam_cli_manifest_path(item$output)
        manifest <- if (file.exists(manifest_path)) tryCatch(
            .cdrgam_cli_read_yaml(manifest_path), error=function(error) NULL
        ) else NULL
        source <- file.path(item$output, 'optimizer-checkpoint.rds')
        same_nonconverged_fit <- !is.null(manifest) &&
            identical(manifest$kind, 'fit') &&
            identical(manifest$identity, item$identity) &&
            identical(
                .cdrgam_cli_fit_outcome(manifest$result$diagnostics),
                'nonconverged'
            )
        if (same_nonconverged_fit && file.exists(source) &&
                !file.copy(source, file.path(path, basename(source)))) {
            .cdrgam_cli_abort('Could not seed the fit attempt from its checkpoint')
        }
    }
    list(path=path, resumed=FALSE)
}

.cdrgam_cli_capture_conditions <- function(expression, path) {
    connection <- file(path, open='at', encoding='UTF-8')
    output_sinks <- sink.number(type='output')
    message_sinks <- sink.number(type='message')
    sink(connection, type='output')
    sink(connection, type='message')
    on.exit({
        while (sink.number(type='message') > message_sinks) sink(type='message')
        while (sink.number(type='output') > output_sinks) sink(type='output')
        close(connection)
    }, add=TRUE)
    withCallingHandlers(
        expression,
        message=function(condition) {
            writeLines(paste0(
                .cdrgam_cli_timestamp(), ' MESSAGE ', conditionMessage(condition)
            ), connection, useBytes=TRUE)
            invokeRestart('muffleMessage')
        },
        warning=function(condition) {
            writeLines(paste0(
                .cdrgam_cli_timestamp(), ' WARNING ', conditionMessage(condition)
            ), connection, useBytes=TRUE)
            invokeRestart('muffleWarning')
        }
    )
}

.cdrgam_cli_start_work_log <- function(definitions, item, started) {
    path <- .cdrgam_cli_work_log_path(definitions, item$kind, item$name)
    directory <- dirname(path)
    if (!dir.exists(directory)) {
        suppressWarnings(dir.create(directory, recursive=TRUE))
    }
    if (!dir.exists(directory)) {
        .cdrgam_cli_abort('Could not create the work-item log directory')
    }
    writeLines(paste0(
        started, ' START ', item$kind, ' ', item$name,
        ' [', item$identity, ']'
    ), path, useBytes=TRUE)
    path
}

.cdrgam_cli_execution_envelope <- function(definitions) {
    source <- if (dir.exists(file.path(definitions$root, '.git'))) {
        inventory <- .cdrgam_cli_source_inventory(definitions$root)
        list(
            revision=.cdrgam_cli_git_revision(definitions$root),
            digest=.cdrgam_cli_inventory_digest(inventory)
        )
    } else NULL
    list(
        cdrgam_root=definitions$site$cdrgam_root,
        project_source=source,
        R=R.home(), R_version=as.character(getRversion()),
        BLAS=unname(extSoftVersion()[['BLAS']]),
        libraries=.libPaths(),
        cdrgam=.cdrgam_cli_implementation('cdrgam'),
        cdrgam_cli=.cdrgam_cli_implementation('cdrgam.cli')
    )
}

.cdrgam_cli_run_item <- function(definitions, item, attempt_path=NULL) {
    destination <- item$output
    if (.cdrgam_cli_complete_artifact(destination, item$identity)) {
        message('Reusing complete ', item$kind, ' artifact ', item$name)
        return(invisible(list(status='reused', path=destination, item=item$key)))
    }
    for (dependency in item$dependencies) {
        dependency_item <- attr(item, 'graph')[[dependency]]
        if (!is.null(dependency_item) && !.cdrgam_cli_complete_artifact(
                dependency_item$output, dependency_item$identity)) {
            .cdrgam_cli_abort(paste0('Dependency is incomplete: ', dependency))
        }
    }
    attempt <- .cdrgam_cli_attempt_directory(definitions, item, attempt_path)
    stage <- attempt$path
    started <- .cdrgam_cli_timestamp()
    metadata <- list(
        schema=1L, status='running', project=item$project, kind=item$kind,
        name=item$name, identity=item$identity, key=item$key, pid=Sys.getpid(),
        host=.cdrgam_cli_host_name(),
        process_started=.cdrgam_cli_process_started(),
        started_at=started, resumed=isTRUE(attempt$resumed)
    )
    job_id <- Sys.getenv('SLURM_JOB_ID', '')
    if (nzchar(job_id)) metadata$job_id <- job_id
    log_path <- .cdrgam_cli_start_work_log(definitions, item, started)
    metadata$log <- .cdrgam_cli_project_relative(definitions, log_path)
    .cdrgam_cli_write_yaml(metadata, file.path(stage, 'attempt.yml'))
    published <- FALSE
    on.exit({
        if (!published && dir.exists(stage)) {
            metadata$status <- 'failed'
            metadata$failed_at <- .cdrgam_cli_timestamp()
            .cdrgam_cli_write_yaml(metadata, file.path(stage, 'attempt.yml'))
        }
    }, add=TRUE)
    message(
        .cdrgam_cli_timestamp(), ' Running ', item$kind, ' ', item$name,
        ' -> ', item$identity
    )
    result <- tryCatch(
        .cdrgam_cli_capture_conditions(switch(
            item$kind,
            fit=.cdrgam_cli_execute_fit(item, stage, publish=function(build) {
                .cdrgam_cli_publish_directory(build, destination, definitions, item)
            }),
            prediction=.cdrgam_cli_execute_prediction(item, stage),
            effect=.cdrgam_cli_execute_effect(item, stage),
            visualization=.cdrgam_cli_execute_visualization(item, stage),
            comparison=.cdrgam_cli_execute_comparison(item, stage),
            .cdrgam_cli_abort(paste0('No executor for work kind ', item$kind))
        ), log_path),
        error=function(error) {
            metadata$status <- 'failed'
            metadata$failed_at <- .cdrgam_cli_timestamp()
            metadata$error <- conditionMessage(error)
            .cdrgam_cli_write_yaml(metadata, file.path(stage, 'attempt.yml'))
            write(paste0(
                .cdrgam_cli_timestamp(), ' ERROR ', conditionMessage(error)
            ), file=log_path, append=TRUE)
            stop(error)
        }
    )
    fit_outcome <- if (identical(item$kind, 'fit')) {
        .cdrgam_cli_fit_outcome(result$diagnostics)
    } else NULL
    nonconverged <- identical(fit_outcome, 'nonconverged')
    unresolved <- !nonconverged &&
        identical(result$derivation$outcome, 'unresolved')
    run_status <- if (nonconverged) 'nonconverged' else if (unresolved) {
        'unresolved'
    } else 'complete'
    metadata$status <- run_status
    if (!is.null(fit_outcome)) metadata$model_outcome <- fit_outcome
    metadata$completed_at <- .cdrgam_cli_timestamp()
    .cdrgam_cli_write_yaml(metadata, file.path(stage, 'attempt.yml'))
    write(paste0(
        metadata$completed_at, ' ',
        toupper(run_status), ' ', item$kind, ' ', item$name,
        if (unresolved) paste0(': ', result$derivation$message) else ''
    ), file=log_path, append=TRUE)
    relative_outputs <- .cdrgam_cli_publication_outputs(stage)
    manifest <- list(
        schema=1L, status='complete', project=item$project, kind=item$kind,
        definition=item$name, identity=item$identity,
        contract=item$resolved_identity$resolved,
        inputs=item$dependencies,
        execution=.cdrgam_cli_execution_envelope(definitions),
        started_at=started, completed_at=.cdrgam_cli_timestamp(),
        log=.cdrgam_cli_project_relative(definitions, log_path),
        result=result,
        outputs=.cdrgam_cli_output_manifest(stage, relative_outputs)
    )
    if (identical(item$kind, 'fit')) {
        manifest$response_name <- result$response_name
        manifest$diagnostics <- result$diagnostics
        manifest$configuration <- result$configuration
        manifest$formulas <- result$formulas
        manifest$simplifications <- result$simplifications
        manifest$inference <- paste(
            'Conditional on selected smoothing parameters and any recorded',
            'automatic simplifications.'
        )
    }
    .cdrgam_cli_write_yaml(manifest, .cdrgam_cli_manifest_path(stage))
    .cdrgam_cli_publish_directory(stage, destination, definitions, item)
    published <- TRUE
    message('Published ', item$kind, ' artifact ', destination)
    invisible(list(
        status=if (run_status == 'complete') 'completed' else run_status,
        path=destination, item=item$key,
        message=if (nonconverged) {
            result$diagnostics$message
        } else if (unresolved) result$derivation$message else NULL
    ))
}
