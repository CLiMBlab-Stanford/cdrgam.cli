.cdrgam_cli_controller_discovery <- function(configuration) {
    file.path(configuration$cdrgam_root, '.cdrgam', 'controller.yml')
}

.cdrgam_cli_controller_read <- function(configuration) {
    path <- .cdrgam_cli_controller_discovery(configuration)
    if (!file.exists(path)) return(NULL)
    value <- tryCatch(.cdrgam_cli_read_yaml(path), error=function(error) NULL)
    required <- c('host', 'port', 'pid', 'token', 'startup')
    if (is.null(value) || !all(required %in% names(value))) return(NULL)
    if (.cdrgam_cli_same_host(value$host)) {
        alive <- .cdrgam_cli_process_alive(value$pid, value$process_started)
        if (identical(alive, FALSE)) return(NULL)
    }
    value
}

.cdrgam_cli_controller_call <- function(endpoint, message, retries=10L) {
    message$token <- endpoint$token
    last_error <- NULL
    for (attempt in seq_len(retries)) {
        connection <- NULL
        result <- tryCatch({
            connection <- socketConnection(
                host=endpoint$host, port=as.integer(endpoint$port),
                open='a+b', blocking=TRUE, timeout=5
            )
            serialize(message, connection)
            flush(connection)
            value <- unserialize(connection)
            close(connection)
            value
        }, error=function(error) {
            if (!is.null(connection)) try(close(connection), silent=TRUE)
            last_error <<- error
            NULL
        })
        if (!is.null(result)) {
            if (!isTRUE(result$ok)) .cdrgam_cli_abort(result$error)
            return(result)
        }
        Sys.sleep(min(0.1 * attempt, 1))
    }
    .cdrgam_cli_abort(paste0(
        'Could not contact the CDR-GAM scheduler: ', conditionMessage(last_error)
    ))
}

.cdrgam_cli_controller_submit <- function(
        graph, request, resources=list(), scheduler_resources=list()
) {
    configuration <- graph$definitions[[1L]]$site
    request_directory <- file.path(configuration$cdrgam_root, '.cdrgam', 'work', 'requests')
    if (!dir.exists(request_directory)) dir.create(request_directory, recursive=TRUE)
    request_file <- file.path(request_directory, paste0(request, '.rds'))
    payload <- .cdrgam_cli_pack_store_paths(list(
        request_id=request, graph=graph, resources=resources,
        submitted_at=.cdrgam_cli_timestamp(),
        execution=.cdrgam_cli_execution_envelope(graph$definitions[[1L]])
    ), configuration)
    .cdrgam_cli_atomic_write(request_file, function(path) {
        saveRDS(payload, path, version=3)
    })
    endpoint <- .cdrgam_cli_controller_read(configuration)
    contacted <- FALSE
    if (!is.null(endpoint)) {
        contacted <- !is.null(suppressWarnings(tryCatch({
            .cdrgam_cli_controller_call(endpoint, list(
                type='submit', request_file=request_file
            ), retries=2L)
        }, error=function(error) NULL)))
    }
    if (contacted && length(scheduler_resources)) {
        metadata_path <- file.path(
            configuration$cdrgam_root, '.cdrgam', 'scheduler.yml'
        )
        metadata <- tryCatch(
            .cdrgam_cli_read_yaml(metadata_path), error=function(error) list()
        )
        requested <- .cdrgam_cli_scheduler_resources(
            configuration, scheduler_resources
        )
        if (!identical(metadata$resources, requested)) {
            .cdrgam_cli_abort(paste(
                'A root scheduler is already active with different resources.',
                'Stop it before changing --partition or --scheduler-time.'
            ))
        }
    }
    scheduler <- if (contacted) NULL else .cdrgam_cli_submit_scheduler(
        configuration, overrides=scheduler_resources
    )
    message('Accepted root request ', request)
    invisible(list(
        status='accepted', request_id=request,
        scheduler_id=if (is.null(scheduler)) endpoint$job_id else scheduler$job_id
    ))
}

.cdrgam_cli_controller_bind <- function() {
    for (attempt in seq_len(100L)) {
        port <- sample.int(20000L, 1L) + 39999L
        server <- tryCatch(serverSocket(port), error=function(error) NULL)
        if (!is.null(server)) return(list(server=server, port=port))
    }
    .cdrgam_cli_abort('Could not allocate a scheduler TCP port')
}

# TRUE and FALSE are authoritative; NULL means Slurm could not answer.
.cdrgam_cli_slurm_active_many <- function(job_ids) {
    job_ids <- unique(as.character(job_ids))
    if (!length(job_ids)) return(stats::setNames(logical(), character()))
    squeue <- getOption('cdrgam.cli.squeue', Sys.which('squeue'))
    if (!length(squeue) || !nzchar(squeue)) return(NULL)
    arguments <- c('-h', '-j', paste(job_ids, collapse=','), '-o', '%i')
    result <- .cdrgam_cli_process_run(
        squeue, arguments, timeout=2000, cleanup_tree=TRUE
    )
    if (is.null(result) || !identical(result$status, 0L) || isTRUE(result$timeout)) {
        return(NULL)
    }
    active <- trimws(strsplit(result$stdout, '\n', fixed=TRUE)[[1L]])
    active <- active[nzchar(active)]
    stats::setNames(job_ids %in% active, job_ids)
}

# Returns the final Slurm state of each job, such as "OUT_OF_MEMORY", with NA
# for jobs that accounting does not report. NULL means sacct is unavailable.
.cdrgam_cli_slurm_exit_states <- function(job_ids) {
    job_ids <- unique(as.character(job_ids))
    if (!length(job_ids)) return(stats::setNames(character(), character()))
    sacct <- getOption('cdrgam.cli.sacct', Sys.which('sacct'))
    if (!length(sacct) || !nzchar(sacct)) return(NULL)
    result <- .cdrgam_cli_process_run(
        sacct,
        c('-n', '-X', '-P', '-j', paste(job_ids, collapse=','),
            '--format=JobID,State'),
        timeout=5000, cleanup_tree=TRUE
    )
    if (is.null(result) || !identical(result$status, 0L) || isTRUE(result$timeout)) {
        return(NULL)
    }
    states <- stats::setNames(rep(NA_character_, length(job_ids)), job_ids)
    for (line in strsplit(result$stdout, '\n', fixed=TRUE)[[1L]]) {
        fields <- strsplit(trimws(line), '|', fixed=TRUE)[[1L]]
        if (length(fields) < 2L || !(fields[[1L]] %in% job_ids)) next
        # Cancellations are reported as "CANCELLED by <uid>".
        states[[fields[[1L]]]] <- strsplit(fields[[2L]], ' ', fixed=TRUE)[[1L]][[1L]]
    }
    states
}

.cdrgam_cli_worker_exit_limit <- 3L

# Decides whether a worker that exited without publishing should fail its
# work item. Returns the failure message, or NULL to return the item to the
# queue. Deterministic job failures fail immediately; other exits, including
# preemption and unknown states, fail after repeated consecutive exits.
.cdrgam_cli_worker_exit_failure <- function(job_id, state, exits,
        limit=.cdrgam_cli_worker_exit_limit) {
    reason <- switch(
        .cdrgam_cli_null(state, NA_character_),
        OUT_OF_MEMORY=paste(
            'ran out of memory. Increase the Slurm memory request for this',
            'work or reduce the model before running it again.'
        ),
        TIMEOUT=paste(
            'reached its Slurm time limit. Increase the time limit before',
            'running it again.'
        ),
        FAILED='failed.',
        NULL
    )
    if (!is.null(reason)) {
        return(paste0(
            'The Slurm worker (job ', job_id, ') ', reason,
            ' The work was not requeued.'
        ))
    }
    if (exits < limit) return(NULL)
    paste0(
        'Slurm workers exited before publishing ', exits,
        ' consecutive times (last job ', job_id, ', state ',
        if (is.na(state)) 'unknown' else state, '). The work was not requeued.'
    )
}

.cdrgam_cli_slurm_active <- function(job_id) {
    active <- .cdrgam_cli_slurm_active_many(job_id)
    if (is.null(active)) NULL else unname(active[[1L]])
}

.cdrgam_cli_available_worker_resource_keys <- function(workers) {
    available <- workers[vapply(workers, function(worker) {
        is.null(worker$current_work_key) &&
            worker$status %in% c('submitted', 'idle')
    }, logical(1))]
    vapply(available, function(worker) worker$resource_key, character(1))
}

.cdrgam_cli_request_files <- function(request_directory) {
    if (!dir.exists(request_directory)) return(character())
    files <- list.files(
        request_directory, pattern='\\.rds$', full.names=TRUE
    )
    if (length(files) < 2L) return(files)
    modified <- file.info(files)$mtime
    files[order(modified, files, na.last=TRUE)]
}

.cdrgam_cli_controller_main <- function(root) {
    options(cdrgam.cli.root=root)
    configuration <- .cdrgam_cli_site(root, create_root=TRUE)
    bound <- .cdrgam_cli_controller_bind()
    on.exit(close(bound$server), add=TRUE)
    startup <- .cdrgam_cli_random_id('scheduler')
    advertised_host <- Sys.getenv('CDRGAM_CONTROLLER_HOST', .cdrgam_cli_host_name())
    endpoint <- list(
        schema=1L, host=advertised_host, port=bound$port,
        pid=Sys.getpid(), job_id=Sys.getenv('SLURM_JOB_ID', NA_character_),
        token=.cdrgam_cli_random_id('token'), startup=startup,
        process_started=.cdrgam_cli_process_started(),
        started_at=.cdrgam_cli_timestamp()
    )
    discovery <- .cdrgam_cli_controller_discovery(configuration)
    .cdrgam_cli_write_yaml(endpoint, discovery)
    Sys.chmod(discovery, mode='0600')
    scheduler_metadata <- file.path(configuration$cdrgam_root, '.cdrgam', 'scheduler.yml')
    if (file.exists(scheduler_metadata)) {
        metadata <- tryCatch(.cdrgam_cli_read_yaml(scheduler_metadata),
            error=function(error) list(schema=1L))
        metadata$state <- 'running'
        metadata$startup <- startup
        metadata$started_at <- endpoint$started_at
        .cdrgam_cli_write_yaml(metadata, scheduler_metadata)
    }
    on.exit({
        current <- tryCatch(.cdrgam_cli_read_yaml(discovery), error=function(error) NULL)
        if (!is.null(current) && identical(current$startup, startup)) unlink(discovery)
        if (file.exists(scheduler_metadata)) {
            metadata <- tryCatch(.cdrgam_cli_read_yaml(scheduler_metadata),
                error=function(error) list(schema=1L))
            if (is.null(metadata$startup) || identical(metadata$startup, startup)) {
                metadata$state <- 'stopped'
                metadata$stopped_at <- .cdrgam_cli_timestamp()
                .cdrgam_cli_write_yaml(metadata, scheduler_metadata)
            }
        }
    }, add=TRUE)
    .cdrgam_cli_registry_initialize(configuration)
    items <- list()
    contexts <- list()
    failed <- character()
    stopped <- character()
    # Consecutive worker exits without a published artifact, by work key.
    exit_counts <- list()
    shutdown <- FALSE
    workers <- .cdrgam_cli_registry_active_workers(configuration)
    known_request_files <- character()
    request_directory <- file.path(configuration$cdrgam_root, '.cdrgam', 'work', 'requests')

    ingest <- function(request_file) {
        submitted <- tryCatch(
            .cdrgam_cli_unpack_store_paths(readRDS(request_file), configuration),
            error=function(error) error
        )
        if (inherits(submitted, 'error') || is.null(submitted$graph$items)) {
            return(list(ok=FALSE, error=if (inherits(submitted, 'error')) {
                conditionMessage(submitted)
            } else 'Request contains no work graph'))
        }
        request_id <- .cdrgam_cli_null(
            submitted$request_id, tools::file_path_sans_ext(basename(request_file))
        )
        is_new <- !.cdrgam_cli_registry_has_request(configuration, request_id)
        if (is_new) {
            .cdrgam_cli_registry_record_graph(
                configuration, submitted$graph, request_file, request_id=request_id
            )
        }
        current_keys <- .cdrgam_cli_registry_current_keys(configuration)
        retained <- intersect(names(items), current_keys)
        items <<- items[retained]
        contexts <<- contexts[retained]
        failed <<- intersect(failed, retained)
        stopped <<- intersect(stopped, retained)
        request_keys <- intersect(
            names(submitted$graph$items),
            .cdrgam_cli_registry_request_keys(configuration, request_id)
        )
        for (key in request_keys) {
            item <- submitted$graph$items[[key]]
            items[[key]] <<- item
            contexts[[key]] <<- list(
                graph=submitted$graph, request_file=request_file,
                resources=.cdrgam_cli_resource_configuration(
                    configuration, submitted$resources
                )
            )
        }
        known_request_files <<- unique(c(known_request_files, request_file))
        states <- .cdrgam_cli_registry_states(configuration, request_keys)
        claimed <- vapply(workers, function(worker) {
            .cdrgam_cli_null(worker$current_work_key, '')
        }, character(1))
        orphaned <- setdiff(
            .cdrgam_cli_registry_active_attempt_keys(configuration, request_keys),
            claimed
        )
        if (length(orphaned)) {
            .cdrgam_cli_registry_reclaim_attempts(
                configuration, items, orphaned,
                'Scheduler lost the worker that owned this active attempt'
            )
            states[orphaned] <- 'pending'
        }
        failed <<- unique(c(
            setdiff(failed, request_keys),
            names(states)[states %in% c('failed', 'blocked')]
        ))
        # A new request for a stopped item resumes it; recording the request
        # set its registry state to pending.
        stopped <<- unique(c(
            setdiff(stopped, request_keys),
            names(states)[states == 'stopped']
        ))
        exit_counts <<- exit_counts[setdiff(names(exit_counts), request_keys)]
        list(ok=TRUE, request_id=request_id)
    }

    scan_requests <- function() {
        request_files <- .cdrgam_cli_request_files(request_directory)
        for (request_file in setdiff(request_files, known_request_files)) ingest(request_file)
    }
    scan_requests()
    last_activity <- Sys.time()
    last_reconcile <- Sys.time()
    last_scan <- Sys.time()

    item_complete <- function(key) {
        item <- items[[key]]
        !is.null(item) && .cdrgam_cli_complete_artifact(
            item$output, item$identity, verify_hashes=FALSE
        )
    }
    ready_keys <- function(resource_key=NULL) {
        assigned <- vapply(workers, function(worker) {
            .cdrgam_cli_null(worker$current_work_key, '')
        }, character(1))
        candidates <- setdiff(names(items), c(failed, stopped, assigned))
        candidates <- candidates[!vapply(candidates, item_complete, logical(1))]
        if (!is.null(resource_key)) candidates <- candidates[vapply(
            candidates,
            function(key) identical(
                .cdrgam_cli_resource_key(contexts[[key]]$resources), resource_key
            ), logical(1)
        )]
        candidates[vapply(candidates, function(key) {
            dependencies <- items[[key]]$dependencies
            length(dependencies) == 0L || all(vapply(dependencies, item_complete, logical(1)))
        }, logical(1))]
    }
    mark_failed <- function(key, error) {
        if (is.null(items[[key]])) return(invisible(FALSE))
        failed <<- unique(c(failed, key))
        .cdrgam_cli_registry_state(configuration, items[[key]], 'failed')
        .cdrgam_cli_registry_attempt_state(configuration, items[[key]], 'failed', error)
        .cdrgam_cli_registry_record_failure(configuration, items[[key]], error)
        invisible(TRUE)
    }

    repeat {
        ready_socket <- tryCatch(
            socketSelect(list(bound$server), timeout=0.2)[[1L]],
            error=function(error) FALSE
        )
        connection <- if (isTRUE(ready_socket)) tryCatch(
            socketAccept(bound$server, blocking=TRUE, open='a+b', timeout=5),
            error=function(error) NULL
        ) else NULL
        if (!is.null(connection)) {
            message <- tryCatch(unserialize(connection), error=function(error) NULL)
            response <- list(ok=TRUE)
            if (is.null(message) || !identical(message$token, endpoint$token)) {
                response <- list(ok=FALSE, error='Scheduler authentication failed')
            } else if (identical(message$type, 'ping')) {
                response$startup <- startup
            } else if (identical(message$type, 'submit')) {
                response <- ingest(message$request_file)
                response$ok <- isTRUE(response$ok)
                last_activity <- Sys.time()
            } else if (identical(message$type, 'claim')) {
                worker <- workers[[message$worker_id]]
                if (is.null(worker) || !identical(worker$resource_key, message$resource_key)) {
                    response <- list(ok=FALSE, error='Scheduler does not recognize this worker')
                } else {
                    candidates <- ready_keys(message$resource_key)
                    if (length(candidates)) {
                        key <- candidates[[1L]]
                        item <- items[[key]]
                        attempt <- tryCatch(.cdrgam_cli_attempt_directory(
                            contexts[[key]]$graph$definitions[[item$project]], item
                        )$path, error=identity)
                        if (inherits(attempt, 'error')) {
                            # A stale or malformed attempt must fail its work
                            # item, not the root scheduler that owns every
                            # other queued item.
                            mark_failed(key, conditionMessage(attempt))
                            response <- list(ok=FALSE, error=conditionMessage(attempt))
                        } else {
                            worker$status <- 'running'
                            worker$current_work_key <- key
                            worker$updated_at <- .cdrgam_cli_timestamp()
                            workers[[message$worker_id]] <- worker
                            submission <- list(
                                status='running', job_id=worker$job_id, path=attempt,
                                definitions=contexts[[key]]$graph$definitions[[item$project]]
                            )
                            .cdrgam_cli_registry_attempt(configuration, item, submission)
                            .cdrgam_cli_registry_state(configuration, item, 'running')
                            .cdrgam_cli_registry_worker(configuration, worker)
                            response$action <- 'run'
                            response$work_key <- key
                            response$request_file <- contexts[[key]]$request_file
                            response$attempt <- attempt
                        }
                    } else {
                        unfinished <- setdiff(names(items), c(failed, stopped))
                        unfinished <- unfinished[!vapply(unfinished, item_complete, logical(1))]
                        compatible <- unfinished[vapply(unfinished, function(key) {
                            identical(.cdrgam_cli_resource_key(
                                contexts[[key]]$resources
                            ), message$resource_key)
                        }, logical(1))]
                        other_ready <- ready_keys()
                        response$action <- if (length(compatible) && !length(other_ready)) {
                            'wait'
                        } else 'stop'
                        if (identical(response$action, 'wait')) response$seconds <- 1
                        worker$status <- if (identical(response$action, 'wait')) {
                            'idle'
                        } else 'stopping'
                        worker$current_work_key <- NULL
                        worker$updated_at <- .cdrgam_cli_timestamp()
                        workers[[message$worker_id]] <- worker
                        .cdrgam_cli_registry_worker(configuration, worker)
                    }
                    last_activity <- Sys.time()
                }
            } else if (identical(message$type, 'stop')) {
                keys <- intersect(as.character(message$work_keys), names(items))
                for (key in keys) {
                    item <- items[[key]]
                    # The message loop runs in this function's own frame, so
                    # scheduler state is updated with local assignment.
                    stopped <- unique(c(stopped, key))
                    .cdrgam_cli_registry_state(configuration, item, 'stopped')
                    .cdrgam_cli_registry_attempt_state(
                        configuration, item, 'stopped', 'Stopped by user'
                    )
                    worker_ids <- names(workers)[vapply(workers, function(worker) {
                        identical(worker$current_work_key, key)
                    }, logical(1))]
                    for (worker_id in worker_ids) {
                        workers[[worker_id]]$status <- 'stopping'
                        workers[[worker_id]]$updated_at <- .cdrgam_cli_timestamp()
                        .cdrgam_cli_registry_worker(configuration, workers[[worker_id]])
                    }
                }
                response$stopped <- keys
                last_activity <- Sys.time()
            } else if (identical(message$type, 'forget')) {
                keys <- intersect(as.character(message$work_keys), names(items))
                active <- keys[vapply(keys, function(key) any(vapply(
                    workers, function(worker) identical(worker$current_work_key, key),
                    logical(1)
                )), logical(1))]
                if (length(active)) {
                    response <- list(ok=FALSE, error=paste0(
                        'Cannot remove project work while active: ', active[[1L]]
                    ))
                } else {
                    retained <- setdiff(names(items), keys)
                    items <- items[retained]
                    contexts <- contexts[retained]
                    failed <- intersect(failed, retained)
                    stopped <- intersect(stopped, retained)
                    exit_counts <- exit_counts[intersect(names(exit_counts), retained)]
                    response$forgot <- keys
                }
                last_activity <- Sys.time()
            } else if (identical(message$type, 'shutdown')) {
                if (length(workers) && !isTRUE(message$force)) {
                    response <- list(ok=FALSE, error=paste(
                        'Cannot stop the root scheduler while workers are active;',
                        'stop work first or use --force'
                    ))
                } else {
                    shutdown <- TRUE
                }
            } else if (message$type %in% c('complete', 'failed')) {
                worker <- workers[[message$worker_id]]
                item <- items[[message$work_key]]
                matches_claim <- !is.null(worker) && !is.null(item) &&
                    identical(worker$current_work_key, message$work_key)
                already_recorded <- !is.null(worker) && !is.null(item) &&
                    is.null(worker$current_work_key) && (
                        identical(message$type, 'complete') && item_complete(message$work_key) ||
                        identical(message$type, 'failed') && message$work_key %in% failed
                    )
                if (!matches_claim && !already_recorded) {
                    response <- list(ok=FALSE, error='Worker completion does not match its claim')
                } else if (matches_claim) {
                    state <- message$type
                    if (!(item$key %in% stopped)) {
                        .cdrgam_cli_registry_state(configuration, item, state)
                        .cdrgam_cli_registry_attempt_state(
                            configuration, item, state,
                            if (identical(state, 'failed')) message$error else NULL
                        )
                        if (identical(state, 'failed')) {
                            failed <- unique(c(failed, item$key))
                        }
                    }
                    worker$status <- 'idle'
                    worker$current_work_key <- NULL
                    worker$updated_at <- .cdrgam_cli_timestamp()
                    workers[[message$worker_id]] <- worker
                    .cdrgam_cli_registry_worker(configuration, worker)
                    last_activity <- Sys.time()
                }
            } else if (identical(message$type, 'stopped')) {
                worker <- workers[[message$worker_id]]
                if (!is.null(worker)) {
                    worker$status <- 'stopped'
                    worker$current_work_key <- NULL
                    worker$updated_at <- .cdrgam_cli_timestamp()
                    .cdrgam_cli_registry_worker(configuration, worker)
                    workers[[message$worker_id]] <- NULL
                }
                last_activity <- Sys.time()
            } else {
                response <- list(ok=FALSE, error='Unknown scheduler message')
            }
            try(serialize(response, connection), silent=TRUE)
            try(flush(connection), silent=TRUE)
            close(connection)
        }

        if (shutdown) break

        if (as.numeric(difftime(Sys.time(), last_scan, units='secs')) >= 1) {
            scan_requests()
            last_scan <- Sys.time()
        }
        if (length(items) &&
                as.numeric(difftime(Sys.time(), last_reconcile, units='secs')) >= 2) {
            # The registry is authoritative for stops. A forced stop that the
            # scheduler never received is recorded only there.
            registry_states <- .cdrgam_cli_registry_states(configuration, names(items))
            stopped <- unique(c(
                stopped, names(registry_states)[registry_states == 'stopped']
            ))
        }
        if (length(workers) &&
                as.numeric(difftime(Sys.time(), last_reconcile, units='secs')) >= 2) {
            active_jobs <- .cdrgam_cli_slurm_active_many(vapply(
                workers, function(worker) worker$job_id, character(1)
            ))
            exited <- names(workers)[vapply(names(workers), function(worker_id) {
                identical(unname(active_jobs[as.character(
                    workers[[worker_id]]$job_id
                )]), FALSE)
            }, logical(1))]
            exit_states <- if (length(exited)) .cdrgam_cli_slurm_exit_states(
                vapply(workers[exited], function(worker) worker$job_id, character(1))
            ) else NULL
            for (worker_id in exited) {
                worker <- workers[[worker_id]]
                published <- FALSE
                key <- worker$current_work_key
                if (!is.null(key)) {
                    if (item_complete(key)) {
                        published <- TRUE
                        item <- items[[key]]
                        .cdrgam_cli_registry_state(configuration, item, 'complete')
                        .cdrgam_cli_registry_attempt_state(
                            configuration, item, 'complete'
                        )
                        exit_counts[[key]] <- NULL
                    } else if (key %in% stopped) {
                        # The user stopped this item; its cancelled worker must
                        # not return it to the queue.
                        exit_counts[[key]] <- NULL
                    } else {
                        exit_state <- if (is.null(exit_states)) NA_character_ else {
                            unname(exit_states[as.character(worker$job_id)])
                        }
                        exit_counts[[key]] <- .cdrgam_cli_null(
                            exit_counts[[key]], 0L
                        ) + 1L
                        failure <- .cdrgam_cli_worker_exit_failure(
                            worker$job_id, exit_state, exit_counts[[key]]
                        )
                        if (is.null(failure)) {
                            .cdrgam_cli_registry_reclaim_attempts(
                                configuration, items, key,
                                paste(
                                    'Slurm worker exited before publishing its claimed',
                                    'artifact; the unfinished work was returned to the queue'
                                )
                            )
                        } else {
                            mark_failed(key, failure)
                            exit_counts[[key]] <- NULL
                        }
                    }
                }
                worker$status <- if (published) 'stopped' else 'failed'
                worker$updated_at <- .cdrgam_cli_timestamp()
                .cdrgam_cli_registry_worker(configuration, worker)
                workers[[worker_id]] <- NULL
                last_activity <- Sys.time()
            }
        }
        if (as.numeric(difftime(Sys.time(), last_reconcile, units='secs')) >= 2) {
            last_reconcile <- Sys.time()
        }

        repeat {
            blocked <- setdiff(names(items), c(failed, stopped))
            blocked <- blocked[!vapply(blocked, item_complete, logical(1))]
            blocked <- blocked[vapply(blocked, function(key) {
                any(items[[key]]$dependencies %in% c(failed, stopped))
            }, logical(1))]
            if (!length(blocked)) break
            for (key in blocked) {
                failed <- unique(c(failed, key))
                .cdrgam_cli_registry_state(configuration, items[[key]], 'blocked')
            }
        }

        capacity <- configuration$concurrency - length(workers)
        if (capacity > 0L) {
            ready <- ready_keys()
            if (length(ready)) {
                resource_keys <- vapply(ready, function(key) {
                    .cdrgam_cli_resource_key(contexts[[key]]$resources)
                }, character(1))
                available_keys <- .cdrgam_cli_available_worker_resource_keys(workers)
                desired <- table(resource_keys)
                present <- table(available_keys)
                launch <- character()
                for (resource_key in names(desired)) {
                    active_count <- if (resource_key %in% names(present)) {
                        present[[resource_key]]
                    } else 0L
                    count <- max(0L, desired[[resource_key]] - active_count)
                    launch <- c(launch, rep(resource_key, count))
                }
                for (resource_key in utils::head(launch, capacity)) {
                    key <- ready[match(resource_key, resource_keys)]
                    worker <- tryCatch(.cdrgam_cli_submit_worker(
                        configuration, endpoint, resource_key, contexts[[key]]$resources
                    ), error=function(error) error)
                    if (inherits(worker, 'error')) {
                        mark_failed(key, paste0(
                            'Could not submit a Slurm worker: ', conditionMessage(worker)
                        ))
                    } else {
                        workers[[worker$worker_id]] <- worker
                        .cdrgam_cli_registry_worker(configuration, worker)
                    }
                    last_activity <- Sys.time()
                }
            }
        }

        Sys.sleep(0.1)
    }
    invisible(TRUE)
}
