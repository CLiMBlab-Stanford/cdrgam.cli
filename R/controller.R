.cdrgam_cli_controller_discovery <- function(configuration) {
    file.path(configuration$cdrgam_root, '.cdrgam', 'controller.yml')
}

.cdrgam_cli_controller_read <- function(configuration) {
    path <- .cdrgam_cli_controller_discovery(configuration)
    if (!file.exists(path)) return(NULL)
    value <- tryCatch(.cdrgam_cli_read_yaml(path), error=function(error) NULL)
    required <- c('host', 'port', 'pid', 'token', 'startup')
    if (is.null(value) || !all(required %in% names(value))) return(NULL)
    if (identical(value$host, unname(Sys.info()[['nodename']])) &&
            !file.exists(file.path('/proc', value$pid))) return(NULL)
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

.cdrgam_cli_controller_submit <- function(graph, request, resources=list()) {
    configuration <- graph$definitions[[1L]]$checkout
    request_directory <- file.path(configuration$cdrgam_root, '.cdrgam', 'work', 'requests')
    if (!dir.exists(request_directory)) dir.create(request_directory, recursive=TRUE)
    request_file <- file.path(request_directory, paste0(request, '.rds'))
    payload <- .cdrgam_cli_pack_store_paths(list(
        request_id=request, graph=graph, resources=resources,
        execution=.cdrgam_cli_execution_envelope(graph$definitions[[1L]])
    ), configuration)
    .cdrgam_cli_atomic_write(request_file, function(path) {
        saveRDS(payload, path, version=3)
    })
    endpoint <- .cdrgam_cli_controller_read(configuration)
    contacted <- FALSE
    if (!is.null(endpoint)) {
        contacted <- !is.null(tryCatch({
            .cdrgam_cli_controller_call(endpoint, list(
                type='submit', request_file=request_file
            ), retries=2L)
        }, error=function(error) NULL))
    }
    scheduler <- if (contacted) NULL else .cdrgam_cli_submit_scheduler(configuration)
    message('Accepted checkout request ', request)
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
    if (!length(job_ids)) return(setNames(logical(), character()))
    squeue <- getOption('cdrgam.cli.squeue', Sys.which('squeue'))
    if (!length(squeue) || !nzchar(squeue)) return(NULL)
    timeout <- getOption('cdrgam.cli.timeout', Sys.which('timeout'))
    if (!length(timeout) || !nzchar(timeout)) return(NULL)
    arguments <- c('-h', '-j', paste(job_ids, collapse=','), '-o', '%i')
    arguments <- c('2s', squeue, arguments)
    output <- suppressWarnings(system2(
        timeout, arguments,
        stdout=TRUE, stderr=TRUE
    ))
    if (!identical(.cdrgam_cli_null(attr(output, 'status'), 0L), 0L)) return(NULL)
    active <- trimws(output)
    setNames(job_ids %in% active, job_ids)
}

.cdrgam_cli_slurm_active <- function(job_id) {
    active <- .cdrgam_cli_slurm_active_many(job_id)
    if (is.null(active)) NULL else unname(active[[1L]])
}

.cdrgam_cli_controller_main <- function(checkout) {
    options(cdrgam.cli.checkout=checkout)
    configuration <- .cdrgam_cli_checkout(checkout, create_root=TRUE)
    bound <- .cdrgam_cli_controller_bind()
    on.exit(close(bound$server), add=TRUE)
    startup <- .cdrgam_cli_random_id('scheduler')
    advertised_host <- Sys.getenv(
        'CDRGAM_CONTROLLER_HOST', unname(Sys.info()[['nodename']])
    )
    endpoint <- list(
        schema=1L, host=advertised_host, port=bound$port,
        pid=Sys.getpid(), job_id=Sys.getenv('SLURM_JOB_ID', NA_character_),
        token=.cdrgam_cli_random_id('token'), startup=startup,
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
    workers <- list()
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
        if (!is_new) {
            interrupted <- names(states)[states %in% c('submitting', 'submitted', 'running')]
            interrupted <- interrupted[!vapply(interrupted, function(key) {
                item <- items[[key]]
                .cdrgam_cli_complete_artifact(item$output, item$identity)
            }, logical(1))]
            for (key in interrupted) {
                .cdrgam_cli_registry_state(configuration, items[[key]], 'failed')
                .cdrgam_cli_registry_attempt_state(
                    configuration, items[[key]], 'failed',
                    'Scheduler restarted while the work item was active'
                )
                states[[key]] <- 'failed'
            }
        }
        failed <<- unique(c(
            setdiff(failed, request_keys),
            names(states)[states %in% c('failed', 'blocked')]
        ))
        list(ok=TRUE, request_id=request_id)
    }

    scan_requests <- function() {
        request_files <- if (dir.exists(request_directory)) {
            list.files(request_directory, pattern='\\.rds$', full.names=TRUE)
        } else character()
        for (request_file in setdiff(request_files, known_request_files)) ingest(request_file)
    }
    scan_requests()
    last_activity <- Sys.time()
    last_reconcile <- Sys.time()
    last_scan <- Sys.time()

    item_complete <- function(key) {
        item <- items[[key]]
        !is.null(item) && .cdrgam_cli_complete_artifact(item$output, item$identity)
    }
    ready_keys <- function(resource_key=NULL) {
        assigned <- vapply(workers, function(worker) {
            .cdrgam_cli_null(worker$current_work_key, '')
        }, character(1))
        candidates <- setdiff(names(items), c(failed, assigned))
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
                        attempt <- .cdrgam_cli_attempt_directory(
                            contexts[[key]]$graph$definitions[[item$project]], item
                        )$path
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
                    } else {
                        unfinished <- setdiff(names(items), failed)
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
                        worker$status <- if (identical(response$action, 'wait')) 'idle' else 'stopping'
                        worker$current_work_key <- NULL
                        worker$updated_at <- .cdrgam_cli_timestamp()
                        workers[[message$worker_id]] <- worker
                        .cdrgam_cli_registry_worker(configuration, worker)
                    }
                    last_activity <- Sys.time()
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
                    .cdrgam_cli_registry_state(configuration, item, state)
                    .cdrgam_cli_registry_attempt_state(
                        configuration, item, state,
                        if (identical(state, 'failed')) message$error else NULL
                    )
                    if (identical(state, 'failed')) failed <- unique(c(failed, item$key))
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

        if (as.numeric(difftime(Sys.time(), last_scan, units='secs')) >= 1) {
            scan_requests()
            last_scan <- Sys.time()
        }
        if (length(workers) &&
                as.numeric(difftime(Sys.time(), last_reconcile, units='secs')) >= 2) {
            active_jobs <- .cdrgam_cli_slurm_active_many(vapply(
                workers, function(worker) worker$job_id, character(1)
            ))
            for (worker_id in names(workers)) {
                worker <- workers[[worker_id]]
                active <- if (is.null(active_jobs)) NULL else {
                    unname(active_jobs[[as.character(worker$job_id)]])
                }
                if (identical(active, FALSE)) {
                    published <- FALSE
                    if (!is.null(worker$current_work_key)) {
                        if (item_complete(worker$current_work_key)) {
                            published <- TRUE
                            item <- items[[worker$current_work_key]]
                            .cdrgam_cli_registry_state(configuration, item, 'complete')
                            .cdrgam_cli_registry_attempt_state(
                                configuration, item, 'complete'
                            )
                        } else {
                            mark_failed(
                                worker$current_work_key,
                                'Slurm worker disappeared before publishing its claimed artifact'
                            )
                        }
                    } else {
                        unclaimed <- ready_keys(worker$resource_key)
                        if (length(unclaimed)) mark_failed(
                            unclaimed[[1L]],
                            'Slurm worker disappeared before claiming ready work'
                        )
                    }
                    worker$status <- if (published) 'stopped' else 'failed'
                    worker$updated_at <- .cdrgam_cli_timestamp()
                    .cdrgam_cli_registry_worker(configuration, worker)
                    workers[[worker_id]] <- NULL
                    last_activity <- Sys.time()
                }
            }
            last_reconcile <- Sys.time()
        }

        repeat {
            blocked <- setdiff(names(items), failed)
            blocked <- blocked[!vapply(blocked, item_complete, logical(1))]
            blocked <- blocked[vapply(blocked, function(key) {
                any(items[[key]]$dependencies %in% failed)
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
                active_keys <- vapply(workers, function(worker) worker$resource_key, character(1))
                desired <- table(resource_keys)
                present <- table(active_keys)
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

        unfinished <- setdiff(names(items), failed)
        unfinished <- unfinished[!vapply(unfinished, item_complete, logical(1))]
        if (!length(workers) && !length(unfinished) &&
                as.numeric(difftime(Sys.time(), last_activity, units='secs')) > 5) {
            unlink(known_request_files[file.exists(known_request_files)])
            break
        }
        Sys.sleep(0.1)
    }
    invisible(TRUE)
}
