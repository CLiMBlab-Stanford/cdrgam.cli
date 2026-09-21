.cdrgam_cli_sql_quote <- function(value) {
    if (is.null(value) || length(value) == 0L || is.na(value)) return('NULL')
    paste0("'", gsub("'", "''", as.character(value), fixed=TRUE), "'")
}

.cdrgam_cli_registry_path <- function(configuration) {
    file.path(configuration$cdrgam_root, '.cdrgam', 'registry.sqlite3')
}

.cdrgam_cli_registry_connect <- function(configuration) {
    path <- .cdrgam_cli_registry_path(configuration)
    if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive=TRUE)
    connection <- DBI::dbConnect(RSQLite::SQLite(), path)
    DBI::dbExecute(connection, 'PRAGMA journal_mode=DELETE')
    DBI::dbExecute(connection, 'PRAGMA busy_timeout=5000')
    DBI::dbExecute(connection, 'PRAGMA foreign_keys=ON')
    connection
}

.cdrgam_cli_registry_exec <- function(configuration, sql, query=FALSE) {
    connection <- .cdrgam_cli_registry_connect(configuration)
    on.exit(DBI::dbDisconnect(connection), add=TRUE)
    if (query) DBI::dbGetQuery(connection, sql) else DBI::dbExecute(connection, sql)
}

.cdrgam_cli_registry_initialize <- function(configuration) {
    connection <- .cdrgam_cli_registry_connect(configuration)
    on.exit(DBI::dbDisconnect(connection), add=TRUE)
    statements <- c(
        paste(
            'CREATE TABLE IF NOT EXISTS requests (',
            'request_id TEXT PRIMARY KEY, submitted_at TEXT NOT NULL,',
            'selector TEXT NOT NULL)'
        ),
        paste(
            'CREATE TABLE IF NOT EXISTS work_items (',
            'work_key TEXT PRIMARY KEY, project TEXT NOT NULL, kind TEXT NOT NULL,',
            'name TEXT NOT NULL, identity TEXT NOT NULL, state TEXT NOT NULL,',
            'artifact_path TEXT NOT NULL, updated_at TEXT NOT NULL)'
        ),
        paste(
            'CREATE TABLE IF NOT EXISTS request_items (',
            'request_id TEXT NOT NULL, work_key TEXT NOT NULL, target INTEGER NOT NULL,',
            'PRIMARY KEY (request_id, work_key))'
        ),
        paste(
            'CREATE TABLE IF NOT EXISTS dependencies (',
            'work_key TEXT NOT NULL, dependency_key TEXT NOT NULL,',
            'PRIMARY KEY (work_key, dependency_key))'
        ),
        paste(
            'CREATE TABLE IF NOT EXISTS attempts (',
            'attempt_id TEXT PRIMARY KEY, work_key TEXT NOT NULL, state TEXT NOT NULL,',
            'scheduler_id TEXT, path TEXT NOT NULL, started_at TEXT, completed_at TEXT,',
            'error TEXT)'
        ),
        paste(
            'CREATE TABLE IF NOT EXISTS workers (',
            'worker_id TEXT PRIMARY KEY, scheduler_id TEXT, resource_key TEXT NOT NULL,',
            'state TEXT NOT NULL, path TEXT NOT NULL, current_work_key TEXT,',
            'submitted_at TEXT, updated_at TEXT NOT NULL, error TEXT)'
        ),
        paste(
            'CREATE UNIQUE INDEX IF NOT EXISTS one_active_attempt',
            "ON attempts(work_key) WHERE state IN ('submitting','submitted','running')"
        ),
        paste(
            'CREATE UNIQUE INDEX IF NOT EXISTS one_current_workload',
            'ON work_items(project,kind,name)'
        )
    )
    for (statement in statements) DBI::dbExecute(connection, statement)
    invisible(.cdrgam_cli_registry_path(configuration))
}

.cdrgam_cli_registry_placeholders <- function(values) {
    paste(rep.int('?', length(values)), collapse=',')
}

.cdrgam_cli_registry_remove_attempt_paths <- function(configuration, paths, keep=NULL) {
    if (!length(paths)) return(invisible(character()))
    root <- .cdrgam_cli_normalize_path(configuration$cdrgam_root, must_work=TRUE)
    keep <- if (is.null(keep)) character() else {
        .cdrgam_cli_normalize_path(keep, must_work=FALSE)
    }
    projects <- file.path(root, 'projects')
    removed <- character()
    for (path in unique(paths[!is.na(paths) & nzchar(paths)])) {
        path <- .cdrgam_cli_managed_resolve(
            configuration, path, field='registry attempt path', must_work=FALSE
        )
        project_work <- .cdrgam_cli_within(path, projects) && grepl(
            '/.cdrgam/work/', path, fixed=TRUE
        )
        if (path %in% keep || !project_work) next
        unlink(path, recursive=TRUE, force=TRUE)
        if (!file.exists(path)) {
            removed <- c(removed, path)
        } else {
            warning('Could not remove superseded attempt ', sQuote(path))
        }
    }
    invisible(removed)
}

.cdrgam_cli_registry_superseded <- function(connection, graph) {
    existing <- DBI::dbGetQuery(connection, paste(
        'SELECT work_key,project,kind,name FROM work_items'
    ))
    if (!nrow(existing) || !length(graph$items)) return(character())
    logical_keys <- vapply(graph$items, function(item) {
        paste(item$project, item$kind, item$name, sep='\034')
    }, character(1))
    if (anyDuplicated(logical_keys)) {
        .cdrgam_cli_abort('A work graph contains multiple identities for one workload')
    }
    incoming <- stats::setNames(names(graph$items), logical_keys)
    existing_logical <- paste(existing$project, existing$kind, existing$name, sep='\034')
    matched <- match(existing_logical, names(incoming))
    replace <- !is.na(matched)
    replace[replace] <- existing$work_key[replace] != unname(incoming[matched[replace]])
    old <- existing$work_key[replace]
    if (!length(old)) return(character())
    dependencies <- DBI::dbGetQuery(
        connection, 'SELECT work_key,dependency_key FROM dependencies'
    )
    superseded <- unique(old)
    repeat {
        downstream <- dependencies$work_key[dependencies$dependency_key %in% superseded]
        expanded <- unique(c(superseded, downstream))
        if (length(expanded) == length(superseded)) break
        superseded <- expanded
    }
    superseded
}

.cdrgam_cli_registry_has_request <- function(configuration, request_id) {
    .cdrgam_cli_registry_initialize(configuration)
    result <- .cdrgam_cli_registry_exec(configuration, paste0(
        'SELECT COUNT(*) AS n FROM requests WHERE request_id=',
        .cdrgam_cli_sql_quote(request_id)
    ), query=TRUE)
    result$n[[1L]] > 0L
}

.cdrgam_cli_registry_record_graph <- function(
        configuration, graph, selector, request_id=NULL
) {
    .cdrgam_cli_registry_initialize(configuration)
    connection <- .cdrgam_cli_registry_connect(configuration)
    on.exit(DBI::dbDisconnect(connection), add=TRUE)
    request_id <- .cdrgam_cli_null(request_id, .cdrgam_cli_random_id('request'))
    removed_paths <- DBI::dbWithTransaction(connection, {
        removed <- character()
        superseded <- .cdrgam_cli_registry_superseded(connection, graph)
        if (length(superseded)) {
            placeholders <- .cdrgam_cli_registry_placeholders(superseded)
            active <- DBI::dbGetQuery(connection, paste0(
                'SELECT work_key FROM attempts WHERE work_key IN (', placeholders,
                ") AND state IN ('submitting','submitted','running')"
            ), params=as.list(superseded))
            if (nrow(active)) .cdrgam_cli_abort(paste0(
                'Cannot replace an active workload: ', active$work_key[[1L]]
            ))
            removed <- DBI::dbGetQuery(connection, paste0(
                'SELECT path FROM attempts WHERE work_key IN (', placeholders, ')'
            ), params=as.list(superseded))$path
            DBI::dbExecute(connection, paste0(
                'DELETE FROM request_items WHERE work_key IN (', placeholders, ')'
            ), params=as.list(superseded))
            DBI::dbExecute(connection, paste0(
                'DELETE FROM dependencies WHERE work_key IN (', placeholders,
                ') OR dependency_key IN (', placeholders, ')'
            ), params=c(as.list(superseded), as.list(superseded)))
            DBI::dbExecute(connection, paste0(
                'DELETE FROM attempts WHERE work_key IN (', placeholders, ')'
            ), params=as.list(superseded))
            DBI::dbExecute(connection, paste0(
                'DELETE FROM work_items WHERE work_key IN (', placeholders, ')'
            ), params=as.list(superseded))
        }
        DBI::dbExecute(connection,
            'INSERT INTO requests VALUES (?, ?, ?)',
            params=list(request_id, .cdrgam_cli_timestamp(), selector)
        )
        for (item in graph$items) {
            state <- if (.cdrgam_cli_complete_artifact(item$output, item$identity)) {
                'complete'
            } else 'pending'
            DBI::dbExecute(connection, paste(
                'INSERT INTO work_items VALUES (?, ?, ?, ?, ?, ?, ?, ?)',
                'ON CONFLICT(work_key) DO UPDATE SET project=excluded.project,',
                'kind=excluded.kind, name=excluded.name, identity=excluded.identity,',
                'state=excluded.state,',
                'artifact_path=excluded.artifact_path, updated_at=excluded.updated_at'
            ), params=list(
                item$key, item$project, item$kind, item$name, item$identity, state,
                .cdrgam_cli_project_relative(
                    graph$definitions[[item$project]], item$output
                ),
                .cdrgam_cli_timestamp()
            ))
            DBI::dbExecute(connection,
                'INSERT OR IGNORE INTO request_items VALUES (?, ?, ?)',
                params=list(request_id, item$key, as.integer(item$key %in% graph$targets))
            )
            for (dependency in item$dependencies) DBI::dbExecute(connection,
                'INSERT OR IGNORE INTO dependencies VALUES (?, ?)',
                params=list(item$key, dependency)
            )
        }
        removed
    })
    .cdrgam_cli_registry_remove_attempt_paths(configuration, removed_paths)
    request_id
}

.cdrgam_cli_registry_request_keys <- function(configuration, request_id) {
    output <- .cdrgam_cli_registry_exec(configuration, paste0(
        'SELECT work_key FROM request_items WHERE request_id=',
        .cdrgam_cli_sql_quote(request_id)
    ), query=TRUE)
    output$work_key
}

.cdrgam_cli_registry_current_keys <- function(configuration) {
    .cdrgam_cli_registry_exec(
        configuration, 'SELECT work_key FROM work_items', query=TRUE
    )$work_key
}

.cdrgam_cli_registry_states <- function(configuration, work_keys) {
    if (!length(work_keys)) return(stats::setNames(character(), character()))
    output <- .cdrgam_cli_registry_exec(configuration, paste0(
        'SELECT work_key,state FROM work_items WHERE work_key IN (',
        paste(vapply(work_keys, .cdrgam_cli_sql_quote, character(1)), collapse=','),
        ')'
    ), query=TRUE)
    stats::setNames(output$state, output$work_key)
}

.cdrgam_cli_registry_state <- function(configuration, item, state) {
    .cdrgam_cli_registry_exec(configuration, paste0(
        'UPDATE work_items SET state=', .cdrgam_cli_sql_quote(state),
        ',updated_at=', .cdrgam_cli_sql_quote(.cdrgam_cli_timestamp()),
        ' WHERE work_key=', .cdrgam_cli_sql_quote(item$key)
    ))
    invisible(state)
}

.cdrgam_cli_registry_attempt <- function(configuration, item, submission) {
    connection <- .cdrgam_cli_registry_connect(configuration)
    on.exit(DBI::dbDisconnect(connection), add=TRUE)
    attempt_id <- paste(item$key, basename(submission$path), sep=':')
    now <- .cdrgam_cli_timestamp()
    removed_paths <- DBI::dbWithTransaction(connection, {
        removed <- DBI::dbGetQuery(connection, paste(
            'SELECT path FROM attempts WHERE work_key=? AND attempt_id<>?'
        ), params=list(item$key, attempt_id))$path
        DBI::dbExecute(connection,
            'DELETE FROM attempts WHERE work_key=? AND attempt_id<>?',
            params=list(item$key, attempt_id)
        )
        DBI::dbExecute(connection, paste(
            'INSERT INTO attempts',
            '(attempt_id,work_key,state,scheduler_id,path,started_at)',
            'VALUES (?,?,?,?,?,?)',
            'ON CONFLICT(attempt_id) DO UPDATE SET',
            'state=excluded.state, scheduler_id=excluded.scheduler_id,',
            'path=excluded.path, completed_at=NULL, error=NULL'
        ), params=list(
            attempt_id, item$key, submission$status,
            .cdrgam_cli_null(submission$job_id, NA_character_),
            .cdrgam_cli_project_relative(submission$definitions, submission$path),
            now
        ))
        removed
    })
    .cdrgam_cli_registry_remove_attempt_paths(
        configuration, removed_paths, keep=submission$path
    )
    invisible(attempt_id)
}

.cdrgam_cli_registry_attempt_state <- function(
        configuration, item, state, error=NULL
) {
    connection <- .cdrgam_cli_registry_connect(configuration)
    on.exit(DBI::dbDisconnect(connection), add=TRUE)
    DBI::dbExecute(connection, paste(
        'UPDATE attempts SET state=?, completed_at=?, error=?',
        "WHERE work_key=? AND state IN ('submitting','submitted','running')"
    ), params=list(
        state,
        if (state %in% c('complete', 'failed')) .cdrgam_cli_timestamp() else NA_character_,
        .cdrgam_cli_null(error, NA_character_), item$key
    ))
    invisible(state)
}

.cdrgam_cli_registry_worker <- function(configuration, worker) {
    .cdrgam_cli_registry_initialize(configuration)
    .cdrgam_cli_registry_exec(configuration, paste0(
        'INSERT INTO workers ',
        '(worker_id,scheduler_id,resource_key,state,path,current_work_key,',
        'submitted_at,updated_at,error) VALUES (',
        paste(vapply(list(
            worker$worker_id,
            .cdrgam_cli_null(worker$job_id, NA_character_),
            worker$resource_key,
            worker$status,
            .cdrgam_cli_store_relative(configuration, worker$path),
            .cdrgam_cli_null(worker$current_work_key, NA_character_),
            .cdrgam_cli_null(worker$submitted_at, NA_character_),
            .cdrgam_cli_null(worker$updated_at, .cdrgam_cli_timestamp()),
            .cdrgam_cli_null(worker$error, NA_character_)
        ), .cdrgam_cli_sql_quote, character(1)), collapse=','),
        ') ON CONFLICT(worker_id) DO UPDATE SET ',
        'scheduler_id=excluded.scheduler_id, resource_key=excluded.resource_key, ',
        'state=excluded.state, path=excluded.path, ',
        'current_work_key=excluded.current_work_key, ',
        'updated_at=excluded.updated_at, error=excluded.error'
    ))
    invisible(worker$worker_id)
}
