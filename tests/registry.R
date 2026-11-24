library(cdrgam.cli)

internal <- function(name) getFromNamespace(name, 'cdrgam.cli')
temporary <- tempfile('cdrgam-cli-registry-')
dir.create(temporary)
on.exit(unlink(temporary, recursive=TRUE), add=TRUE)

store <- file.path(temporary, 'store')
cdrgam_cli_configure(store)
project <- file.path(store, 'projects', 'example')
dir.create(file.path(project, 'definitions'), recursive=TRUE)
configuration <- internal('.cdrgam_cli_site')(store)
request_order_directory <- file.path(store, '.cdrgam', 'work', 'request-order')
dir.create(request_order_directory, recursive=TRUE)
older_request <- file.path(request_order_directory, 'z-old.rds')
newer_request <- file.path(request_order_directory, 'a-new.rds')
saveRDS(list(), older_request)
saveRDS(list(), newer_request)
request_time <- Sys.time()
Sys.setFileTime(older_request, request_time - 10)
Sys.setFileTime(newer_request, request_time)
stopifnot(identical(
    internal('.cdrgam_cli_request_files')(request_order_directory),
    c(older_request, newer_request)
))
project_definition <- list(project=list(
    schema=1L, id='project-registry-test', name='example'
))
yaml::write_yaml(
    project_definition,
    file.path(project, 'definitions', 'project.yml')
)
definitions <- list(
    root=project,
    site=configuration,
    project=project_definition
)

key <- 'fit:project-registry-test:main:test-identity'
output <- file.path(project, 'results', 'models', 'main')
item <- list(
    key=key,
    project='example',
    kind='fit',
    name='main',
    identity='test-identity',
    output=output,
    dependencies=character()
)
graph <- list(
    items=stats::setNames(list(item), key),
    targets=key,
    definitions=list(example=definitions)
)
internal('.cdrgam_cli_registry_record_graph')(
    configuration, graph, 'registry-contract-test'
)

attempt <- file.path(project, '.cdrgam', 'work', 'fit_main', 'attempt')
dir.create(attempt, recursive=TRUE)
internal('.cdrgam_cli_registry_attempt')(
    configuration,
    item,
    list(
        status='failed',
        job_id=NULL,
        path=attempt,
        definitions=definitions
    )
)

internal('.cdrgam_cli_registry_attempt')(
    configuration,
    item,
    list(
        status='running', job_id='12345', path=attempt,
        definitions=definitions
    )
)
internal('.cdrgam_cli_registry_state')(configuration, item, 'running')
internal('.cdrgam_cli_registry_record_graph')(
    configuration, graph, 'registry-contract-repeat'
)
preserved <- internal('.cdrgam_cli_registry_states')(configuration, key)[[key]]
active <- internal('.cdrgam_cli_registry_active_attempt_keys')(configuration, key)
jobs <- internal('.cdrgam_cli_registry_active_job_ids')(configuration, key)
stopifnot(
    identical(preserved, 'running'), identical(active, key), identical(jobs, '12345')
)
active_worker <- list(
    worker_id='active-worker', job_id='67890', resource_key='test',
    status='running', path=file.path(store, '.cdrgam', 'workers', 'active-worker'),
    current_work_key=key, submitted_at=as.character(Sys.time())
)
internal('.cdrgam_cli_registry_worker')(configuration, active_worker)
stopifnot(identical(
    internal('.cdrgam_cli_registry_active_worker_job_ids')(configuration),
    '67890'
))
restored_workers <- internal('.cdrgam_cli_registry_active_workers')(configuration)
stopifnot(
    identical(names(restored_workers), 'active-worker'),
    identical(restored_workers[[1L]]$job_id, '67890'),
    identical(restored_workers[[1L]]$current_work_key, key),
    identical(
        restored_workers[[1L]]$path,
        internal('.cdrgam_cli_normalize_path')(active_worker$path, must_work=FALSE)
    )
)
active_worker$status <- 'stopped'
internal('.cdrgam_cli_registry_worker')(configuration, active_worker)
stopifnot(!length(
    internal('.cdrgam_cli_registry_active_worker_job_ids')(configuration)
))
internal('.cdrgam_cli_registry_reclaim_attempts')(
    configuration, graph$items, key, 'test orphaned worker'
)
reclaimed <- internal('.cdrgam_cli_registry_states')(configuration, key)[[key]]
reclaimed_connection <- internal('.cdrgam_cli_registry_connect')(configuration)
reclaimed_attempt <- DBI::dbGetQuery(
    reclaimed_connection,
    'SELECT state,error FROM attempts WHERE work_key=? ORDER BY attempt_id DESC LIMIT 1',
    params=list(key)
)
DBI::dbDisconnect(reclaimed_connection)
stopifnot(
    identical(reclaimed, 'pending'),
    identical(reclaimed_attempt$state[[1L]], 'failed'),
    identical(reclaimed_attempt$error[[1L]], 'test orphaned worker')
)

dependent_key <- 'prediction:project-registry-test:main_validation:test-identity'
dependent <- list(
    key=dependent_key,
    project='example',
    kind='prediction',
    name='main_validation',
    identity='test-identity',
    output=file.path(output, 'predictions', 'validation'),
    dependencies=key
)
dependency_graph <- list(
    items=stats::setNames(list(item, dependent), c(key, dependent_key)),
    targets=dependent_key,
    definitions=list(example=definitions)
)
internal('.cdrgam_cli_registry_record_graph')(
    configuration, dependency_graph, 'registry-dependency-status'
)
internal('.cdrgam_cli_registry_state')(configuration, item, 'complete')
states <- internal('.cdrgam_cli_registry_dependency_states')(configuration)
stopifnot(identical(states[[dependent_key]], 'queued'))
internal('.cdrgam_cli_registry_state')(configuration, item, 'failed')
states <- internal('.cdrgam_cli_registry_dependency_states')(configuration)
stopifnot(identical(states[[dependent_key]], 'blocked'))
internal('.cdrgam_cli_registry_state')(configuration, item, 'pending')
states <- internal('.cdrgam_cli_registry_dependency_states')(configuration)
stopifnot(identical(states[[dependent_key]], 'waiting'))

snapshot <- internal('.cdrgam_cli_registry_status_snapshot')(
    configuration, projects='example',
    project_ids=c(example='project-registry-test')
)
stopifnot(
    nrow(snapshot$records) == 2L,
    identical(snapshot$states[[dependent_key]], 'waiting')
)
read_only <- internal('.cdrgam_cli_registry_connect')(
    configuration, read_only=TRUE
)
write_error <- tryCatch({
    DBI::dbExecute(read_only, 'DELETE FROM work_items')
    NULL
}, error=identity)
DBI::dbDisconnect(read_only)
stopifnot(inherits(write_error, 'error'))

stopifnot(
    key %in% internal('.cdrgam_cli_registry_keys_for_artifacts')(
        configuration, output
    ),
    key %in% internal('.cdrgam_cli_registry_keys_for_project')(
        configuration, 'example', 'project-registry-test'
    )
)

internal('.cdrgam_cli_registry_forget')(
    configuration, c(key, dependent_key)
)
remaining <- internal('.cdrgam_cli_registry_exec')(
    configuration,
    'SELECT COUNT(*) AS n FROM work_items',
    query=TRUE
)$n[[1L]]
stopifnot(remaining == 0L, !dir.exists(attempt))

internal('.cdrgam_cli_registry_record_graph')(
    configuration, graph, 'project-removal-test', request_id='project-removal-test'
)
request_path <- file.path(
    store, '.cdrgam', 'work', 'requests', 'project-removal-test.rds'
)
dir.create(dirname(request_path), recursive=TRUE, showWarnings=FALSE)
saveRDS(graph, request_path)
project_keys <- internal('.cdrgam_cli_registry_project_keys')(
    configuration, 'example'
)
stopifnot(identical(project_keys, key))
internal('.cdrgam_cli_remove_project')('example', force=TRUE, cdrgam_root=store)
stopifnot(!dir.exists(project), !file.exists(request_path))
remaining <- internal('.cdrgam_cli_registry_exec')(
    configuration, 'SELECT COUNT(*) AS n FROM work_items', query=TRUE
)$n[[1L]]
stopifnot(remaining == 0L)

# Parallel jobs sharing a work item: a freshly claimed attempt is marked
# running, so a second allocation neither prunes it nor starts a duplicate,
# and an explicit worker path leaves the other claim in place.
shared <- list(
    key='effect:project-registry-test:shared:race-identity',
    project='example', kind='effect', name='shared', identity='race-identity',
    output=file.path(project, 'results', 'effects', 'shared'),
    dependencies=character()
)
first <- internal('.cdrgam_cli_attempt_directory')(definitions, shared)$path
claim <- yaml::read_yaml(file.path(first, 'attempt.yml'))
stopifnot(identical(claim$status, 'running'), identical(claim$pid, Sys.getpid()))
second <- tryCatch(
    internal('.cdrgam_cli_attempt_directory')(definitions, shared),
    error=conditionMessage
)
stopifnot(is.character(second), grepl('appears active', second), dir.exists(first))
active <- tryCatch(
    internal('.cdrgam_cli_attempt_directory')(definitions, shared),
    cdrgam_cli_attempt_active=function(condition) condition
)
stopifnot(inherits(active, 'cdrgam_cli_attempt_active'))
# A waiting job reuses the artifact once the running job publishes it.
manifest_path <- internal('.cdrgam_cli_manifest_path')(shared$output)
dir.create(dirname(manifest_path), recursive=TRUE, showWarnings=FALSE)
saveRDS(1, file.path(shared$output, 'effect-grid.rds'))
internal('.cdrgam_cli_write_yaml')(list(
    schema=1L, status='complete', kind='effect', identity=shared$identity,
    outputs=internal('.cdrgam_cli_output_manifest')(shared$output, 'effect-grid.rds')
), manifest_path)
stopifnot(
    internal('.cdrgam_cli_complete_artifact')(shared$output, shared$identity),
    is.null(internal('.cdrgam_cli_await_attempt_directory')(
        definitions, shared, poll=0
    ))
)
unlink(shared$output, recursive=TRUE)
other <- file.path(dirname(first), 'explicit')
dir.create(other)
invisible(internal('.cdrgam_cli_attempt_directory')(definitions, shared, explicit=other))
stopifnot(dir.exists(first), dir.exists(other))
unlink(dirname(first), recursive=TRUE)

cat('registry: ok\n')
