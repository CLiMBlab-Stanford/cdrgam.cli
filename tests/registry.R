library(cdrgam.cli)

internal <- function(name) getFromNamespace(name, 'cdrgam.cli')
temporary <- tempfile('cdrgam-cli-registry-')
dir.create(temporary)
on.exit(unlink(temporary, recursive=TRUE), add=TRUE)

store <- file.path(temporary, 'store')
project <- file.path(store, 'projects', 'example')
dir.create(file.path(project, 'definitions'), recursive=TRUE)
configuration <- list(cdrgam_root=store)
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

stopifnot(
    identical(
        internal('.cdrgam_cli_registry_keys_for_artifacts')(
            configuration, output
        ),
        key
    ),
    identical(
        internal('.cdrgam_cli_registry_keys_for_project')(
            configuration, 'example', 'project-registry-test'
        ),
        key
    )
)

internal('.cdrgam_cli_registry_forget')(configuration, key)
remaining <- internal('.cdrgam_cli_registry_exec')(
    configuration,
    'SELECT COUNT(*) AS n FROM work_items',
    query=TRUE
)$n[[1L]]
stopifnot(remaining == 0L, !dir.exists(attempt))

cat('registry: ok\n')
