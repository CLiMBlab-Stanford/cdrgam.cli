library(cdrgam.cli)

internal <- function(name) getFromNamespace(name, 'cdrgam.cli')
temporary <- tempfile('cdrgam-cli-paths-')
dir.create(temporary)
on.exit(unlink(temporary, recursive=TRUE), add=TRUE)

store <- file.path(temporary, 'store')
project <- file.path(store, 'projects', 'example')
dir.create(file.path(project, 'definitions'), recursive=TRUE)
configuration <- list(cdrgam_root=store)
definitions <- list(
    root=project,
    checkout=configuration,
    project=list(project=list(id='project-path-test', name='example'))
)
yaml::write_yaml(
    definitions$project,
    file.path(project, 'definitions', 'project.yml')
)

model <- internal('.cdrgam_cli_path')(definitions, 'model', 'main')
prediction <- internal('.cdrgam_cli_path')(
    definitions, 'prediction', 'main', dataset='test'
)
reference <- internal('.cdrgam_cli_project_relative')(definitions, prediction)
roots <- internal('.cdrgam_cli_project_roots')(configuration)

stopifnot(
    identical(model, file.path(project, 'results', 'models', 'main')),
    identical(
        prediction,
        file.path(project, 'results', 'models', 'main', 'predictions', 'test')
    ),
    identical(
        reference,
        'cdrgam-project://project-path-test/results/models/main/predictions/test'
    ),
    identical(
        unname(internal('.cdrgam_cli_registry_resolve')(
            configuration, reference, project_roots=roots
        )),
        prediction
    ),
    identical(
        internal('.cdrgam_cli_managed_resolve')(
            configuration, reference, project_roots=roots
        ),
        prediction
    )
)

unsafe <- tryCatch({
    internal('.cdrgam_cli_registry_resolve')(
        configuration, '../outside', project_roots=roots
    )
    NULL
}, error=identity)
unknown <- tryCatch({
    internal('.cdrgam_cli_managed_resolve')(
        configuration,
        'cdrgam-project://unknown/results/models/main',
        project_roots=roots
    )
    NULL
}, error=identity)
stopifnot(
    inherits(unsafe, 'error'),
    inherits(unknown, 'error')
)

cat('paths: ok\n')
