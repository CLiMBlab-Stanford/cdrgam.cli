library(cdrgam.cli)

assign_identity <- getFromNamespace(
    '.cdrgam_cli_assign_definition_identity', 'cdrgam.cli'
)

definitions <- list(
    dataset=list(
        schema=1L,
        sources=list(
            impulses=list(path='impulses.rds', format='rds'),
            responses=list(path='responses.rds', format='rds')
        ),
        columns=list(impulse_time='time', response_time='time')
    ),
    model=list(
        schema=1L, datasets=list(train='training'), formula='response ~ 1'
    ),
    visualization=list(
        schema=1L, model='main', kind='booklet', pages=1L
    ),
    comparison=list(
        schema=1L, models=c('first', 'second'), methods='mse'
    )
)

validate <- function(value, type) {
    validator <- getFromNamespace(
        paste0('.cdrgam_cli_validate_', type), 'cdrgam.cli'
    )
    if (identical(type, 'dataset')) {
        validator(
            value, 'matching-name.yml', tempdir(), check_exists=FALSE
        )
    } else {
        validator(value, 'matching-name.yml')
    }
}

for (type in c('dataset', 'model', 'visualization', 'comparison')) {
    field <- type
    value <- definitions[[type]]
    validated <- validate(value, type)
    stopifnot(!(field %in% names(validated)))

    value[[field]] <- 'matching-name'
    message <- tryCatch({
        validate(value, type)
        NA_character_
    }, error=function(error) conditionMessage(error))
    stopifnot(
        grepl(field, message, fixed=TRUE),
        grepl('derived from the file name', message, fixed=TRUE),
        grepl('must be omitted', message, fixed=TRUE)
    )

    assigned <- assign_identity(
        list(schema=1L), type, 'matching-name', 'matching-name.yml'
    )
    stopifnot(identical(assigned[[field]], 'matching-name'))
}

invalid_name <- tryCatch({
    assign_identity(list(schema=1L), 'model', 'invalid_name', 'invalid_name.yml')
    NA_character_
}, error=function(error) conditionMessage(error))
stopifnot(grepl('file name', invalid_name, fixed=TRUE))

local({
    temporary <- tempfile('cdrgam-definition-identities-')
    checkout <- file.path(temporary, 'checkout')
    root <- file.path(temporary, 'root')
    dir.create(checkout, recursive=TRUE)
    on.exit(unlink(temporary, recursive=TRUE), add=TRUE)

    cdrgam_cli_configure(checkout, root)
    cdrgam_cli_def('example', checkout=checkout)
    cdrgam_cli_def(
        'example', 'dataset', 'training', checkout=checkout,
        editor=function(path) yaml::write_yaml(definitions$dataset, path)
    )
    cdrgam_cli_def(
        'example', 'model', 'main', checkout=checkout,
        editor=function(path) yaml::write_yaml(definitions$model, path)
    )
    cdrgam_cli_def(
        'example', 'model', 'alternative', source='main', checkout=checkout,
        editor=function(path) Sys.setFileTime(path, Sys.time())
    )
    cdrgam_cli_def(
        'example', 'visualization', 'diagnostics', checkout=checkout,
        editor=function(path) {
            value <- yaml::read_yaml(path)
            value$model <- 'main'
            yaml::write_yaml(value, path)
        }
    )
    cdrgam_cli_def(
        'example', 'comparison', 'models', checkout=checkout,
        editor=function(path) yaml::write_yaml(definitions$comparison, path)
    )
    cdrgam_cli_def(
        'example', 'model', 'spare', source='main', checkout=checkout,
        editor=function(path) Sys.setFileTime(path, Sys.time())
    )

    project <- file.path(root, 'projects', 'example')
    paths <- c(
        dataset=file.path(project, 'definitions', 'datasets', 'training.yml'),
        model=file.path(project, 'definitions', 'models', 'main.yml'),
        visualization=file.path(
            project, 'definitions', 'visualizations', 'diagnostics.yml'
        ),
        comparison=file.path(
            project, 'definitions', 'comparisons', 'models.yml'
        )
    )
    for (type in names(paths)) {
        stopifnot(!(type %in% names(yaml::read_yaml(paths[[type]]))))
    }
    stopifnot(identical(
        yaml::read_yaml(file.path(
            project, 'definitions', 'models', 'alternative.yml'
        )),
        yaml::read_yaml(paths[['model']])
    ))

    loaded <- getFromNamespace(
        '.cdrgam_cli_read_definitions', 'cdrgam.cli'
    )('example', check_sources=FALSE, checkout=checkout)
    stopifnot(
        identical(loaded$datasets$training$dataset, 'training'),
        identical(loaded$models$main$model, 'main'),
        identical(loaded$models$alternative$model, 'alternative'),
        identical(
            loaded$visualizations$diagnostics$visualization,
            'diagnostics'
        ),
        identical(loaded$comparisons$models$comparison, 'models')
    )

    project_listing <- capture.output(stopifnot(identical(
        cli_main(c('def', 'ls')), 0L
    )))
    stopifnot(
        any(grepl('project', project_listing, fixed=TRUE)),
        any(grepl('example', project_listing, fixed=TRUE)),
        !any(grepl('training', project_listing, fixed=TRUE))
    )

    list_definitions <- getFromNamespace(
        '.cdrgam_cli_list_definitions', 'cdrgam.cli'
    )
    invisible(capture.output(
        selected <- list_definitions(
            'example', list(dataset='train*', model=c('main', 'sp*')),
            checkout=checkout
        )
    ))
    stopifnot(identical(
        paste(selected$type, selected$name),
        c('dataset training', 'model main', 'model spare')
    ))
    invisible(capture.output(stopifnot(identical(
        cli_main(c(
            'def', 'ls', 'example', '--dataset', 'train*',
            '--model', 'main', 'sp*'
        )),
        0L
    ))))
    model_listing <- capture.output(stopifnot(identical(
        cli_main(c('def', 'ls', 'example', '--model')), 0L
    )))
    stopifnot(
        any(grepl('alternative', model_listing, fixed=TRUE)),
        any(grepl('main', model_listing, fixed=TRUE)),
        any(grepl('spare', model_listing, fixed=TRUE)),
        !any(grepl('training', model_listing, fixed=TRUE))
    )
    missing_run_selector <- tryCatch({
        cli_main(c('run', '--model'))
        NA_character_
    }, error=function(error) conditionMessage(error))
    stopifnot(grepl(
        '--model requires a value', missing_run_selector, fixed=TRUE
    ))

    alternative_path <- file.path(
        project, 'definitions', 'models', 'alternative.yml'
    )
    alternative_definition <- readLines(alternative_path, warn=FALSE)
    writeLines('model: [', alternative_path)
    referenced_error <- tryCatch({
        cli_main(c('def', 'rm', 'example', '--model', 'alternative'))
        NA_character_
    }, error=function(error) conditionMessage(error))
    stopifnot(
        grepl('referenced by', referenced_error, fixed=TRUE),
        file.exists(alternative_path)
    )
    writeLines(alternative_definition, alternative_path)

    spare_path <- file.path(
        project, 'definitions', 'models', 'spare.yml'
    )
    writeLines('model: [', spare_path)
    spare_artifact <- file.path(project, 'models', 'spare')
    dir.create(spare_artifact, recursive=TRUE)
    results_error <- tryCatch({
        cli_main(c('def', 'rm', 'example', '--model', 'spare'))
        NA_character_
    }, error=function(error) conditionMessage(error))
    stopifnot(
        grepl('while results exist', results_error, fixed=TRUE),
        file.exists(spare_path)
    )
    unlink(spare_artifact, recursive=TRUE)
    diagnostics_path <- file.path(
        project, 'definitions', 'visualizations', 'diagnostics.yml'
    )
    diagnostics_definition <- yaml::read_yaml(diagnostics_path)
    writeLines('model: [', diagnostics_path)
    unreadable_reference_error <- tryCatch({
        cli_main(c('def', 'rm', 'example', '--model', 'spare'))
        NA_character_
    }, error=function(error) conditionMessage(error))
    stopifnot(
        grepl('could not be read', unreadable_reference_error, fixed=TRUE),
        file.exists(spare_path)
    )
    diagnostics_definition$unknown <- TRUE
    yaml::write_yaml(diagnostics_definition, diagnostics_path)
    stopifnot(identical(
        cli_main(c('def', 'rm', 'example', '--model', 'sp*')), 0L
    ))
    stopifnot(!file.exists(spare_path))
    old_del_error <- tryCatch({
        cli_main(c('def', 'del', 'example', '--model', 'main'))
        NA_character_
    }, error=function(error) conditionMessage(error))
    stopifnot(grepl(
        'Unknown command: cdrgam def del', old_del_error, fixed=TRUE
    ))
})
