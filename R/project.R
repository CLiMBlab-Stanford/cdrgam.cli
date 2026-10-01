.cdrgam_cli_marker <- file.path('definitions', 'project.yml')

#' Resolve a configured CDR-GAM project
#'
#' @param project Atomic project directory name. When omitted, infer it from
#'   the current directory if it lies beneath the configured projects directory.
#' @param cdrgam_root Configured CDR-GAM root.
#' @return The normalized project root.
#' @export
find_cdrgam_project <- function(project=NULL, cdrgam_root=NULL) {
    if (is.null(project) || !length(project) || !nzchar(project[[1L]])) {
        project <- .cdrgam_cli_infer_project(cdrgam_root)
    }
    if (length(project) != 1L) .cdrgam_cli_abort('Select exactly one project')
    root <- .cdrgam_cli_project_root(project, cdrgam_root, must_work=TRUE)
    if (!file.exists(file.path(root, .cdrgam_cli_marker))) {
        .cdrgam_cli_abort(paste0(
            'Project ', sQuote(project), ' is missing ', .cdrgam_cli_marker
        ))
    }
    root
}

.cdrgam_cli_project_layout <- function(root) {
    file.path(root, c(
        'definitions',
        file.path('definitions', 'datasets'),
        file.path('definitions', 'models'),
        file.path('definitions', 'visualizations'),
        file.path('definitions', 'comparisons'),
        file.path('definitions', 'analyses'),
        'code', 'results', file.path('results', 'datasets'),
        file.path('results', 'models'), file.path('results', 'comparisons'),
        file.path('results', 'analyses')
    ))
}

.cdrgam_cli_starter_project <- function(name, id) {
    list(schema=1L, project=list(name=name, id=id))
}

.cdrgam_cli_project_readme <- function(name) {
    paste0(
        '# ', name, '\n\n',
        'This directory is a CDR-GAM analysis project. Edit definitions under ',
        '`definitions/`, keep project scripts under `code/`, and let the ',
        'configured root manage generated state under `results/`.\n'
    )
}

.cdrgam_cli_project_gitignore <- function() {
    paste(c(
        '/.cdrgam/', '/results/', '/*-results.tar.gz', '/.Rproj.user/',
        '.Rhistory', '.RData',
        '.DS_Store', 'Thumbs.db', '*~', '*.swp', '*.swo'
    ), collapse='\n')
}

.cdrgam_cli_create_project <- function(name, checkout=NULL) {
    name <- .cdrgam_cli_name(name, 'project.name')
    .cdrgam_cli_site(checkout, create_root=TRUE)
    root <- .cdrgam_cli_project_root(name, checkout, must_work=FALSE)
    created_root <- !dir.exists(root)
    if (!dir.exists(root) && !dir.create(root, recursive=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not create project root ', sQuote(root)))
    }
    existing <- list.files(root, all.files=TRUE, no..=TRUE)
    if (length(existing)) {
        .cdrgam_cli_abort(paste0(
            'Project directory is not empty and is not a CDR-GAM project: ',
            root
        ))
    }
    targets <- list(
        project=file.path(root, .cdrgam_cli_marker),
        gitignore=file.path(root, '.gitignore'),
        readme=file.path(root, 'README.md')
    )
    if (file.exists(targets$project)) {
        existing_project <- .cdrgam_cli_validate_project(
            .cdrgam_cli_read_yaml(targets$project), targets$project
        )
        if (!identical(existing_project$project$name, name)) {
            .cdrgam_cli_abort(paste0(
                'Existing project name is ', sQuote(existing_project$project$name),
                ', not ', sQuote(name)
            ))
        }
    }
    created <- character()
    complete <- FALSE
    on.exit({
        if (!complete && created_root) {
            unlink(root, recursive=TRUE, force=TRUE)
        } else if (!complete && length(created)) {
            unlink(created, force=TRUE)
        }
    }, add=TRUE)
    for (directory in .cdrgam_cli_project_layout(root)) {
        if (!dir.exists(directory) && !dir.create(directory, recursive=TRUE)) {
            .cdrgam_cli_abort(paste0('Could not create ', sQuote(directory)))
        }
    }
    if (!file.exists(targets$project)) {
        created <- c(created, targets$project)
        .cdrgam_cli_write_yaml(
            .cdrgam_cli_starter_project(name, .cdrgam_cli_random_id()), targets$project
        )
    }
    if (!file.exists(targets$gitignore)) {
        created <- c(created, targets$gitignore)
        .cdrgam_cli_atomic_write(targets$gitignore, function(path) {
            writeLines(.cdrgam_cli_project_gitignore(), path, useBytes=TRUE)
        })
    }
    if (!file.exists(targets$readme)) {
        created <- c(created, targets$readme)
        .cdrgam_cli_atomic_write(targets$readme, function(path) {
            writeLines(.cdrgam_cli_project_readme(name), path, useBytes=TRUE)
        })
    }
    .cdrgam_cli_read_definitions(name, check_sources=FALSE, checkout=checkout)
    .cdrgam_cli_git_track_project(root)
    complete <- TRUE
    message('Initialized CDR-GAM project ', name, ' at ', root)
    invisible(.cdrgam_cli_normalize_path(root, TRUE))
}

.cdrgam_cli_copy_project_sources <- function(from, to, checkout=NULL) {
    from <- .cdrgam_cli_name(from, 'source project')
    to <- .cdrgam_cli_name(to, 'destination project')
    if (identical(from, to)) {
        .cdrgam_cli_abort('Source and destination projects must differ')
    }
    source_definitions <- .cdrgam_cli_read_definitions(
        from, check_sources=FALSE, checkout=checkout
    )
    .cdrgam_cli_site(checkout, create_root=TRUE)
    destination <- .cdrgam_cli_project_root(
        to, root=checkout, must_work=FALSE
    )
    projects <- dirname(destination)
    if (file.exists(destination) || dir.exists(destination)) {
        .cdrgam_cli_abort(paste0(
            'Destination project already exists: ', destination
        ))
    }
    stage <- tempfile(paste0('.', to, '-'), tmpdir=projects)
    if (!dir.create(stage)) {
        .cdrgam_cli_abort(paste0('Could not create project stage ', stage))
    }
    complete <- FALSE
    published <- FALSE
    on.exit({
        if (!complete) unlink(
            if (published) destination else stage,
            recursive=TRUE, force=TRUE
        )
    }, add=TRUE)
    if (!file.copy(
            file.path(source_definitions$root, 'definitions'), stage,
            recursive=TRUE, copy.mode=TRUE, copy.date=TRUE
    )) {
        .cdrgam_cli_abort('Could not copy the source definition directory')
    }
    source_code <- file.path(source_definitions$root, 'code')
    if (dir.exists(source_code) && !file.copy(
            source_code, stage,
            recursive=TRUE, copy.mode=TRUE, copy.date=TRUE
    )) {
        .cdrgam_cli_abort('Could not copy the source code directory')
    }
    for (directory in .cdrgam_cli_project_layout(stage)) {
        if (!dir.exists(directory) && !dir.create(directory, recursive=TRUE)) {
            .cdrgam_cli_abort(paste0('Could not create ', sQuote(directory)))
        }
    }
    .cdrgam_cli_write_yaml(
        .cdrgam_cli_starter_project(to, .cdrgam_cli_random_id()),
        file.path(stage, .cdrgam_cli_marker)
    )
    .cdrgam_cli_atomic_write(file.path(stage, '.gitignore'), function(path) {
        writeLines(.cdrgam_cli_project_gitignore(), path, useBytes=TRUE)
    })
    .cdrgam_cli_atomic_write(file.path(stage, 'README.md'), function(path) {
        writeLines(.cdrgam_cli_project_readme(to), path, useBytes=TRUE)
    })
    if (!.cdrgam_cli_try_move_path(stage, destination)) {
        .cdrgam_cli_abort(paste0(
            'Could not atomically publish copied project ', sQuote(to)
        ))
    }
    published <- TRUE
    .cdrgam_cli_read_definitions(to, check_sources=FALSE, checkout=checkout)
    .cdrgam_cli_git_track_project(destination)
    complete <- TRUE
    message('Copied project sources from ', from, ' to ', to)
    invisible(.cdrgam_cli_normalize_path(destination, TRUE))
}
