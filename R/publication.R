.cdrgam_cli_publication_path <- function(root) {
    file.path(root, 'publication.yml')
}

.cdrgam_cli_source_inventory <- function(root) {
    files <- setdiff(.cdrgam_cli_git_source_files(root), 'publication.yml')
    files <- sort(files[file.exists(file.path(root, files)) &
        !dir.exists(file.path(root, files))])
    links <- vapply(file.path(root, files), fs::is_link, logical(1))
    if (any(links)) .cdrgam_cli_abort(paste0(
        'Publication sources must not contain symbolic links: ',
        paste(files[links], collapse=', ')
    ))
    lapply(files, function(path) list(
        path=path,
        size=unname(file.info(file.path(root, path))$size),
        sha256=.cdrgam_cli_sha256(file.path(root, path))
    ))
}

.cdrgam_cli_inventory_digest <- function(inventory) {
    value <- vapply(inventory, function(file) {
        paste(file$path, file$size, file$sha256, sep='\034')
    }, character(1))
    digest::digest(paste(value, collapse='\035'), algo='sha256', serialize=FALSE)
}

.cdrgam_cli_copy_file <- function(source, destination) {
    if (isTRUE(fs::is_link(source))) {
        .cdrgam_cli_abort(paste0(
            'Publication artifacts must not contain symbolic links: ', source
        ))
    }
    if (!dir.exists(dirname(destination)) &&
            !dir.create(dirname(destination), recursive=TRUE)) {
        .cdrgam_cli_abort(paste0(
            'Could not create publication directory ', sQuote(dirname(destination))
        ))
    }
    if (!file.copy(source, destination, overwrite=TRUE, copy.mode=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not copy ', sQuote(source)))
    }
    invisible(destination)
}

.cdrgam_cli_copy_source_snapshot <- function(root, destination, inventory) {
    for (file in inventory) .cdrgam_cli_copy_file(
        file.path(root, file$path), file.path(destination, file$path)
    )
    invisible(destination)
}

.cdrgam_cli_publication_items <- function(
        project, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, checkout=NULL
) {
    graph <- .cdrgam_cli_combined_graph(
        projects=project, models=models, predictions=predictions,
        visualizations=visualizations, comparisons=comparisons,
        checkout=checkout
    )
    incomplete <- vapply(graph$items, function(item) {
        !.cdrgam_cli_complete_artifact(item$output, item$identity)
    }, logical(1))
    if (any(incomplete)) {
        labels <- vapply(graph$items[incomplete], function(item) {
            paste(item$kind, item$name, sep=':')
        }, character(1))
        .cdrgam_cli_abort(paste0(
            'Cannot publish incomplete, stale, or nonconverged artifacts: ',
            paste(labels, collapse=', ')
        ))
    }
    graph
}

.cdrgam_cli_artifact_files <- function(item) {
    manifest_path <- file.path(item$output, 'manifest.yml')
    manifest <- .cdrgam_cli_read_yaml(manifest_path)
    paths <- c('manifest.yml', vapply(
        manifest$outputs, `[[`, character(1), 'path'
    ))
    .cdrgam_cli_safe_archive_paths(paths, 'artifact manifest')
    unique(file.path(item$output, paths))
}

.cdrgam_cli_copy_artifact <- function(item, root, stage) {
    relative_directory <- as.character(fs::path_rel(
        item$output, start=root$root
    ))
    if (!startsWith(relative_directory, 'results/')) {
        .cdrgam_cli_abort('Publication artifact is outside the project results tree')
    }
    files <- .cdrgam_cli_artifact_files(item)
    for (source in files) {
        path <- as.character(fs::path_rel(source, start=item$output))
        destination <- file.path(stage, relative_directory, path)
        .cdrgam_cli_copy_file(source, destination)
    }
    list(
        kind=item$kind, name=item$name, identity=item$identity,
        path=relative_directory,
        contract=item$resolved_identity$resolved
    )
}

.cdrgam_cli_results_file_inventory <- function(stage) {
    paths <- fs::dir_ls(
        file.path(stage, 'results'), recurse=TRUE, type='file',
        all=TRUE, fail=FALSE
    )
    paths <- sort(as.character(paths))
    lapply(paths, function(path) list(
        path=as.character(fs::path_rel(path, start=stage)),
        size=unname(file.info(path)$size), sha256=.cdrgam_cli_sha256(path)
    ))
}

.cdrgam_cli_build_results_archive <- function(
        project, archive, models=NULL, predictions=NULL,
        visualizations=NULL, comparisons=NULL, checkout=NULL
) {
    graph <- .cdrgam_cli_publication_items(
        project, models, predictions, visualizations, comparisons, checkout
    )
    definitions <- graph$definitions[[project]]
    root <- definitions$root
    archive <- .cdrgam_cli_normalize_path(archive, must_work=FALSE)
    if (!dir.exists(dirname(archive)) &&
            !dir.create(dirname(archive), recursive=TRUE)) {
        .cdrgam_cli_abort(paste0(
            'Could not create archive directory ', sQuote(dirname(archive))
        ))
    }
    stage <- tempfile('cdrgam-results-', tmpdir=dirname(archive))
    if (!dir.create(stage)) .cdrgam_cli_abort('Could not create publication stage')
    on.exit(unlink(stage, recursive=TRUE, force=TRUE), add=TRUE)
    dir.create(file.path(stage, 'results'), recursive=TRUE)
    artifacts <- lapply(graph$items, function(item) {
        .cdrgam_cli_copy_artifact(item, definitions, stage)
    })
    source_inventory <- .cdrgam_cli_source_inventory(root)
    .cdrgam_cli_copy_source_snapshot(
        root, file.path(stage, 'results', 'source-snapshot'), source_inventory
    )
    files <- .cdrgam_cli_results_file_inventory(stage)
    manifest <- list(
        schema=1L,
        project=list(
            name=project, id=definitions$project$project$id
        ),
        created_at=.cdrgam_cli_timestamp(),
        source=list(
            revision=.cdrgam_cli_git_revision(root),
            digest=.cdrgam_cli_inventory_digest(source_inventory),
            files=source_inventory
        ),
        artifacts=unname(artifacts), files=files
    )
    .cdrgam_cli_write_yaml(
        manifest, file.path(stage, 'results', 'publication.yml')
    )
    temporary <- tempfile(
        paste0('.', basename(archive), '-'), tmpdir=dirname(archive),
        fileext='.tar.gz'
    )
    on.exit(unlink(temporary, force=TRUE), add=TRUE)
    previous <- getwd()
    on.exit(setwd(previous), add=TRUE)
    setwd(stage)
    status <- suppressWarnings(utils::tar(
        temporary, files='results', compression='gzip', tar='internal'
    ))
    setwd(previous)
    if (!identical(status, 0L) || !file.exists(temporary)) {
        .cdrgam_cli_abort('Could not create the results archive')
    }
    .cdrgam_cli_replace_path(temporary, archive)
    list(
        path=archive, sha256=.cdrgam_cli_sha256(archive),
        artifacts=length(artifacts), manifest=manifest
    )
}

.cdrgam_cli_publication_manifest <- function(definitions, results) {
    list(
        schema=1L,
        project=list(
            name=definitions$project$project$name,
            id=definitions$project$project$id
        ),
        results=results
    )
}

#' Publish a CDR-GAM project
#'
#' Validate and stage project publication metadata. Results may remain
#' unpublished, be written to a verified archive, be referenced by URL, or be
#' staged directly in the project Git repository.
#'
#' @param project Project name.
#' @param results One of `"none"`, `"archive"`, `"url"`, or `"git"`.
#' @param archive Destination for a generated `.tar.gz` results archive.
#' @param url Direct download URL for a separately hosted results archive.
#' @param sha256 SHA-256 checksum for `url`.
#' @param models,predictions,visualizations,comparisons Optional workload
#'   selectors. Selected downstream artifacts include their dependencies.
#' @param commit Optional Git commit message. All already staged definition
#'   edits and publication changes are committed together.
#' @param push Whether to push the current branch after publication.
#' @param cdrgam_root Configured CDR-GAM root.
#' @return Publication metadata, invisibly.
#' @export
cdrgam_cli_publish <- function(
        project, results=c('none', 'archive', 'url', 'git'), archive=NULL,
        url=NULL, sha256=NULL, models=NULL, predictions=NULL,
        visualizations=NULL, comparisons=NULL, commit=NULL, push=FALSE,
        cdrgam_root=NULL
) {
    project <- .cdrgam_cli_name(project, 'project')
    results <- match.arg(results)
    push <- .cdrgam_cli_scalar_logical(push, 'push')
    if (push && is.null(commit)) {
        .cdrgam_cli_abort('push requires commit so publication changes reach the remote')
    }
    definitions <- .cdrgam_cli_read_definitions(
        project, check_sources=TRUE, checkout=cdrgam_root
    )
    root <- definitions$root
    .cdrgam_cli_git_track_project(root)
    result_record <- list(mode=results)
    if (identical(results, 'archive')) {
        if (is.null(archive)) archive <- file.path(
            getwd(), paste0(project, '-results.tar.gz')
        )
        built <- .cdrgam_cli_build_results_archive(
            project, archive, models, predictions, visualizations,
            comparisons, cdrgam_root
        )
        result_record$sha256 <- built$sha256
        result_record$archive <- basename(built$path)
        result_record$artifacts <- built$artifacts
        if (!is.null(url)) result_record$url <- .cdrgam_cli_scalar_character(url, 'url')
    } else if (identical(results, 'url')) {
        result_record$url <- .cdrgam_cli_scalar_character(url, 'url')
        sha256 <- tolower(.cdrgam_cli_scalar_character(sha256, 'sha256'))
        if (!grepl('^[0-9a-f]{64}$', sha256)) {
            .cdrgam_cli_abort('sha256 must contain exactly 64 hexadecimal digits')
        }
        result_record$sha256 <- sha256
    } else if (identical(results, 'git')) {
        graph <- .cdrgam_cli_publication_items(
            project, models, predictions, visualizations, comparisons, cdrgam_root
        )
        .cdrgam_cli_git_stage(
            root, unlist(lapply(
                graph$items, .cdrgam_cli_artifact_files
            ), use.names=FALSE), force=TRUE
        )
        result_record$artifacts <- length(graph$items)
    } else if (!is.null(archive) || !is.null(url) || !is.null(sha256)) {
        .cdrgam_cli_abort('archive, url, and sha256 require a results publication mode')
    }
    publication <- .cdrgam_cli_publication_manifest(definitions, result_record)
    path <- .cdrgam_cli_publication_path(root)
    .cdrgam_cli_write_yaml(publication, path)
    .cdrgam_cli_git_stage(root, path)
    if (!is.null(commit)) {
        commit <- .cdrgam_cli_scalar_character(commit, 'commit')
        .cdrgam_cli_git(root, c('commit', '-m', commit))
    }
    if (push) .cdrgam_cli_git(root, 'push')
    message('Prepared publication metadata at ', path)
    if (identical(results, 'archive')) message(
        'Wrote results archive ', built$path, ' (sha256 ', built$sha256, ')'
    )
    invisible(publication)
}

.cdrgam_cli_safe_archive_paths <- function(paths, field='archive') {
    paths <- paths[nzchar(paths)]
    unsafe <- vapply(paths, function(path) {
        path <- gsub('\\\\', '/', path)
        pieces <- strsplit(path, '/', fixed=TRUE)[[1L]]
        startsWith(path, '/') || grepl('^[A-Za-z]:/', path) ||
            any(pieces == '..')
    }, logical(1))
    if (any(unsafe)) .cdrgam_cli_abort(paste0(
        field, ' contains an unsafe path: ', paths[which(unsafe)[[1L]]]
    ))
    invisible(paths)
}

.cdrgam_cli_download <- function(location, destination) {
    location <- .cdrgam_cli_scalar_character(location, 'download location')
    if (file.exists(location)) {
        if (!file.copy(location, destination, overwrite=TRUE)) {
            .cdrgam_cli_abort(paste0('Could not copy ', sQuote(location)))
        }
        return(invisible(destination))
    }
    status <- tryCatch(
        suppressWarnings(utils::download.file(
            location, destination, mode='wb', quiet=TRUE
        )),
        error=function(error) error
    )
    if (inherits(status, 'error') || !identical(status, 0L)) {
        detail <- if (inherits(status, 'error')) conditionMessage(status) else {
            paste('download status', status)
        }
        .cdrgam_cli_abort(paste0(
            'Could not download ', sQuote(location), ': ', detail
        ))
    }
    invisible(destination)
}

.cdrgam_cli_assert_no_links <- function(root) {
    paths <- fs::dir_ls(root, recurse=TRUE, all=TRUE, fail=FALSE)
    links <- vapply(paths, fs::is_link, logical(1))
    if (any(links)) .cdrgam_cli_abort(paste0(
        'Fetched publications must not contain symbolic links: ',
        as.character(paths[which(links)[[1L]]])
    ))
    invisible(root)
}

.cdrgam_cli_unpack_archive <- function(archive, destination) {
    if (!dir.exists(destination) && !dir.create(destination, recursive=TRUE)) {
        .cdrgam_cli_abort('Could not create archive extraction directory')
    }
    zip <- grepl('\\.zip$', archive, ignore.case=TRUE)
    if (zip) {
        listing <- tryCatch(
            utils::unzip(archive, list=TRUE)$Name,
            error=function(error) .cdrgam_cli_abort(paste0(
                'Could not inspect ZIP archive: ', conditionMessage(error)
            ))
        )
        .cdrgam_cli_safe_archive_paths(listing)
        utils::unzip(archive, exdir=destination)
    } else {
        listing <- tryCatch(
            utils::untar(archive, list=TRUE),
            error=function(error) .cdrgam_cli_abort(paste0(
                'Could not inspect tar archive: ', conditionMessage(error)
            ))
        )
        .cdrgam_cli_safe_archive_paths(listing)
        status <- utils::untar(archive, exdir=destination)
        if (!identical(status, 0L)) {
            .cdrgam_cli_abort('Could not extract tar archive')
        }
    }
    .cdrgam_cli_assert_no_links(destination)
    invisible(destination)
}

.cdrgam_cli_find_extracted_project <- function(root) {
    markers <- c(
        file.path(root, .cdrgam_cli_marker),
        list.files(
            root, pattern='project\\.yml$', recursive=TRUE,
            full.names=TRUE, all.files=TRUE
        )
    )
    markers <- unique(markers[file.exists(markers)])
    markers <- markers[endsWith(
        gsub('\\\\', '/', markers), '/definitions/project.yml'
    )]
    if (length(markers) != 1L) .cdrgam_cli_abort(
        'A source archive must contain exactly one CDR-GAM project'
    )
    dirname(dirname(markers[[1L]]))
}

.cdrgam_cli_clone_or_unpack <- function(location, stage) {
    git <- Sys.which('git')
    cloned <- FALSE
    archive_location <- grepl(
        '\\.(zip|tar|tar\\.gz|tgz|tar\\.bz2|tbz2|tar\\.xz|txz)($|[?#])',
        location, ignore.case=TRUE
    )
    if (nzchar(git) && !archive_location) {
        result <- .cdrgam_cli_process_run(
            git, c('clone', '--', location, stage), timeout=300000
        )
        cloned <- !is.null(result) && identical(result$status, 0L)
    }
    if (cloned) return(invisible(stage))
    if (file.exists(stage) || dir.exists(stage)) {
        unlink(stage, recursive=TRUE, force=TRUE)
    }
    download <- tempfile('cdrgam-source-', fileext=if (
        grepl('\\.zip($|[?#])', location, ignore.case=TRUE)
    ) '.zip' else '.tar.gz')
    extraction <- tempfile('cdrgam-source-extract-')
    on.exit(unlink(c(download, extraction), recursive=TRUE, force=TRUE), add=TRUE)
    .cdrgam_cli_download(location, download)
    .cdrgam_cli_unpack_archive(download, extraction)
    project <- .cdrgam_cli_find_extracted_project(extraction)
    if (!.cdrgam_cli_try_move_path(project, stage)) {
        .cdrgam_cli_abort('Could not stage the extracted project')
    }
    invisible(stage)
}

.cdrgam_cli_validate_results_tree <- function(results, project_id) {
    manifest_path <- file.path(results, 'publication.yml')
    if (!file.exists(manifest_path)) {
        .cdrgam_cli_abort('Results archive is missing results/publication.yml')
    }
    manifest <- .cdrgam_cli_read_yaml(manifest_path)
    if (!identical(manifest$schema, 1L) ||
            !identical(manifest$project$id, project_id) ||
            !is.list(manifest$files)) {
        .cdrgam_cli_abort('Results publication metadata is invalid or for another project')
    }
    declared <- vapply(manifest$files, `[[`, character(1), 'path')
    .cdrgam_cli_safe_archive_paths(declared, 'results manifest')
    for (file in manifest$files) {
        path <- file.path(dirname(results), file$path)
        if (!file.exists(path) || dir.exists(path) ||
                !identical(.cdrgam_cli_sha256(path), file$sha256)) {
            .cdrgam_cli_abort(paste0(
                'Results checksum verification failed for ', file$path
            ))
        }
    }
    actual <- fs::dir_ls(results, recurse=TRUE, type='file', all=TRUE, fail=FALSE)
    actual <- as.character(fs::path_rel(actual, start=dirname(results)))
    actual <- setdiff(actual, 'results/publication.yml')
    if (!setequal(actual, declared)) {
        .cdrgam_cli_abort('Results archive contents do not match its file inventory')
    }
    manifest
}

.cdrgam_cli_install_results <- function(
        location, sha256, project_stage, project_id
) {
    archive <- tempfile('cdrgam-results-', fileext='.tar.gz')
    extraction <- tempfile('cdrgam-results-extract-')
    on.exit(unlink(c(archive, extraction), recursive=TRUE, force=TRUE), add=TRUE)
    .cdrgam_cli_download(location, archive)
    if (!is.null(sha256) && !identical(
            tolower(.cdrgam_cli_sha256(archive)), tolower(sha256)
    )) {
        .cdrgam_cli_abort('Downloaded results archive failed SHA-256 verification')
    }
    .cdrgam_cli_unpack_archive(archive, extraction)
    results <- file.path(extraction, 'results')
    if (!dir.exists(results)) .cdrgam_cli_abort(
        'Results archive must contain one top-level results directory'
    )
    .cdrgam_cli_validate_results_tree(results, project_id)
    destination <- file.path(project_stage, 'results')
    if (dir.exists(destination)) {
        existing <- list.files(destination, all.files=TRUE, no..=TRUE)
        if (length(existing)) .cdrgam_cli_abort(
            'Source publication already contains results; refusing to replace them'
        )
        unlink(destination, recursive=TRUE, force=TRUE)
    }
    if (!.cdrgam_cli_try_move_path(results, destination)) {
        .cdrgam_cli_abort('Could not install verified results into the project stage')
    }
    invisible(destination)
}

#' Fetch a published CDR-GAM project
#'
#' Clone a Git repository or extract a direct source archive into the configured
#' project store. When publication metadata supplies a results URL, verify and
#' install that archive before atomically publishing the project locally.
#'
#' @param location Git repository URL, local Git repository, or direct source
#'   archive URL.
#' @param project Optional local project directory name.
#' @param source_only Whether to skip published results.
#' @param results Optional local path or direct URL overriding the published
#'   results location.
#' @param cdrgam_root Configured CDR-GAM root.
#' @return The installed project root, invisibly.
#' @export
cdrgam_cli_fetch <- function(
        location, project=NULL, source_only=FALSE, results=NULL, cdrgam_root=NULL
) {
    location <- .cdrgam_cli_scalar_character(location, 'location')
    source_only <- .cdrgam_cli_scalar_logical(source_only, 'source_only')
    configuration <- .cdrgam_cli_site(cdrgam_root, create_root=TRUE)
    projects <- file.path(configuration$cdrgam_root, 'projects')
    stage <- tempfile('.cdrgam-fetch-', tmpdir=projects)
    complete <- FALSE
    published <- FALSE
    on.exit({
        if (!complete) unlink(
            if (published) destination else stage,
            recursive=TRUE, force=TRUE
        )
    }, add=TRUE)
    .cdrgam_cli_clone_or_unpack(location, stage)
    .cdrgam_cli_assert_no_links(stage)
    marker <- file.path(stage, .cdrgam_cli_marker)
    definition <- .cdrgam_cli_validate_project(
        .cdrgam_cli_read_yaml(marker), marker
    )
    local_name <- if (is.null(project)) {
        .cdrgam_cli_name(definition$project$name, 'published project name')
    } else .cdrgam_cli_name(project, 'project')
    destination <- .cdrgam_cli_project_root(
        local_name, root=cdrgam_root, must_work=FALSE
    )
    if (file.exists(destination) || dir.exists(destination)) {
        .cdrgam_cli_abort(paste0('Project already exists: ', destination))
    }
    publication_path <- .cdrgam_cli_publication_path(stage)
    if (!file.exists(publication_path)) {
        .cdrgam_cli_abort('Fetched source is missing publication.yml')
    }
    publication <- .cdrgam_cli_read_yaml(publication_path)
    if (!identical(publication$schema, 1L) ||
            !identical(publication$project$id, definition$project$id) ||
            !is.list(publication$results) ||
            !is.character(publication$results$mode) ||
            length(publication$results$mode) != 1L ||
            is.na(publication$results$mode) ||
            !(publication$results$mode %in% c('none', 'archive', 'url', 'git'))) {
        .cdrgam_cli_abort('Project publication metadata is invalid')
    }
    if (!source_only) {
        result_location <- .cdrgam_cli_null(results, publication$results$url)
        if (!is.null(result_location)) {
            expected <- if (is.null(results)) publication$results$sha256 else NULL
            if (!is.null(expected) && (!is.character(expected) ||
                    length(expected) != 1L || is.na(expected) ||
                    !grepl('^[0-9a-fA-F]{64}$', expected))) {
                .cdrgam_cli_abort('Published results have an invalid SHA-256 checksum')
            }
            .cdrgam_cli_install_results(
                result_location, expected, stage, definition$project$id
            )
        } else if (identical(publication$results$mode, 'url')) {
            .cdrgam_cli_abort('Published results have no downloadable URL')
        }
    }
    if (!dir.exists(file.path(stage, '.git'))) {
        .cdrgam_cli_git_track_project(stage)
    }
    if (!.cdrgam_cli_try_move_path(stage, destination)) {
        .cdrgam_cli_abort(paste0(
            'Could not atomically install fetched project ', sQuote(local_name)
        ))
    }
    published <- TRUE
    definitions <- .cdrgam_cli_read_definitions(
        local_name, check_sources=TRUE, checkout=cdrgam_root
    )
    graph <- .cdrgam_cli_resolve_graph(definitions, all=TRUE)
    .cdrgam_cli_registry_import_project(definitions, graph$items)
    complete <- TRUE
    message('Fetched CDR-GAM project ', local_name, ' at ', destination)
    invisible(.cdrgam_cli_normalize_path(destination, must_work=TRUE))
}
