.cdrgam_cli_git <- function(root, arguments, fail=TRUE) {
    git <- Sys.which('git')
    if (!nzchar(git)) {
        .cdrgam_cli_abort('Git is required for CDR-GAM project source management')
    }
    result <- .cdrgam_cli_process_run(
        git, c('-C', root, arguments), timeout=120000
    )
    if (is.null(result) || !identical(result$status, 0L)) {
        if (!isTRUE(fail)) return(result)
        detail <- if (is.null(result)) {
            'Git could not be started'
        } else {
            trimws(paste(c(result$stderr, result$stdout), collapse='\n'))
        }
        .cdrgam_cli_abort(paste0(
            'Git command failed in ', sQuote(root), ': ',
            paste(c('git', arguments), collapse=' '),
            if (nzchar(detail)) paste0('\n', detail) else ''
        ))
    }
    result
}

.cdrgam_cli_git_initialize <- function(root) {
    if (!dir.exists(file.path(root, '.git'))) {
        result <- .cdrgam_cli_git(
            root, c('init', '--initial-branch=main'), fail=FALSE
        )
        if (is.null(result) || !identical(result$status, 0L)) {
            .cdrgam_cli_git(root, 'init')
            .cdrgam_cli_git(root, c('branch', '-M', 'main'))
        }
    }
    invisible(root)
}

.cdrgam_cli_git_stage <- function(root, paths, force=FALSE) {
    paths <- unique(paths)
    if (!length(paths)) return(invisible(character()))
    root <- .cdrgam_cli_normalize_path(root, must_work=TRUE)
    relative <- vapply(paths, function(path) {
        path <- .cdrgam_cli_normalize_path(path, must_work=FALSE)
        if (!.cdrgam_cli_within(path, root)) {
            .cdrgam_cli_abort('Cannot stage a path outside its project repository')
        }
        as.character(fs::path_rel(path, start=root))
    }, character(1))
    .cdrgam_cli_git(root, c('add', if (isTRUE(force)) '-f', '--', relative))
    invisible(relative)
}

.cdrgam_cli_git_track_project <- function(root) {
    initialized <- !dir.exists(file.path(root, '.git'))
    .cdrgam_cli_git_initialize(root)
    if (initialized) .cdrgam_cli_git(root, c('add', '--all', '--', '.'))
    invisible(initialized)
}

.cdrgam_cli_git_head <- function(root) {
    result <- .cdrgam_cli_git(root, c('rev-parse', 'HEAD'), fail=FALSE)
    if (is.null(result) || !identical(result$status, 0L)) return(NULL)
    trimws(result$stdout)
}

.cdrgam_cli_git_dirty <- function(root) {
    result <- .cdrgam_cli_git(
        root, c('status', '--porcelain=v1', '--untracked-files=all')
    )
    nzchar(trimws(result$stdout))
}

.cdrgam_cli_git_tracked_files <- function(root) {
    result <- .cdrgam_cli_git(root, 'ls-files')
    output <- strsplit(result$stdout, '\n', fixed=TRUE)[[1L]]
    output[nzchar(output)]
}

.cdrgam_cli_git_source_files <- function(root) {
    result <- .cdrgam_cli_git(
        root, c('ls-files', '--cached', '--others', '--exclude-standard')
    )
    output <- strsplit(result$stdout, '\n', fixed=TRUE)[[1L]]
    output <- output[nzchar(output)]
    output[!grepl('^(results|\\.cdrgam|\\.git)(/|$)', output)]
}

.cdrgam_cli_git_revision <- function(root) {
    if (!dir.exists(file.path(root, '.git'))) return(NULL)
    list(commit=.cdrgam_cli_git_head(root), dirty=.cdrgam_cli_git_dirty(root))
}
