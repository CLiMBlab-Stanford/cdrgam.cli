.cdrgam_cli_abort <- function(message, call.=FALSE) {
    stop(message, call.=call.)
}

.cdrgam_cli_scalar_character <- function(value, field, allow_empty=FALSE) {
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
            (!allow_empty && !nzchar(value))) {
        .cdrgam_cli_abort(paste0(field, ' must be a character scalar'))
    }
    value
}

.cdrgam_cli_scalar_logical <- function(value, field) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        .cdrgam_cli_abort(paste0(field, ' must be true or false'))
    }
    value
}

.cdrgam_cli_check_keys <- function(value, allowed, field, required=character()) {
    if (!is.list(value) || (length(value) && is.null(names(value)))) {
        .cdrgam_cli_abort(paste0(field, ' must be a mapping'))
    }
    unknown <- setdiff(names(value), allowed)
    if (length(unknown)) {
        .cdrgam_cli_abort(paste0(
            field, ' contains unknown field', if (length(unknown) == 1L) '' else 's',
            ': ', paste(unknown, collapse=', '),
            '. Allowed fields: ', paste(allowed, collapse=', ')
        ))
    }
    missing <- setdiff(required, names(value))
    if (length(missing)) {
        .cdrgam_cli_abort(paste0(
            field, ' is missing required field', if (length(missing) == 1L) '' else 's',
            ': ', paste(missing, collapse=', ')
        ))
    }
    invisible(value)
}

.cdrgam_cli_name <- function(value, field='name') {
    value <- .cdrgam_cli_scalar_character(value, field)
    if (!grepl('^[a-z][a-z0-9-]*$', value)) {
        .cdrgam_cli_abort(paste0(
            field, ' must begin with a lowercase letter and contain only ',
            'lowercase letters, digits, and hyphens; underscores are reserved'
        ))
    }
    if (grepl('^(con|prn|aux|nul|com[1-9]|lpt[1-9])$', value)) {
        .cdrgam_cli_abort(paste0(
            field, ' is reserved as a device name on Windows: ', value
        ))
    }
    value
}

.cdrgam_cli_positive_integer <- function(value, field) {
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
            !is.finite(value) || value < 1 || value %% 1 != 0 ||
            value > .Machine$integer.max) {
        .cdrgam_cli_abort(paste0(field, ' must be a positive integer'))
    }
    as.integer(value)
}

.cdrgam_cli_normalize_path <- function(path, must_work=FALSE) {
    if (isTRUE(must_work)) return(as.character(fs::path_real(path)))
    normalized <- vapply(path, function(value) {
        absolute <- fs::path_abs(fs::path_expand(value))
        current <- absolute
        suffix <- character()
        repeat {
            exists <- fs::file_exists(current) || fs::dir_exists(current) ||
                fs::link_exists(current)
            if (exists) break
            parent <- fs::path_dir(current)
            if (identical(as.character(parent), as.character(current))) break
            suffix <- c(as.character(fs::path_file(current)), suffix)
            current <- parent
        }
        prefix <- if (exists) fs::path_real(current) else current
        resolved <- if (length(suffix)) {
            do.call(fs::path, c(list(prefix), as.list(suffix)))
        } else {
            prefix
        }
        as.character(fs::path_norm(resolved))
    }, character(1), USE.NAMES=FALSE)
    names(normalized) <- names(path)
    normalized
}

.cdrgam_cli_os_type <- function() {
    .cdrgam_cli_null(getOption('cdrgam.cli.os_type'), .Platform$OS.type)
}

.cdrgam_cli_is_windows <- function() {
    identical(.cdrgam_cli_os_type(), 'windows')
}

.cdrgam_cli_host_name <- function() {
    .cdrgam_cli_null(unname(Sys.info()[['nodename']]), '')
}

.cdrgam_cli_same_host <- function(left, right=.cdrgam_cli_host_name()) {
    if (!is.character(left) || length(left) != 1L || is.na(left) ||
            !is.character(right) || length(right) != 1L || is.na(right)) {
        return(FALSE)
    }
    identical(tolower(left), tolower(right))
}

.cdrgam_cli_process_started <- function(pid=Sys.getpid()) {
    tryCatch(
        as.numeric(ps::ps_create_time(ps::ps_handle(as.integer(pid)))),
        error=function(error) NA_real_
    )
}

.cdrgam_cli_process_alive <- function(pid, started=NULL) {
    pid <- suppressWarnings(as.integer(pid))
    if (length(pid) != 1L || is.na(pid) || pid < 1L) return(FALSE)
    tryCatch({
        handle <- if (is.null(started)) {
            ps::ps_handle(pid)
        } else {
            started <- suppressWarnings(as.numeric(started))
            if (length(started) != 1L || is.na(started)) return(NA)
            ps::ps_handle(pid, time=as.POSIXct(started, origin='1970-01-01', tz='GMT'))
        }
        isTRUE(ps::ps_is_running(handle))
    }, error=function(error) {
        if (inherits(error, 'no_such_process')) FALSE else NA
    })
}

.cdrgam_cli_process_run <- function(
        command, arguments=character(), timeout=Inf, stdin=NULL,
        stdout='|', stderr='|', cleanup_tree=FALSE
) {
    tryCatch(
        processx::run(
            command, arguments, error_on_status=FALSE, timeout=timeout,
            stdin=stdin, stdout=stdout, stderr=stderr,
            cleanup_tree=cleanup_tree, windows_hide_window=TRUE
        ),
        error=function(error) NULL
    )
}

.cdrgam_cli_absolute_path <- function(path) {
    fs::is_absolute_path(path)
}

.cdrgam_cli_within <- function(path, root) {
    root <- .cdrgam_cli_normalize_path(root, must_work=FALSE)
    path <- .cdrgam_cli_normalize_path(path, must_work=FALSE)
    isTRUE(fs::path_has_parent(path, root))
}

.cdrgam_cli_try_move_path <- function(source, destination) {
    tryCatch({
        fs::file_move(source, destination)
        TRUE
    }, error=function(error) FALSE)
}

.cdrgam_cli_with_lock <- function(path, code, timeout=5000) {
    directory <- fs::path_dir(path)
    if (!fs::dir_exists(directory)) fs::dir_create(directory, recurse=TRUE)
    handle <- filelock::lock(path, timeout=timeout)
    if (is.null(handle)) {
        .cdrgam_cli_abort(paste0(
            'Timed out waiting for checkout lock ', sQuote(as.character(path))
        ))
    }
    on.exit(filelock::unlock(handle), add=TRUE)
    force(code)
}

.cdrgam_cli_replace_path <- function(source, destination) {
    source_is_directory <- fs::dir_exists(source)
    if (!fs::file_exists(source) && !source_is_directory) {
        .cdrgam_cli_abort(paste0('Replacement source does not exist: ', source))
    }
    destination_is_directory <- fs::dir_exists(destination)
    destination_exists <- fs::file_exists(destination) || destination_is_directory
    if (!destination_is_directory) {
        moved <- .cdrgam_cli_try_move_path(source, destination)
        if (moved) return(invisible(destination))
    }
    if (!destination_exists) {
        .cdrgam_cli_abort(paste0('Could not publish ', sQuote(destination)))
    }
    backup <- tempfile(
        paste0('.', basename(destination), '-previous-'),
        tmpdir=dirname(destination)
    )
    archived <- .cdrgam_cli_try_move_path(destination, backup)
    if (!archived) {
        .cdrgam_cli_abort(paste0(
            'Could not replace ', sQuote(destination),
            '; another process may have it open'
        ))
    }
    published <- .cdrgam_cli_try_move_path(source, destination)
    if (!published) {
        restored <- .cdrgam_cli_try_move_path(backup, destination)
        .cdrgam_cli_abort(paste0(
            'Could not publish ', sQuote(destination),
            if (restored) '' else '; the previous value remains at ',
            if (restored) '' else sQuote(backup)
        ))
    }
    if (fs::dir_exists(backup)) {
        fs::dir_delete(backup)
    } else if (fs::file_exists(backup) || fs::link_exists(backup)) {
        fs::file_delete(backup)
    }
    invisible(destination)
}

.cdrgam_cli_atomic_write <- function(path, writer) {
    directory <- dirname(path)
    if (!dir.exists(directory) && !dir.create(directory, recursive=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not create directory ', sQuote(directory)))
    }
    temporary <- tempfile(paste0('.', basename(path), '-'), tmpdir=directory)
    on.exit(unlink(temporary, recursive=TRUE, force=TRUE), add=TRUE)
    writer(temporary)
    .cdrgam_cli_replace_path(temporary, path)
    invisible(path)
}

.cdrgam_cli_write_yaml <- function(value, path) {
    .cdrgam_cli_atomic_write(path, function(temporary) {
        yaml::write_yaml(value, temporary)
    })
}

.cdrgam_cli_read_yaml <- function(path) {
    tryCatch(
        yaml::read_yaml(path, eval.expr=FALSE),
        error=function(error) .cdrgam_cli_abort(paste0(
            path, ': ', conditionMessage(error)
        ))
    )
}

.cdrgam_cli_hash <- function(value) {
    temporary <- tempfile('cdrgam-cli-hash-')
    on.exit(unlink(temporary), add=TRUE)
    saveRDS(value, temporary, version=3, compress=FALSE)
    unname(tools::md5sum(temporary))
}

.cdrgam_cli_short_hash <- function(value, length=16L) {
    substr(.cdrgam_cli_hash(value), 1L, length)
}

.cdrgam_cli_source_hash <- function(path) {
    unname(tools::md5sum(path))
}

.cdrgam_cli_timestamp <- function() {
    format(Sys.time(), '%Y-%m-%dT%H:%M:%S%z')
}

.cdrgam_cli_random_id <- function(prefix='project') {
    seed <- list(
        time=as.numeric(Sys.time()),
        pid=Sys.getpid(),
        random=stats::runif(4L)
    )
    paste0(prefix, '-', .cdrgam_cli_short_hash(seed, 24L))
}

.cdrgam_cli_null <- function(value, default) {
    if (is.null(value)) default else value
}
