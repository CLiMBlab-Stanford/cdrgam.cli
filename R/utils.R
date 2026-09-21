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
    normalizePath(path, winslash='/', mustWork=must_work)
}

.cdrgam_cli_within <- function(path, root) {
    root <- sub('/+$', '', .cdrgam_cli_normalize_path(root, must_work=FALSE))
    path <- .cdrgam_cli_normalize_path(path, must_work=FALSE)
    identical(path, root) || startsWith(path, paste0(root, '/'))
}

.cdrgam_cli_atomic_write <- function(path, writer) {
    directory <- dirname(path)
    if (!dir.exists(directory) && !dir.create(directory, recursive=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not create directory ', sQuote(directory)))
    }
    temporary <- tempfile(paste0('.', basename(path), '-'), tmpdir=directory)
    on.exit(unlink(temporary, recursive=TRUE, force=TRUE), add=TRUE)
    writer(temporary)
    if (!file.rename(temporary, path)) {
        .cdrgam_cli_abort(paste0('Could not publish ', sQuote(path)))
    }
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
