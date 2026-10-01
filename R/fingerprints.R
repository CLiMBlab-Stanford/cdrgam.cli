.cdrgam_cli_fingerprint_cache_path <- function(configuration) {
    file.path(
        configuration$cdrgam_root, '.cdrgam', 'cache',
        'source-fingerprints.rds'
    )
}

.cdrgam_cli_read_fingerprint_records <- function(path) {
    if (!file.exists(path)) return(list())
    value <- tryCatch(readRDS(path), error=function(error) NULL)
    if (!is.list(value) || is.null(value$records) || !is.list(value$records)) {
        return(list())
    }
    value$records
}

.cdrgam_cli_fingerprint_context <- function(configuration, progress=NULL) {
    path <- .cdrgam_cli_fingerprint_cache_path(configuration)
    context <- new.env(parent=emptyenv())
    context$configuration <- configuration
    context$path <- path
    context$records <- .cdrgam_cli_read_fingerprint_records(path)
    context$updates <- list()
    context$datasets <- new.env(hash=TRUE, parent=emptyenv())
    context$fits <- new.env(hash=TRUE, parent=emptyenv())
    context$progress <- progress
    context
}

.cdrgam_cli_file_signature <- function(path) {
    info <- file.info(path)
    if (!nrow(info) || is.na(info$size[[1L]]) || is.na(info$mtime[[1L]])) {
        .cdrgam_cli_abort(paste0('Cannot inspect source file ', sQuote(path)))
    }
    list(
        size=unname(info$size[[1L]]),
        mtime=unname(as.numeric(info$mtime[[1L]]))
    )
}

.cdrgam_cli_source_fingerprint <- function(path, context=NULL) {
    if (is.null(context)) return(.cdrgam_cli_source_hash(path))
    normalized <- .cdrgam_cli_normalize_path(path, must_work=TRUE)
    signature <- .cdrgam_cli_file_signature(normalized)
    record <- context$records[[normalized]]
    valid <- is.list(record) && identical(record$size, signature$size) &&
        identical(record$mtime, signature$mtime) &&
        is.character(record$md5) && length(record$md5) == 1L &&
        !is.na(record$md5) && nzchar(record$md5)
    if (valid) return(record$md5)
    .cdrgam_cli_progress(
        context$progress,
        paste0('fingerprinting data source ', basename(normalized)),
        pulse=TRUE
    )
    record <- c(signature, list(md5=.cdrgam_cli_source_hash(normalized)))
    context$records[[normalized]] <- record
    context$updates[[normalized]] <- record
    record$md5
}

.cdrgam_cli_flush_fingerprint_context <- function(context) {
    if (is.null(context) || !length(context$updates)) return(invisible(FALSE))
    path <- context$path
    lock <- paste0(path, '.lock')
    .cdrgam_cli_with_lock(lock, {
        records <- .cdrgam_cli_read_fingerprint_records(path)
        records[names(context$updates)] <- context$updates
        .cdrgam_cli_atomic_write(path, function(temporary) {
            saveRDS(list(schema=1L, records=records), temporary, version=3)
        })
    })
    context$updates <- list()
    invisible(TRUE)
}
