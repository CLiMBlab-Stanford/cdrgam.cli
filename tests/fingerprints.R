library(cdrgam.cli)

internal <- function(name) getFromNamespace(name, 'cdrgam.cli')
root <- tempfile('cdrgam-cli-fingerprints-')
dir.create(root)
on.exit(unlink(root, recursive=TRUE), add=TRUE)

configuration <- list(cdrgam_root=root)
source <- file.path(root, 'source.csv')
writeLines(c('x', '1'), source)

first <- internal('.cdrgam_cli_fingerprint_context')(configuration)
first_hash <- internal('.cdrgam_cli_source_fingerprint')(source, first)
stopifnot(length(first$updates) == 1L)
internal('.cdrgam_cli_flush_fingerprint_context')(first)
stopifnot(file.exists(internal('.cdrgam_cli_fingerprint_cache_path')(
    configuration
)))

second <- internal('.cdrgam_cli_fingerprint_context')(configuration)
second_hash <- internal('.cdrgam_cli_source_fingerprint')(source, second)
stopifnot(identical(first_hash, second_hash), !length(second$updates))

writeLines(c('x', '2'), source)
Sys.setFileTime(source, Sys.time() + 2)
third <- internal('.cdrgam_cli_fingerprint_context')(configuration)
third_hash <- internal('.cdrgam_cli_source_fingerprint')(source, third)
stopifnot(!identical(second_hash, third_hash), length(third$updates) == 1L)

progress <- internal('.cdrgam_cli_progress_context')()
progress$terminal <- FALSE
stages <- capture.output({
    internal('.cdrgam_cli_progress')(progress, 'reading definitions')
    internal('.cdrgam_cli_progress')(progress, 'source detail', pulse=TRUE)
    internal('.cdrgam_cli_progress_finish')(progress, 'request accepted')
}, type='message')
stopifnot(
    identical(stages, c(
        'cdrgam run: reading definitions',
        'cdrgam run: request accepted'
    ))
)

terminal_progress <- internal('.cdrgam_cli_progress_context')()
terminal_progress$terminal <- TRUE
terminal_stages <- paste(capture.output({
    internal('.cdrgam_cli_progress')(terminal_progress, 'first')
    internal('.cdrgam_cli_progress')(terminal_progress, 'second')
    internal('.cdrgam_cli_progress_finish')(terminal_progress)
}, type='message'), collapse='')
escape <- intToUtf8(27L)
stopifnot(
    grepl(paste0(escape, '[36m'), terminal_stages, fixed=TRUE),
    grepl(paste0(escape, '[34m'), terminal_stages, fixed=TRUE),
    grepl('cdrgam run: second', terminal_stages, fixed=TRUE)
)

cat('fingerprints: ok\n')
