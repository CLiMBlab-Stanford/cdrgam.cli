source_candidates <- c(
    'R',
    file.path('..', 'R'),
    file.path('..', '00_pkg_src', 'cdrgam.cli', 'R')
)
source_root <- source_candidates[dir.exists(source_candidates)][[1L]]
stopifnot(dir.exists(source_root))

source_files <- list.files(source_root, pattern='[.]R$', full.names=TRUE)
relative <- basename(source_files)
contents <- lapply(source_files, readLines, warn=FALSE)
names(contents) <- relative

files_matching <- function(pattern) {
    names(Filter(function(lines) any(grepl(pattern, lines, perl=TRUE)), contents))
}

core_clients <- files_matching('cdrgam::|asNamespace\\([\'\"]cdrgam[\'\"]')
database_clients <- files_matching('DBI::|RSQLite::')
raw_registry_clients <- files_matching('\\.cdrgam_cli_registry_exec\\(')

stopifnot(
    identical(core_clients, 'core-adapter.R'),
    identical(database_clients, 'registry.R'),
    identical(raw_registry_clients, 'registry.R')
)

fit_internal_clients <- names(Filter(function(lines) {
    any(grepl(
        '(?<!\\$)fit\\$(cdrgam|family|method|sparse)',
        lines,
        perl=TRUE
    ))
}, contents))
stopifnot(all(fit_internal_clients %in% 'core-adapter.R'))

cat('architecture: ok\n')
