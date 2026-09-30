library(cdrgam.cli)

capture <- function(arguments) paste(capture.output(
    stopifnot(identical(cli_main(arguments), 0L))
), collapse='\n')

root_help <- capture('--help')
version <- capture('--version')
stopifnot(
    identical(root_help, capture('-h')),
    identical(root_help, capture('help')),
    grepl('Usage: cdrgam <COMMAND>', root_help, fixed=TRUE),
    grepl('Commands:', root_help, fixed=TRUE),
    grepl('-V, --version', root_help, fixed=TRUE),
    identical(version, capture('-V')),
    grepl('cdrgam.cli ', version, fixed=TRUE),
    grepl(' (cdrgam ', version, fixed=TRUE),
    !grepl('scheduler', root_help, fixed=TRUE),
    !grepl('worker', root_help, fixed=TRUE)
)

run_help <- capture(c('run', '--help'))
def_help <- capture(c('def', '--help'))
purge_help <- capture(c('purge', '--help'))
stopifnot(
    grepl('Usage: cdrgam run', run_help, fixed=TRUE),
    grepl('-P, --project PROJECT ...', run_help, fixed=TRUE),
    grepl('re: regex', run_help, fixed=TRUE),
    grepl('--dry-run', run_help, fixed=TRUE),
    all(vapply(
        c('edit', 'init', 'ls', 'rm', 'val'),
        grepl,
        logical(1),
        x=def_help,
        fixed=TRUE
    )),
    grepl('Definitions are never selected', purge_help, fixed=TRUE),
    grepl('--yes', purge_help, fixed=TRUE)
)

cat('cli: ok\n')
