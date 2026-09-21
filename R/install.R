#' Install the `cdrgam` command launcher
#'
#' @param path Destination path. It normally belongs to a directory on `PATH`.
#' @param checkout Configured source checkout to bind to the launcher.
#' @details The launcher uses the R executable and package library that contain
#'   the installed `cdrgam.cli` package. This prevents packages from another R
#'   installation's user library from shadowing its dependencies.
#' @return The normalized launcher path, invisibly.
#' @export
install_cli <- function(
        path=file.path('~', '.local', 'bin', 'cdrgam'), checkout=NULL
) {
    path <- path.expand(path)
    checkout <- .cdrgam_cli_checkout_root(checkout)
    .cdrgam_cli_checkout(checkout, create_root=TRUE)
    rscript <- file.path(R.home('bin'), 'Rscript')
    if (!file.exists(rscript)) {
        .cdrgam_cli_abort(paste0('Rscript was not found at ', rscript))
    }
    library <- dirname(find.package('cdrgam.cli'))
    directory <- dirname(path)
    if (!dir.exists(directory) && !dir.create(directory, recursive=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not create ', directory))
    }
    .cdrgam_cli_atomic_write(path, function(temporary) {
        writeLines(c(
            '#!/bin/sh',
            paste('export CDRGAM_CHECKOUT=', shQuote(checkout), sep=''),
            paste('export R_LIBS_USER=', shQuote(library), sep=''),
            paste(
                'exec', shQuote(rscript),
                "-e 'cdrgam.cli::cli_main(commandArgs(trailingOnly=TRUE))'",
                '--args "$@"'
            )
        ), temporary, useBytes=TRUE)
    })
    Sys.chmod(path, mode='0755')
    message('Installed cdrgam launcher at ', path)
    invisible(.cdrgam_cli_normalize_path(path, must_work=TRUE))
}
