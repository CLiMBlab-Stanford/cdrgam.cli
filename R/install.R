#' Install the `cdrgam` command launcher
#'
#' @param path Destination path. It normally belongs to a directory on `PATH`.
#'   The platform default is `~/.local/bin/cdrgam` on Unix and
#'   `~/.local/bin/cdrgam.cmd` on Windows.
#' @param root CDR-GAM root to use when `CDRGAM_ROOT` is unset. If it is not
#'   configured, this function creates it with local execution defaults.
#' @details The launcher uses the R executable and package library that contain
#'   the installed `cdrgam.cli` package. This prevents packages from another R
#'   installation's user library from shadowing its dependencies. The default
#'   root is `tools::R_user_dir("cdrgam.cli", "data")`; change it later with
#'   `cdrgam def edit site`.
#' @return The normalized launcher path, invisibly.
#' @export
install_cli <- function(
        path=NULL, root=NULL
) {
    if (is.null(path)) path <- file.path(
        '~', '.local', 'bin',
        if (.cdrgam_cli_is_windows()) 'cdrgam.cmd' else 'cdrgam'
    )
    path <- path.expand(path)
    root <- .cdrgam_cli_root(root, must_work=FALSE)
    marker <- file.path(root, .cdrgam_cli_site_marker)
    if (!file.exists(marker)) {
        cdrgam_cli_configure(cdrgam_root=root)
    }
    .cdrgam_cli_site(root, create_root=TRUE)
    rscript <- file.path(
        R.home('bin'), if (.cdrgam_cli_is_windows()) 'Rscript.exe' else 'Rscript'
    )
    if (!file.exists(rscript)) {
        .cdrgam_cli_abort(paste0('Rscript was not found at ', rscript))
    }
    library <- dirname(find.package('cdrgam.cli'))
    directory <- dirname(path)
    if (!dir.exists(directory) && !dir.create(directory, recursive=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not create ', directory))
    }
    .cdrgam_cli_atomic_write(path, function(temporary) writeLines(
        .cdrgam_cli_launcher_lines(root, library, rscript),
        temporary, useBytes=TRUE
    ))
    if (!.cdrgam_cli_is_windows()) Sys.chmod(path, mode='0755')
    message('Installed cdrgam launcher at ', path)
    invisible(.cdrgam_cli_normalize_path(path, must_work=TRUE))
}

.cdrgam_cli_launcher_lines <- function(root, library, rscript) {
    if (.cdrgam_cli_is_windows()) {
        escape <- function(value) gsub('%', '%%', value, fixed=TRUE)
        return(c(
            '@echo off',
            paste0(
                'if not defined CDRGAM_ROOT set "CDRGAM_ROOT=',
                escape(root), '"'
            ),
            paste0('set "R_LIBS_USER=', escape(library), '"'),
            paste0(
                '"', escape(rscript), '" -e ',
                '"cdrgam.cli::cli_main(commandArgs(trailingOnly=TRUE))" ',
                '--args %*'
            )
        ))
    }
    c(
        '#!/bin/sh',
        paste0(
            'if [ -z "${CDRGAM_ROOT:-}" ]; then CDRGAM_ROOT=',
            shQuote(root), '; fi'
        ),
        'export CDRGAM_ROOT',
        paste('export R_LIBS_USER=', shQuote(library), sep=''),
        paste(
            'exec', shQuote(rscript),
            "-e 'cdrgam.cli::cli_main(commandArgs(trailingOnly=TRUE))'",
            '--args "$@"'
        )
    )
}
