.cdrgam_cli_checkout_marker <- file.path('.cdrgam', 'checkout.yml')

.cdrgam_cli_checkout_root <- function(checkout=NULL, must_work=TRUE) {
    checkout <- .cdrgam_cli_null(checkout, getOption('cdrgam.cli.checkout'))
    checkout <- .cdrgam_cli_null(checkout, Sys.getenv('CDRGAM_CHECKOUT', ''))
    if (!nzchar(checkout)) {
        candidate <- getwd()
        repeat {
            if (file.exists(file.path(candidate, .cdrgam_cli_checkout_marker))) {
                checkout <- candidate
                break
            }
            parent <- dirname(candidate)
            if (identical(parent, candidate)) break
            candidate <- parent
        }
    }
    if (!nzchar(checkout)) {
        .cdrgam_cli_abort(paste0(
            'No configured CDR-GAM checkout was selected; run the checkout ',
            'installer or set CDRGAM_CHECKOUT'
        ))
    }
    checkout <- .cdrgam_cli_normalize_path(checkout, must_work=must_work)
    if (must_work && !file.exists(file.path(checkout, .cdrgam_cli_checkout_marker))) {
        .cdrgam_cli_abort(paste0(
            'Checkout configuration is missing: ',
            file.path(checkout, .cdrgam_cli_checkout_marker)
        ))
    }
    checkout
}

.cdrgam_cli_validate_checkout <- function(value, path) {
    .cdrgam_cli_validate_schema(value, path)
    allowed <- c(
        'schema', 'cdrgam_root', 'concurrency', 'slurm_partition',
        'slurm_account', 'slurm_cpus', 'slurm_memory', 'slurm_time',
        'slurm_qos'
    )
    .cdrgam_cli_check_keys(
        value, allowed, path, c('schema', 'cdrgam_root', 'concurrency')
    )
    value$cdrgam_root <- .cdrgam_cli_normalize_path(
        .cdrgam_cli_scalar_character(value$cdrgam_root, paste0(path, ': cdrgam_root')),
        must_work=FALSE
    )
    value$concurrency <- .cdrgam_cli_positive_integer(
        value$concurrency, paste0(path, ': concurrency')
    )
    has_partition <- !is.null(value$slurm_partition)
    has_account <- !is.null(value$slurm_account)
    if (xor(has_partition, has_account)) {
        .cdrgam_cli_abort(paste0(
            path, ': slurm_partition and slurm_account must be defined together'
        ))
    }
    for (field in c(
            'slurm_partition', 'slurm_account', 'slurm_memory', 'slurm_time',
            'slurm_qos'
    )) {
        if (!is.null(value[[field]])) {
            value[[field]] <- .cdrgam_cli_scalar_character(
                value[[field]], paste0(path, ': ', field)
            )
            if (grepl('[[:space:]]', value[[field]])) {
                .cdrgam_cli_abort(paste0(path, ': ', field, ' must not contain whitespace'))
            }
        }
    }
    if (!is.null(value$slurm_cpus)) {
        value$slurm_cpus <- .cdrgam_cli_positive_integer(
            value$slurm_cpus, paste0(path, ': slurm_cpus')
        )
    }
    value$scheduler <- if (has_partition) 'slurm' else 'local'
    value
}

.cdrgam_cli_checkout <- function(checkout=NULL, create_root=FALSE) {
    checkout <- .cdrgam_cli_checkout_root(checkout)
    path <- file.path(checkout, .cdrgam_cli_checkout_marker)
    value <- .cdrgam_cli_validate_checkout(.cdrgam_cli_read_yaml(path), path)
    value$checkout <- checkout
    if (isTRUE(create_root)) {
        for (directory in c(
                value$cdrgam_root,
                file.path(value$cdrgam_root, 'projects'),
                file.path(value$cdrgam_root, '.cdrgam'),
                file.path(value$cdrgam_root, '.cdrgam', 'work'),
                file.path(value$cdrgam_root, '.cdrgam', 'logs')
        )) {
            if (!dir.exists(directory) && !dir.create(directory, recursive=TRUE)) {
                .cdrgam_cli_abort(paste0('Could not create ', sQuote(directory)))
            }
        }
    }
    value
}

#' Configure a harness instance
#'
#' @param checkout Writable harness instance directory. It is normally the
#'   source checkout during development, but need not contain package code.
#' @param cdrgam_root Root containing projects and private orchestration state.
#' @param concurrency Maximum concurrent Slurm workers across the instance.
#' @param slurm_partition,slurm_account Optional Slurm routing fields. Supply
#'   both to enable Slurm, or neither for local execution.
#' @param slurm_cpus,slurm_memory,slurm_time,slurm_qos Optional Slurm defaults.
#' @details A checkout-level lock serializes configuration publication.
#' @return The validated configuration, invisibly.
#' @export
cdrgam_cli_configure <- function(
        checkout='.', cdrgam_root, concurrency=1L,
        slurm_partition=NULL, slurm_account=NULL, slurm_cpus=NULL,
        slurm_memory=NULL, slurm_time=NULL, slurm_qos=NULL
) {
    checkout <- .cdrgam_cli_normalize_path(checkout, must_work=TRUE)
    value <- list(
        schema=1L,
        cdrgam_root=.cdrgam_cli_normalize_path(path.expand(cdrgam_root), FALSE),
        concurrency=concurrency
    )
    optional <- list(
        slurm_partition=slurm_partition, slurm_account=slurm_account,
        slurm_cpus=slurm_cpus, slurm_memory=slurm_memory,
        slurm_time=slurm_time, slurm_qos=slurm_qos
    )
    value <- c(value, optional[!vapply(optional, is.null, logical(1))])
    path <- file.path(checkout, .cdrgam_cli_checkout_marker)
    validated <- .cdrgam_cli_validate_checkout(value, path)
    validated$scheduler <- NULL
    .cdrgam_cli_with_lock(
        file.path(checkout, '.cdrgam', 'locks', 'checkout.lock'),
        .cdrgam_cli_write_yaml(validated, path)
    )
    options(cdrgam.cli.checkout=checkout)
    .cdrgam_cli_checkout(checkout, create_root=TRUE)
    message('Configured CDR-GAM checkout at ', checkout)
    invisible(.cdrgam_cli_checkout(checkout))
}

.cdrgam_cli_project_root <- function(project, checkout=NULL, must_work=TRUE) {
    project <- .cdrgam_cli_name(project, 'project')
    configuration <- .cdrgam_cli_checkout(checkout)
    path <- file.path(configuration$cdrgam_root, 'projects', project)
    .cdrgam_cli_normalize_path(path, must_work=must_work)
}

.cdrgam_cli_infer_project <- function(checkout=NULL) {
    configuration <- .cdrgam_cli_checkout(checkout)
    projects <- .cdrgam_cli_normalize_path(
        file.path(configuration$cdrgam_root, 'projects'), FALSE
    )
    current <- .cdrgam_cli_normalize_path(getwd(), TRUE)
    if (!.cdrgam_cli_within(current, projects) || identical(current, projects)) {
        .cdrgam_cli_abort('Select a project with -P/--project')
    }
    relative <- substring(current, nchar(projects) + 2L)
    .cdrgam_cli_name(strsplit(relative, '/', fixed=TRUE)[[1L]][[1L]], 'project')
}
