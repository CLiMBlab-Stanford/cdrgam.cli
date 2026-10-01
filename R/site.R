.cdrgam_cli_site_marker <- file.path('.cdrgam', 'site.yml')

.cdrgam_cli_default_root <- function() {
    .cdrgam_cli_normalize_path(
        tools::R_user_dir('cdrgam.cli', 'data'), must_work=FALSE
    )
}

.cdrgam_cli_root <- function(root=NULL, must_work=TRUE) {
    if (is.null(root)) {
        environment_root <- Sys.getenv('CDRGAM_ROOT', '')
        if (nzchar(environment_root)) root <- environment_root
    }
    root <- .cdrgam_cli_null(root, getOption('cdrgam.cli.root'))
    if (is.null(root) || !nzchar(root)) {
        candidate <- getwd()
        repeat {
            if (file.exists(file.path(candidate, .cdrgam_cli_site_marker))) {
                root <- candidate
                break
            }
            parent <- dirname(candidate)
            if (identical(parent, candidate)) break
            candidate <- parent
        }
    }
    if (is.null(root) || !nzchar(root)) {
        default <- .cdrgam_cli_default_root()
        if (!must_work || file.exists(file.path(
                default, .cdrgam_cli_site_marker
        ))) {
            root <- default
        } else {
            .cdrgam_cli_abort(paste0(
                'No configured CDR-GAM root was selected; run install_cli(), ',
                'cdrgam def edit site, or set CDRGAM_ROOT'
            ))
        }
    }
    root <- .cdrgam_cli_normalize_path(root, must_work=must_work)
    if (must_work && !file.exists(file.path(root, .cdrgam_cli_site_marker))) {
        .cdrgam_cli_abort(paste0(
            'Site configuration is missing: ',
            file.path(root, .cdrgam_cli_site_marker)
        ))
    }
    root
}

.cdrgam_cli_validate_site <- function(value, path) {
    .cdrgam_cli_validate_schema(value, path)
    allowed <- c(
        'schema', 'concurrency', 'slurm_partition', 'slurm_account',
        'slurm_cpus', 'slurm_memory', 'slurm_time', 'slurm_qos'
    )
    .cdrgam_cli_check_keys(value, allowed, path, c('schema', 'concurrency'))
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
                .cdrgam_cli_abort(paste0(
                    path, ': ', field, ' must not contain whitespace'
                ))
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

.cdrgam_cli_site <- function(root=NULL, create_root=FALSE) {
    root <- .cdrgam_cli_root(root)
    path <- file.path(root, .cdrgam_cli_site_marker)
    value <- .cdrgam_cli_validate_site(.cdrgam_cli_read_yaml(path), path)
    value$cdrgam_root <- root
    if (isTRUE(create_root)) {
        for (directory in c(
                root, file.path(root, 'projects'), file.path(root, '.cdrgam'),
                file.path(root, '.cdrgam', 'work'),
                file.path(root, '.cdrgam', 'logs')
        )) {
            if (!dir.exists(directory) &&
                    !dir.create(directory, recursive=TRUE)) {
                .cdrgam_cli_abort(paste0('Could not create ', sQuote(directory)))
            }
        }
    }
    value
}

#' Configure a CDR-GAM root
#'
#' @param cdrgam_root Root containing site configuration, projects, and private
#'   orchestration state. The default is R's platform-specific user data
#'   directory for `cdrgam.cli`.
#' @param concurrency Maximum concurrent Slurm workers across the root.
#' @param slurm_partition,slurm_account Optional Slurm routing fields. Supply
#'   both to enable Slurm, or neither for local execution.
#' @param slurm_cpus,slurm_memory,slurm_time,slurm_qos Optional Slurm defaults.
#' @details A root-local lock serializes site configuration publication.
#' @return The validated configuration, invisibly.
#' @export
cdrgam_cli_configure <- function(
        cdrgam_root=.cdrgam_cli_default_root(), concurrency=1L,
        slurm_partition=NULL, slurm_account=NULL, slurm_cpus=NULL,
        slurm_memory=NULL, slurm_time=NULL, slurm_qos=NULL
) {
    root <- .cdrgam_cli_normalize_path(cdrgam_root, must_work=FALSE)
    if (!dir.exists(root) && !dir.create(root, recursive=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not create ', root))
    }
    value <- list(schema=1L, concurrency=concurrency)
    optional <- list(
        slurm_partition=slurm_partition, slurm_account=slurm_account,
        slurm_cpus=slurm_cpus, slurm_memory=slurm_memory,
        slurm_time=slurm_time, slurm_qos=slurm_qos
    )
    value <- c(value, optional[!vapply(optional, is.null, logical(1))])
    path <- file.path(root, .cdrgam_cli_site_marker)
    validated <- .cdrgam_cli_validate_site(value, path)
    validated$scheduler <- NULL
    .cdrgam_cli_with_lock(
        file.path(root, '.cdrgam', 'locks', 'site.lock'),
        .cdrgam_cli_write_yaml(validated, path)
    )
    options(cdrgam.cli.root=root)
    .cdrgam_cli_site(root, create_root=TRUE)
    message('Configured CDR-GAM root at ', root)
    invisible(.cdrgam_cli_site(root))
}

.cdrgam_cli_project_root <- function(project, root=NULL, must_work=TRUE) {
    project <- .cdrgam_cli_name(project, 'project')
    configuration <- .cdrgam_cli_site(root)
    path <- file.path(configuration$cdrgam_root, 'projects', project)
    .cdrgam_cli_normalize_path(path, must_work=must_work)
}

.cdrgam_cli_infer_project <- function(root=NULL) {
    configuration <- .cdrgam_cli_site(root)
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
