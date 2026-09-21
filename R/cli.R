.cdrgam_cli_usage <- function() {
    paste(
        'Usage:',
        '  cdrgam def site',
        '  cdrgam def PROJECT [--dataset DATASET | --model MODEL |',
        '                      --visualization VISUALIZATION | --comparison COMPARISON]',
        '  cdrgam def --copy-from-to SOURCE DESTINATION',
        '  cdrgam validate [-P PROJECT ...] [--deep]',
        '  cdrgam list [-P PROJECT ...]',
        '  cdrgam plan [-P PROJECT ...] [-m MODEL] [-p PARTITION ...]',
        '              [-v VISUALIZATION] [-c COMPARISON]',
        '  cdrgam run [-P PROJECT ...] [-m MODEL] [-p PARTITION ...]',
        '             [-v VISUALIZATION] [-c COMPARISON] [--dry-run]',
        '             [--cpus N] [--memory SIZE] [--time LIMIT] [--qos NAME]',
        '  cdrgam status [-P PROJECT ...] [--no-pager]',
        '  cdrgam log [-P PROJECT ...] [-m MODEL ...] [-p PARTITION ...]',
        '             [-v VISUALIZATION ...] [-c COMPARISON ...] [--lines N]',
        '  cdrgam purge [-P PROJECT ...] [-m MODEL ...] [--work] [--logs] [--yes]',
        sep='\n'
    )
}

.cdrgam_cli_parse <- function(arguments) {
    aliases <- c(
        '-P'='--project', '-m'='--model', '-p'='--prediction',
        '-v'='--visualization', '-c'='--comparison'
    )
    arguments <- vapply(arguments, function(argument) {
        if (argument %in% names(aliases)) aliases[[argument]] else argument
    }, character(1))
    flags <- list()
    positional <- character()
    value_flags <- c(
        '--project', '--model', '--prediction', '--visualization', '--comparison',
        '--lines', '--dataset', '--copy-from-to', '--attempt', '--request-file',
        '--work-key', '--controller-host', '--controller-port',
        '--controller-token', '--checkout', '--worker-id', '--resource-key',
        '--cpus', '--memory', '--time', '--qos'
    )
    boolean_flags <- c('--deep', '--dry-run',
        '--work', '--logs', '--yes', '--no-pager')
    i <- 1L
    while (i <= length(arguments)) {
        argument <- arguments[[i]]
        if (identical(argument, '--copy-from-to')) {
            if (i + 2L > length(arguments) ||
                    any(startsWith(arguments[c(i + 1L, i + 2L)], '-'))) {
                .cdrgam_cli_abort('--copy-from-to requires source and destination projects')
            }
            flags$`copy-from-to` <- arguments[c(i + 1L, i + 2L)]
            i <- i + 3L
        } else if (argument %in% value_flags) {
            if (i == length(arguments)) .cdrgam_cli_abort(paste0(argument, ' requires a value'))
            key <- substring(argument, 3L)
            flags[[key]] <- c(flags[[key]], arguments[[i + 1L]])
            i <- i + 2L
        } else if (argument %in% boolean_flags) {
            flags[[substring(argument, 3L)]] <- TRUE
            i <- i + 1L
        } else if (startsWith(argument, '-')) {
            .cdrgam_cli_abort(paste0('Unknown option: ', argument))
        } else {
            positional <- c(positional, argument)
            i <- i + 1L
        }
    }
    list(positional=positional, flags=flags)
}

.cdrgam_cli_check_flags <- function(flags, allowed, command) {
    unexpected <- setdiff(names(flags), allowed)
    if (length(unexpected)) .cdrgam_cli_abort(paste0(
        command, ' does not accept ', paste0('--', unexpected, collapse=', ')
    ))
}

.cdrgam_cli_one <- function(value, field, required=FALSE) {
    if (!length(value)) {
        if (required) .cdrgam_cli_abort(paste0(field, ' is required'))
        return(NULL)
    }
    if (length(value) != 1L) .cdrgam_cli_abort(paste0(field, ' accepts one value'))
    value[[1L]]
}

.cdrgam_cli_def_command <- function(parsed) {
    positional <- parsed$positional
    flags <- parsed$flags
    if ('copy-from-to' %in% names(flags)) {
        if (length(positional) || length(flags) != 1L) {
            .cdrgam_cli_abort('--copy-from-to cannot be combined with other arguments')
        }
        return(cdrgam_cli_def(copy_from_to=flags$`copy-from-to`))
    }
    if (length(positional) != 1L) {
        .cdrgam_cli_abort('def requires site or one project name')
    }
    supported <- c('dataset', 'model', 'visualization', 'comparison')
    .cdrgam_cli_check_flags(flags, supported, 'def')
    selected <- intersect(names(flags), supported)
    if (length(selected) > 1L) {
        .cdrgam_cli_abort('def accepts at most one definition selector')
    }
    type <- if (length(selected)) selected[[1L]] else NULL
    name <- if (is.null(type)) NULL else
        .cdrgam_cli_one(flags[[type]], paste0('--', type), required=TRUE)
    cdrgam_cli_def(positional[[1L]], type=type, name=name)
}

.cdrgam_cli_selector_arguments <- function(flags) list(
    projects=flags$project, models=flags$model, predictions=flags$prediction,
    visualizations=flags$visualization, comparisons=flags$comparison
)

#' Run the command-line dispatcher
#'
#' @param args Command arguments excluding the executable name.
#' @return An integer exit status, invisibly.
#' @export
cli_main <- function(args=commandArgs(trailingOnly=TRUE)) {
    if (length(args) && identical(args[[1L]], '--args')) args <- args[-1L]
    if (!length(args) || args[[1L]] %in% c('-h', '--help', 'help')) {
        cat(.cdrgam_cli_usage(), '\n')
        return(invisible(0L))
    }
    command <- args[[1L]]
    parsed <- .cdrgam_cli_parse(args[-1L])
    flags <- parsed$flags
    if (identical(command, 'scheduler')) {
        .cdrgam_cli_check_flags(flags, 'checkout', 'scheduler')
        if (length(parsed$positional)) .cdrgam_cli_abort('scheduler accepts no positional values')
        .cdrgam_cli_controller_main(.cdrgam_cli_one(
            flags$checkout, '--checkout', required=TRUE
        ))
        return(invisible(0L))
    }
    if (identical(command, 'worker')) {
        required <- c(
            'worker-id', 'resource-key', 'controller-host', 'controller-port',
            'controller-token'
        )
        .cdrgam_cli_check_flags(flags, required, 'worker')
        if (length(parsed$positional)) .cdrgam_cli_abort('worker accepts no positional values')
        values <- lapply(required, function(name) {
            .cdrgam_cli_one(flags[[name]], paste0('--', name), required=TRUE)
        })
        do.call(.cdrgam_cli_worker, stats::setNames(values, gsub('-', '_', required)))
        return(invisible(0L))
    }
    if (identical(command, 'def')) {
        .cdrgam_cli_def_command(parsed)
        return(invisible(0L))
    }
    if (length(parsed$positional)) {
        .cdrgam_cli_abort('Project paths are not accepted; use -P/--project')
    }
    selectors <- .cdrgam_cli_selector_arguments(flags)
    if (identical(command, 'validate')) {
        .cdrgam_cli_check_flags(flags, c('project', 'deep'), 'validate')
        cdrgam_cli_validate(flags$project, deep=isTRUE(flags$deep))
    } else if (identical(command, 'list')) {
        .cdrgam_cli_check_flags(flags, 'project', 'list')
        cdrgam_cli_list(flags$project)
    } else if (identical(command, 'plan')) {
        .cdrgam_cli_check_flags(
            flags, c('project', 'model', 'prediction', 'visualization', 'comparison'),
            'plan'
        )
        do.call(cdrgam_cli_plan, selectors)
    } else if (identical(command, 'run')) {
        .cdrgam_cli_check_flags(flags, c(
            'project', 'model', 'prediction', 'visualization', 'comparison',
            'dry-run', 'cpus', 'memory', 'time', 'qos'
        ), 'run')
        arguments <- c(selectors, list(
            dry_run=isTRUE(flags$`dry-run`),
            cpus=if (length(flags$cpus)) as.integer(.cdrgam_cli_one(flags$cpus, '--cpus')) else NULL,
            memory=.cdrgam_cli_one(flags$memory, '--memory'),
            time=.cdrgam_cli_one(flags$time, '--time'),
            qos=.cdrgam_cli_one(flags$qos, '--qos')
        ))
        do.call(cdrgam_cli_run, arguments)
    } else if (identical(command, 'status')) {
        .cdrgam_cli_check_flags(flags, c('project', 'no-pager'), 'status')
        cdrgam_cli_status(
            flags$project, use_pager=!isTRUE(flags$`no-pager`)
        )
    } else if (identical(command, 'log')) {
        .cdrgam_cli_check_flags(flags, c(
            'project', 'model', 'prediction', 'visualization', 'comparison', 'lines'
        ), 'log')
        arguments <- c(selectors, list(
            lines=if (length(flags$lines)) {
                .cdrgam_cli_positive_integer(
                    suppressWarnings(as.numeric(
                        .cdrgam_cli_one(flags$lines, '--lines')
                    )), '--lines'
                )
            } else NULL
        ))
        do.call(cdrgam_cli_log, arguments)
    } else if (identical(command, 'purge')) {
        .cdrgam_cli_check_flags(
            flags, c('project', 'model', 'work', 'logs', 'yes'), 'purge'
        )
        cdrgam_cli_purge(
            projects=flags$project, models=flags$model, work=isTRUE(flags$work),
            logs=isTRUE(flags$logs), yes=isTRUE(flags$yes)
        )
    } else {
        .cdrgam_cli_abort(paste0('Unknown command: ', command, '\n', .cdrgam_cli_usage()))
    }
    invisible(0L)
}
