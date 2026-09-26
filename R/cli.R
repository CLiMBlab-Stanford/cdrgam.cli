.cdrgam_cli_option <- function(
        name, help, short=NULL, metavar=NULL, arity='one', required=FALSE,
        type='character', hidden=FALSE
) {
    list(
        name=name, long=paste0('--', name), short=short, metavar=metavar,
        arity=arity, required=required, type=type, help=help, hidden=hidden
    )
}

.cdrgam_cli_argument <- function(name, help, required=TRUE) {
    list(name=name, help=help, required=required)
}

.cdrgam_cli_command <- function(
        name, summary, description=summary, options=list(), arguments=list(),
        commands=list(), examples=character(), handler=NULL, hidden=FALSE
) {
    list(
        name=name, summary=summary, description=description, options=options,
        arguments=arguments, commands=commands, examples=examples,
        handler=handler, hidden=hidden
    )
}

.cdrgam_cli_selector_options <- function(arity='one-or-more') list(
    .cdrgam_cli_option(
        'project', paste(
            'Select projects. Omission uses the current project when possible,',
            'otherwise all projects.'
        ),
        short='-P', metavar='PROJECT', arity=arity
    ),
    .cdrgam_cli_option(
        'model', 'Select model workloads. Multiple selector dimensions are conjunctive.',
        short='-m', metavar='MODEL', arity=arity
    ),
    .cdrgam_cli_option(
        'prediction', 'Select model partitions or prediction datasets.',
        short='-p', metavar='PARTITION', arity=arity
    ),
    .cdrgam_cli_option(
        'visualization', 'Select visualization workloads.',
        short='-v', metavar='VISUALIZATION', arity=arity
    ),
    .cdrgam_cli_option(
        'comparison', 'Select comparison workloads.',
        short='-c', metavar='COMPARISON', arity=arity
    )
)

.cdrgam_cli_definition_options <- function(arity) list(
    .cdrgam_cli_option(
        'dataset', 'Select dataset definitions by file name.',
        metavar='DATASET', arity=arity
    ),
    .cdrgam_cli_option(
        'model', 'Select model definitions by file name.',
        metavar='MODEL', arity=arity
    ),
    .cdrgam_cli_option(
        'visualization', 'Select visualization definitions by file name.',
        metavar='VISUALIZATION', arity=arity
    ),
    .cdrgam_cli_option(
        'comparison', 'Select comparison definitions by file name.',
        metavar='COMPARISON', arity=arity
    )
)

.cdrgam_cli_command_spec <- function() {
    selectors <- .cdrgam_cli_selector_options()
    definition_target <- list(.cdrgam_cli_argument(
        'TARGET', 'A project name, or site for checkout configuration.'
    ))
    def <- .cdrgam_cli_command(
        'def', 'Create, edit, list, remove, or validate definitions.',
        description=paste(
            'Manage checkout and project definitions. Definition names are',
            'derived from their YAML file names.'
        ),
        commands=list(
            edit=.cdrgam_cli_command(
                'edit', 'Create or edit definitions.',
                description=paste(
                    'Create a missing definition or open an existing definition',
                    'in the configured editor. At most one definition type may',
                    'be selected in one invocation.'
                ),
                options=c(
                    .cdrgam_cli_definition_options('one-or-more'),
                    list(.cdrgam_cli_option(
                        'source', 'Initialize from an existing project or definition.',
                        metavar='SOURCE'
                    ))
                ),
                arguments=definition_target,
                examples=c(
                    'cdrgam def edit brown',
                    'cdrgam def edit brown --model main',
                    'cdrgam def edit brown --model alternative --source main'
                ),
                handler='def-edit'
            ),
            ls=.cdrgam_cli_command(
                'ls', 'List matching definitions.',
                description=paste(
                    'List projects or definitions. A type selector without values',
                    'matches every definition of that type.'
                ),
                options=.cdrgam_cli_definition_options('zero-or-more'),
                arguments=list(.cdrgam_cli_argument(
                    'PROJECT', 'Project whose definitions should be listed.',
                    required=FALSE
                )),
                examples=c(
                    'cdrgam def ls',
                    'cdrgam def ls brown --model',
                    "cdrgam def ls brown --dataset 'brown-*' --model 'main*'"
                ),
                handler='def-ls'
            ),
            rm=.cdrgam_cli_command(
                'rm', 'Remove unused definitions.',
                description=paste(
                    'Remove definitions matching exactly one definition type.',
                    'Referenced definitions and definitions with results cannot',
                    'be removed.'
                ),
                options=.cdrgam_cli_definition_options('one-or-more'),
                arguments=definition_target,
                examples='cdrgam def rm brown --model alternative',
                handler='def-rm'
            ),
            val=.cdrgam_cli_command(
                'val', 'Validate definitions.',
                description=paste(
                    'Validate a project or matching definitions without changing',
                    'them. At most one definition type may be selected.'
                ),
                options=c(
                    .cdrgam_cli_definition_options('one-or-more'),
                    list(.cdrgam_cli_option(
                        'deep', 'Read datasets and compile training model designs.',
                        arity='flag'
                    ))
                ),
                arguments=definition_target,
                examples=c(
                    'cdrgam def val brown',
                    'cdrgam def val brown --model main alternative --deep'
                ),
                handler='def-val'
            )
        )
    )
    list_command=.cdrgam_cli_command(
        'list', 'List configured projects.',
        options=selectors[1L],
        examples=c('cdrgam list', 'cdrgam list -P brown'),
        handler='list'
    )
    plan=.cdrgam_cli_command(
        'plan', 'Resolve and display a dependency-closed work plan.',
        options=selectors,
        examples='cdrgam plan -P brown -m main -p val',
        handler='plan'
    )
    run=.cdrgam_cli_command(
        'run', 'Request matching workloads and their dependencies.',
        description=paste(
            'Run every workload matching the conjunctive selectors. Downstream',
            'requests implicitly include their upstream dependencies.'
        ),
        options=c(selectors, list(
            .cdrgam_cli_option(
                'dry-run', 'Display the dependency-closed plan without executing it.',
                arity='flag'
            ),
            .cdrgam_cli_option(
                'cpus', 'Override the configured Slurm CPU allocation.',
                metavar='N', type='positive-integer'
            ),
            .cdrgam_cli_option(
                'memory', 'Override the configured Slurm memory allocation.',
                metavar='SIZE'
            ),
            .cdrgam_cli_option(
                'time', 'Override the configured Slurm time limit.', metavar='LIMIT'
            ),
            .cdrgam_cli_option(
                'qos', 'Override the configured Slurm quality of service.',
                metavar='NAME'
            )
        )),
        examples=c(
            'cdrgam run -P brown -m main',
            'cdrgam run -P brown -m main -p val test',
            'cdrgam run -P brown -v diagnostics --dry-run'
        ),
        handler='run'
    )
    status=.cdrgam_cli_command(
        'status', 'Display current work-item state.',
        options=list(
            selectors[[1L]],
            .cdrgam_cli_option(
                'no-pager', 'Write the report directly instead of opening a pager.',
                arity='flag'
            )
        ),
        examples=c('cdrgam status', 'cdrgam status -P brown --no-pager'),
        handler='status'
    )
    log=.cdrgam_cli_command(
        'log', 'View live or completed managed logs.',
        description=paste(
            'Show matching work-item logs by default. --worker instead shows',
            'generic worker lifecycle logs and cannot be combined with selectors.'
        ),
        options=c(selectors, list(
            .cdrgam_cli_option(
                'lines', 'Show at most the trailing N lines from each log.',
                metavar='N', type='positive-integer'
            ),
            .cdrgam_cli_option(
                'worker', 'Show worker lifecycle logs instead of work-item logs.',
                arity='flag'
            )
        )),
        examples=c(
            'cdrgam log -P brown -m main',
            'cdrgam log --worker --lines 50'
        ),
        handler='log'
    )
    purge=.cdrgam_cli_command(
        'purge', 'Preview or remove generated results.',
        description=paste(
            'Select generated artifacts and private work state. Definitions are',
            'never selected. Removal requires --yes.'
        ),
        options=c(selectors, list(
            .cdrgam_cli_option(
                'dataset', 'Select generated dataset artifacts.',
                metavar='DATASET', arity='one-or-more'
            ),
            .cdrgam_cli_option(
                'work', 'Include private work attempts.', arity='flag'
            ),
            .cdrgam_cli_option(
                'logs', 'Include project-private work-item logs.', arity='flag'
            ),
            .cdrgam_cli_option(
                'yes', 'Remove the selected paths after preview.', arity='flag'
            )
        )),
        examples=c(
            'cdrgam purge -P brown -m main',
            'cdrgam purge -P brown -m main --work --logs --yes'
        ),
        handler='purge'
    )
    scheduler=.cdrgam_cli_command(
        'scheduler', 'Run the internal checkout scheduler.',
        options=list(.cdrgam_cli_option(
            'checkout', 'Checkout directory.', metavar='PATH', required=TRUE
        )),
        handler='scheduler', hidden=TRUE
    )
    worker=.cdrgam_cli_command(
        'worker', 'Run an internal generic worker.',
        options=lapply(c(
            'worker-id', 'resource-key', 'controller-host', 'controller-port',
            'controller-token'
        ), function(name) .cdrgam_cli_option(
            name, paste(gsub('-', ' ', name), 'used by the scheduler.'),
            metavar=toupper(gsub('-', '_', name)), required=TRUE
        )),
        handler='worker', hidden=TRUE
    )
    .cdrgam_cli_command(
        'cdrgam', 'Manage CDR-GAM projects and workloads.',
        description=paste(
            'Create and validate definitions, resolve dependency graphs, run',
            'workloads locally or through Slurm, and inspect generated results.'
        ),
        commands=list(
            def=def, list=list_command, plan=plan, run=run, status=status,
            log=log, purge=purge, scheduler=scheduler, worker=worker
        )
    )
}

.cdrgam_cli_command_path <- function(path) {
    paste(c('cdrgam', path), collapse=' ')
}

.cdrgam_cli_option_usage <- function(option) {
    name <- .cdrgam_cli_null(option$short, option$long)
    value <- switch(
        option$arity,
        flag='',
        one=paste0(' ', option$metavar),
        `one-or-more`=paste0(' ', option$metavar, ' ...'),
        `zero-or-more`=paste0(' [', option$metavar, ' ...]')
    )
    token <- paste0(name, value)
    if (isTRUE(option$required)) token else paste0('[', token, ']')
}

.cdrgam_cli_usage_line <- function(command, path=character()) {
    tokens <- c(
        .cdrgam_cli_command_path(path),
        vapply(command$arguments, function(argument) {
            if (isTRUE(argument$required)) argument$name else {
                paste0('[', argument$name, ']')
            }
        }, character(1)),
        vapply(command$options, .cdrgam_cli_option_usage, character(1)),
        if (length(command$commands)) '<COMMAND>' else character()
    )
    paste('Usage:', paste(tokens, collapse=' '))
}

.cdrgam_cli_wrap <- function(text, initial='', subsequent=initial, width=NULL) {
    width <- .cdrgam_cli_null(width, max(60L, min(100L, getOption('width', 80L))))
    available <- width - max(nchar(initial), nchar(subsequent))
    wrapped <- strwrap(text, width=available)
    prefixes <- c(initial, rep(subsequent, max(0L, length(wrapped) - 1L)))
    paste(paste0(prefixes, wrapped), collapse='\n')
}

.cdrgam_cli_help <- function(command, path=character()) {
    title <- if (length(path)) {
        paste0(.cdrgam_cli_command_path(path), ' - ', command$summary)
    } else command$summary
    sections <- c(
        title, '', .cdrgam_cli_wrap(command$description), '',
        .cdrgam_cli_wrap(
            .cdrgam_cli_usage_line(command, path), subsequent='    '
        )
    )
    public_commands <- Filter(function(value) !isTRUE(value$hidden), command$commands)
    if (length(public_commands)) {
        rows <- unlist(lapply(public_commands, function(value) c(
            paste0('  ', value$name),
            .cdrgam_cli_wrap(value$summary, initial='      ', subsequent='      ')
        )), use.names=FALSE)
        sections <- c(sections, '', 'Commands:', rows)
    }
    if (length(command$arguments)) {
        rows <- unlist(lapply(command$arguments, function(argument) c(
            paste0('  ', argument$name),
            .cdrgam_cli_wrap(
                argument$help, initial='      ', subsequent='      '
            )
        )), use.names=FALSE)
        sections <- c(sections, '', 'Arguments:', rows)
    }
    public_options <- Filter(function(value) !isTRUE(value$hidden), command$options)
    if (length(public_options)) {
        rows <- unlist(lapply(public_options, function(option) {
            label <- paste(c(option$short, option$long), collapse=', ')
            if (!identical(option$arity, 'flag')) {
                label <- paste(label, switch(
                    option$arity,
                    one=option$metavar,
                    `one-or-more`=paste(option$metavar, '...'),
                    `zero-or-more`=paste0('[', option$metavar, ' ...]')
                ))
            }
            c(
                paste0('  ', label),
                .cdrgam_cli_wrap(
                    option$help, initial='      ', subsequent='      '
                )
            )
        }), use.names=FALSE)
        sections <- c(sections, '', 'Options:', rows)
    }
    sections <- c(
        sections, '', '  -h, --help',
        '      Show help for this command and exit.'
    )
    if (length(command$examples)) {
        sections <- c(
            sections, '', 'Examples:', paste0('  ', command$examples)
        )
    }
    if (length(public_commands)) {
        sections <- c(
            sections, '', paste0(
                "Run '", .cdrgam_cli_command_path(path),
                " COMMAND --help' for command-specific help."
            )
        )
    }
    paste0(paste(sections, collapse='\n'), '\n')
}

.cdrgam_cli_resolve_command <- function(arguments, public=FALSE) {
    command <- .cdrgam_cli_command_spec()
    path <- character()
    remaining <- arguments
    while (length(command$commands) && length(remaining) &&
            !startsWith(remaining[[1L]], '-')) {
        name <- remaining[[1L]]
        next_command <- command$commands[[name]]
        if (is.null(next_command) || (isTRUE(public) && isTRUE(next_command$hidden))) {
            .cdrgam_cli_abort(paste0(
                'Unknown command: ', .cdrgam_cli_command_path(c(path, name)),
                '\nRun ', sQuote(paste(
                    .cdrgam_cli_command_path(path), '--help'
                )), ' for available commands.'
            ))
        }
        command <- next_command
        path <- c(path, name)
        remaining <- remaining[-1L]
    }
    list(command=command, path=path, remaining=remaining)
}

.cdrgam_cli_parse_abort <- function(message, command, path) {
    .cdrgam_cli_abort(paste0(
        message, '\n\n', .cdrgam_cli_usage_line(command, path), '\nRun ',
        sQuote(paste(.cdrgam_cli_command_path(path), '--help')),
        ' for details.'
    ))
}

.cdrgam_cli_parse <- function(arguments, command, path) {
    options <- command$options
    lookup <- list()
    for (option in options) {
        lookup[[option$long]] <- option
        if (!is.null(option$short)) lookup[[option$short]] <- option
    }
    flags <- list()
    positional <- character()
    i <- 1L
    while (i <= length(arguments)) {
        token <- arguments[[i]]
        if (!startsWith(token, '-')) {
            positional <- c(positional, token)
            i <- i + 1L
            next
        }
        option <- lookup[[token]]
        if (is.null(option)) {
            .cdrgam_cli_parse_abort(
                paste0('Unknown option for ', .cdrgam_cli_command_path(path),
                       ': ', token),
                command, path
            )
        }
        if (identical(option$arity, 'flag')) {
            flags[[option$name]] <- TRUE
            i <- i + 1L
            next
        }
        if (identical(option$arity, 'one')) {
            if (i == length(arguments)) .cdrgam_cli_parse_abort(
                paste0(option$long, ' requires a value'), command, path
            )
            values <- arguments[[i + 1L]]
            i <- i + 2L
        } else {
            end <- i + 1L
            while (end <= length(arguments) &&
                    !startsWith(arguments[[end]], '-')) {
                end <- end + 1L
            }
            values <- if (end == i + 1L) character() else {
                arguments[seq.int(i + 1L, end - 1L)]
            }
            if (!length(values) && identical(option$arity, 'one-or-more')) {
                .cdrgam_cli_parse_abort(
                    paste0(option$long, ' requires a value'),
                    command, path
                )
            }
            if (!length(values)) values <- '*'
            i <- end
        }
        if (identical(option$type, 'positive-integer')) {
            numeric <- suppressWarnings(as.numeric(values))
            values <- vapply(numeric, function(value) {
                .cdrgam_cli_positive_integer(value, option$long)
            }, integer(1))
        }
        flags[[option$name]] <- c(flags[[option$name]], values)
    }
    required <- vapply(options, `[[`, logical(1), 'required')
    missing <- vapply(options[required], function(option) {
        is.null(flags[[option$name]])
    }, logical(1))
    if (any(missing)) .cdrgam_cli_parse_abort(paste0(
        'Missing required option', if (sum(missing) == 1L) '' else 's', ': ',
        paste(vapply(
            options[required][missing], `[[`, character(1), 'long'
        ), collapse=', ')
    ), command, path)
    minimum <- sum(vapply(command$arguments, `[[`, logical(1), 'required'))
    maximum <- length(command$arguments)
    if (length(positional) < minimum || length(positional) > maximum) {
        expected <- if (!maximum) {
            'accepts no positional arguments'
        } else if (minimum == maximum) {
            paste0('requires ', minimum, ' positional argument',
                   if (minimum == 1L) '' else 's')
        } else paste0('accepts between ', minimum, ' and ', maximum,
                      ' positional arguments')
        .cdrgam_cli_parse_abort(paste(
            .cdrgam_cli_command_path(path), expected
        ), command, path)
    }
    list(positional=positional, flags=flags)
}

.cdrgam_cli_one <- function(value, field, required=FALSE) {
    if (!length(value)) {
        if (required) .cdrgam_cli_abort(paste0(field, ' is required'))
        return(NULL)
    }
    if (length(value) != 1L) .cdrgam_cli_abort(paste0(field, ' accepts one value'))
    value[[1L]]
}

.cdrgam_cli_def_command <- function(parsed, operation) {
    positional <- parsed$positional
    flags <- parsed$flags
    project <- if (length(positional)) positional[[1L]] else NULL
    supported <- c('dataset', 'model', 'visualization', 'comparison')
    selected <- intersect(names(flags), supported)
    if (identical(operation, 'ls')) {
        if (is.null(project) && length(selected)) {
            .cdrgam_cli_abort(
                'def ls requires a project name when definition selectors are supplied'
            )
        }
        return(.cdrgam_cli_list_definitions(
            project, flags[selected]
        ))
    }
    expected <- if (identical(operation, 'rm')) 1L else c(0L, 1L)
    if (!(length(selected) %in% expected)) {
        .cdrgam_cli_abort(paste0(
            'def ', operation, ' accepts ',
            if (identical(operation, 'rm')) 'exactly' else 'at most',
            ' one definition selector'
        ))
    }
    type <- if (length(selected)) selected[[1L]] else NULL
    name <- if (is.null(type)) NULL else flags[[type]]
    source <- .cdrgam_cli_one(flags$source, '--source')
    cdrgam_cli_def(
        project, type=type, name=name, source=source, operation=operation,
        deep=isTRUE(flags$deep)
    )
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
    if (!length(args)) {
        cat(.cdrgam_cli_help(.cdrgam_cli_command_spec()))
        return(invisible(0L))
    }
    if (identical(args[[1L]], 'help')) {
        resolved <- .cdrgam_cli_resolve_command(args[-1L], public=TRUE)
        if (length(resolved$remaining)) .cdrgam_cli_abort(paste0(
            'help accepts command names only: ',
            paste(resolved$remaining, collapse=' ')
        ))
        cat(.cdrgam_cli_help(resolved$command, resolved$path))
        return(invisible(0L))
    }
    help_at <- which(args %in% c('-h', '--help'))
    if (length(help_at)) {
        prefix <- args[seq_len(help_at[[1L]] - 1L)]
        resolved <- .cdrgam_cli_resolve_command(prefix, public=TRUE)
        cat(.cdrgam_cli_help(resolved$command, resolved$path))
        return(invisible(0L))
    }
    resolved <- .cdrgam_cli_resolve_command(args)
    command <- resolved$command
    path <- resolved$path
    if (length(command$commands)) {
        .cdrgam_cli_parse_abort(
            paste(.cdrgam_cli_command_path(path), 'requires a command'),
            command, path
        )
    }
    parsed <- .cdrgam_cli_parse(resolved$remaining, command, path)
    flags <- parsed$flags
    handler <- command$handler
    if (identical(handler, 'scheduler')) {
        .cdrgam_cli_controller_main(.cdrgam_cli_one(
            flags$checkout, '--checkout', required=TRUE
        ))
        return(invisible(0L))
    }
    if (identical(handler, 'worker')) {
        required <- c(
            'worker-id', 'resource-key', 'controller-host', 'controller-port',
            'controller-token'
        )
        values <- lapply(required, function(name) {
            .cdrgam_cli_one(flags[[name]], paste0('--', name), required=TRUE)
        })
        do.call(.cdrgam_cli_worker, stats::setNames(values, gsub('-', '_', required)))
        return(invisible(0L))
    }
    if (startsWith(handler, 'def-')) {
        .cdrgam_cli_def_command(parsed, sub('^def-', '', handler))
        return(invisible(0L))
    }
    selectors <- .cdrgam_cli_selector_arguments(flags)
    if (identical(handler, 'list')) {
        cdrgam_cli_list(flags$project)
    } else if (identical(handler, 'plan')) {
        do.call(cdrgam_cli_plan, selectors)
    } else if (identical(handler, 'run')) {
        arguments <- c(selectors, list(
            dry_run=isTRUE(flags$`dry-run`),
            cpus=.cdrgam_cli_one(flags$cpus, '--cpus'),
            memory=.cdrgam_cli_one(flags$memory, '--memory'),
            time=.cdrgam_cli_one(flags$time, '--time'),
            qos=.cdrgam_cli_one(flags$qos, '--qos')
        ))
        do.call(cdrgam_cli_run, arguments)
    } else if (identical(handler, 'status')) {
        cdrgam_cli_status(
            flags$project, use_pager=!isTRUE(flags$`no-pager`)
        )
    } else if (identical(handler, 'log')) {
        arguments <- c(selectors, list(
            lines=.cdrgam_cli_one(flags$lines, '--lines'),
            worker=isTRUE(flags$worker)
        ))
        do.call(cdrgam_cli_log, arguments)
    } else if (identical(handler, 'purge')) {
        cdrgam_cli_purge(
            projects=flags$project, models=flags$model,
            predictions=flags$prediction,
            visualizations=flags$visualization,
            comparisons=flags$comparison, datasets=flags$dataset,
            work=isTRUE(flags$work), logs=isTRUE(flags$logs),
            yes=isTRUE(flags$yes)
        )
    } else {
        .cdrgam_cli_abort(paste0('No handler for ', .cdrgam_cli_command_path(path)))
    }
    invisible(0L)
}
