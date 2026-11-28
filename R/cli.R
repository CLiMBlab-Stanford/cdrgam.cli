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
            'otherwise all projects. Accepts exact names, * globs, and re: regexes.'
        ),
        short='-P', metavar='PROJECT', arity=arity
    ),
    .cdrgam_cli_option(
        'model', paste(
            'Select model workloads by exact name, * glob, or re: regex.',
            'Multiple selector dimensions are conjunctive.'
        ),
        short='-m', metavar='MODEL', arity=arity
    ),
    .cdrgam_cli_option(
        'prediction', 'Select exact model partitions or prediction datasets.',
        short='-p', metavar='PARTITION', arity=arity
    ),
    .cdrgam_cli_option(
        'visualization', paste(
            'Select visualization workloads by exact name, * glob, or re: regex.'
        ),
        short='-v', metavar='VISUALIZATION', arity=arity
    ),
    .cdrgam_cli_option(
        'comparison', paste(
            'Select comparison workloads by exact name, * glob, or re: regex.'
        ),
        short='-c', metavar='COMPARISON', arity=arity
    ),
    .cdrgam_cli_option(
        'test', paste(
            'Select test workloads by exact name, * glob, or re: regex.'
        ),
        short='-t', metavar='TEST', arity=arity
    )
)

.cdrgam_cli_definition_options <- function(arity) list(
    .cdrgam_cli_option(
        'dataset', 'Select dataset definitions by file name.',
        short='-d', metavar='DATASET', arity=arity
    ),
    .cdrgam_cli_option(
        'model', 'Select model definitions by file name.',
        short='-m', metavar='MODEL', arity=arity
    ),
    .cdrgam_cli_option(
        'visualization', 'Select visualization definitions by file name.',
        short='-v', metavar='VISUALIZATION', arity=arity
    ),
    .cdrgam_cli_option(
        'comparison', 'Select comparison definitions by file name.',
        short='-c', metavar='COMPARISON', arity=arity
    ),
    .cdrgam_cli_option(
        'test', 'Select test definitions by file name.',
        short='-t', metavar='TEST', arity=arity
    )
)

.cdrgam_cli_command_spec <- function() {
    selectors <- .cdrgam_cli_selector_options()
    project_selector <- .cdrgam_cli_option(
        'project', paste(
            'Select one project. Omission infers the project from the current',
            'directory where possible.'
        ),
        short='-P', metavar='PROJECT'
    )
    definition_target <- list(.cdrgam_cli_argument(
        'TARGET', 'A project name, or site for root configuration.',
        required=FALSE
    ))
    def <- .cdrgam_cli_command(
        'def', 'Create, edit, list, rename, remove, or validate definitions.',
        description=paste(
            'Manage root and project definitions. Definition names are',
            'derived from their YAML file names.'
        ),
        commands=list(
            init=.cdrgam_cli_command(
                'init', 'Initialize a version-controlled project.',
                description=paste(
                    'Create a project, initialize its Git repository on main,',
                    'and stage its source files. Source copies definitions and code.'
                ),
                options=list(.cdrgam_cli_option(
                    'source', 'Initialize from an existing project.',
                    metavar='SOURCE'
                )),
                arguments=list(.cdrgam_cli_argument(
                    'PROJECT', 'New project name.'
                )),
                examples=c(
                    'cdrgam def init brown',
                    'cdrgam def init brown-replication --source brown'
                ),
                handler='def-init'
            ),
            edit=.cdrgam_cli_command(
                'edit', 'Create or edit definitions.',
                description=paste(
                    'Create a missing definition or open an existing definition',
                    'in the configured editor. At most one definition type may',
                    'be selected in one invocation. Names may be * globs or',
                    're:-prefixed regular expressions, which open every existing',
                    'definition they match in turn; quote them so the shell does',
                    'not expand them.'
                ),
                options=c(
                    list(project_selector),
                    .cdrgam_cli_definition_options('one-or-more'),
                    list(.cdrgam_cli_option(
                        'source', 'Initialize from an existing project or definition.',
                        metavar='SOURCE'
                    ))
                ),
                arguments=definition_target,
                examples=c(
                    'cdrgam def edit -P brown',
                    'cdrgam def edit -P brown --model main',
                    'cdrgam def edit -P brown --model alternative --source main',
                    "cdrgam def edit -P brown --visualization 'main-*'"
                ),
                handler='def-edit'
            ),
            ls=.cdrgam_cli_command(
                'ls', 'List matching definitions.',
                description=paste(
                    'List projects or definitions. A type selector without values',
                    'matches every definition of that type. Values accept exact',
                    'names, * globs, and re: regular expressions.'
                ),
                options=c(
                    list(project_selector),
                    .cdrgam_cli_definition_options('zero-or-more')
                ),
                arguments=list(.cdrgam_cli_argument(
                    'PROJECT', 'Project whose definitions should be listed.',
                    required=FALSE
                )),
                examples=c(
                    'cdrgam def ls',
                    'cdrgam def ls -P brown --model',
                    "cdrgam def ls -P brown --dataset 'brown-*' --model 'main*'"
                ),
                handler='def-ls'
            ),
            rm=.cdrgam_cli_command(
                'rm', 'Remove unused definitions.',
                description=paste(
                    'Remove definitions matching exactly one definition type.',
                    'Referenced definitions and definitions with results cannot',
                    'be removed. Values accept exact names, * globs, and re:',
                    'regular expressions.'
                ),
                options=c(
                    list(project_selector),
                    .cdrgam_cli_definition_options('one-or-more')
                ),
                arguments=list(.cdrgam_cli_argument(
                    'PROJECT', 'Project containing the definition to remove.',
                    required=FALSE
                )),
                examples='cdrgam def rm -P brown --model alternative',
                handler='def-rm'
            ),
            mv=.cdrgam_cli_command(
                'mv', 'Rename an unused definition.',
                description=paste(
                    'Rename exactly one subordinate definition without changing',
                    'its contents. The source must have no references, generated',
                    'results, registry workloads, or active work.'
                ),
                options=c(
                    list(project_selector),
                    .cdrgam_cli_definition_options('one-or-more')
                ),
                arguments=list(.cdrgam_cli_argument(
                    'PROJECT', 'Project containing the definition to rename.',
                    required=FALSE
                )),
                examples='cdrgam def mv -P brown --model old-name new-name',
                handler='def-mv'
            ),
            val=.cdrgam_cli_command(
                'val', 'Validate definitions.',
                description=paste(
                    'Validate a project or matching definitions without changing',
                    'them. At most one definition type may be selected. Values',
                    'accept exact names, * globs, and re: regular expressions.'
                ),
                options=c(
                    list(project_selector),
                    .cdrgam_cli_definition_options('one-or-more'),
                    list(.cdrgam_cli_option(
                        'deep', 'Read datasets and compile training model designs.',
                        arity='flag'
                    ))
                ),
                arguments=definition_target,
                examples=c(
                    'cdrgam def val -P brown',
                    'cdrgam def val -P brown --model main alternative --deep'
                ),
                handler='def-val'
            ),
            terms=.cdrgam_cli_command(
                'terms', 'List the term labels of fitted models.',
                description=paste(
                    'Print the smooth-term labels of each selected model, as',
                    'test definitions accept them under terms, from its latest',
                    'fit artifact. Without --model, every model is listed. Values',
                    'accept exact names, * globs, and re: regular expressions.'
                ),
                options=list(
                    project_selector,
                    .cdrgam_cli_option(
                        'model', 'Select model definitions by file name.',
                        short='-m', metavar='MODEL', arity='zero-or-more'
                    )
                ),
                arguments=list(.cdrgam_cli_argument(
                    'PROJECT', 'Project whose models should be listed.',
                    required=FALSE
                )),
                examples=c(
                    'cdrgam def terms -P rep',
                    "cdrgam def terms -P rep -m 'spr-*'"
                ),
                handler='def-terms'
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
                'partition', 'Override the Slurm partition for this request.',
                metavar='NAME'
            ),
            .cdrgam_cli_option(
                'memory', 'Override the configured Slurm memory allocation.',
                metavar='SIZE'
            ),
            .cdrgam_cli_option(
                'time', 'Override the configured Slurm time limit.', metavar='LIMIT'
            ),
            .cdrgam_cli_option(
                'scheduler-time', 'Override the root scheduler time limit.',
                metavar='LIMIT'
            ),
            .cdrgam_cli_option(
                'qos', 'Override the configured Slurm quality of service.',
                metavar='NAME'
            )
        )),
        examples=c(
            'cdrgam run -P brown -m main',
            'cdrgam run -P brown -m main -p val test',
            paste(
                'cdrgam run -P brown -m main --partition john',
                '--scheduler-time 24:00:00'
            ),
            'cdrgam run -P brown -v diagnostics --dry-run'
        ),
        handler='run'
    )
    stop=.cdrgam_cli_command(
        'stop', 'Stop selected Slurm-managed work.',
        description=paste(
            'Cancel selected active Slurm workers and prevent selected queued',
            'work from being claimed. Definitions, results, logs, and',
            'checkpoints are retained; run the selection again to resume it.',
            'Use --scheduler to stop an idle root scheduler.'
        ),
        options=c(selectors, list(.cdrgam_cli_option(
            'force', 'Record the stop after the root scheduler has become unreachable.',
            arity='flag'
        ), .cdrgam_cli_option(
            'scheduler', 'Stop the idle root scheduler instead of selected work.',
            arity='flag'
        ))),
        examples=c(
            'cdrgam stop -P brown -m main',
            'cdrgam stop --scheduler',
            'cdrgam stop -P brown -m main -p test'
        ),
        handler='stop'
    )
    methods=.cdrgam_cli_command(
        'methods', 'Write a methods section for selected work items.',
        description=paste(
            'Describe the selected work items and their upstream lineage as a',
            'methods section with citations, and write methods.json, a trace of',
            'the definitions, artifacts, and templates behind each section.',
            'Only work items with a complete artifact for their current identity',
            'contribute; stale and missing work items are omitted and counted.',
            'With --preview, resolved definitions are described without reading',
            'artifacts. Several projects are pooled into one report.'
        ),
        options=c(selectors, list(
            .cdrgam_cli_option(
                'preview', 'Describe resolved definitions without reading artifacts.',
                arity='flag'
            ),
            .cdrgam_cli_option(
                'format', paste('Output format: rtf (default), md, or tex. tex writes',
                    'natbib citations and methods.bib.'),
                metavar='FORMAT'
            ),
            .cdrgam_cli_option(
                'output', paste(
                    'Output directory. Defaults to results/methods in the selected',
                    'project; required when several projects are selected.'
                ),
                short='-o', metavar='DIR'
            )
        )),
        examples=c(
            'cdrgam methods -P brown',
            'cdrgam methods -P brown -m main -t main --preview',
            'cdrgam methods -P brown brown-replication -o methods'
        ),
        handler='methods'
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
            ),
            .cdrgam_cli_option(
                'running', 'Show only logs for work items currently running.',
                arity='flag'
            )
        )),
        examples=c(
            'cdrgam log -P brown -m main',
            'cdrgam log -P brown --running',
            'cdrgam log --worker --lines 50'
        ),
        handler='log'
    )
    purge=.cdrgam_cli_command(
        'purge', 'Review and remove generated results.',
        description=paste(
            'Select generated artifacts, private work state, and corresponding',
            'registry workloads. Definitions are never selected. In a terminal,',
            'the selection is shown in a pager and removal is confirmed after',
            'the pager closes.'
        ),
        options=c(selectors, list(
            .cdrgam_cli_option(
                'dataset', 'Select generated dataset artifacts.',
                short='-d', metavar='DATASET', arity='one-or-more'
            ),
            .cdrgam_cli_option(
                'work', 'Include private work attempts.', arity='flag'
            ),
            .cdrgam_cli_option(
                'logs', 'Include project-private work-item logs.', arity='flag'
            ),
            .cdrgam_cli_option(
                'force', 'Remove without showing the preview or prompting.',
                short='-f', arity='flag'
            )
        )),
        examples=c(
            'cdrgam purge -P brown -m main',
            'cdrgam purge -P brown -m main --work --logs',
            'cdrgam purge -P brown -m main --force'
        ),
        handler='purge'
    )
    rm=.cdrgam_cli_command(
        'rm', 'Remove an entire project.',
        description=paste(
            'Remove the project directory, including its definitions, code,',
            'results, private state, and registry workloads. Active work or',
            'cross-project dependencies prevent removal.'
        ),
        options=list(.cdrgam_cli_option(
            'force', 'Remove without showing the preview or prompting.',
            short='-f', arity='flag'
        )),
        arguments=list(.cdrgam_cli_argument(
            'PROJECT', 'Exact project name to remove.'
        )),
        examples=c('cdrgam rm brown', 'cdrgam rm brown --force'),
        handler='project-rm'
    )
    publish=.cdrgam_cli_command(
        'publish', 'Prepare and optionally push a project publication.',
        description=paste(
            'Validate project sources and selected complete results, write',
            'publication metadata, and stage the changes in the project Git',
            'repository. Uploading results is separate from archive creation.'
        ),
        options=c(list(project_selector), selectors[2:5], list(
            .cdrgam_cli_option(
                'results', 'Results mode: none, archive, url, or git.',
                metavar='MODE'
            ),
            .cdrgam_cli_option(
                'archive', 'Destination for a generated results tar archive.',
                metavar='PATH'
            ),
            .cdrgam_cli_option(
                'url', 'Direct download URL for separately hosted results.',
                metavar='URL'
            ),
            .cdrgam_cli_option(
                'sha256', 'Expected SHA-256 checksum for --results url.',
                metavar='DIGEST'
            ),
            .cdrgam_cli_option(
                'commit', 'Commit all staged project changes with this message.',
                metavar='MESSAGE'
            ),
            .cdrgam_cli_option(
                'push', 'Push the current project branch after publication.',
                arity='flag'
            )
        )),
        arguments=list(.cdrgam_cli_argument(
            'PROJECT', 'Project to publish. Omission infers the current project.',
            required=FALSE
        )),
        examples=c(
            'cdrgam publish -P brown --results archive',
            'cdrgam publish -P brown --results url --url URL --sha256 DIGEST',
            "cdrgam publish -P brown --results none --commit 'Publish sources' --push"
        ),
        handler='publish'
    )
    fetch=.cdrgam_cli_command(
        'fetch', 'Fetch a published project and optional results.',
        description=paste(
            'Clone a Git repository or unpack a direct source archive, verify',
            'its publication metadata, hydrate published results when available,',
            'and rebuild local registry state from artifact manifests.'
        ),
        options=list(
            .cdrgam_cli_option(
                'project', 'Override the local project directory name.',
                short='-P', metavar='PROJECT'
            ),
            .cdrgam_cli_option(
                'source-only', 'Do not download separately published results.',
                arity='flag'
            ),
            .cdrgam_cli_option(
                'results', 'Override the results archive path or direct URL.',
                metavar='LOCATION'
            )
        ),
        arguments=list(.cdrgam_cli_argument(
            'LOCATION', 'Git repository or direct source archive location.'
        )),
        examples=c(
            'cdrgam fetch https://github.com/example/brown-cdrgam',
            'cdrgam fetch ./brown.git --source-only',
            'cdrgam fetch SOURCE --results ./brown-results.tar.gz'
        ),
        handler='fetch'
    )
    viz=.cdrgam_cli_command(
        'viz', 'Serve the browser viewer for completed fits.',
        description=paste(
            'Start or reuse your viewer server. On a Slurm-configured root the',
            'server runs in a long-lived Slurm job, and the command prints the',
            'ssh command that forwards it to your workstation. Otherwise, or',
            'with --local, it serves from this process on this machine, opens',
            'your browser, and runs until interrupted. The viewer draws effect',
            'lines and surfaces for any completed fit under the root.'
        ),
        options=list(
            .cdrgam_cli_option('stop', 'Cancel your viewer job.', arity='flag'),
            .cdrgam_cli_option(
                'restart', 'Cancel your viewer job and start a new one.', arity='flag'
            ),
            .cdrgam_cli_option(
                'local', 'Serve from this process even on a Slurm-configured root.',
                arity='flag'
            ),
            .cdrgam_cli_option(
                'no-browser', 'Do not open a browser for a local viewer.',
                arity='flag'
            ),
            .cdrgam_cli_option(
                'port', 'Listen on PORT instead of your default port.',
                metavar='PORT', type='positive-integer'
            ),
            .cdrgam_cli_option(
                'cpus', 'Viewer job CPUs (default 1).', metavar='N',
                type='positive-integer'
            ),
            .cdrgam_cli_option('memory', 'Viewer job memory (default 16G).', metavar='MEM'),
            .cdrgam_cli_option(
                'time', 'Viewer job time limit (default 12:00:00).', metavar='TIME'
            ),
            .cdrgam_cli_option('partition', 'Viewer job partition.', metavar='NAME'),
            .cdrgam_cli_option('qos', 'Viewer job QOS.', metavar='NAME'),
            .cdrgam_cli_option(
                'wait', 'Seconds to wait for a new job to start (default 120).',
                metavar='SECONDS', type='positive-integer'
            )
        ),
        examples=c('cdrgam viz', 'cdrgam viz --time 2-00:00:00', 'cdrgam viz --stop'),
        handler='viz'
    )
    viz_server=.cdrgam_cli_command(
        'viz-server', 'Run the internal viewer server.',
        options=list(
            .cdrgam_cli_option('root', 'CDR-GAM root.', metavar='PATH', required=TRUE),
            .cdrgam_cli_option('state', 'Viewer state directory.', metavar='PATH', required=TRUE),
            .cdrgam_cli_option('port', 'Port.', metavar='PORT', type='positive-integer')
        ),
        handler='viz-server', hidden=TRUE
    )
    scheduler=.cdrgam_cli_command(
        'scheduler', 'Run the internal root scheduler.',
        options=list(.cdrgam_cli_option(
            'root', 'CDR-GAM root.', metavar='PATH', required=TRUE
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
            def=def, list=list_command, plan=plan, run=run, stop=stop, methods=methods,
            status=status,
            log=log, purge=purge, rm=rm, publish=publish, fetch=fetch, viz=viz,
            `viz-server`=viz_server, scheduler=scheduler, worker=worker
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
    if (!length(path)) sections <- c(
        sections, '  -V, --version',
        '      Show installed CLI and core package versions and exit.'
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
    positional_project <- if (length(positional)) positional[[1L]] else NULL
    selected_project <- .cdrgam_cli_one(flags$project, '--project')
    if (!is.null(positional_project) && !is.null(selected_project)) {
        .cdrgam_cli_abort(
            'Select the project either positionally or with -P/--project, not both'
        )
    }
    project <- if (!is.null(selected_project)) {
        selected_project
    } else positional_project
    supported <- c('dataset', 'model', 'visualization', 'comparison', 'test')
    selected <- intersect(names(flags), supported)
    if (identical(operation, 'ls')) {
        return(.cdrgam_cli_list_definitions(
            project, flags[selected]
        ))
    }
    if (identical(operation, 'terms')) {
        return(.cdrgam_cli_model_terms(project, flags$model))
    }
    if (identical(operation, 'init')) {
        if (length(selected)) {
            .cdrgam_cli_abort('def init does not accept definition selectors')
        }
        return(cdrgam_cli_def(
            project, source=.cdrgam_cli_one(flags$source, '--source'),
            operation='init'
        ))
    }
    exact_type <- operation %in% c('rm', 'mv')
    expected <- if (exact_type) 1L else c(0L, 1L)
    if (!(length(selected) %in% expected)) {
        .cdrgam_cli_abort(paste0(
            'def ', operation, ' accepts ',
            if (exact_type) 'exactly' else 'at most',
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
    visualizations=flags$visualization, comparisons=flags$comparison,
    tests=flags$test
)

#' Run the command-line dispatcher
#'
#' @param args Command arguments excluding the executable name.
#' @return An integer exit status, invisibly.
#' @export
cli_main <- function(args=commandArgs(trailingOnly=TRUE)) {
    if (length(args) && identical(args[[1L]], '--args')) args <- args[-1L]
    if (length(args) == 1L && args[[1L]] %in% c('-V', '--version')) {
        cat(
            'cdrgam.cli ', as.character(utils::packageVersion('cdrgam.cli')),
            ' (cdrgam ', as.character(utils::packageVersion('cdrgam')), ')\n',
            sep=''
        )
        return(invisible(0L))
    }
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
            flags$root, '--root', required=TRUE
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
    if (identical(handler, 'viz-server')) {
        root <- .cdrgam_cli_one(flags$root, '--root', required=TRUE)
        configuration <- .cdrgam_cli_site(root)
        .cdrgam_cli_viz_serve(
            configuration, .cdrgam_cli_one(flags$state, '--state', required=TRUE),
            port=.cdrgam_cli_one(flags$port, '--port'), host='0.0.0.0'
        )
        return(invisible(0L))
    }
    if (identical(handler, 'viz')) {
        arguments <- list(
            stop=isTRUE(flags$stop), restart=isTRUE(flags$restart),
            local=isTRUE(flags$local), port=.cdrgam_cli_one(flags$port, '--port'),
            browse=if (isTRUE(flags[['no-browser']])) FALSE,
            cpus=.cdrgam_cli_one(flags$cpus, '--cpus'),
            memory=.cdrgam_cli_one(flags$memory, '--memory'),
            time=.cdrgam_cli_one(flags$time, '--time'),
            partition=.cdrgam_cli_one(flags$partition, '--partition'),
            qos=.cdrgam_cli_one(flags$qos, '--qos')
        )
        wait <- .cdrgam_cli_one(flags$wait, '--wait')
        if (!is.null(wait)) arguments$wait <- wait
        do.call(cdrgam_cli_viz, arguments)
        return(invisible(0L))
    }
    if (startsWith(handler, 'def-')) {
        .cdrgam_cli_def_command(parsed, sub('^def-', '', handler))
        return(invisible(0L))
    }
    if (identical(handler, 'project-rm')) {
        .cdrgam_cli_remove_project(
            parsed$positional[[1L]], force=isTRUE(flags$force)
        )
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
            partition=.cdrgam_cli_one(flags$partition, '--partition'),
            memory=.cdrgam_cli_one(flags$memory, '--memory'),
            time=.cdrgam_cli_one(flags$time, '--time'),
            scheduler_time=.cdrgam_cli_one(
                flags$`scheduler-time`, '--scheduler-time'
            ),
            qos=.cdrgam_cli_one(flags$qos, '--qos')
        ))
        do.call(cdrgam_cli_run, arguments)
    } else if (identical(handler, 'stop')) {
        do.call(cdrgam_cli_stop, c(selectors, list(
            force=isTRUE(flags$force), scheduler=isTRUE(flags$scheduler)
        )))
    } else if (identical(handler, 'methods')) {
        arguments <- c(selectors, list(
            preview=isTRUE(flags$preview),
            output=.cdrgam_cli_one(flags$output, '--output')
        ))
        format <- .cdrgam_cli_one(flags$format, '--format')
        if (!is.null(format)) arguments$format <- format
        do.call(cdrgam_cli_methods, arguments)
    } else if (identical(handler, 'status')) {
        cdrgam_cli_status(
            flags$project, use_pager=!isTRUE(flags$`no-pager`)
        )
    } else if (identical(handler, 'log')) {
        arguments <- c(selectors, list(
            lines=.cdrgam_cli_one(flags$lines, '--lines'),
            worker=isTRUE(flags$worker), running=isTRUE(flags$running)
        ))
        do.call(cdrgam_cli_log, arguments)
    } else if (identical(handler, 'purge')) {
        cdrgam_cli_purge(
            projects=flags$project, models=flags$model,
            predictions=flags$prediction,
            visualizations=flags$visualization,
            comparisons=flags$comparison, tests=flags$test, datasets=flags$dataset,
            work=isTRUE(flags$work), logs=isTRUE(flags$logs),
            force=isTRUE(flags$force)
        )
    } else if (identical(handler, 'publish')) {
        positional_project <- if (length(parsed$positional)) {
            parsed$positional[[1L]]
        } else NULL
        selected_project <- .cdrgam_cli_one(flags$project, '--project')
        if (!is.null(positional_project) && !is.null(selected_project)) {
            .cdrgam_cli_abort(paste(
                'Select the project either positionally or with',
                '-P/--project, not both'
            ))
        }
        cdrgam_cli_publish(
            if (!is.null(selected_project)) selected_project else positional_project,
            results=.cdrgam_cli_null(
                .cdrgam_cli_one(flags$results, '--results'), 'none'
            ),
            archive=.cdrgam_cli_one(flags$archive, '--archive'),
            url=.cdrgam_cli_one(flags$url, '--url'),
            sha256=.cdrgam_cli_one(flags$sha256, '--sha256'),
            models=flags$model, predictions=flags$prediction,
            visualizations=flags$visualization,
            comparisons=flags$comparison,
            commit=.cdrgam_cli_one(flags$commit, '--commit'),
            push=isTRUE(flags$push)
        )
    } else if (identical(handler, 'fetch')) {
        cdrgam_cli_fetch(
            parsed$positional[[1L]],
            project=.cdrgam_cli_one(flags$project, '--project'),
            source_only=isTRUE(flags$`source-only`),
            results=.cdrgam_cli_one(flags$results, '--results')
        )
    } else {
        .cdrgam_cli_abort(paste0('No handler for ', .cdrgam_cli_command_path(path)))
    }
    invisible(0L)
}
