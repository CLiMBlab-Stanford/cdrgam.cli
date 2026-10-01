.cdrgam_cli_path <- function(
        definitions, kind, name=NULL, dataset=NULL, product=NULL, identity=NULL
) {
    root <- definitions$root
    configuration <- definitions$site
    results <- file.path(root, 'results')
    if (identical(kind, 'dataset')) {
        parts <- c(results, 'datasets', name)
    } else if (identical(kind, 'model')) {
        parts <- c(results, 'models', name)
    } else if (identical(kind, 'prediction')) {
        parts <- c(results, 'models', name, 'predictions', dataset)
    } else if (identical(kind, 'visualization')) {
        parts <- c(results, 'models', name, 'visualizations', dataset)
    } else if (identical(kind, 'effect')) {
        parts <- c(results, 'models', name, 'effects', identity)
    } else if (kind %in% c('comparison', 'analysis')) {
        parts <- c(results, paste0(kind, 's'), name)
    } else if (identical(kind, 'work')) {
        parts <- c(root, '.cdrgam', 'work')
        if (!is.null(name)) parts <- c(parts, name)
        if (!is.null(identity)) parts <- c(parts, identity)
    } else if (identical(kind, 'log')) {
        parts <- c(root, '.cdrgam', 'logs')
    } else if (identical(kind, 'registry')) {
        parts <- c(configuration$cdrgam_root, '.cdrgam', 'registry.sqlite3')
    } else if (identical(kind, 'controller')) {
        parts <- c(configuration$cdrgam_root, '.cdrgam', 'controller.yml')
    } else {
        .cdrgam_cli_abort(paste0('Unknown managed path kind: ', kind))
    }
    if (!is.null(product)) {
        product <- .cdrgam_cli_scalar_character(product, 'product')
        if (basename(product) != product || product %in% c('.', '..')) {
            .cdrgam_cli_abort('product must be one safe path component')
        }
        parts <- c(parts, product)
    }
    path <- do.call(file.path, as.list(parts))
    allowed <- .cdrgam_cli_normalize_path(c(
        root, file.path(configuration$cdrgam_root, '.cdrgam')
    ), must_work=FALSE)
    normalized <- .cdrgam_cli_normalize_path(path, must_work=FALSE)
    if (!any(vapply(allowed, function(parent) {
            isTRUE(fs::path_has_parent(normalized, parent))
        }, logical(1)))) {
        .cdrgam_cli_abort('Managed path escapes the configured roots')
    }
    as.character(normalized)
}

.cdrgam_cli_work_log_path <- function(definitions, kind, name) {
    kind <- .cdrgam_cli_scalar_character(kind, 'work log kind')
    name <- .cdrgam_cli_scalar_character(name, 'work log name')
    if (!(kind %in% c(
            'fit', 'prediction', 'effect', 'visualization',
            'comparison', 'analysis'
        )) || basename(name) != name || name %in% c('.', '..')) {
        .cdrgam_cli_abort('Work log kind and name must be safe path components')
    }
    path <- file.path(
        definitions$root, '.cdrgam', 'logs', kind, paste0(name, '.log')
    )
    if (!.cdrgam_cli_within(path, definitions$root)) {
        .cdrgam_cli_abort('Work log path escapes its project root')
    }
    path
}

.cdrgam_cli_worker_log_path <- function(configuration, worker_id) {
    worker_id <- .cdrgam_cli_scalar_character(worker_id, 'worker ID')
    if (basename(worker_id) != worker_id || worker_id %in% c('.', '..')) {
        .cdrgam_cli_abort('Worker ID must be one safe path component')
    }
    path <- file.path(
        configuration$cdrgam_root, '.cdrgam', 'logs', 'workers',
        paste0(worker_id, '.log')
    )
    if (!.cdrgam_cli_within(path, configuration$cdrgam_root)) {
        .cdrgam_cli_abort('Worker log path escapes cdrgam_root')
    }
    path
}

.cdrgam_cli_draft_path <- function(root, type, name) {
    type <- .cdrgam_cli_scalar_character(type, 'draft type')
    name <- .cdrgam_cli_scalar_character(name, 'draft name')
    if (basename(type) != type || type %in% c('.', '..') ||
            basename(name) != name || name %in% c('.', '..')) {
        .cdrgam_cli_abort('Draft type and name must be safe path components')
    }
    path <- file.path(root, '.cdrgam', 'drafts', type, paste0(name, '.yml'))
    if (!.cdrgam_cli_within(path, root)) {
        .cdrgam_cli_abort('Draft path escapes its managed root')
    }
    path
}

.cdrgam_cli_store_relative <- function(configuration, path) {
    root <- sub('/+$', '', .cdrgam_cli_normalize_path(
        configuration$cdrgam_root, must_work=FALSE
    ))
    path <- .cdrgam_cli_normalize_path(path, must_work=FALSE)
    if (!.cdrgam_cli_within(path, root)) {
        .cdrgam_cli_abort(paste0(
            'Managed path is outside cdrgam_root: ', sQuote(path)
        ))
    }
    if (identical(path, root)) '.' else substring(path, nchar(root) + 2L)
}

.cdrgam_cli_store_resolve <- function(
        configuration, path, field='managed path', must_work=FALSE
) {
    path <- .cdrgam_cli_scalar_character(path, field)
    if (.cdrgam_cli_absolute_path(path) ||
            any(strsplit(path, '[/\\\\]')[[1L]] == '..')) {
        .cdrgam_cli_abort(paste0(field, ' must be relative to cdrgam_root'))
    }
    resolved <- .cdrgam_cli_normalize_path(
        file.path(configuration$cdrgam_root, path), must_work=must_work
    )
    if (!.cdrgam_cli_within(resolved, configuration$cdrgam_root)) {
        .cdrgam_cli_abort(paste0(field, ' escapes cdrgam_root'))
    }
    resolved
}

.cdrgam_cli_project_relative <- function(definitions, path) {
    root <- sub('/+$', '', .cdrgam_cli_normalize_path(
        definitions$root, must_work=FALSE
    ))
    path <- .cdrgam_cli_normalize_path(path, must_work=FALSE)
    if (!.cdrgam_cli_within(path, root)) {
        .cdrgam_cli_abort(paste0(
            'Managed project path is outside the project root: ', sQuote(path)
        ))
    }
    relative <- if (identical(path, root)) '.' else {
        substring(path, nchar(root) + 2L)
    }
    paste0('cdrgam-project://', definitions$project$project$id, '/', relative)
}

.cdrgam_cli_project_roots <- function(configuration) {
    projects <- file.path(configuration$cdrgam_root, 'projects')
    roots <- if (dir.exists(projects)) {
        list.dirs(projects, recursive=FALSE, full.names=TRUE)
    } else character()
    output <- list()
    for (root in roots) {
        marker <- file.path(root, 'definitions', 'project.yml')
        if (!file.exists(marker)) next
        value <- tryCatch(.cdrgam_cli_read_yaml(marker), error=function(error) NULL)
        id <- value$project$id
        if (is.null(id) || !is.character(id) || length(id) != 1L || !nzchar(id)) next
        if (!is.null(output[[id]])) {
            .cdrgam_cli_abort(paste0(
                'Duplicate project.id ', sQuote(id), ' in the configured project store'
            ))
        }
        output[[id]] <- .cdrgam_cli_normalize_path(root, must_work=FALSE)
    }
    output
}

.cdrgam_cli_managed_resolve <- function(
        configuration, path, field='managed path', must_work=FALSE,
        project_roots=NULL
) {
    path <- .cdrgam_cli_scalar_character(path, field)
    prefix <- 'cdrgam-project://'
    if (!startsWith(path, prefix)) {
        return(.cdrgam_cli_store_resolve(configuration, path, field, must_work))
    }
    reference <- substring(path, nchar(prefix) + 1L)
    slash <- regexpr('/', reference, fixed=TRUE)
    if (slash < 2L) .cdrgam_cli_abort(paste0(field, ' is not a valid project path'))
    id <- substring(reference, 1L, slash - 1L)
    relative <- substring(reference, slash + 1L)
    if (!nzchar(relative) || grepl('^(/|[A-Za-z]:[/\\\\])', relative) ||
            any(strsplit(relative, '[/\\\\]')[[1L]] == '..')) {
        .cdrgam_cli_abort(paste0(field, ' is not a valid project-relative path'))
    }
    if (is.null(project_roots)) {
        project_roots <- .cdrgam_cli_project_roots(configuration)
    }
    root <- project_roots[[id]]
    if (is.null(root)) {
        .cdrgam_cli_abort(paste0(field, ' refers to unknown project.id ', sQuote(id)))
    }
    resolved <- .cdrgam_cli_normalize_path(
        file.path(root, relative), must_work=must_work
    )
    if (!.cdrgam_cli_within(resolved, root)) {
        .cdrgam_cli_abort(paste0(field, ' escapes its project root'))
    }
    resolved
}

.cdrgam_cli_registry_resolve <- function(
        configuration, paths, field='registry managed path',
        project_roots=NULL
) {
    if (!length(paths)) return(character())
    if (!is.character(paths) || anyNA(paths) || any(!nzchar(paths))) {
        .cdrgam_cli_abort(paste0(field, ' must contain nonempty paths'))
    }
    if (is.null(project_roots)) {
        project_roots <- .cdrgam_cli_project_roots(configuration)
    }
    store_root <- .cdrgam_cli_normalize_path(
        configuration$cdrgam_root, must_work=FALSE
    )
    prefix <- 'cdrgam-project://'
    output <- vapply(paths, function(path) {
        root <- store_root
        relative <- path
        if (startsWith(path, prefix)) {
            reference <- substring(path, nchar(prefix) + 1L)
            slash <- regexpr('/', reference, fixed=TRUE)
            if (slash < 2L) .cdrgam_cli_abort(paste0(
                field, ' is not a valid project path'
            ))
            id <- substring(reference, 1L, slash - 1L)
            relative <- substring(reference, slash + 1L)
            root <- project_roots[[id]]
            if (is.null(root)) .cdrgam_cli_abort(paste0(
                field, ' refers to unknown project.id ', sQuote(id)
            ))
        }
        if (!nzchar(relative) || .cdrgam_cli_absolute_path(relative) ||
                any(strsplit(relative, '[/\\\\]')[[1L]] == '..')) {
            .cdrgam_cli_abort(paste0(field, ' is not a valid relative path'))
        }
        resolved <- as.character(fs::path_norm(fs::path(root, relative)))
        if (!isTRUE(fs::path_has_parent(resolved, root))) {
            .cdrgam_cli_abort(paste0(field, ' escapes its managed root'))
        }
        resolved
    }, character(1), USE.NAMES=FALSE)
    names(output) <- names(paths)
    output
}

.cdrgam_cli_registry_work_log_paths <- function(
        configuration, project_ids, kinds, names, project_roots=NULL
) {
    count <- length(project_ids)
    if (length(kinds) != count || length(names) != count) {
        .cdrgam_cli_abort('Registry work-log fields have incompatible lengths')
    }
    allowed <- c(
        'fit', 'prediction', 'effect', 'visualization', 'comparison', 'analysis'
    )
    valid <- kinds %in% allowed & basename(names) == names &
        !(names %in% c('.', '..'))
    if (anyNA(valid) || !all(valid)) {
        .cdrgam_cli_abort('Registry work-log fields contain unsafe values')
    }
    if (is.null(project_roots)) {
        project_roots <- .cdrgam_cli_project_roots(configuration)
    }
    output <- rep.int(NA_character_, count)
    known <- project_ids %in% names(project_roots)
    if (!any(known)) return(output)
    references <- paste0(
        'cdrgam-project://', project_ids[known], '/.cdrgam/logs/', kinds[known],
        '/', names[known], '.log'
    )
    output[known] <- .cdrgam_cli_registry_resolve(
        configuration, references, field='registry work log path',
        project_roots=project_roots
    )
    output
}

.cdrgam_cli_pack_store_paths <- function(value, configuration) {
    prefix <- 'cdrgam-root://'
    store_root <- .cdrgam_cli_normalize_path(
        configuration$cdrgam_root, must_work=FALSE
    )
    project_roots <- .cdrgam_cli_project_roots(configuration)
    project_roots <- project_roots[order(
        nchar(unlist(project_roots, use.names=FALSE)), decreasing=TRUE
    )]
    converted <- new.env(hash=TRUE, parent=emptyenv())
    pack_path <- function(element) {
        if (is.na(element) || !.cdrgam_cli_absolute_path(element)) return(element)
        if (exists(element, envir=converted, inherits=FALSE)) {
            return(get(element, envir=converted, inherits=FALSE))
        }
        normalized <- .cdrgam_cli_normalize_path(element, must_work=FALSE)
        output <- element
        for (id in names(project_roots)) {
            root <- project_roots[[id]]
            if (isTRUE(fs::path_has_parent(normalized, root))) {
                relative <- if (identical(normalized, root)) '.' else {
                    substring(normalized, nchar(root) + 2L)
                }
                output <- paste0('cdrgam-project://', id, '/', relative)
                break
            }
        }
        if (identical(output, element) &&
                isTRUE(fs::path_has_parent(normalized, store_root))) {
            relative <- if (identical(normalized, store_root)) '.' else {
                substring(normalized, nchar(store_root) + 2L)
            }
            output <- paste0(prefix, relative)
        }
        assign(element, output, envir=converted)
        output
    }
    transform <- function(object) {
        original_attributes <- attributes(object)
        if (is.character(object)) {
            object[] <- vapply(
                object, pack_path, character(1), USE.NAMES=FALSE
            )
        } else if (is.list(object)) {
            object <- lapply(object, transform)
        }
        for (name in names(original_attributes)) {
            attr(object, name) <- if (name %in% c('names', 'class', 'row.names')) {
                original_attributes[[name]]
            } else transform(original_attributes[[name]])
        }
        object
    }
    transform(value)
}

.cdrgam_cli_unpack_store_paths <- function(value, configuration) {
    prefix <- 'cdrgam-root://'
    project_prefix <- 'cdrgam-project://'
    store_root <- .cdrgam_cli_normalize_path(
        configuration$cdrgam_root, must_work=FALSE
    )
    project_roots <- .cdrgam_cli_project_roots(configuration)
    converted <- new.env(hash=TRUE, parent=emptyenv())
    unpack_path <- function(element) {
        if (is.na(element) || (!startsWith(element, prefix) &&
                !startsWith(element, project_prefix))) {
            return(element)
        }
        if (exists(element, envir=converted, inherits=FALSE)) {
            return(get(element, envir=converted, inherits=FALSE))
        }
        if (startsWith(element, prefix)) {
            root <- store_root
            relative <- substring(element, nchar(prefix) + 1L)
        } else {
            reference <- substring(element, nchar(project_prefix) + 1L)
            slash <- regexpr('/', reference, fixed=TRUE)
            if (slash < 2L) {
                .cdrgam_cli_abort('stored request path is not a valid project path')
            }
            id <- substring(reference, 1L, slash - 1L)
            relative <- substring(reference, slash + 1L)
            root <- project_roots[[id]]
            if (is.null(root)) .cdrgam_cli_abort(paste0(
                'stored request path refers to unknown project.id ', sQuote(id)
            ))
        }
        if (!nzchar(relative) || .cdrgam_cli_absolute_path(relative) ||
                any(strsplit(relative, '[/\\]')[[1L]] == '..')) {
            .cdrgam_cli_abort('stored request path is not a valid relative path')
        }
        output <- .cdrgam_cli_normalize_path(
            fs::path(root, relative), must_work=FALSE
        )
        if (!isTRUE(fs::path_has_parent(output, root))) {
            .cdrgam_cli_abort('stored request path escapes its managed root')
        }
        assign(element, output, envir=converted)
        output
    }
    transform <- function(object) {
        original_attributes <- attributes(object)
        if (is.character(object)) {
            object[] <- vapply(
                object, unpack_path, character(1), USE.NAMES=FALSE
            )
        } else if (is.list(object)) {
            object <- lapply(object, transform)
        }
        for (name in names(original_attributes)) {
            attr(object, name) <- if (name %in% c('names', 'class', 'row.names')) {
                original_attributes[[name]]
            } else transform(original_attributes[[name]])
        }
        object
    }
    transform(value)
}
