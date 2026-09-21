.cdrgam_cli_path <- function(
        definitions, kind, name=NULL, dataset=NULL, product=NULL, identity=NULL
) {
    root <- definitions$root
    project <- definitions$project$project$name
    configuration <- definitions$checkout
    if (identical(kind, 'dataset')) {
        parts <- c(root, 'datasets', name)
    } else if (identical(kind, 'model')) {
        parts <- c(root, 'models', name)
    } else if (identical(kind, 'prediction')) {
        parts <- c(root, 'models', name, 'predictions', dataset)
    } else if (identical(kind, 'visualization')) {
        parts <- c(root, 'models', name, 'visualizations', dataset)
    } else if (identical(kind, 'effect')) {
        parts <- c(root, 'models', name, 'effects', identity)
    } else if (kind %in% c('comparison', 'analysis')) {
        parts <- c(root, paste0(kind, 's'), name)
    } else if (identical(kind, 'work')) {
        parts <- c(root, '.cdrgam', 'work')
        if (!is.null(name)) parts <- c(parts, name)
        if (!is.null(identity)) parts <- c(parts, identity)
    } else if (identical(kind, 'log')) {
        parts <- c(configuration$cdrgam_root, '.cdrgam', 'logs', project)
        if (!is.null(name)) parts <- c(parts, name)
        if (!is.null(identity)) parts <- c(parts, identity)
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
    allowed <- c(root, file.path(configuration$cdrgam_root, '.cdrgam'))
    if (!any(vapply(allowed, function(parent) .cdrgam_cli_within(path, parent), logical(1)))) {
        .cdrgam_cli_abort('Managed path escapes the configured roots')
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
    if (grepl('^(/|[A-Za-z]:[/\\\\])', path) ||
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
        configuration, path, field='managed path', must_work=FALSE
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
    root <- .cdrgam_cli_project_roots(configuration)[[id]]
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

.cdrgam_cli_pack_store_paths <- function(value, configuration) {
    prefix <- 'cdrgam-root://'
    project_roots <- .cdrgam_cli_project_roots(configuration)
    project_roots <- project_roots[order(
        nchar(unlist(project_roots, use.names=FALSE)), decreasing=TRUE
    )]
    transform <- function(object) {
        original_attributes <- attributes(object)
        if (is.character(object)) {
            object[] <- vapply(object, function(element) {
                if (is.na(element) || !grepl('^(/|[A-Za-z]:[/\\\\])', element)) {
                    return(element)
                }
                normalized <- .cdrgam_cli_normalize_path(element, FALSE)
                for (id in names(project_roots)) {
                    root <- project_roots[[id]]
                    if (.cdrgam_cli_within(normalized, root)) {
                        relative <- if (identical(normalized, root)) '.' else {
                            substring(normalized, nchar(root) + 2L)
                        }
                        return(paste0('cdrgam-project://', id, '/', relative))
                    }
                }
                if (.cdrgam_cli_within(normalized, configuration$cdrgam_root)) {
                    paste0(prefix, .cdrgam_cli_store_relative(configuration, normalized))
                } else element
            }, character(1), USE.NAMES=FALSE)
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
    transform <- function(object) {
        original_attributes <- attributes(object)
        if (is.character(object)) {
            object[] <- vapply(object, function(element) {
                if (!is.na(element) && startsWith(element, prefix)) {
                    .cdrgam_cli_store_resolve(
                        configuration, substring(element, nchar(prefix) + 1L),
                        field='stored request path'
                    )
                } else if (!is.na(element) && startsWith(
                        element, 'cdrgam-project://'
                )) {
                    .cdrgam_cli_managed_resolve(
                        configuration, element, field='stored request path'
                    )
                } else element
            }, character(1), USE.NAMES=FALSE)
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
