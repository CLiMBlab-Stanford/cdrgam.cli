.cdrgam_cli_manifest_path <- function(directory) file.path(directory, 'manifest.yml')

.cdrgam_cli_complete_artifact <- function(directory, identity=NULL) {
    manifest_path <- .cdrgam_cli_manifest_path(directory)
    if (!file.exists(manifest_path)) return(FALSE)
    manifest <- tryCatch(.cdrgam_cli_read_yaml(manifest_path), error=function(error) NULL)
    if (is.null(manifest) || !identical(manifest$status, 'complete') ||
            is.null(manifest$outputs)) return(FALSE)
    if (!is.null(identity) && !identical(manifest$identity, identity)) return(FALSE)
    all(vapply(manifest$outputs, function(output) {
        path <- file.path(directory, output$path)
        file.exists(path) && identical(.cdrgam_cli_source_hash(path), output$md5)
    }, logical(1)))
}

.cdrgam_cli_output_manifest <- function(directory, relative_paths) {
    lapply(relative_paths, function(path) {
        full_path <- file.path(directory, path)
        list(
            path=path, size=unname(file.info(full_path)$size),
            md5=.cdrgam_cli_source_hash(full_path)
        )
    })
}

.cdrgam_cli_publish_directory <- function(stage, destination, definitions, item) {
    parent <- dirname(destination)
    if (!dir.exists(parent) && !dir.create(parent, recursive=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not create artifact parent ', sQuote(parent)))
    }
    archived <- NULL
    if (file.exists(destination)) {
        archived <- file.path(
            .cdrgam_cli_path(
                definitions, 'work', name='archive',
                identity=paste0(item$kind, '_', item$name, '_', item$identity)
            ),
            format(Sys.time(), '%Y%m%dT%H%M%S')
        )
        if (!dir.exists(dirname(archived))) dir.create(dirname(archived), recursive=TRUE)
        if (!.cdrgam_cli_try_move_path(destination, archived)) {
            .cdrgam_cli_abort(paste0('Could not archive previous artifact ', destination))
        }
    }
    published <- tryCatch({
        .cdrgam_cli_replace_path(stage, destination)
        TRUE
    }, error=function(error) FALSE)
    if (!published) {
        if (!is.null(archived) && !file.exists(destination)) {
            .cdrgam_cli_try_move_path(archived, destination)
        }
        .cdrgam_cli_abort(paste0('Could not atomically publish ', sQuote(destination)))
    }
    if (!is.null(archived)) unlink(archived, recursive=TRUE, force=TRUE)
    invisible(destination)
}

.cdrgam_cli_prune_attempt_directories <- function(paths, keep=character()) {
    keep <- .cdrgam_cli_normalize_path(keep, must_work=FALSE)
    for (path in setdiff(paths, keep)) {
        metadata_path <- file.path(path, 'attempt.yml')
        metadata <- if (file.exists(metadata_path)) tryCatch(
            .cdrgam_cli_read_yaml(metadata_path), error=function(error) NULL
        ) else NULL
        alive <- if (!is.null(metadata$pid)) {
            .cdrgam_cli_process_alive(metadata$pid)
        } else {
            FALSE
        }
        active <- !is.null(metadata) && identical(metadata$status, 'running') &&
            !identical(alive, FALSE)
        if (active) .cdrgam_cli_abort(paste0(
            'A superseded attempt still appears active: ', path
        ))
        unlink(path, recursive=TRUE, force=TRUE)
        if (file.exists(path)) .cdrgam_cli_abort(paste0(
            'Could not remove superseded attempt ', sQuote(path)
        ))
    }
    invisible(keep)
}

.cdrgam_cli_attempt_directory <- function(definitions, item, explicit=NULL) {
    parent <- .cdrgam_cli_path(
        definitions, 'work', name=paste(item$kind, item$name, sep='_'),
        identity=item$identity
    )
    if (!dir.exists(parent) && !dir.create(parent, recursive=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not create work directory ', parent))
    }
    existing <- sort(list.dirs(parent, recursive=FALSE, full.names=TRUE), decreasing=TRUE)
    if (!is.null(explicit)) {
        explicit <- .cdrgam_cli_normalize_path(explicit, must_work=TRUE)
        if (!.cdrgam_cli_within(explicit, parent)) {
            .cdrgam_cli_abort('Worker attempt path does not match its work item')
        }
        .cdrgam_cli_prune_attempt_directories(existing, keep=explicit)
        return(list(path=explicit, resumed=FALSE))
    }
    resume <- NULL
    for (path in existing) {
        metadata_path <- file.path(path, 'attempt.yml')
        if (!file.exists(metadata_path)) next
        metadata <- tryCatch(.cdrgam_cli_read_yaml(metadata_path), error=function(error) NULL)
        if (is.null(metadata) || !(metadata$status %in% c('running', 'failed'))) next
        alive <- if (!is.null(metadata$pid)) {
            .cdrgam_cli_process_alive(metadata$pid)
        } else {
            FALSE
        }
        active <- identical(metadata$status, 'running') &&
            !identical(alive, FALSE)
        if (active) .cdrgam_cli_abort(paste0('Work item appears active: ', item$key))
        if (is.null(resume)) resume <- path
    }
    if (!is.null(resume)) {
        .cdrgam_cli_prune_attempt_directories(existing, keep=resume)
        return(list(path=resume, resumed=TRUE))
    }
    .cdrgam_cli_prune_attempt_directories(existing)
    label <- paste0(format(Sys.time(), '%Y%m%dT%H%M%S'), '-', Sys.getpid())
    path <- file.path(parent, label)
    if (!dir.create(path)) .cdrgam_cli_abort(paste0('Could not create ', path))
    list(path=path, resumed=FALSE)
}

.cdrgam_cli_capture_conditions <- function(expression, path) {
    connection <- file(path, open='at', encoding='UTF-8')
    on.exit(close(connection), add=TRUE)
    withCallingHandlers(
        expression,
        message=function(condition) {
            writeLines(paste0(
                .cdrgam_cli_timestamp(), ' MESSAGE ', conditionMessage(condition)
            ), connection, useBytes=TRUE)
            cat(conditionMessage(condition), file=stderr())
            invokeRestart('muffleMessage')
        },
        warning=function(condition) {
            writeLines(paste0(
                .cdrgam_cli_timestamp(), ' WARNING ', conditionMessage(condition)
            ), connection, useBytes=TRUE)
            cat('Warning: ', conditionMessage(condition), '\n', sep='', file=stderr())
            invokeRestart('muffleWarning')
        }
    )
}

.cdrgam_cli_diagnostics <- function(fit) {
    convergence <- if (inherits(fit, 'cdrgam_sparse')) {
        fit$sparse$convergence
    } else if (inherits(fit, 'cdrgam_block')) {
        list(
            converged=identical(fit$optimizer$convergence, 0L),
            code=fit$optimizer$convergence,
            message=.cdrgam_cli_null(fit$optimizer$message, '')
        )
    } else {
        list(
            converged=isTRUE(fit$converged),
            code=if (isTRUE(fit$converged)) 0L else NA_integer_,
            message='native mgcv convergence state'
        )
    }
    keep <- c(
        'converged', 'code', 'message', 'total_objective_evaluations',
        'gradient_norm', 'boundary', 'hessian_positive_definite',
        'hessian_min_eigenvalue', 'hessian_requested', 'hessian_method',
        'hessian_selection', 'hessian_evaluations', 'hessian_rhs',
        'restart_count', 'best_restart', 'resumed',
        'global_optimum_certified'
    )
    convergence[intersect(keep, names(convergence))]
}

.cdrgam_cli_effective_model_configuration <- function(fit, design) {
    rank <- fit$cdrgam$rank
    fitting <- list(
        family=fit$family$family, link=fit$family$link, method=fit$method,
        engine=fit$cdrgam$engine, backend=fit$cdrgam$backend,
        rank_action=rank$action, rank_tolerance=rank$tolerance,
        rank_penalty=rank$regularization
    )
    if (inherits(fit, 'cdrgam_sparse')) fitting$sparse_control <- fit$sparse$control
    list(preparation=design$configuration, fitting=fitting)
}

.cdrgam_cli_fit_family <- function(name) {
    switch(
        .cdrgam_cli_null(name, 'gaussian'),
        gaussian=stats::gaussian(), binomial=stats::binomial(),
        poisson=stats::poisson(), Gamma=stats::Gamma(),
        .cdrgam_cli_abort(paste0('Unsupported family: ', name))
    )
}

.cdrgam_cli_default_booklet <- function(fit, path) {
    terms <- fit$cdrgam$terms
    if (!length(terms)) return(NULL)
    population <- which(vapply(terms, function(term) {
        !length(term$group_levels)
    }, logical(1)))
    select <- if (length(population)) population else NULL
    pages <- if (length(population)) length(population) else length(terms)
    cdrgam::save_cdrgam_plots(
        fit,
        path,
        select=select,
        pages=max(1L, pages)
    )
    list(
        path=basename(path),
        terms=if (is.null(select)) length(terms) else length(select),
        scope=if (length(population)) 'population' else 'all'
    )
}

.cdrgam_cli_execute_fit <- function(item, stage) {
    data <- .cdrgam_cli_load_dataset(item$dataset)
    .cdrgam_cli_check_dataset_columns(item$dataset, data)
    design <- .cdrgam_cli_prepare_model(item$model, item$dataset, data)
    fit_control <- item$model$fit
    checkpoint <- file.path(stage, 'optimizer-checkpoint.rds')
    backend <- .cdrgam_cli_null(fit_control[['backend', exact=TRUE]], 'mgcv')
    arguments <- list(design=design)
    family <- fit_control[['family', exact=TRUE]]
    if (!is.null(family)) arguments$family <- .cdrgam_cli_fit_family(family)
    for (field in c('method', 'engine', 'backend')) {
        value <- fit_control[[field, exact=TRUE]]
        if (!is.null(value)) arguments[[field]] <- value
    }
    if (backend %in% c('block', 'sparse')) {
        arguments$checkpoint <- checkpoint
        arguments$solver_trace <- TRUE
        for (field in c('rank_action', 'rank_tol', 'rank_penalty')) {
            value <- fit_control[[field, exact=TRUE]]
            if (!is.null(value)) arguments[[field]] <- value
        }
    }
    if (identical(backend, 'sparse')) {
        sparse_control <- fit_control[['sparse_control', exact=TRUE]]
        if (!is.null(sparse_control)) arguments$sparse_control <- sparse_control
    }
    fit <- do.call(
        'cdrgam.fit',
        arguments,
        envir=asNamespace('cdrgam')
    )
    saveRDS(fit, file.path(stage, 'fit.rds'), version=3)
    fit_summary <- summary(fit)
    writeLines(
        utils::capture.output(print(fit_summary)), file.path(stage, 'summary.txt'),
        useBytes=TRUE
    )
    coefficients <- if (is.null(fit_summary$p.table)) {
        data.frame()
    } else {
        data.frame(
            term=rownames(fit_summary$p.table),
            fit_summary$p.table,
            row.names=NULL,
            check.names=FALSE
        )
    }
    utils::write.csv(
        coefficients, file.path(stage, 'coefficients.csv'), row.names=FALSE
    )
    smooths <- if (is.null(fit_summary$s.table)) {
        data.frame()
    } else {
        data.frame(
            term=rownames(fit_summary$s.table),
            fit_summary$s.table,
            row.names=NULL,
            check.names=FALSE
        )
    }
    utils::write.csv(smooths, file.path(stage, 'smooths.csv'), row.names=FALSE)
    .cdrgam_cli_write_yaml(
        .cdrgam_cli_diagnostics(fit), file.path(stage, 'diagnostics.yml')
    )
    booklet <- tryCatch(
        .cdrgam_cli_default_booklet(fit, file.path(stage, 'booklet.pdf')),
        error=function(error) {
            warning(
                'Could not generate the default plot booklet: ',
                conditionMessage(error),
                call.=FALSE
            )
            NULL
        }
    )
    if (file.exists(checkpoint)) unlink(checkpoint)
    list(
        diagnostics=.cdrgam_cli_diagnostics(fit),
        configuration=list(
            requested=item$model$fit,
            effective=.cdrgam_cli_effective_model_configuration(fit, design)
        ),
        response_name=design$response_name,
        formulas=list(
            user=paste(deparse(fit$cdrgam$formula$user), collapse=' '),
            normalized=paste(deparse(fit$cdrgam$formula$normalized), collapse=' '),
            effective=paste(deparse(fit$cdrgam$formula$effective), collapse=' ')
        ),
        simplifications=design$simplifications,
        booklet=booklet
    )
}

.cdrgam_cli_execute_prediction <- function(item, stage) {
    fit <- readRDS(file.path(item$fit$output, 'fit.rds'))
    data <- .cdrgam_cli_load_dataset(item$dataset)
    .cdrgam_cli_check_dataset_columns(item$dataset, data)
    prediction <- stats::predict(
        fit,
        newdata=list(impulses=data$impulses, responses=data$responses),
        se.fit=TRUE
    )
    if (is.list(prediction) && all(c('fit', 'se.fit') %in% names(prediction))) {
        prediction_value <- prediction$fit
        prediction_se <- prediction$se.fit
    } else {
        prediction_value <- prediction
        prediction_se <- rep.int(NA_real_, length(prediction_value))
    }
    fit_manifest <- .cdrgam_cli_read_yaml(file.path(item$fit$output, 'manifest.yml'))
    response_name <- fit_manifest$response_name
    if (!(response_name %in% names(data$responses))) {
        .cdrgam_cli_abort(paste0(
            'Prediction dataset ', item$dataset_name,
            ' lacks model response column ', response_name
        ))
    }
    table <- data.frame(
        source_row=seq_len(nrow(data$responses)),
        observed=data$responses[[response_name]],
        prediction=as.numeric(prediction_value),
        residual=data$responses[[response_name]] - as.numeric(prediction_value),
        prediction_se=as.numeric(prediction_se),
        model_identity=item$fit$identity,
        dataset_identity=item$dataset_identity$identity,
        prediction_scale='response', stringsAsFactors=FALSE
    )
    row_id <- item$dataset$columns$row_id
    if (!is.null(row_id)) {
        table <- cbind(stats::setNames(data.frame(data$responses[[row_id]]), row_id), table)
    }
    utils::write.csv(table, file.path(stage, 'predictions.csv'), row.names=FALSE)
    list(rows=nrow(table), response_name=response_name)
}

.cdrgam_cli_execute_visualization <- function(item, stage) {
    if (is.null(item$definition$kind)) {
        return(.cdrgam_cli_render_effect(item, stage))
    }
    if (!identical(item$definition$kind, 'booklet')) {
        .cdrgam_cli_abort(paste0('Unsupported visualization kind: ', item$definition$kind))
    }
    fit <- readRDS(file.path(item$fit$output, 'fit.rds'))
    cdrgam::save_cdrgam_plots(
        fit, file.path(stage, 'booklet.pdf'),
        pages=.cdrgam_cli_null(item$definition$pages, 1L)
    )
    list(kind='booklet')
}

.cdrgam_cli_execute_comparison <- function(item, stage) {
    tables <- lapply(item$predictions, function(prediction) {
        utils::read.csv(file.path(prediction$output, 'predictions.csv'), check.names=FALSE)
    })
    metrics <- lapply(seq_along(tables), function(index) {
        table <- tables[[index]]
        values <- list(model=item$definition$models[[index]])
        if ('mse' %in% item$definition$methods) {
            values$mse <- mean(table$residual^2, na.rm=TRUE)
        }
        if ('mae' %in% item$definition$methods) {
            values$mae <- mean(abs(table$residual), na.rm=TRUE)
        }
        as.data.frame(values, stringsAsFactors=FALSE)
    })
    output <- do.call(rbind, metrics)
    utils::write.csv(output, file.path(stage, 'metrics.csv'), row.names=FALSE)
    list(rows=nrow(output), methods=item$definition$methods)
}

.cdrgam_cli_execution_envelope <- function(definitions) {
    list(
        checkout=definitions$checkout$checkout,
        R=R.home(), R_version=as.character(getRversion()),
        BLAS=unname(extSoftVersion()[['BLAS']]),
        libraries=.libPaths(),
        cdrgam=.cdrgam_cli_implementation('cdrgam'),
        cdrgam_cli=.cdrgam_cli_implementation('cdrgam.cli')
    )
}

.cdrgam_cli_run_item <- function(definitions, item, attempt_path=NULL) {
    destination <- item$output
    if (.cdrgam_cli_complete_artifact(destination, item$identity)) {
        message('Reusing complete ', item$kind, ' artifact ', item$name)
        return(invisible(list(status='reused', path=destination, item=item$key)))
    }
    for (dependency in item$dependencies) {
        dependency_item <- attr(item, 'graph')[[dependency]]
        if (!is.null(dependency_item) && !.cdrgam_cli_complete_artifact(
                dependency_item$output, dependency_item$identity)) {
            .cdrgam_cli_abort(paste0('Dependency is incomplete: ', dependency))
        }
    }
    attempt <- .cdrgam_cli_attempt_directory(definitions, item, attempt_path)
    stage <- attempt$path
    started <- .cdrgam_cli_timestamp()
    metadata <- list(
        schema=1L, status='running', project=item$project, kind=item$kind,
        name=item$name, identity=item$identity, key=item$key, pid=Sys.getpid(),
        started_at=started, resumed=isTRUE(attempt$resumed)
    )
    job_id <- Sys.getenv('SLURM_JOB_ID', '')
    if (nzchar(job_id)) metadata$job_id <- job_id
    .cdrgam_cli_write_yaml(metadata, file.path(stage, 'attempt.yml'))
    published <- FALSE
    on.exit({
        if (!published && dir.exists(stage)) {
            metadata$status <- 'failed'
            metadata$failed_at <- .cdrgam_cli_timestamp()
            .cdrgam_cli_write_yaml(metadata, file.path(stage, 'attempt.yml'))
        }
    }, add=TRUE)
    message('Running ', item$kind, ' ', item$name, ' -> ', item$identity)
    log_path <- file.path(stage, 'run.log')
    result <- tryCatch(
        .cdrgam_cli_capture_conditions(switch(
            item$kind,
            fit=.cdrgam_cli_execute_fit(item, stage),
            prediction=.cdrgam_cli_execute_prediction(item, stage),
            effect=.cdrgam_cli_execute_effect(item, stage),
            visualization=.cdrgam_cli_execute_visualization(item, stage),
            comparison=.cdrgam_cli_execute_comparison(item, stage),
            .cdrgam_cli_abort(paste0('No executor for work kind ', item$kind))
        ), log_path),
        error=function(error) {
            metadata$status <- 'failed'
            metadata$failed_at <- .cdrgam_cli_timestamp()
            metadata$error <- conditionMessage(error)
            .cdrgam_cli_write_yaml(metadata, file.path(stage, 'attempt.yml'))
            write(paste0(
                .cdrgam_cli_timestamp(), ' ERROR ', conditionMessage(error)
            ), file=log_path, append=TRUE)
            stop(error)
        }
    )
    metadata$status <- 'complete'
    metadata$completed_at <- .cdrgam_cli_timestamp()
    .cdrgam_cli_write_yaml(metadata, file.path(stage, 'attempt.yml'))
    relative_outputs <- setdiff(list.files(
        stage, recursive=TRUE, all.files=FALSE, include.dirs=FALSE
    ), 'manifest.yml')
    manifest <- list(
        schema=1L, status='complete', project=item$project, kind=item$kind,
        definition=item$name, identity=item$identity,
        contract=item$resolved_identity$resolved,
        inputs=item$dependencies,
        execution=.cdrgam_cli_execution_envelope(definitions),
        started_at=started, completed_at=.cdrgam_cli_timestamp(),
        result=result,
        outputs=.cdrgam_cli_output_manifest(stage, relative_outputs)
    )
    if (identical(item$kind, 'fit')) {
        manifest$response_name <- result$response_name
        manifest$diagnostics <- result$diagnostics
        manifest$configuration <- result$configuration
        manifest$formulas <- result$formulas
        manifest$simplifications <- result$simplifications
        manifest$inference <- paste(
            'Conditional on selected smoothing parameters and any recorded',
            'automatic simplifications.'
        )
    }
    .cdrgam_cli_write_yaml(manifest, .cdrgam_cli_manifest_path(stage))
    .cdrgam_cli_publish_directory(stage, destination, definitions, item)
    published <- TRUE
    message('Published ', item$kind, ' artifact ', destination)
    invisible(list(status='completed', path=destination, item=item$key))
}
