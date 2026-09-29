.cdrgam_cli_manifest_path <- function(directory) file.path(directory, 'manifest.yml')

.cdrgam_cli_valid_derivation <- function(directory, derivation) {
    if (is.null(derivation)) return(TRUE)
    required <- c('intent_contract', 'final_contract', 'chain', 'derived_config')
    if (!all(required %in% names(derivation))) return(FALSE)
    chain_path <- file.path(directory, derivation$chain)
    derived_path <- file.path(directory, derivation$derived_config)
    private <- dirname(chain_path)
    intended_path <- file.path(private, 'intended-config.yml')
    if (!all(file.exists(c(chain_path, derived_path, intended_path)))) return(FALSE)
    value <- tryCatch({
        chain <- .cdrgam_cli_read_yaml(chain_path)
        model <- .cdrgam_cli_read_yaml(intended_path)
        if (!identical(chain$intent_contract, derivation$intent_contract) ||
                !identical(chain$final_contract, derivation$final_contract) ||
                length(chain$steps) != as.integer(derivation$steps)) return(FALSE)
        if (!identical(
                .cdrgam_cli_model_contract(model), derivation$intent_contract
        )) return(FALSE)
        for (step in chain$steps) {
            if (!identical(
                    .cdrgam_cli_model_contract(model), step$input_contract
            )) return(FALSE)
            model <- .cdrgam_cli_apply_formula_patches(
                model, .cdrgam_cli_null(step$preparation_patches, list())
            )
            if (!is.null(step$patch)) {
                model <- .cdrgam_cli_apply_formula_patch(model, step$patch)
            }
            if (!identical(
                    .cdrgam_cli_model_contract(model), step$result_contract
            )) return(FALSE)
        }
        derived <- .cdrgam_cli_read_yaml(derived_path)
        identical(.cdrgam_cli_model_contract(model), derivation$final_contract) &&
            identical(.cdrgam_cli_model_contract(derived), derivation$final_contract)
    }, error=function(error) FALSE)
    isTRUE(value)
}

.cdrgam_cli_complete_artifact <- function(directory, identity=NULL) {
    manifest_path <- .cdrgam_cli_manifest_path(directory)
    if (!file.exists(manifest_path)) return(FALSE)
    manifest <- tryCatch(.cdrgam_cli_read_yaml(manifest_path), error=function(error) NULL)
    if (is.null(manifest) || !identical(manifest$status, 'complete') ||
            is.null(manifest$outputs)) return(FALSE)
    if (!is.null(identity) && !identical(manifest$identity, identity)) return(FALSE)
    if (identical(manifest$kind, 'fit') &&
            identical(manifest$result$diagnostics$converged, FALSE)) return(FALSE)
    if (identical(manifest$kind, 'fit') && !.cdrgam_cli_valid_derivation(
            directory, manifest$result$derivation
    )) return(FALSE)
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
    if (identical(item$kind, 'fit')) {
        manifest_path <- .cdrgam_cli_manifest_path(item$output)
        manifest <- if (file.exists(manifest_path)) tryCatch(
            .cdrgam_cli_read_yaml(manifest_path), error=function(error) NULL
        ) else NULL
        source <- file.path(item$output, 'optimizer-checkpoint.rds')
        same_nonconverged_fit <- !is.null(manifest) &&
            identical(manifest$kind, 'fit') &&
            identical(manifest$identity, item$identity) &&
            identical(manifest$result$diagnostics$converged, FALSE)
        if (same_nonconverged_fit && file.exists(source) &&
                !file.copy(source, file.path(path, basename(source)))) {
            .cdrgam_cli_abort('Could not seed the fit attempt from its checkpoint')
        }
    }
    list(path=path, resumed=FALSE)
}

.cdrgam_cli_capture_conditions <- function(expression, path) {
    connection <- file(path, open='at', encoding='UTF-8')
    output_sinks <- sink.number(type='output')
    message_sinks <- sink.number(type='message')
    sink(connection, type='output')
    sink(connection, type='message')
    on.exit({
        while (sink.number(type='message') > message_sinks) sink(type='message')
        while (sink.number(type='output') > output_sinks) sink(type='output')
        close(connection)
    }, add=TRUE)
    withCallingHandlers(
        expression,
        message=function(condition) {
            writeLines(paste0(
                .cdrgam_cli_timestamp(), ' MESSAGE ', conditionMessage(condition)
            ), connection, useBytes=TRUE)
            invokeRestart('muffleMessage')
        },
        warning=function(condition) {
            writeLines(paste0(
                .cdrgam_cli_timestamp(), ' WARNING ', conditionMessage(condition)
            ), connection, useBytes=TRUE)
            invokeRestart('muffleWarning')
        }
    )
}

.cdrgam_cli_start_work_log <- function(definitions, item, started) {
    path <- .cdrgam_cli_work_log_path(definitions, item$kind, item$name)
    if (!dir.exists(dirname(path)) &&
            !dir.create(dirname(path), recursive=TRUE)) {
        .cdrgam_cli_abort('Could not create the work-item log directory')
    }
    writeLines(paste0(
        started, ' START ', item$kind, ' ', item$name,
        ' [', item$identity, ']'
    ), path, useBytes=TRUE)
    path
}

.cdrgam_cli_diagnostics <- function(fit) {
    convergence <- cdrgam::fit_diagnostics(fit)
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
        engine=fit$cdrgam$engine, backend=fit$cdrgam$backend
    )
    if (!is.null(rank)) fitting <- c(fitting, list(
        rank_action=rank$action,
        rank_tolerance=rank$tolerance,
        rank_penalty=rank$regularization
    ))
    if (inherits(fit, 'cdrgam_sparse')) fitting$sparse_control <- fit$sparse$control
    list(preparation=design$configuration, fitting=fitting)
}

.cdrgam_cli_fit_family <- function(name, link=NULL) {
    cdrgam::cdrgam_family(.cdrgam_cli_null(name, 'gaussian'), link=link)
}

.cdrgam_cli_formula_text <- function(value) {
    if (inherits(value, 'formula')) {
        return(paste(deparse(value), collapse=' '))
    }
    if (is.list(value)) return(lapply(value, .cdrgam_cli_formula_text))
    as.character(value)
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

.cdrgam_cli_execute_fit_once <- function(
        item, stage, model=item$model, data=NULL, render_booklet=TRUE
) {
    if (is.null(data)) data <- .cdrgam_cli_load_dataset(item$dataset)
    .cdrgam_cli_check_dataset_columns(item$dataset, data)
    design <- .cdrgam_cli_prepare_model(model, item$dataset, data)
    fit_control <- model$fit
    checkpoint <- file.path(stage, 'optimizer-checkpoint.rds')
    backend <- .cdrgam_cli_null(fit_control[['backend', exact=TRUE]], 'mgcv')
    distributional <- identical(
        .cdrgam_cli_null(fit_control[['family', exact=TRUE]], 'gaussian'),
        'gaulss'
    )
    arguments <- list(design=design)
    family <- fit_control[['family', exact=TRUE]]
    if (!is.null(family)) {
        arguments$family <- .cdrgam_cli_fit_family(
            family,
            fit_control[['link', exact=TRUE]]
        )
    }
    for (field in c('method', 'engine', 'backend')) {
        value <- fit_control[[field, exact=TRUE]]
        if (!is.null(value)) arguments[[field]] <- value
    }
    if (backend %in% c('block', 'sparse')) {
        if (!distributional || identical(backend, 'sparse')) {
            arguments$checkpoint <- checkpoint
        }
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
        test_status <- fit_summary$s.test
        if (is.null(test_status) ||
                length(test_status) != nrow(fit_summary$s.table)) {
            test_status <- ifelse(
                is.finite(fit_summary$s.table[, ncol(fit_summary$s.table)]),
                'approximate',
                'not computed'
            )
        }
        data.frame(
            term=rownames(fit_summary$s.table),
            fit_summary$s.table,
            test_status=unname(test_status),
            row.names=NULL,
            check.names=FALSE
        )
    }
    utils::write.csv(smooths, file.path(stage, 'smooths.csv'), row.names=FALSE)
    .cdrgam_cli_write_yaml(
        .cdrgam_cli_diagnostics(fit), file.path(stage, 'diagnostics.yml')
    )
    booklet <- if (isTRUE(render_booklet)) tryCatch(
        .cdrgam_cli_default_booklet(fit, file.path(stage, 'booklet.pdf')),
        error=function(error) {
            warning(
                'Could not generate the default plot booklet: ',
                conditionMessage(error),
                call.=FALSE
            )
            NULL
        }
    ) else NULL
    diagnostics <- .cdrgam_cli_diagnostics(fit)
    if (isTRUE(diagnostics$converged) && file.exists(checkpoint)) unlink(checkpoint)
    list(
        diagnostics=diagnostics,
        configuration=list(
            requested=model$fit,
            effective=.cdrgam_cli_effective_model_configuration(fit, design)
        ),
        response_name=design$response_name,
        formulas=lapply(
            fit$cdrgam$formula[c('user', 'normalized', 'effective')],
            .cdrgam_cli_formula_text
        ),
        simplifications=design$simplifications,
        booklet=booklet,
        .fit=fit
    )
}

.cdrgam_cli_model_definition <- function(model) {
    attr(model, 'path') <- NULL
    model$model <- NULL
    model
}

.cdrgam_cli_model_contract <- function(model) {
    value <- .cdrgam_cli_scientific_definition(
        .cdrgam_cli_model_definition(model)
    )
    canonical <- yaml::yaml.load(yaml::as.yaml(value), eval.expr=FALSE)
    .cdrgam_cli_short_hash(canonical)
}

.cdrgam_cli_formula_patch <- function(path, before, after, source) {
    list(
        operation='replace_formula', path=path,
        before=.cdrgam_cli_canonical_formula(before, paste0(source, ' before')),
        after=.cdrgam_cli_canonical_formula(after, paste0(source, ' after')),
        source=source
    )
}

.cdrgam_cli_apply_formula_patch <- function(model, patch) {
    if (!identical(patch$operation, 'replace_formula')) {
        .cdrgam_cli_abort(paste0(
            'Unsupported autosimplification operation: ', patch$operation
        ))
    }
    pieces <- strsplit(patch$path, '.', fixed=TRUE)[[1L]]
    if (!identical(pieces[[1L]], 'formula') || length(pieces) > 2L) {
        .cdrgam_cli_abort(paste0(
            'Invalid autosimplification formula path: ', patch$path
        ))
    }
    current <- if (length(pieces) == 1L) {
        model$formula
    } else model$formula[[pieces[[2L]], exact=TRUE]]
    current <- .cdrgam_cli_canonical_formula(current, patch$path)
    before <- .cdrgam_cli_canonical_formula(patch$before, patch$path)
    if (!identical(current, before)) {
        .cdrgam_cli_abort(paste0(
            'Autosimplification patch precondition failed at ', patch$path
        ))
    }
    after <- .cdrgam_cli_canonical_formula(patch$after, patch$path)
    if (length(pieces) == 1L) {
        model$formula <- after
    } else model$formula[[pieces[[2L]]]] <- after
    model
}

.cdrgam_cli_materialize_formula_patches <- function(model, effective) {
    distributional <- is.list(model$formula)
    parameters <- if (distributional) names(model$formula) else NULL
    paths <- if (distributional) paste0('formula.', parameters) else 'formula'
    current <- if (distributional) model$formula else list(model$formula)
    target <- if (distributional) effective else list(effective)
    patches <- list()
    for (i in seq_along(paths)) {
        before <- .cdrgam_cli_canonical_formula(current[[i]], paths[[i]])
        after <- .cdrgam_cli_canonical_formula(target[[i]], paths[[i]])
        if (!identical(before, after)) {
            patches[[length(patches) + 1L]] <- .cdrgam_cli_formula_patch(
                paths[[i]], before, after, 'model preparation'
            )
        }
    }
    patches
}

.cdrgam_cli_apply_formula_patches <- function(model, patches) {
    for (patch in patches) model <- .cdrgam_cli_apply_formula_patch(model, patch)
    model
}

.cdrgam_cli_report_records <- function(report) {
    candidates <- as.data.frame(report)
    if (!nrow(candidates)) return(list())
    lapply(seq_len(nrow(candidates)), function(index) {
        as.list(candidates[index, , drop=FALSE])
    })
}

.cdrgam_cli_select_simplification <- function(report, policy) {
    candidates <- as.data.frame(report)
    eligible <- which(
        candidates$automatable & candidates$action %in% policy$allow &
        !(candidates$term %in% policy$protect)
    )
    if (!length(eligible)) return(NULL)
    candidate <- candidates[eligible[[1L]], , drop=FALSE]
    list(candidate=as.list(candidate), patch=report$patches[[candidate$patch_id]])
}

.cdrgam_cli_move_step_outputs <- function(step, stage) {
    paths <- list.files(step, all.files=TRUE, no..=TRUE, full.names=TRUE)
    paths <- paths[!(basename(paths) %in% c(
        'input-config.yml', 'derived-config.yml', 'simplification-report.yml'
    ))]
    for (path in paths) {
        .cdrgam_cli_replace_path(path, file.path(stage, basename(path)))
    }
    unlink(step, recursive=TRUE, force=TRUE)
    invisible(stage)
}

.cdrgam_cli_execute_fit <- function(item, stage) {
    policy <- item$model$autosimplify
    if (is.null(policy)) policy <- list(enabled=FALSE)
    if (!isTRUE(policy$enabled)) {
        result <- .cdrgam_cli_execute_fit_once(item, stage)
        result$.fit <- NULL
        return(result)
    }
    data <- .cdrgam_cli_load_dataset(item$dataset)
    .cdrgam_cli_check_dataset_columns(item$dataset, data)
    model <- item$model
    intent <- .cdrgam_cli_model_definition(model)
    intent_contract <- .cdrgam_cli_model_contract(model)
    private <- file.path(stage, '.cdrgam', 'autosimplify')
    steps <- file.path(private, 'steps')
    if (!dir.create(steps, recursive=TRUE, showWarnings=FALSE) &&
            !dir.exists(steps)) {
        .cdrgam_cli_abort('Could not create the autosimplification archive')
    }
    .cdrgam_cli_write_yaml(intent, file.path(private, 'intended-config.yml'))
    chain <- list(
        schema=1L, intent_contract=intent_contract, policy=policy,
        steps=list()
    )
    final <- NULL
    for (step_index in seq_len(policy$max_steps)) {
        step <- file.path(steps, sprintf('%03d', step_index))
        if (!dir.create(step, recursive=TRUE, showWarnings=FALSE) &&
                !dir.exists(step)) {
            .cdrgam_cli_abort('Could not create an autosimplification step')
        }
        input_contract <- .cdrgam_cli_model_contract(model)
        .cdrgam_cli_write_yaml(
            .cdrgam_cli_model_definition(model), file.path(step, 'input-config.yml')
        )
        message(
            'Autosimplification fit ', step_index, ' of ', policy$max_steps,
            ' [', input_contract, ']'
        )
        result <- .cdrgam_cli_execute_fit_once(
            item, step, model=model, data=data, render_booklet=FALSE
        )
        fit <- result$.fit
        result$.fit <- NULL
        materialization <- .cdrgam_cli_materialize_formula_patches(
            model, result$formulas$effective
        )
        model <- .cdrgam_cli_apply_formula_patches(model, materialization)
        record <- list(
            step=step_index, input_contract=input_contract,
            diagnostics=result$diagnostics,
            preparation_patches=materialization
        )
        if (isTRUE(result$diagnostics$converged)) {
            record$result_contract <- .cdrgam_cli_model_contract(model)
            chain$steps[[length(chain$steps) + 1L]] <- record
            final <- list(result=result, fit=fit, step=step)
            break
        }
        report <- cdrgam::suggest_simplifications(
            fit, max_candidates=Inf, conservatism=policy$conservatism
        )
        .cdrgam_cli_write_yaml(
            list(
                converged=report$converged,
                gradient_tolerance=report$gradient_tolerance,
                conservatism=report$conservatism,
                candidates=.cdrgam_cli_report_records(report)
            ),
            file.path(step, 'simplification-report.yml')
        )
        selected <- if (step_index < policy$max_steps) {
            .cdrgam_cli_select_simplification(report, policy)
        } else NULL
        if (is.null(selected)) {
            record$result_contract <- .cdrgam_cli_model_contract(model)
            chain$steps[[length(chain$steps) + 1L]] <- record
            break
        }
        patch <- selected$patch
        patch$source <- 'fit diagnostics'
        model <- .cdrgam_cli_apply_formula_patch(model, patch)
        record$selection <- selected$candidate
        record$patch <- patch
        record$result_contract <- .cdrgam_cli_model_contract(model)
        chain$steps[[length(chain$steps) + 1L]] <- record
        .cdrgam_cli_write_yaml(
            .cdrgam_cli_model_definition(model), file.path(step, 'derived-config.yml')
        )
        message(
            'Selected ', selected$candidate$term, ' (',
            selected$candidate$action, '); refitting from a fresh optimizer state'
        )
    }
    chain$final_contract <- .cdrgam_cli_model_contract(model)
    .cdrgam_cli_write_yaml(chain, file.path(private, 'chain.yml'))
    .cdrgam_cli_write_yaml(
        .cdrgam_cli_model_definition(model), file.path(stage, 'derived-config.yml')
    )
    if (is.null(final)) {
        .cdrgam_cli_abort(paste0(
            'Autosimplification did not produce a converged model after ',
            length(chain$steps), ' fit attempts; archived attempts remain in ',
            private
        ))
    }
    final$result$booklet <- tryCatch(
        .cdrgam_cli_default_booklet(final$fit, file.path(final$step, 'booklet.pdf')),
        error=function(error) {
            warning(
                'Could not generate the default plot booklet: ',
                conditionMessage(error), call.=FALSE
            )
            NULL
        }
    )
    final$result$derivation <- list(
        intent_contract=intent_contract,
        final_contract=chain$final_contract,
        steps=length(chain$steps),
        chain=file.path('.cdrgam', 'autosimplify', 'chain.yml'),
        derived_config='derived-config.yml'
    )
    .cdrgam_cli_move_step_outputs(final$step, stage)
    final$result
}

.cdrgam_cli_execute_prediction <- function(item, stage) {
    fit <- readRDS(file.path(item$fit$output, 'fit.rds'))
    data <- .cdrgam_cli_load_dataset(item$dataset)
    .cdrgam_cli_check_dataset_columns(item$dataset, data)
    link_prediction <- stats::predict(
        fit,
        newdata=list(impulses=data$impulses, responses=data$responses),
        type='link',
        se.fit=TRUE
    )
    if (is.list(link_prediction) &&
            all(c('fit', 'se.fit') %in% names(link_prediction))) {
        link_value <- link_prediction$fit
        link_se <- link_prediction$se.fit
    } else {
        link_value <- link_prediction
        link_se <- rep.int(NA_real_, length(link_value))
    }
    if (isTRUE(fit$cdrgam$distributional)) {
        response_prediction <- stats::predict(
            fit,
            newdata=list(impulses=data$impulses, responses=data$responses),
            type='response',
            se.fit=TRUE
        )
        response_value <- response_prediction$fit
        response_se <- response_prediction$se.fit
        parameter_names <- fit$cdrgam$parameter_names
        colnames(link_value) <- parameter_names
        colnames(link_se) <- parameter_names
        colnames(response_value) <- parameter_names
        colnames(response_se) <- parameter_names
        prediction_value <- response_value[, 'location']
        prediction_se <- response_se[, 'location']
        primary_link_value <- link_value[, 'location']
        primary_link_se <- link_se[, 'location']
    } else {
        prediction_value <- fit$family$linkinv(link_value)
        prediction_se <- link_se * abs(fit$family$mu.eta(link_value))
        primary_link_value <- link_value
        primary_link_se <- link_se
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
        link_prediction=as.numeric(primary_link_value),
        link_prediction_se=as.numeric(primary_link_se),
        model_identity=item$fit$identity,
        dataset_identity=item$dataset_identity$identity,
        prediction_scale='response', stringsAsFactors=FALSE
    )
    if (isTRUE(fit$cdrgam$distributional)) {
        for (parameter in parameter_names) {
            table[[paste0(parameter, '_prediction')]] <-
                response_value[, parameter]
            table[[paste0(parameter, '_prediction_se')]] <-
                response_se[, parameter]
            table[[paste0(parameter, '_link_prediction')]] <-
                link_value[, parameter]
            table[[paste0(parameter, '_link_prediction_se')]] <-
                link_se[, parameter]
        }
        table$standard_deviation_prediction <-
            1 / response_value[, 'scale']
        table$standard_deviation_prediction_se <-
            response_se[, 'scale'] / response_value[, 'scale']^2
    }
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
    source <- if (dir.exists(file.path(definitions$root, '.git'))) {
        inventory <- .cdrgam_cli_source_inventory(definitions$root)
        list(
            revision=.cdrgam_cli_git_revision(definitions$root),
            digest=.cdrgam_cli_inventory_digest(inventory)
        )
    } else NULL
    list(
        checkout=definitions$checkout$checkout,
        project_source=source,
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
    log_path <- .cdrgam_cli_start_work_log(definitions, item, started)
    metadata$log <- .cdrgam_cli_project_relative(definitions, log_path)
    .cdrgam_cli_write_yaml(metadata, file.path(stage, 'attempt.yml'))
    published <- FALSE
    on.exit({
        if (!published && dir.exists(stage)) {
            metadata$status <- 'failed'
            metadata$failed_at <- .cdrgam_cli_timestamp()
            .cdrgam_cli_write_yaml(metadata, file.path(stage, 'attempt.yml'))
        }
    }, add=TRUE)
    message(
        .cdrgam_cli_timestamp(), ' Running ', item$kind, ' ', item$name,
        ' -> ', item$identity
    )
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
    nonconverged <- identical(item$kind, 'fit') &&
        identical(result$diagnostics$converged, FALSE)
    metadata$status <- if (nonconverged) 'nonconverged' else 'complete'
    metadata$completed_at <- .cdrgam_cli_timestamp()
    .cdrgam_cli_write_yaml(metadata, file.path(stage, 'attempt.yml'))
    write(paste0(
        metadata$completed_at, ' ',
        if (nonconverged) 'NONCONVERGED' else 'COMPLETE',
        ' ', item$kind, ' ', item$name
    ), file=log_path, append=TRUE)
    relative_outputs <- setdiff(list.files(
        stage, recursive=TRUE, all.files=TRUE, include.dirs=FALSE, no..=TRUE
    ), 'manifest.yml')
    manifest <- list(
        schema=1L, status='complete', project=item$project, kind=item$kind,
        definition=item$name, identity=item$identity,
        contract=item$resolved_identity$resolved,
        inputs=item$dependencies,
        execution=.cdrgam_cli_execution_envelope(definitions),
        started_at=started, completed_at=.cdrgam_cli_timestamp(),
        log=.cdrgam_cli_project_relative(definitions, log_path),
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
    invisible(list(
        status=if (nonconverged) 'nonconverged' else 'completed',
        path=destination, item=item$key,
        message=if (nonconverged) result$diagnostics$message else NULL
    ))
}
