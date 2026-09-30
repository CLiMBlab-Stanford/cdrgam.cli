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
        arguments$family <- .cdrgam_cli_core_family(
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
    fit <- .cdrgam_cli_core_fit(arguments)
    saveRDS(fit, file.path(stage, 'fit.rds'), version=3)
    report <- .cdrgam_cli_core_fit_report(fit, design)
    writeLines(
        report$summary_text, file.path(stage, 'summary.txt'),
        useBytes=TRUE
    )
    utils::write.csv(
        report$coefficients,
        file.path(stage, 'coefficients.csv'),
        row.names=FALSE
    )
    utils::write.csv(
        report$smooths, file.path(stage, 'smooths.csv'), row.names=FALSE
    )
    .cdrgam_cli_write_yaml(
        report$diagnostics, file.path(stage, 'diagnostics.yml')
    )
    booklet <- if (isTRUE(render_booklet)) tryCatch(
        .cdrgam_cli_core_default_booklet(fit, file.path(stage, 'booklet.pdf')),
        error=function(error) {
            warning(
                'Could not generate the default plot booklet: ',
                conditionMessage(error),
                call.=FALSE
            )
            NULL
        }
    ) else NULL
    diagnostics <- report$diagnostics
    if (isTRUE(diagnostics$converged) && file.exists(checkpoint)) unlink(checkpoint)
    list(
        diagnostics=diagnostics,
        configuration=list(
            requested=model$fit,
            effective=report$effective_configuration
        ),
        response_name=design$response_name,
        formulas=report$formulas,
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
        report <- .cdrgam_cli_core_suggest_simplifications(
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
        .cdrgam_cli_core_default_booklet(
            final$fit, file.path(final$step, 'booklet.pdf')
        ),
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
    prediction <- .cdrgam_cli_core_prediction(
        fit, data$impulses, data$responses
    )
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
        prediction=as.numeric(prediction$prediction),
        residual=data$responses[[response_name]] -
            as.numeric(prediction$prediction),
        prediction_se=as.numeric(prediction$prediction_se),
        link_prediction=as.numeric(prediction$link_prediction),
        link_prediction_se=as.numeric(prediction$link_prediction_se),
        model_identity=item$fit$identity,
        dataset_identity=item$dataset_identity$identity,
        prediction_scale='response', stringsAsFactors=FALSE
    )
    if (isTRUE(prediction$distributional)) {
        for (parameter in prediction$parameter_names) {
            table[[paste0(parameter, '_prediction')]] <-
                prediction$response_parameters[, parameter]
            table[[paste0(parameter, '_prediction_se')]] <-
                prediction$response_parameter_se[, parameter]
            table[[paste0(parameter, '_link_prediction')]] <-
                prediction$link_parameters[, parameter]
            table[[paste0(parameter, '_link_prediction_se')]] <-
                prediction$link_parameter_se[, parameter]
        }
        table$standard_deviation_prediction <-
            1 / prediction$response_parameters[, 'scale']
        table$standard_deviation_prediction_se <-
            prediction$response_parameter_se[, 'scale'] /
            prediction$response_parameters[, 'scale']^2
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
    .cdrgam_cli_core_default_booklet(
        fit,
        file.path(stage, 'booklet.pdf'),
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
