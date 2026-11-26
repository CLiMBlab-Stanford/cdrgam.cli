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
    if (!is.null(report$parametric_terms)) utils::write.csv(
        report$parametric_terms, file.path(stage, 'parametric-terms.csv'),
        row.names=FALSE
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

.cdrgam_cli_needs_simplification_report <- function(diagnostics) {
    .cdrgam_cli_fit_outcome(diagnostics) %in% c('warning', 'nonconverged')
}

.cdrgam_cli_write_simplification_report <- function(
        report, diagnostics, directory
) {
    candidates <- .cdrgam_cli_report_records(report)
    yaml_path <- file.path(directory, 'simplification-report.yml')
    text_path <- file.path(directory, 'simplification-report.txt')
    .cdrgam_cli_write_yaml(
        list(
            outcome=.cdrgam_cli_fit_outcome(diagnostics),
            converged=report$converged,
            gradient_tolerance=report$gradient_tolerance,
            conservatism=report$conservatism,
            candidates=candidates
        ),
        yaml_path
    )
    writeLines(utils::capture.output(print(report)), text_path, useBytes=TRUE)
    list(
        yaml=basename(yaml_path), text=basename(text_path),
        candidates=length(candidates)
    )
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

.cdrgam_cli_fit_history <- 'model_history'

.cdrgam_cli_history_label <- function(step_index) sprintf('%03d', as.integer(step_index))

.cdrgam_cli_read_manifest <- function(directory) {
    path <- .cdrgam_cli_manifest_path(directory)
    if (!file.exists(path)) return(NULL)
    tryCatch(.cdrgam_cli_read_yaml(path), error=function(error) NULL)
}

.cdrgam_cli_provisional_fit <- function(item, stage) {
    manifest <- .cdrgam_cli_read_manifest(item$output)
    if (is.null(manifest) || !identical(manifest$status, 'provisional') ||
            !identical(manifest$identity, item$identity) ||
            !identical(manifest$attempt, basename(stage))) return(NULL)
    manifest
}

.cdrgam_cli_move_entries <- function(source, target, exclude=character()) {
    entries <- list.files(source, all.files=TRUE, no..=TRUE)
    entries <- setdiff(entries, exclude)
    if (length(entries) && !dir.exists(target) &&
            !dir.create(target, recursive=TRUE)) {
        .cdrgam_cli_abort(paste0('Could not create ', sQuote(target)))
    }
    for (entry in entries) {
        .cdrgam_cli_replace_path(file.path(source, entry), file.path(target, entry))
    }
    invisible(entries)
}

.cdrgam_cli_publication_outputs <- function(directory) {
    paths <- setdiff(list.files(
        directory, recursive=TRUE, all.files=TRUE, include.dirs=FALSE, no..=TRUE
    ), 'manifest.yml')
    paths[!startsWith(paths, paste0(.cdrgam_cli_fit_history, '/'))]
}

# Publishes one finished autosimplification step as the provisional model.
# The previously published step of the same attempt moves to
# model_history/<step>; steps at or after this one are discarded because a
# resumed attempt refits them.
.cdrgam_cli_publish_fit_step <- function(
        item, stage, step, step_index, diagnostics, publish
) {
    build <- file.path(stage, '.cdrgam', 'publication')
    unlink(build, recursive=TRUE, force=TRUE)
    if (!dir.create(build, recursive=TRUE)) {
        .cdrgam_cli_abort('Could not stage the provisional fit publication')
    }
    destination <- item$output
    previous <- .cdrgam_cli_provisional_fit(item, stage)
    if (!is.null(previous)) {
        history <- file.path(destination, .cdrgam_cli_fit_history)
        for (entry in list.files(history)) {
            index <- suppressWarnings(as.integer(entry))
            if (!is.na(index) && index < step_index) {
                target <- file.path(build, .cdrgam_cli_fit_history)
                if (!dir.exists(target)) dir.create(target)
                .cdrgam_cli_replace_path(
                    file.path(history, entry), file.path(target, entry)
                )
            }
        }
        previous_step <- as.integer(previous$autosimplify_step)
        if (previous_step < step_index) {
            .cdrgam_cli_move_entries(
                destination,
                file.path(
                    build, .cdrgam_cli_fit_history,
                    .cdrgam_cli_history_label(previous_step)
                ),
                exclude=c('manifest.yml', .cdrgam_cli_fit_history)
            )
        }
    }
    .cdrgam_cli_move_entries(step, build)
    manifest <- list(
        schema=1L, status='provisional', project=item$project, kind=item$kind,
        definition=item$name, identity=item$identity, attempt=basename(stage),
        autosimplify_step=as.integer(step_index),
        published_at=.cdrgam_cli_timestamp(),
        result=list(diagnostics=diagnostics),
        outputs=.cdrgam_cli_output_manifest(
            build, .cdrgam_cli_publication_outputs(build)
        )
    )
    .cdrgam_cli_write_yaml(manifest, .cdrgam_cli_manifest_path(build))
    publish(build)
    message(
        'Published autosimplification step ', step_index,
        ' as the provisional model ', destination
    )
    invisible(destination)
}

# Moves the provisional publication of this attempt back into the stage so the
# final publication replaces it with its history intact.
.cdrgam_cli_collect_fit_publication <- function(item, stage) {
    if (is.null(.cdrgam_cli_provisional_fit(item, stage))) return(invisible(FALSE))
    .cdrgam_cli_move_entries(item$output, stage, exclude='manifest.yml')
    invisible(TRUE)
}

.cdrgam_cli_render_step_booklet <- function(fit, step) {
    tryCatch(
        .cdrgam_cli_core_default_booklet(fit, file.path(step, 'booklet.pdf')),
        error=function(error) {
            warning(
                'Could not generate the default plot booklet: ',
                conditionMessage(error), call.=FALSE
            )
            NULL
        }
    )
}

.cdrgam_cli_execute_fit <- function(item, stage, publish=NULL) {
    policy <- item$model$autosimplify
    if (is.null(policy)) policy <- list(enabled=FALSE)
    if (!isTRUE(policy$enabled)) {
        result <- .cdrgam_cli_execute_fit_once(item, stage)
        if (.cdrgam_cli_needs_simplification_report(result$diagnostics)) {
            report <- .cdrgam_cli_core_suggest_simplifications(result$.fit)
            result$simplification_report <-
                .cdrgam_cli_write_simplification_report(
                    report, result$diagnostics, stage
                )
        }
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
    stop_reason <- NULL
    for (step_index in seq_len(policy$max_steps)) {
        step <- file.path(steps, .cdrgam_cli_history_label(step_index))
        unlink(step, recursive=TRUE, force=TRUE)
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
        selected <- NULL
        if (.cdrgam_cli_needs_simplification_report(result$diagnostics)) {
            report <- .cdrgam_cli_core_suggest_simplifications(
                fit, max_candidates=Inf, conservatism=policy$conservatism
            )
            record$simplification_report <-
                .cdrgam_cli_write_simplification_report(
                    report, result$diagnostics, step
                )
            selected <- .cdrgam_cli_select_simplification(report, policy)
            if (is.null(selected)) {
                stop_reason <- paste(
                    'no allowed simplification addresses the remaining fit',
                    'diagnostics'
                )
            } else if (step_index >= policy$max_steps) {
                selected <- NULL
                stop_reason <- paste0(
                    'the step limit (max_steps: ', policy$max_steps,
                    ') was reached'
                )
            }
        }
        if (!is.null(selected)) {
            patch <- selected$patch
            patch$source <- 'fit diagnostics'
            model <- .cdrgam_cli_apply_formula_patch(model, patch)
            record$selection <- selected$candidate
            record$patch <- patch
            .cdrgam_cli_write_yaml(
                .cdrgam_cli_model_definition(model),
                file.path(step, 'derived-config.yml')
            )
        }
        record$result_contract <- .cdrgam_cli_model_contract(model)
        chain$steps[[length(chain$steps) + 1L]] <- record
        last <- is.null(selected)
        if (last) {
            final <- list(
                result=result,
                booklet=.cdrgam_cli_render_step_booklet(fit, step)
            )
        }
        if (is.function(publish)) {
            .cdrgam_cli_publish_fit_step(
                item, stage, step, step_index, result$diagnostics, publish
            )
        }
        if (last) break
        message(
            'Selected ', selected$candidate$term, ' (',
            selected$candidate$action, '); refitting from a fresh optimizer state'
        )
    }
    chain$final_contract <- .cdrgam_cli_model_contract(model)
    if (!is.null(stop_reason)) chain$stop_reason <- stop_reason
    .cdrgam_cli_write_yaml(chain, file.path(private, 'chain.yml'))
    if (is.function(publish)) {
        .cdrgam_cli_collect_fit_publication(item, stage)
    } else {
        for (index in seq_len(length(chain$steps) - 1L)) {
            label <- .cdrgam_cli_history_label(index)
            .cdrgam_cli_move_entries(
                file.path(steps, label),
                file.path(stage, .cdrgam_cli_fit_history, label)
            )
        }
        .cdrgam_cli_move_entries(step, stage)
    }
    unlink(steps, recursive=TRUE, force=TRUE)
    .cdrgam_cli_write_yaml(
        .cdrgam_cli_model_definition(model), file.path(stage, 'derived-config.yml')
    )
    result <- final$result
    result$booklet <- final$booklet
    result$derivation <- list(
        intent_contract=intent_contract,
        final_contract=chain$final_contract,
        steps=length(chain$steps),
        chain=file.path('.cdrgam', 'autosimplify', 'chain.yml'),
        derived_config='derived-config.yml',
        outcome=if (is.null(stop_reason)) 'resolved' else 'unresolved'
    )
    if (!is.null(stop_reason)) {
        result$derivation$message <- paste0(
            'Autosimplification stopped after ', length(chain$steps),
            if (length(chain$steps) == 1L) ' fit' else ' fits', ': ',
            stop_reason, '. The last fit is published with its warnings',
            if (length(chain$steps) > 1L) {
                paste0('; earlier fits are in ', .cdrgam_cli_fit_history, '/')
            } else '',
            '.'
        )
    }
    result
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
