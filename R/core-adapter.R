.cdrgam_cli_core_family <- function(name, link=NULL) {
    cdrgam::cdrgam_family(.cdrgam_cli_null(name, 'gaussian'), link=link)
}

.cdrgam_cli_core_irf <- function(...) cdrgam::irf(...)

.cdrgam_cli_core_prepare <- function(arguments) {
    do.call(cdrgam::prepare_cdrgam, arguments)
}

.cdrgam_cli_core_fit <- function(arguments) {
    do.call(cdrgam::cdrgam.fit, arguments)
}

.cdrgam_cli_core_diagnostics <- function(fit) {
    convergence <- cdrgam::fit_diagnostics(fit)
    keep <- c(
        'outcome', 'reasons', 'notes', 'converged', 'code', 'message',
        'total_objective_evaluations',
        'gradient_norm', 'boundary', 'hessian_positive_definite',
        'hessian_min_eigenvalue', 'hessian_full_min_eigenvalue',
        'hessian_boundary', 'hessian_negative_directions',
        'newton_refinement', 'predicted_decrease', 'exact_gradient_maximum',
        'boundary_profile_level',
        'hessian_requested', 'hessian_method',
        'hessian_selection', 'hessian_evaluations', 'hessian_rhs',
        'condition_indicator', 'condition_indicator_full',
        'condition_tolerance',
        'restart_count', 'best_restart', 'resumed',
        'global_optimum_certified'
    )
    output <- convergence[intersect(keep, names(convergence))]
    # Publish tables as one record per row.
    records <- function(table) lapply(seq_len(nrow(table)), function(row) {
        as.list(table[row, , drop=FALSE])
    })
    for (field in c('hessian_boundary', 'hessian_negative_directions')) {
        table <- output[[field, exact=TRUE]]
        if (is.data.frame(table)) output[[field]] <- records(table)
    }
    newton <- output[['newton_refinement', exact=TRUE]]
    if (is.list(newton) && is.data.frame(newton$steps)) {
        output$newton_refinement$steps <- records(newton$steps)
    }
    output
}

.cdrgam_cli_fit_outcome <- function(diagnostics) {
    outcome <- diagnostics[['outcome', exact=TRUE]]
    if (is.character(outcome) && length(outcome) == 1L &&
            outcome %in% c('success', 'warning', 'nonconverged')) {
        return(outcome)
    }
    if (identical(diagnostics$converged, FALSE)) {
        'nonconverged'
    } else {
        'success'
    }
}

.cdrgam_cli_diagnostic_messages <- function(diagnostics) {
    reasons <- diagnostics[['reasons', exact=TRUE]]
    if (!is.list(reasons) || !length(reasons)) {
        message <- diagnostics[['message', exact=TRUE]]
        if (is.character(message) && length(message) == 1L) {
            message
        } else {
            character()
        }
    } else {
        unique(vapply(reasons, function(reason) {
            message <- reason[['message', exact=TRUE]]
            if (is.character(message) && length(message) == 1L) {
                message
            } else {
                reason[['code', exact=TRUE]]
            }
        }, character(1)))
    }
}

.cdrgam_cli_core_fit_report <- function(fit, design) {
    report <- cdrgam::fit_report(fit)
    report$diagnostics <- .cdrgam_cli_core_diagnostics(fit)
    report$effective_configuration <- list(
        preparation=design$configuration,
        fitting=report$fitting
    )
    report
}

.cdrgam_cli_core_default_booklet <- function(fit, path, pages=NULL) {
    if (!is.null(pages)) {
        cdrgam::save_cdrgam_plots(fit, path, pages=pages)
        return(list(path=basename(path), scope='all'))
    }
    plotting <- cdrgam::fit_metadata(fit)$plotting
    if (!plotting$term_count) return(NULL)
    population <- plotting$population_terms
    select <- if (length(population)) population else NULL
    page_count <- if (length(population)) {
        length(population)
    } else plotting$term_count
    cdrgam::save_cdrgam_plots(
        fit,
        path,
        select=select,
        pages=max(1L, page_count)
    )
    list(
        path=basename(path),
        terms=if (is.null(select)) plotting$term_count else length(select),
        scope=if (length(population)) 'population' else 'all'
    )
}

.cdrgam_cli_core_suggest_simplifications <- function(
        fit, max_candidates=Inf, conservatism=1
) {
    cdrgam::suggest_simplifications(
        fit,
        max_candidates=max_candidates,
        conservatism=conservatism
    )
}

.cdrgam_cli_core_prediction <- function(fit, impulses, responses) {
    cdrgam::predict_components(
        fit,
        newdata=list(impulses=impulses, responses=responses)
    )
}

.cdrgam_cli_core_effect_catalog <- function(fit) {
    cdrgam::effect_catalog(fit)
}

.cdrgam_cli_core_estimate_effect <- function(fit, ...) {
    cdrgam::estimate_effect(fit, ...)
}

.cdrgam_cli_core_test_effect <- function(fit, ...) {
    cdrgam::test_effect(fit, ...)
}
