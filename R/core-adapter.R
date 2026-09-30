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
        'converged', 'code', 'message', 'total_objective_evaluations',
        'gradient_norm', 'boundary', 'hessian_positive_definite',
        'hessian_min_eigenvalue', 'hessian_requested', 'hessian_method',
        'hessian_selection', 'hessian_evaluations', 'hessian_rhs',
        'restart_count', 'best_restart', 'resumed',
        'global_optimum_certified'
    )
    convergence[intersect(keep, names(convergence))]
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
