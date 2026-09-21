#' Validate configured CDR-GAM projects
#'
#' @param projects Project selectors.
#' @param deep Read datasets and compile training model designs.
#' @param checkout Source checkout root.
#' @return Validation reports grouped by project, invisibly.
#' @export
cdrgam_cli_validate <- function(projects=NULL, deep=FALSE, checkout=NULL) {
    selected <- .cdrgam_cli_select_projects(projects, checkout)
    reports <- lapply(stats::setNames(selected, selected), function(project) {
        definitions <- .cdrgam_cli_read_definitions(
            project, check_sources=TRUE, checkout=checkout
        )
        external <- unlist(lapply(definitions$datasets, function(dataset) {
            names(dataset$sources)[vapply(
                dataset$sources, function(source) isTRUE(source$external), logical(1)
            )]
        }), use.names=FALSE)
        if (isTRUE(deep)) {
            for (name in names(definitions$datasets)) {
                dataset <- definitions$datasets[[name]]
                data <- .cdrgam_cli_load_dataset(dataset)
                .cdrgam_cli_check_dataset_columns(dataset, data)
                for (model in definitions$models) {
                    if (identical(model$datasets$train, name)) {
                        .cdrgam_cli_prepare_model(model, dataset, data)
                    }
                }
            }
        }
        list(
            valid=TRUE, root=definitions$root,
            datasets=length(definitions$datasets), models=length(definitions$models),
            visualizations=length(definitions$visualizations),
            comparisons=length(definitions$comparisons),
            external_sources=length(external), deep=isTRUE(deep)
        )
    })
    for (project in names(reports)) message(
        'Valid CDR-GAM project ', project, ': ', reports[[project]]$datasets,
        ' dataset(s), ', reports[[project]]$models, ' model(s), ',
        reports[[project]]$visualizations, ' visualization(s), ',
        reports[[project]]$comparisons, ' comparison(s)'
    )
    invisible(reports)
}
