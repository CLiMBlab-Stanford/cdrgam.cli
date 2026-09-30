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

.cdrgam_cli_complete_artifact <- function(
        directory, identity=NULL, verify_hashes=TRUE
) {
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
        file.exists(path) && (!isTRUE(verify_hashes) ||
            identical(.cdrgam_cli_source_hash(path), output$md5))
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
