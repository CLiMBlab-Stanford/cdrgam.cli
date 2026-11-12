library(cdrgam.cli)

validate <- getFromNamespace('.cdrgam_cli_validate_effect_query', 'cdrgam.cli')
series_axes <- getFromNamespace('.cdrgam_cli_series_axes', 'cdrgam.cli')
discrete <- getFromNamespace('.cdrgam_cli_discrete_mappings', 'cdrgam.cli')
plot_effect <- getFromNamespace('.cdrgam_cli_visualization_plot', 'cdrgam.cli')

expect_error <- function(expression, pattern) {
    message <- tryCatch({
        force(expression)
        NULL
    }, error=function(error) conditionMessage(error))
    stopifnot(!is.null(message), grepl(pattern, message))
}

base <- list(
    terms='wlen:repetition_index', composition='total',
    axes=list(
        lag=list(grid='fitted', n=5L),
        predictors=list(
            wlen=list(at=list(summary='mean', 'offset-sd'=1)),
            repetition_index=list(values=1:5)
        )
    )
)

# Queries without a reference keep their normalized form, and therefore their
# effect identity.
plain <- validate(base, 'query')
stopifnot(is.null(plain$reference), is.null(plain$test))

query <- base
query$reference <- list(wlen=list(at=list(summary='mean')))
query$test <- TRUE
query <- validate(query, 'query')
stopifnot(
    identical(query$reference$wlen$at$summary, 'mean'),
    isTRUE(query$test)
)
numeric <- base
numeric$reference <- list(repetition_index=1)
stopifnot(identical(validate(numeric, 'query')$reference$repetition_index, 1))

bad <- base
bad$reference <- list(wlen=list(values=c(1, 2)))
expect_error(validate(bad, 'query'), 'must resolve to one value')
bad$reference <- list(wlen=list(grid='fitted'))
expect_error(validate(bad, 'query'), 'must resolve to one value')
bad$reference <- list(1)
expect_error(validate(bad, 'query'), 'nonempty named mapping')
bad$reference <- list(lag='default')
expect_error(validate(bad, 'query'), 'no model default')
defaulted <- base
defaulted$reference <- list(wlen='default', repetition_index=2)
defaulted <- validate(defaulted, 'query')
stopifnot(identical(defaulted$reference$wlen, 'default'))
bad <- base
bad$test <- 'yes'
expect_error(validate(bad, 'query'), 'test must be true or false')

# Line series split on every varying evaluation axis except the x mapping.
grid <- expand.grid(lag=seq(0, 1, length.out=4), wlen=5, repetition_index=1:5)
grid <- transform(
    grid, term_id='irf-6', term='wlen:repetition_index', group=NA_character_,
    composition='total', grouping='population', estimate=lag * repetition_index,
    se=0.1, lower=lag * repetition_index - 0.2, upper=lag * repetition_index + 0.2,
    wlen_reference=4, layer='estimate'
)
mappings <- list(x='lag', y='estimate', color='predictor:repetition_index')
stopifnot(identical(series_axes(grid, mappings), 'repetition_index'))
shown <- discrete(grid, mappings)
stopifnot(
    is.factor(shown$repetition_index),
    identical(levels(shown$repetition_index), as.character(1:5))
)
definition <- list(
    query=validate(query, 'query'),
    render=list(geometry='line', mappings=mappings, interval='ribbon', theme='paper')
)
plot <- plot_effect(grid, definition)
built <- ggplot2::ggplot_build(plot)
stopifnot(length(unique(built$data[[length(built$data)]]$group)) == 5L)

cat('Effect query tests passed\n')
