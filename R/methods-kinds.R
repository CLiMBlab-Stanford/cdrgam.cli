# Methods contexts for datasets, predictions, comparisons, tests, and
# visualizations. Each builder pools per-record facts as the fit builder does
# and declares which definition fields it reports and which it omits.

.cdrgam_cli_methods_format_value <- function(value) {
    if (is.numeric(value)) return(vapply(value, .cdrgam_cli_methods_number, character(1)))
    paste0('"', as.character(value), '"')
}

.cdrgam_cli_methods_flatten <- function(value) {
    if (is.null(value)) return('')
    text <- if (is.list(value) || length(value) > 1L) {
        as.character(jsonlite::toJSON(value, auto_unbox=TRUE, digits=NA, null='null'))
    } else as.character(value)
    gsub('[\t\r\n]+', ' ', text)
}

# --- Datasets ----------------------------------------------------------------

.cdrgam_cli_methods_dataset_fields <- c(
    columns.series='series', columns.factors='factors',
    columns.factor_interactions='interactions', sources.types='factors',
    filters='filters', preprocess.script='preprocess', preprocess.function='preprocess'
)

.cdrgam_cli_methods_dataset_silent <- c(
    schema='Definition format version, not a scientific setting.',
    sources.path='File locations; source content enters the dataset identity.',
    sources.format='File format of the sources.',
    sources.separator='Delimiter of delimited sources.',
    columns.impulse_time='Name of the impulse time column.',
    columns.response_time='Name of the response time column.',
    columns.row_id='Name of the response row key used to align output rows.'
)

.cdrgam_cli_methods_dataset_facts <- c(
    'series', 'factors', 'interactions', 'filters', 'preprocess'
)

.cdrgam_cli_methods_dataset_labels <- c(
    series='series', factors='factor columns', interactions='constructed factors',
    filters='response filters', preprocess='preprocessing'
)

.cdrgam_cli_methods_comparison_operators <- c('>', '<', '>=', '<=', '==', '!=')

.cdrgam_cli_methods_filter_phrase <- function(filter) {
    if (!is.null(filter$factor)) {
        return(paste0('the ', filter$factor, ' level had at least ', filter$min,
            ' responses among those retained so far'))
    }
    arguments <- .cdrgam_cli_methods_format_value(unlist(filter$args))
    if (filter$fun %in% .cdrgam_cli_methods_comparison_operators) {
        return(paste(filter$column, filter$fun, paste(arguments, collapse=', ')))
    }
    if (identical(filter$fun, '%in%')) {
        return(paste(filter$column, 'was', .cdrgam_cli_methods_join(arguments, 'or')))
    }
    paste0(filter$fun, '(', paste(c(filter$column, arguments), collapse=', '),
        ') was true')
}

.cdrgam_cli_methods_dataset_record_facts <- function(record) {
    dataset <- record$item$dataset
    columns <- dataset$columns
    typed <- unlist(lapply(dataset$sources, function(source) {
        types <- source$types
        names(types)[vapply(types, identical, logical(1), 'factor')]
    }), use.names=FALSE)
    factors <- sort(unique(as.character(c(unlist(columns$factors), typed))),
        method='radix')
    series <- as.character(unlist(columns$series))
    interactions <- columns$factor_interactions
    filters <- vapply(dataset$filters, .cdrgam_cli_methods_filter_phrase, character(1))
    preprocess <- dataset$preprocess
    list(
        series=.cdrgam_cli_methods_fact(.cdrgam_cli_methods_join(series),
            paste(series, collapse=',')),
        factors=.cdrgam_cli_methods_fact(.cdrgam_cli_methods_join(factors),
            paste(factors, collapse=',')),
        interactions=.cdrgam_cli_methods_fact(
            paste(vapply(names(interactions), function(name) {
                paste0('The factor ', name, ' was formed from the observed combinations of ',
                    .cdrgam_cli_methods_join(interactions[[name]]), '.')
            }, character(1)), collapse=' '),
            paste(vapply(names(interactions), function(name) {
                paste0(name, '=', paste(interactions[[name]], collapse='*'))
            }, character(1)), collapse='; ')
        ),
        filters=.cdrgam_cli_methods_fact(paste(filters, collapse=', and then when '),
            paste(filters, collapse='; ')),
        preprocess=if (is.null(preprocess)) .cdrgam_cli_methods_fact('', '') else {
            .cdrgam_cli_methods_fact(
                paste0('the function ', preprocess$`function`, ' in the project script ',
                    preprocess$script),
                paste0(preprocess$script, '::', preprocess$`function`)
            )
        }
    )
}

.cdrgam_cli_methods_dataset_build <- function(records, preview) {
    facts <- lapply(records, .cdrgam_cli_methods_dataset_record_facts)
    c(list(
        datasets=.cdrgam_cli_methods_count(length(records), 'dataset'),
        several=length(records) > 1L
    ), .cdrgam_cli_methods_pool(
        facts, .cdrgam_cli_methods_dataset_facts, .cdrgam_cli_methods_dataset_labels
    ))
}

.cdrgam_cli_methods_dataset_builder <- function() list(
    title='Data', context_version=1L,
    build=.cdrgam_cli_methods_dataset_build,
    table=list(name='supplementary_datasets.tsv', build=function(records) {
        .cdrgam_cli_methods_fact_table(records,
            lapply(records, .cdrgam_cli_methods_dataset_record_facts))
    }),
    configuration_fields=.cdrgam_cli_methods_dataset_fields,
    silent=.cdrgam_cli_methods_dataset_silent,
    facts=.cdrgam_cli_methods_dataset_facts
)

# --- Predictions -------------------------------------------------------------

.cdrgam_cli_methods_prediction_build <- function(records, preview) {
    datasets <- unique(vapply(records, function(record) {
        paste(record$project, record$item$dataset_name)
    }, character(1)))
    list(
        predictions=.cdrgam_cli_methods_count(length(records), 'prediction set'),
        datasets=.cdrgam_cli_methods_count(length(datasets), 'dataset')
    )
}

.cdrgam_cli_methods_prediction_builder <- function() list(
    title='Predictions', context_version=1L,
    build=.cdrgam_cli_methods_prediction_build,
    table=list(name='supplementary_predictions.tsv', build=function(records) {
        .cdrgam_cli_methods_fact_table(records, lapply(records, function(record) list()),
            columns=list(model=function(record) record$item$model_name,
                dataset=function(record) record$item$dataset_name))
    }),
    configuration_fields=character(),
    silent=character(),
    facts=character()
)

# --- Comparisons -------------------------------------------------------------

.cdrgam_cli_methods_comparison_fields <- c(
    models='table:models', evaluation.dataset='evaluation', methods='metrics'
)

.cdrgam_cli_methods_comparison_silent <- c(
    schema='Definition format version, not a scientific setting.',
    evaluation.row_id=paste(
        'Declared response row key; each metric is computed from one model',
        'prediction table at a time.'
    )
)

.cdrgam_cli_methods_metric_names <- c(
    mse='mean squared error', mae='mean absolute error'
)

.cdrgam_cli_methods_comparison_record_facts <- function(record) {
    methods <- record$item$definition$methods
    trained <- unique(vapply(record$item$predictions, function(prediction) {
        prediction$fit$dataset_name
    }, character(1)))
    training <- identical(trained, record$item$dataset_name)
    list(
        metrics=.cdrgam_cli_methods_fact(
            .cdrgam_cli_methods_join(unname(.cdrgam_cli_methods_metric_names[methods])),
            paste(methods, collapse=',')
        ),
        evaluation=.cdrgam_cli_methods_fact(
            if (training) 'the training dataset' else
                'a dataset other than the training dataset',
            if (training) 'training' else 'other'
        )
    )
}

.cdrgam_cli_methods_comparison_build <- function(records, preview) {
    facts <- lapply(records, .cdrgam_cli_methods_comparison_record_facts)
    c(list(
        comparisons=.cdrgam_cli_methods_count(length(records), 'comparison'),
        several=length(records) > 1L
    ), .cdrgam_cli_methods_pool(facts, c('metrics', 'evaluation'),
        c(metrics='metrics', evaluation='evaluation datasets')))
}

.cdrgam_cli_methods_comparison_builder <- function() list(
    title='Model comparison', context_version=1L,
    build=.cdrgam_cli_methods_comparison_build,
    table=list(name='supplementary_comparisons.tsv', build=function(records) {
        .cdrgam_cli_methods_fact_table(records,
            lapply(records, .cdrgam_cli_methods_comparison_record_facts),
            columns=list(
                models=function(record) paste(record$item$definition$models, collapse=','),
                dataset=function(record) record$item$dataset_name
            ))
    }),
    configuration_fields=.cdrgam_cli_methods_comparison_fields,
    silent=.cdrgam_cli_methods_comparison_silent,
    facts=c('metrics', 'evaluation')
)

# --- Tests -------------------------------------------------------------------

.cdrgam_cli_methods_test_fields <- c(
    terms='has_terms', effects='table:definition', estimands='has_estimands', null='null_value',
    uncertainty.level='level', uncertainty.unconditional='unconditional',
    uncertainty.draws='draws'
)

.cdrgam_cli_methods_test_silent <- c(
    schema='Definition format version, not a scientific setting.',
    model='Name of the tested model, given in supplementary_tests.tsv.',
    uncertainty.seed='Random seed of the coefficient draws; it affects only Monte Carlo error.'
)

.cdrgam_cli_methods_test_facts <- c('level', 'unconditional', 'draws', 'null_value')

.cdrgam_cli_methods_test_labels <- c(
    level='interval level', unconditional='covariance correction',
    draws='number of coefficient draws', null_value='null value'
)

.cdrgam_cli_methods_test_forms_of <- function(definition) {
    vapply(definition$estimands, `[[`, character(1), 'form')
}

.cdrgam_cli_methods_test_record_facts <- function(record) {
    definition <- record$item$definition
    uncertainty <- definition$uncertainty
    forms <- .cdrgam_cli_methods_test_forms_of(definition)
    peaks <- any(forms %in% .cdrgam_cli_test_peak_forms)
    level <- paste0(.cdrgam_cli_methods_number(100 * uncertainty$level), '%')
    list(
        level=.cdrgam_cli_methods_fact(level),
        unconditional=.cdrgam_cli_methods_fact(
            if (isTRUE(uncertainty$unconditional)) 'yes' else '',
            tolower(as.character(isTRUE(uncertainty$unconditional)))
        ),
        draws=if (peaks) .cdrgam_cli_methods_fact(as.character(uncertainty$draws)) else {
            .cdrgam_cli_methods_fact('', '')
        },
        null_value=.cdrgam_cli_methods_fact(.cdrgam_cli_methods_number(definition$null))
    )
}

# Counts the realized term tests of complete artifacts by the method recorded
# in tests.tsv.
# The term tests the core applies, as "description [@key]" phrases.
.cdrgam_cli_methods_test_term_methods <- function() {
    tests <- .cdrgam_cli_core_methods_facts('mgcv', 'bam', NULL, 'gaussian')$term_tests
    .cdrgam_cli_methods_join(paste0(tests$description, ' [@', tests$citation, ']'), 'and')
}

.cdrgam_cli_methods_test_term_counts <- function(records) {
    methods <- unlist(lapply(records, function(record) {
        if (is.null(record$manifest)) return(character())
        path <- file.path(record$item$output, 'tests.tsv')
        if (!file.exists(path)) return(character())
        table <- utils::read.delim(path, stringsAsFactors=FALSE, quote='',
            na.strings='NA')
        table$method[table$form == 'term']
    }), use.names=FALSE)
    if (!length(methods)) return('')
    # Older cores record smooth tests only as computed by summary.gam.
    smooth <- c('Wood (2013)', 'mgcv summary.gam')
    known <- c('Wald (parametric term)', smooth, 'Wood (2013b) random effect',
        'not computed by mgcv')
    groups <- c(
        parametric=sum(methods == 'Wald (parametric term)'),
        smooth=sum(methods %in% smooth),
        random=sum(methods == 'Wood (2013b) random effect'),
        missing=sum(methods == 'not computed by mgcv'),
        other=sum(!(methods %in% known))
    )
    phrases <- c(
        if (groups[['parametric']]) .cdrgam_cli_methods_count(groups[['parametric']],
            'parametric term'),
        if (groups[['smooth']]) .cdrgam_cli_methods_count(groups[['smooth']], 'smooth term'),
        if (groups[['random']]) .cdrgam_cli_methods_count(groups[['random']],
            'fully penalized term'),
        if (groups[['other']]) paste(.cdrgam_cli_methods_count(groups[['other']], 'term'),
            'with the methods recorded in tests.tsv'),
        if (groups[['missing']]) paste(.cdrgam_cli_methods_count(groups[['missing']], 'term'),
            'for which no test could be computed')
    )
    .cdrgam_cli_methods_join(phrases)
}

.cdrgam_cli_methods_form_names <- c(
    weights='weighted combinations of effect values', mean='means over effect grids',
    area='trapezoidal areas under the effect over lag', peak_lag='peak lags',
    peak_value='peak values'
)

.cdrgam_cli_methods_test_build <- function(records, preview) {
    definitions <- lapply(records, function(record) record$item$definition)
    forms <- unique(unlist(lapply(definitions, .cdrgam_cli_methods_test_forms_of)))
    linear <- intersect(.cdrgam_cli_test_linear_forms, forms)
    peaks <- intersect(.cdrgam_cli_test_peak_forms, forms)
    own_null <- any(vapply(definitions, function(definition) {
        any(vapply(definition$estimands, function(estimand) !is.null(estimand$null),
            logical(1)))
    }, logical(1)))
    counts <- if (isTRUE(preview)) '' else .cdrgam_cli_methods_test_term_counts(records)
    context <- list(
        has_terms=any(vapply(definitions, function(value) !is.null(value$terms), logical(1))),
        has_estimands=length(forms) > 0L,
        has_linear=length(linear) > 0L,
        linear_forms=.cdrgam_cli_methods_join(unname(.cdrgam_cli_methods_form_names[linear])),
        has_peak=length(peaks) > 0L,
        peak_forms=.cdrgam_cli_methods_join(unname(.cdrgam_cli_methods_form_names[peaks])),
        has_wood='wood' %in% forms,
        own_null=own_null,
        term_counts=counts,
        term_counts_present=nzchar(counts),
        term_methods=.cdrgam_cli_methods_test_term_methods()
    )
    # Interval settings apply only to estimands, so tests of terms alone do
    # not enter their pooling.
    estimand_records <- Filter(function(record) {
        length(record$item$definition$estimands) > 0L
    }, records)
    c(context, .cdrgam_cli_methods_pool(
        lapply(estimand_records, .cdrgam_cli_methods_test_record_facts),
        .cdrgam_cli_methods_test_facts, .cdrgam_cli_methods_test_labels
    ))
}

.cdrgam_cli_methods_test_table <- function(records) {
    rows <- list()
    for (record in records) {
        definition <- record$item$definition
        base <- list(project=record$project, test=record$name,
            model=record$item$model_name, identity=record$identity, state=record$state)
        for (term in definition$terms) rows[[length(rows) + 1L]] <- c(base, list(
            entry='term', name=term, form='term', effects='', null='', definition=''
        ))
        for (name in names(definition$estimands)) {
            estimand <- definition$estimands[[name]]
            effects <- .cdrgam_cli_null(names(estimand$weights), estimand$effect)
            rows[[length(rows) + 1L]] <- c(base, list(
                entry='estimand', name=name, form=estimand$form,
                effects=paste(effects, collapse=','),
                null=.cdrgam_cli_methods_number(.cdrgam_cli_null(estimand$null,
                    definition$null)),
                definition=.cdrgam_cli_methods_flatten(definition$effects[effects])
            ))
        }
    }
    as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors=FALSE)),
        stringsAsFactors=FALSE)
}

.cdrgam_cli_methods_test_builder <- function() list(
    title='Statistical tests', context_version=2L,
    build=.cdrgam_cli_methods_test_build,
    table=list(name='supplementary_tests.tsv', build=.cdrgam_cli_methods_test_table),
    configuration_fields=.cdrgam_cli_methods_test_fields,
    silent=.cdrgam_cli_methods_test_silent,
    facts=c('has_terms', 'has_estimands', .cdrgam_cli_methods_test_facts)
)

# --- Visualizations ----------------------------------------------------------

.cdrgam_cli_methods_visualization_fields <- c(
    kind='booklets_present', query='intervals', lines='intervals', preset='intervals',
    options='intervals'
)

.cdrgam_cli_methods_visualization_silent <- c(
    schema='Definition format version, not a scientific setting.',
    model='Name of the plotted model, given in supplementary_visualizations.tsv.',
    predictions='Prediction overlays; the predictions are described separately.',
    pages='Page selection of a plot booklet.',
    layers='Presentation of effect layers; their intervals are reported from the query.',
    render='Graphical presentation.'
)

.cdrgam_cli_methods_visualization_requests <- function(record) {
    item <- record$item
    if (!is.null(item$effects)) return(item$effects)
    if (!is.null(item$effect)) return(list(item$effect))
    list()
}

.cdrgam_cli_methods_visualization_record_facts <- function(record) {
    requests <- .cdrgam_cli_methods_visualization_requests(record)
    uncertainties <- lapply(requests, function(request) request$query$uncertainty)
    phrases <- unique(vapply(uncertainties, function(value) {
        level <- paste0(.cdrgam_cli_methods_number(100 * .cdrgam_cli_null(value$level, 0.95)),
            '%')
        kind <- .cdrgam_cli_null(value$kind, 'pointwise')
        paste(level, kind, 'intervals')
    }, character(1)))
    unconditional <- any(vapply(uncertainties, function(value) {
        isTRUE(value$unconditional)
    }, logical(1)))
    simultaneous <- any(vapply(uncertainties, function(value) {
        identical(value$kind, 'simultaneous')
    }, logical(1)))
    list(
        intervals=.cdrgam_cli_methods_fact(.cdrgam_cli_methods_join(phrases),
            paste(phrases, collapse=';')),
        unconditional=.cdrgam_cli_methods_fact(if (unconditional) 'yes' else '',
            tolower(as.character(unconditional))),
        simultaneous=.cdrgam_cli_methods_fact(if (simultaneous) 'yes' else '',
            tolower(as.character(simultaneous)))
    )
}

.cdrgam_cli_methods_visualization_build <- function(records, preview) {
    booklet <- vapply(records, function(record) {
        identical(record$item$definition$kind, 'booklet')
    }, logical(1))
    effects <- records[!booklet & vapply(records, function(record) {
        length(.cdrgam_cli_methods_visualization_requests(record)) > 0L
    }, logical(1))]
    context <- c(list(
        effect_figures_present=length(effects) > 0L,
        booklets_present=any(booklet)
    ), .cdrgam_cli_methods_pool(
        lapply(effects, .cdrgam_cli_methods_visualization_record_facts),
        c('intervals', 'unconditional', 'simultaneous'),
        c(intervals='intervals', unconditional='covariance correction',
            simultaneous='simultaneous intervals')
    ))
    context$any_simultaneous <- isTRUE(context$simultaneous_present) ||
        isTRUE(context$simultaneous_varies)
    context
}

.cdrgam_cli_methods_visualization_builder <- function() list(
    title='Figures', context_version=1L,
    build=.cdrgam_cli_methods_visualization_build,
    table=list(name='supplementary_visualizations.tsv', build=function(records) {
        .cdrgam_cli_methods_fact_table(records,
            lapply(records, .cdrgam_cli_methods_visualization_record_facts),
            columns=list(
                model=function(record) .cdrgam_cli_null(record$item$model_name, ''),
                kind=function(record) .cdrgam_cli_null(record$item$definition$kind, 'effect'),
                queries=function(record) .cdrgam_cli_methods_flatten(lapply(
                    .cdrgam_cli_methods_visualization_requests(record), `[[`, 'query'))
            ))
    }),
    configuration_fields=.cdrgam_cli_methods_visualization_fields,
    silent=.cdrgam_cli_methods_visualization_silent,
    facts=c('booklets_present', 'intervals')
)
