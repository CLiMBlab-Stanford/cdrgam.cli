# Methods context for fit work items: the model specification and its
# estimation. Each fact is computed per model and pooled: a fact shared by
# every model is stated once, and a fact that differs is named in the
# differences sentence and given per model in supplementary_models.tsv.

.cdrgam_cli_methods_fit_context_version <- 1L

# Each scientific field of a model definition maps to the context fact that
# reports it, or to table:COLUMN for a supplementary_models.tsv column.
.cdrgam_cli_methods_fit_configuration_fields <- c(
    formula='table:formula',
    window='window',
    knots_l='lag_basis', k_l='lag_basis', bs_l='lag_basis',
    k_t='time_basis', bs_t='time_basis',
    k_p='predictor_basis', bs_p='predictor_basis',
    fit.family='family_link', fit.link='family_link',
    fit.method='method',
    fit.engine='estimator', fit.backend='estimator',
    fit.rescale_predictors='rescale', fit.center_predictors='center',
    fit.rank_action='rank', fit.rank_tol='rank', fit.rank_penalty='rank',
    autosimplify.enabled='autosimplify', autosimplify.max_steps='autosimplify',
    autosimplify.conservatism='autosimplify', autosimplify.allow='autosimplify',
    autosimplify.protect='autosimplify'
)

.cdrgam_cli_methods_fit_silent <- c(
    schema='Definition format version, not a scientific setting.',
    datasets=paste(
        'Partition names are labels; the datasets they select are described in',
        'the dataset section.'
    ),
    fit.history=paste(
        'Dense or ragged storage of impulse histories; both represent the same',
        'design.'
    ),
    fit.chunk_size='Number of rows processed at once; it does not change the design.',
    fit.sparse_control=paste(
        'Numerical controls of the sparse solver; convergence is reported from',
        'the fit diagnostics instead.'
    ),
    fit.drop.unused.levels=paste(
        'Factor levels without training observations contribute no data to the',
        'fit.'
    )
)

.cdrgam_cli_methods_fit_facts <- c(
    'family_link', 'method', 'estimator', 'window', 'lag_basis', 'time_basis',
    'predictor_basis', 'rescale', 'center', 'rank', 'autosimplify'
)

.cdrgam_cli_methods_fit_fact_labels <- c(
    family_link='response distribution and link', method='smoothing-parameter selection',
    estimator='estimation software', window='lag window', lag_basis='lag basis',
    time_basis='response-time basis', predictor_basis='predictor basis',
    rescale='predictor rescaling', center='predictor centering',
    rank='handling of rank deficiency', autosimplify='automatic simplification'
)

.cdrgam_cli_methods_number <- function(value) {
    format(signif(as.numeric(value), 6L), trim=TRUE, scientific=FALSE,
        drop0trailing=TRUE)
}

.cdrgam_cli_methods_join <- function(values, conjunction='and') {
    values <- as.character(values)
    if (length(values) <= 1L) return(paste(values, collapse=''))
    if (length(values) == 2L) return(paste(values[[1L]], conjunction, values[[2L]]))
    paste0(paste(values[-length(values)], collapse=', '), ', ', conjunction, ' ',
        values[[length(values)]])
}

.cdrgam_cli_methods_article <- function(word) {
    if (grepl('^[aeiouAEIOU]', word)) 'an' else 'a'
}

.cdrgam_cli_methods_count <- function(count, singular, plural=paste0(singular, 's')) {
    paste(count, if (count == 1L) singular else plural)
}

.cdrgam_cli_methods_family_names <- c(
    gaussian='Gaussian', binomial='binomial', poisson='Poisson', Gamma='gamma',
    gaulss='Gaussian location-scale'
)

.cdrgam_cli_methods_basis_names <- list(
    cr=list(name='cubic regression spline', cite='wood2017generalized'),
    cs=list(name='shrinkage cubic regression spline', cite='wood2017generalized'),
    cc=list(name='cyclic cubic regression spline', cite='wood2017generalized'),
    tp=list(name='thin plate regression spline', cite='wood2003thin'),
    ts=list(name='shrinkage thin plate regression spline', cite='wood2003thin'),
    ps=list(name='P-spline', cite='eilers1996flexible'),
    bs=list(name='B-spline', cite='wood2017generalized')
)

.cdrgam_cli_methods_basis <- function(bs, k) {
    known <- .cdrgam_cli_methods_basis_names[[bs]]
    name <- if (is.null(known)) paste0('mgcv "', bs, '"') else known$name
    cite <- if (is.null(known)) 'wood2017generalized' else known$cite
    list(
        text=paste0('a ', name, ' basis of dimension ', k, ' [@', cite, ']'),
        value=paste0(bs, ', k=', k)
    )
}

# The settings that applied to one fit: effective values recorded in a
# complete manifest, otherwise the requested values with the installed
# core's defaults for omitted ones.
.cdrgam_cli_methods_fit_settings <- function(record, defaults) {
    model <- record$item$model
    requested <- .cdrgam_cli_null(model$fit, list())
    effective <- record$manifest$result$configuration$effective
    pick <- function(field, source) {
        value <- source[[field]]
        if (!is.null(value)) return(value)
        value <- requested[[field]]
        if (!is.null(value)) return(value)
        defaults[[field]]
    }
    fitting <- .cdrgam_cli_null(effective$fitting, list())
    preparation <- .cdrgam_cli_null(effective$preparation, list())
    term_default <- function(field) {
        value <- model[[field]]
        if (is.null(value)) defaults[[field]] else value
    }
    list(
        family=pick('family', fitting), link=pick('link', fitting),
        method=.cdrgam_cli_null(fitting$method, requested$method),
        engine=pick('engine', fitting), backend=pick('backend', fitting),
        rescale=pick('rescale_predictors', preparation),
        center=pick('center_predictors', preparation),
        rank_action=pick('rank_action', fitting),
        rank_tol=.cdrgam_cli_null(fitting$rank_tolerance, requested$rank_tol),
        rank_penalty=.cdrgam_cli_null(fitting$rank_penalty, requested$rank_penalty),
        window=model$window, knots_l=model$knots_l,
        k_l=term_default('k_l'), bs_l=term_default('bs_l'),
        k_t=term_default('k_t'), bs_t=term_default('bs_t'),
        k_p=term_default('k_p'), bs_p=term_default('bs_p'),
        autosimplify=.cdrgam_cli_null(model$autosimplify, list(enabled=FALSE))
    )
}

.cdrgam_cli_methods_fact <- function(text, value=text) list(text=text, value=value)

.cdrgam_cli_methods_fit_record_facts <- function(settings) {
    family <- .cdrgam_cli_null(
        .cdrgam_cli_methods_family_names[settings$family][[1L]], settings$family
    )
    family_link <- .cdrgam_cli_methods_fact(
        paste0(.cdrgam_cli_methods_article(family), ' ', family,
            ' distribution and ',
            if (identical(settings$family, 'gaulss')) {
                'its default links for location and scale [@wood2016smoothing]'
            } else paste0(.cdrgam_cli_methods_article(settings$link), ' ', settings$link,
                ' link')),
        paste0(settings$family, '(', settings$link, ')')
    )
    core <- .cdrgam_cli_core_methods_facts(
        settings$backend, settings$engine, settings$method,
        if (identical(settings$family, 'gaulss')) 'gaulss' else
            get(settings$family, asNamespace('stats'))(link=settings$link)
    )
    cite <- function(keys) {
        if (length(keys)) paste0(' [', paste0('@', keys, collapse='; '), ']') else ''
    }
    method <- .cdrgam_cli_methods_fact(
        paste0(core$criterion, cite(core$citations$criterion)), core$method
    )
    estimator <- .cdrgam_cli_methods_fact(
        paste0(core$estimator, cite(core$citations$estimator)),
        paste(c(settings$backend, core$engine), collapse=' ')
    )
    window <- if (is.null(settings$window)) .cdrgam_cli_methods_fact('', '') else {
        bounds <- vapply(settings$window, .cdrgam_cli_methods_number, character(1))
        .cdrgam_cli_methods_fact(paste(bounds[[1L]], 'to', bounds[[2L]]),
            paste(bounds, collapse=','))
    }
    lag <- .cdrgam_cli_methods_basis(settings$bs_l, settings$k_l)
    if (!is.null(settings$knots_l)) {
        knots <- vapply(settings$knots_l, .cdrgam_cli_methods_number, character(1))
        lag$text <- paste0(lag$text, ' with knots at lags ', .cdrgam_cli_methods_join(knots))
        lag$value <- paste0(lag$value, ', knots=', paste(knots, collapse=','))
    }
    time <- if (is.null(settings$k_t)) .cdrgam_cli_methods_fact('', 'none') else {
        .cdrgam_cli_methods_basis(settings$bs_t, settings$k_t)
    }
    predictor <- if (is.null(settings$k_p)) {
        .cdrgam_cli_methods_fact('', 'linear')
    } else {
        k_p <- unlist(settings$k_p)
        .cdrgam_cli_methods_basis(settings$bs_p[[1L]], paste(k_p, collapse=', '))
    }
    rank <- if (identical(settings$rank_action, 'error')) {
        .cdrgam_cli_methods_fact('', 'error')
    } else {
        text <- switch(settings$rank_action,
            minimum_norm='by taking the minimum-norm solution among equivalent coefficient vectors',
            drop='by dropping unidentifiable coefficients',
            penalize=paste0('by adding a ridge penalty',
                if (!is.null(settings$rank_penalty)) paste0(' of ',
                    .cdrgam_cli_methods_number(settings$rank_penalty)) else '',
                ' to unidentifiable directions'),
            paste('with rank action', settings$rank_action)
        )
        .cdrgam_cli_methods_fact(text, paste(c(settings$rank_action,
            if (!is.null(settings$rank_tol)) paste0('tol=', settings$rank_tol),
            if (!is.null(settings$rank_penalty)) paste0('penalty=', settings$rank_penalty)
        ), collapse=', '))
    }
    policy <- settings$autosimplify
    autosimplify <- if (!isTRUE(policy$enabled)) .cdrgam_cli_methods_fact('', 'disabled') else {
        actions <- gsub('_', ' ', policy$allow, fixed=TRUE)
        text <- paste0(
            .cdrgam_cli_methods_count(policy$max_steps, 'fit'),
            ' per model with conservatism ', .cdrgam_cli_methods_number(policy$conservatism),
            '; the allowed edits were ', .cdrgam_cli_methods_join(actions, 'or'),
            if (length(policy$protect)) paste0(
                '; the terms ', .cdrgam_cli_methods_join(policy$protect),
                ' were protected from simplification'
            ) else ''
        )
        .cdrgam_cli_methods_fact(text, paste0(
            'max_steps=', policy$max_steps, ', conservatism=', policy$conservatism,
            ', allow=', paste(policy$allow, collapse='|'),
            if (length(policy$protect)) paste0(', protect=',
                paste(policy$protect, collapse='|')) else ''
        ))
    }
    list(
        family_link=family_link, method=method, estimator=estimator, window=window,
        lag_basis=lag, time_basis=time, predictor_basis=predictor,
        rescale=.cdrgam_cli_methods_fact(if (isTRUE(settings$rescale)) 'yes' else '',
            tolower(as.character(isTRUE(settings$rescale)))),
        center=.cdrgam_cli_methods_fact(if (isTRUE(settings$center)) 'yes' else '',
            tolower(as.character(isTRUE(settings$center)))),
        rank=rank, autosimplify=autosimplify
    )
}

# Reads the edits that automatic simplification applied to a complete fit.
.cdrgam_cli_methods_fit_simplifications <- function(record) {
    derivation <- record$manifest$result$derivation
    if (is.null(derivation$chain)) return(list())
    path <- file.path(record$item$output, derivation$chain)
    if (!file.exists(path)) return(list())
    chain <- .cdrgam_cli_read_yaml(path)
    edits <- Filter(Negate(is.null), lapply(chain$steps, `[[`, 'selection'))
    lapply(edits, function(selection) list(
        model=record$name, term=selection$term, action=selection$action
    ))
}

.cdrgam_cli_methods_action_phrases <- c(
    drop_grouped_deviation='removed the grouped deviation of',
    drop_random_effect='removed the random effect',
    drop_term='removed the term',
    intercept_only_parameter='reduced to an intercept the distributional parameter'
)

.cdrgam_cli_methods_fit_build <- function(records, preview) {
    defaults <- .cdrgam_cli_core_fit_defaults()
    facts <- lapply(records, function(record) {
        .cdrgam_cli_methods_fit_record_facts(
            .cdrgam_cli_methods_fit_settings(record, defaults)
        )
    })
    context <- list(
        models=.cdrgam_cli_methods_count(length(records), 'model'),
        several=length(records) > 1L
    )
    context <- c(context, .cdrgam_cli_methods_pool(
        facts, .cdrgam_cli_methods_fit_facts, .cdrgam_cli_methods_fit_fact_labels
    ))
    context$autosimplify_used <- isTRUE(context$autosimplify_present) ||
        isTRUE(context$autosimplify_varies)
    context$realized <- !isTRUE(preview)
    outcomes <- vapply(records, function(record) {
        if (is.null(record$manifest)) return('')
        .cdrgam_cli_fit_outcome(record$manifest$result$diagnostics)
    }, character(1))
    warned <- sum(outcomes == 'warning')
    criteria <- "met the convergence criteria of cdrgam's optimizer diagnostics"
    context$convergence <- if (warned) paste0(
        'All ', .cdrgam_cli_methods_count(length(records), 'fit'), ' ', criteria, '; ',
        warned, if (warned == 1L) ' fit has' else ' fits have',
        ' warnings recorded in the diagnostics'
    ) else if (length(records) == 1L) paste('The fit', criteria, 'without warnings') else {
        paste('All', length(records), 'fits', criteria, 'without warnings')
    }
    edits <- if (isTRUE(preview)) list() else {
        unlist(lapply(records, .cdrgam_cli_methods_fit_simplifications), recursive=FALSE)
    }
    context$simplified_present <- length(edits) > 0L
    context$simplified <- .cdrgam_cli_methods_join(vapply(edits, function(edit) {
        phrase <- .cdrgam_cli_methods_action_phrases[edit$action]
        if (is.na(phrase)) phrase <- paste('applied', edit$action, 'to')
        paste0(phrase, ' ', edit$term, ' in model ', edit$model)
    }, character(1)))
    context$unsimplified_present <- isTRUE(context$autosimplify_present) &&
        !isTRUE(preview) && !length(edits)
    context
}

.cdrgam_cli_methods_fit_table <- function(records) {
    defaults <- .cdrgam_cli_core_fit_defaults()
    rows <- lapply(records, function(record) {
        facts <- .cdrgam_cli_methods_fit_record_facts(
            .cdrgam_cli_methods_fit_settings(record, defaults)
        )
        formula <- record$item$model$formula
        fitted <- record$manifest$result$formulas$user
        flatten <- function(value) {
            if (is.null(value)) return('')
            if (is.list(value)) value <- paste(names(value), unlist(value), sep=': ',
                collapse='; ')
            gsub('[\t\r\n]+', ' ', paste(value, collapse=' '))
        }
        c(
            list(project=record$project, model=record$name, identity=record$identity,
                state=record$state, formula=flatten(formula),
                fitted_formula=flatten(fitted)),
            lapply(facts, `[[`, 'value')
        )
    })
    as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors=FALSE)),
        stringsAsFactors=FALSE)
}

.cdrgam_cli_methods_fit_builder <- function() list(
    title='Model specification and estimation',
    context_version=.cdrgam_cli_methods_fit_context_version,
    build=.cdrgam_cli_methods_fit_build,
    table=list(name='supplementary_models.tsv', build=.cdrgam_cli_methods_fit_table),
    configuration_fields=.cdrgam_cli_methods_fit_configuration_fields,
    silent=.cdrgam_cli_methods_fit_silent,
    facts=.cdrgam_cli_methods_fit_facts
)
