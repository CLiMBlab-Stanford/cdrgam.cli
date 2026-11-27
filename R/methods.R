# Methods reporting: collects the selected artifacts and their lineage,
# renders section templates against per-kind contexts, and writes the report.
# Context builders and templates for each artifact kind are registered in
# .cdrgam_cli_methods_builders(); kinds without a builder are listed in
# methods.json but contribute no prose.

.cdrgam_cli_methods_kinds <- c(
    'dataset', 'fit', 'prediction', 'comparison', 'test', 'visualization'
)


.cdrgam_cli_methods_json_schema <- 1L

# Each entry is list(title=, context_version=, build=function(records, preview),
# table=, configuration_fields=, silent=). build returns the named context
# values for the kind's templates; table optionally names a supplementary TSV
# and the function that builds its rows from the same records.
.cdrgam_cli_methods_builders <- function() list(
    dataset=.cdrgam_cli_methods_dataset_builder(),
    fit=.cdrgam_cli_methods_fit_builder(),
    prediction=.cdrgam_cli_methods_prediction_builder(),
    comparison=.cdrgam_cli_methods_comparison_builder(),
    test=.cdrgam_cli_methods_test_builder(),
    visualization=.cdrgam_cli_methods_visualization_builder()
)

# Every field must be either reported or listed as silent, and no declaration
# may name a field that does not exist or a context fact the builder lacks.
.cdrgam_cli_methods_check_coverage <- function(builder, fields, facts=builder$facts) {
    reported <- names(builder$configuration_fields)
    silent <- names(builder$silent)
    problems <- c(
        if (length(intersect(reported, silent))) paste0(
            'reported and silent: ', paste(intersect(reported, silent), collapse=', ')),
        if (length(setdiff(fields, c(reported, silent)))) paste0(
            'neither reported nor silent: ',
            paste(setdiff(fields, c(reported, silent)), collapse=', ')),
        if (length(setdiff(c(reported, silent), fields))) paste0(
            'declared but not a field: ',
            paste(setdiff(c(reported, silent), fields), collapse=', '))
    )
    targets <- unique(builder$configuration_fields)
    targets <- targets[!startsWith(targets, 'table:')]
    if (length(setdiff(targets, facts))) problems <- c(problems, paste0(
        'reported by unknown facts: ', paste(setdiff(targets, facts), collapse=', ')))
    if (length(problems)) .cdrgam_cli_abort(paste0(
        'Methods coverage for ', builder$title, ' is incomplete: ',
        paste(problems, collapse='; ')
    ))
    invisible(TRUE)
}

.cdrgam_cli_methods_template_root <- function() {
    system.file('methods', 'templates', package='cdrgam.cli')
}

# --- Pooling -----------------------------------------------------------------

# Pools per-record facts, each list(text=, value=). A fact whose value is the
# same for every record keeps its text; one that differs gets empty text and
# is named in differences. For each fact F the context holds F, F_varies,
# and F_present (shared and nonempty).
.cdrgam_cli_methods_pool <- function(facts, names, labels) {
    context <- list()
    varying <- character()
    for (fact in names) {
        values <- vapply(facts, function(value) value[[fact]]$value, character(1))
        varies <- length(unique(values)) > 1L
        text <- if (varies || !length(facts)) '' else facts[[1L]][[fact]]$text
        context[[fact]] <- text
        context[[paste0(fact, '_varies')]] <- varies
        context[[paste0(fact, '_present')]] <- !varies && nzchar(text)
        if (varies) varying <- c(varying, labels[[fact]])
    }
    context$differences <- .cdrgam_cli_methods_join(varying)
    context$differences_present <- length(varying) > 0L
    context
}

# One supplementary-table row per record: identifying columns, then each
# fact's value.
.cdrgam_cli_methods_fact_table <- function(records, facts, columns=list()) {
    rows <- lapply(seq_along(records), function(index) {
        record <- records[[index]]
        extra <- lapply(columns, function(column) column(record))
        c(list(project=record$project, name=record$name, identity=record$identity,
            state=record$state), extra, lapply(facts[[index]], `[[`, 'value'))
    })
    as.data.frame(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors=FALSE)),
        stringsAsFactors=FALSE)
}

# --- Collection --------------------------------------------------------------

.cdrgam_cli_methods_record <- function(item, preview) {
    state <- if (isTRUE(preview)) 'preview' else if (
        .cdrgam_cli_complete_artifact(item$output, item$identity)
    ) 'complete' else if (file.exists(item$output)) 'stale' else 'missing'
    manifest <- if (identical(state, 'complete')) {
        .cdrgam_cli_read_yaml(.cdrgam_cli_manifest_path(item$output))
    } else NULL
    list(
        project=item$project, kind=item$kind, name=item$name,
        identity=item$identity, state=state,
        included=state %in% c('preview', 'complete'),
        manifest=manifest, item=item
    )
}

# Datasets are not work items; each one is reported once per identity for the
# included fits and predictions that read it.
.cdrgam_cli_methods_dataset_records <- function(records) {
    output <- list()
    for (record in records) {
        if (!isTRUE(record$included) ||
                !(record$kind %in% c('fit', 'prediction'))) next
        item <- record$item
        key <- paste(item$project, item$dataset_name, item$dataset_identity$identity)
        if (!is.null(output[[key]])) next
        output[[key]] <- list(
            project=item$project, kind='dataset', name=item$dataset_name,
            identity=item$dataset_identity$identity,
            state=if (identical(record$state, 'preview')) 'preview' else 'resolved',
            included=TRUE, manifest=NULL,
            item=list(dataset=item$dataset, dataset_identity=item$dataset_identity)
        )
    }
    unname(output)
}

.cdrgam_cli_methods_sort <- function(records) {
    if (!length(records)) return(records)
    order <- order(
        match(vapply(records, `[[`, character(1), 'kind'), .cdrgam_cli_methods_kinds),
        vapply(records, `[[`, character(1), 'project'),
        vapply(records, `[[`, character(1), 'name'),
        vapply(records, `[[`, character(1), 'identity'),
        method='radix'
    )
    records[order]
}

.cdrgam_cli_methods_collect <- function(graph, preview=FALSE) {
    records <- lapply(graph$items, .cdrgam_cli_methods_record, preview=preview)
    records <- c(unname(records), .cdrgam_cli_methods_dataset_records(records))
    .cdrgam_cli_methods_sort(records)
}

# --- Templates ---------------------------------------------------------------

.cdrgam_cli_methods_field_pattern <- '[A-Za-z_][A-Za-z0-9_]*(\\.[A-Za-z_][A-Za-z0-9_]*)*'

.cdrgam_cli_methods_read_template <- function(path) {
    lines <- readLines(path, warn=FALSE, encoding='UTF-8')
    where <- paste0('Methods template ', sQuote(path))
    if (!length(lines) || !identical(lines[[1L]], '---')) {
        .cdrgam_cli_abort(paste0(where, ' must begin with a --- front matter line'))
    }
    end <- which(lines == '---')
    end <- end[end > 1L]
    if (!length(end)) .cdrgam_cli_abort(paste0(where, ' has unterminated front matter'))
    end <- end[[1L]]
    # Keep YAML 1.1 booleans such as y, yes, and on as strings: they are
    # valid field names in requires.
    keep <- function(value) value
    header <- yaml::yaml.load(
        paste(lines[seq.int(2L, length.out=end - 2L)], collapse='\n'),
        handlers=list(`bool#yes`=keep, `bool#no`=keep)
    )
    if (is.null(header)) header <- list()
    unknown <- setdiff(names(header), c('section', 'context_version', 'when', 'requires'))
    if (length(unknown)) .cdrgam_cli_abort(paste0(
        where, ' has unknown front matter fields: ', paste(unknown, collapse=', ')
    ))
    section <- header$section
    if (!is.character(section) || length(section) != 1L || !nzchar(section)) {
        .cdrgam_cli_abort(paste0(where, ' requires a section heading'))
    }
    version <- header$context_version
    if (!is.numeric(version) || length(version) != 1L || version != round(version)) {
        .cdrgam_cli_abort(paste0(where, ' requires an integer context_version'))
    }
    when <- header$when
    if (!is.null(when) && (!is.character(when) || length(when) != 1L ||
            !grepl(paste0('^', .cdrgam_cli_methods_field_pattern, '$'), when))) {
        .cdrgam_cli_abort(paste0(where, ' when must name one context field'))
    }
    requires <- header$requires
    if (is.null(requires)) requires <- character()
    if (!is.character(requires) && !(is.list(requires) && all(vapply(
            requires, function(value) is.character(value) && length(value) == 1L,
            logical(1))))) {
        .cdrgam_cli_abort(paste0(where, ' requires must be a list of field names'))
    }
    requires <- as.character(unlist(requires))
    body <- paste(lines[-seq_len(end)], collapse='\n')
    # As in mustache, a line holding only section tags leaves no line break.
    body <- gsub(
        '(?m)^[ \t]*((\\{\\{\\s*[#^/][^}]*\\}\\}[ \t]*)+)(\n|$)', '\\1', body,
        perl=TRUE
    )
    tokens <- .cdrgam_cli_methods_tokenize(body, where)
    used <- unique(c(when, vapply(
        Filter(function(token) !identical(token$type, 'text'), tokens),
        `[[`, character(1), 'name'
    )))
    unlisted <- setdiff(used, requires)
    unused <- setdiff(requires, used)
    if (length(unlisted)) .cdrgam_cli_abort(paste0(
        where, ' uses fields missing from requires: ', paste(unlisted, collapse=', ')
    ))
    if (length(unused)) .cdrgam_cli_abort(paste0(
        where, ' lists fields it does not use: ', paste(unused, collapse=', ')
    ))
    list(
        path=path, file=basename(path), section=section,
        context_version=as.integer(version), when=when, requires=requires,
        tokens=tokens
    )
}

# A strict subset of mustache: {{field}} inserts a scalar value without
# escaping, and {{#field}}...{{/field}} or {{^field}}...{{/field}} include
# their contents when a logical field is TRUE or FALSE. Writers escape text
# for their format, so templates and contexts hold plain prose.
.cdrgam_cli_methods_tokenize <- function(body, where='Methods template') {
    pattern <- paste0('\\{\\{\\s*([#^/]?)\\s*(', .cdrgam_cli_methods_field_pattern, ')\\s*\\}\\}')
    matches <- gregexpr(pattern, body, perl=TRUE)[[1L]]
    tokens <- list()
    position <- 1L
    if (matches[[1L]] != -1L) {
        starts <- as.integer(matches)
        lengths <- attr(matches, 'match.length')
        capture_starts <- attr(matches, 'capture.start')
        capture_lengths <- attr(matches, 'capture.length')
        for (index in seq_along(starts)) {
            if (starts[[index]] > position) tokens[[length(tokens) + 1L]] <- list(
                type='text', text=substr(body, position, starts[[index]] - 1L)
            )
            sigil <- substr(
                body, capture_starts[index, 1L],
                capture_starts[index, 1L] + capture_lengths[index, 1L] - 1L
            )
            name <- substr(
                body, capture_starts[index, 2L],
                capture_starts[index, 2L] + capture_lengths[index, 2L] - 1L
            )
            type <- switch(sigil, `#`='open', `^`='inverse', `/`='close', 'field')
            tokens[[length(tokens) + 1L]] <- list(type=type, name=name)
            position <- starts[[index]] + lengths[[index]]
        }
    }
    if (position <= nchar(body)) tokens[[length(tokens) + 1L]] <- list(
        type='text', text=substr(body, position, nchar(body))
    )
    remaining <- vapply(Filter(function(token) identical(token$type, 'text'), tokens),
        `[[`, character(1), 'text')
    if (any(grepl('{{', remaining, fixed=TRUE) | grepl('}}', remaining, fixed=TRUE))) {
        .cdrgam_cli_abort(paste0(where, ' contains a malformed {{ }} tag'))
    }
    stack <- character()
    for (token in tokens) {
        if (token$type %in% c('open', 'inverse')) stack <- c(stack, token$name)
        if (identical(token$type, 'close')) {
            if (!length(stack) || !identical(stack[[length(stack)]], token$name)) {
                .cdrgam_cli_abort(paste0(
                    where, ' closes section ', token$name, ' that is not open'
                ))
            }
            stack <- stack[-length(stack)]
        }
    }
    if (length(stack)) .cdrgam_cli_abort(paste0(
        where, ' leaves section ', stack[[length(stack)]], ' open'
    ))
    tokens
}

.cdrgam_cli_methods_lookup <- function(context, name, where) {
    value <- context
    for (part in strsplit(name, '.', fixed=TRUE)[[1L]]) {
        if (!is.list(value) || !(part %in% names(value))) {
            .cdrgam_cli_abort(paste0(where, ' reads undefined context field ', name))
        }
        value <- value[[part]]
    }
    value
}

.cdrgam_cli_methods_flag <- function(context, name, where) {
    value <- .cdrgam_cli_methods_lookup(context, name, where)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
        .cdrgam_cli_abort(paste0(where, ' section field ', name, ' must be TRUE or FALSE'))
    }
    value
}

.cdrgam_cli_methods_value <- function(context, name, where) {
    value <- .cdrgam_cli_methods_lookup(context, name, where)
    # Doubles must be formatted by the context builder so that templates
    # cannot print unrounded numbers.
    if (!(is.character(value) || is.integer(value)) || length(value) != 1L ||
            is.na(value)) {
        .cdrgam_cli_abort(paste0(
            where, ' field ', name, ' must be one character string or integer'
        ))
    }
    as.character(value)
}

.cdrgam_cli_methods_render_tokens <- function(tokens, context, where) {
    output <- character()
    skip <- 0L
    stack <- list()
    for (token in tokens) {
        if (token$type %in% c('open', 'inverse')) {
            active <- skip == 0L && (
                .cdrgam_cli_methods_flag(context, token$name, where) ==
                    identical(token$type, 'open')
            )
            stack[[length(stack) + 1L]] <- active
            if (!active) skip <- skip + 1L
            next
        }
        if (identical(token$type, 'close')) {
            if (!isTRUE(stack[[length(stack)]])) skip <- skip - 1L
            stack[[length(stack)]] <- NULL
            next
        }
        if (skip > 0L) next
        output <- c(output, if (identical(token$type, 'text')) token$text else {
            .cdrgam_cli_methods_value(context, token$name, where)
        })
    }
    paste(output, collapse='')
}

# Returns the paragraphs of a rendered template, each with its whitespace
# collapsed, or NULL when the template's when field is FALSE.
.cdrgam_cli_methods_render <- function(template, context) {
    where <- paste0('Methods template ', template$file)
    if (!is.null(template$when) &&
            !.cdrgam_cli_methods_flag(context, template$when, where)) return(NULL)
    text <- .cdrgam_cli_methods_render_tokens(template$tokens, context, where)
    paragraphs <- strsplit(text, '\n[ \t]*\n', perl=TRUE)[[1L]]
    paragraphs <- trimws(gsub('\\s+', ' ', paragraphs, perl=TRUE))
    paragraphs[nzchar(paragraphs)]
}

.cdrgam_cli_methods_templates <- function(kind, root=.cdrgam_cli_methods_template_root()) {
    directory <- file.path(root, kind)
    if (!nzchar(root) || !dir.exists(directory)) return(list())
    files <- sort(list.files(directory, pattern='^[0-9]{2}_[A-Za-z0-9_-]+[.]md$'),
        method='radix')
    lapply(file.path(directory, files), .cdrgam_cli_methods_read_template)
}

# --- Citations ---------------------------------------------------------------

# A citation is [@key] or [@key1; @key2] for a parenthetical citation and
# @key for a narrative one. Keys use letters, digits, and _:- characters.
.cdrgam_cli_methods_citation_pattern <-
    '\\[@[A-Za-z0-9_:-]+(\\s*;\\s*@[A-Za-z0-9_:-]+)*\\]|(?<![A-Za-z0-9_@.])@[A-Za-z0-9_:-]+'

.cdrgam_cli_methods_citation_keys <- function(paragraphs) {
    matches <- regmatches(paragraphs, gregexpr(
        .cdrgam_cli_methods_citation_pattern, paragraphs, perl=TRUE
    ))
    keys <- unlist(lapply(unlist(matches), function(marker) {
        regmatches(marker, gregexpr('(?<=@)[A-Za-z0-9_:-]+', marker, perl=TRUE))[[1L]]
    }), use.names=FALSE)
    unique(as.character(keys))
}

# Splits a paragraph into text and citation parts for a writer. Each element
# is list(type='text', text=) or list(type='parenthetical'|'narrative', keys=).
.cdrgam_cli_methods_split_citations <- function(paragraph) {
    matches <- gregexpr(.cdrgam_cli_methods_citation_pattern, paragraph, perl=TRUE)[[1L]]
    if (matches[[1L]] == -1L) return(list(list(type='text', text=paragraph)))
    parts <- list()
    position <- 1L
    starts <- as.integer(matches)
    lengths <- attr(matches, 'match.length')
    for (index in seq_along(starts)) {
        if (starts[[index]] > position) parts[[length(parts) + 1L]] <- list(
            type='text', text=substr(paragraph, position, starts[[index]] - 1L)
        )
        marker <- substr(paragraph, starts[[index]], starts[[index]] + lengths[[index]] - 1L)
        keys <- regmatches(marker, gregexpr('(?<=@)[A-Za-z0-9_:-]+', marker, perl=TRUE))[[1L]]
        parts[[length(parts) + 1L]] <- list(
            type=if (startsWith(marker, '[')) 'parenthetical' else 'narrative', keys=keys
        )
        position <- starts[[index]] + lengths[[index]]
    }
    if (position <= nchar(paragraph)) parts[[length(parts) + 1L]] <- list(
        type='text', text=substr(paragraph, position, nchar(paragraph))
    )
    parts
}

# --- Document ----------------------------------------------------------------

# bibliography is the parsed reference file; the document keeps only the
# entries its rendered text cites.
.cdrgam_cli_methods_document <- function(
        records, preview=FALSE, builders=.cdrgam_cli_methods_builders(),
        template_root=.cdrgam_cli_methods_template_root(),
        bibliography=.cdrgam_cli_methods_read_bibliography()
) {
    included <- Filter(function(record) isTRUE(record$included), records)
    kinds <- unique(vapply(included, `[[`, character(1), 'kind'))
    kinds <- .cdrgam_cli_methods_kinds[.cdrgam_cli_methods_kinds %in% kinds]
    modules <- list()
    tables <- list()
    undescribed <- character()
    for (kind in kinds) {
        builder <- builders[[kind]]
        templates <- .cdrgam_cli_methods_templates(kind, template_root)
        if (is.null(builder) || !length(templates)) {
            undescribed <- c(undescribed, kind)
            next
        }
        for (template in templates) if (template$context_version != builder$context_version) {
            .cdrgam_cli_abort(paste0(
                'Methods template ', template$file, ' was written for ', kind,
                ' context version ', template$context_version, '; the current version is ',
                builder$context_version
            ))
        }
        kind_records <- Filter(function(record) identical(record$kind, kind), included)
        context <- builder$build(kind_records, preview)
        sections <- list()
        for (template in templates) {
            paragraphs <- .cdrgam_cli_methods_render(template, context)
            if (is.null(paragraphs) || !length(paragraphs)) next
            sections[[length(sections) + 1L]] <- list(
                heading=template$section,
                template=file.path(kind, template$file),
                paragraphs=paragraphs,
                fields=context[intersect(
                    template$requires, names(context)
                )],
                citations=.cdrgam_cli_methods_citation_keys(paragraphs)
            )
        }
        if (length(sections)) modules[[length(modules) + 1L]] <- list(
            kind=kind, title=builder$title, sections=sections
        )
        if (!is.null(builder$table)) {
            tables[[builder$table$name]] <- builder$table$build(kind_records)
        }
    }
    citations <- unique(unlist(lapply(modules, function(module) {
        lapply(module$sections, `[[`, 'citations')
    }), use.names=FALSE))
    list(
        modules=modules, undescribed=undescribed, tables=tables,
        references=.cdrgam_cli_methods_cited_references(
            bibliography, sort(as.character(citations), method='radix')
        )
    )
}

# --- Writers -----------------------------------------------------------------

# Each format supplies escape, emphasis, heading, paragraph, reference-list,
# and document wrappers; .cdrgam_cli_methods_write_report() walks the
# document once for all of them. Text from templates and references is plain
# Unicode, so every format escapes it before adding its own markup.

.cdrgam_cli_methods_author_year_citation <- function(part, references, escape) {
    entries <- references[part$keys]
    if (identical(part$type, 'narrative')) {
        return(escape(paste0(entries[[1L]]$narrative_label, ' (',
            entries[[1L]]$year_label, ')')))
    }
    escape(paste0('(', paste(vapply(entries, function(entry) {
        paste0(entry$author_label, ', ', entry$year_label)
    }, character(1)), collapse='; '), ')'))
}

.cdrgam_cli_methods_rtf_escape <- function(text) {
    text <- gsub('([\\\\{}])', '\\\\\\1', text, perl=TRUE)
    codes <- utf8ToInt(enc2utf8(text))
    if (all(codes < 128L)) return(text)
    paste(vapply(codes, function(code) {
        if (code < 128L) intToUtf8(code) else {
            # RTF \u takes a signed 16-bit value; '?' is the ANSI fallback.
            paste0('\\u', if (code > 32767L) code - 65536L else code, '?')
        }
    }, character(1)), collapse='')
}

.cdrgam_cli_methods_tex_escape <- function(text) {
    text <- gsub('\\', '\001', text, fixed=TRUE)
    text <- gsub('([&%$#_{}])', '\\\\\\1', text, perl=TRUE)
    text <- gsub('~', '\\textasciitilde{}', text, fixed=TRUE)
    text <- gsub('^', '\\textasciicircum{}', text, fixed=TRUE)
    gsub('\001', '\\textbackslash{}', text, fixed=TRUE)
}

# The cited entries as BibTeX, for the tex format.
.cdrgam_cli_methods_write_bib <- function(references) {
    derived <- c('type', 'key', 'authors', 'suffix', 'year_label', 'author_label',
        'narrative_label')
    entries <- vapply(references[order(names(references), method='radix')], function(entry) {
        fields <- setdiff(names(entry), derived)
        paste0('@', entry$type, '{', entry$key, ',\n',
            paste0('  ', fields, ' = {', unlist(entry[fields]), '}', collapse=',\n'),
            '\n}')
    }, character(1))
    paste0(paste(entries, collapse='\n\n'), '\n')
}

.cdrgam_cli_methods_writers <- function() {
    markdown_escape <- function(text) text
    list(
        md=list(
            escape=markdown_escape,
            emphasis=function(text) paste0('*', text, '*'),
            cite=function(part, references) .cdrgam_cli_methods_author_year_citation(
                part, references, markdown_escape),
            heading=function(level, text) c('', paste0(strrep('#', level), ' ', text)),
            paragraph=function(text) c('', text),
            references=function(entries) c('', '## References', '', paste0('- ', entries)),
            wrap=function(lines) paste0(paste(lines[-1L], collapse='\n'), '\n'),
            author_year_references=TRUE
        ),
        rtf=list(
            escape=.cdrgam_cli_methods_rtf_escape,
            emphasis=function(text) paste0('{\\i ', text, '}'),
            cite=function(part, references) .cdrgam_cli_methods_author_year_citation(
                part, references, .cdrgam_cli_methods_rtf_escape),
            heading=function(level, text) paste0(
                '{\\pard\\keepn\\sb240\\sa120\\b\\fs', c(32L, 28L, 24L)[[level]],
                ' ', text, '\\par}'),
            paragraph=function(text) paste0('{\\pard\\sa120 ', text, '\\par}'),
            references=function(entries) c(
                '{\\pard\\keepn\\sb240\\sa120\\b\\fs28 References\\par}',
                paste0('{\\pard\\fi-720\\li720\\sa120 ', entries, '\\par}')
            ),
            wrap=function(lines) paste0(paste(c(
                '{\\rtf1\\ansi\\ansicpg1252\\deff0',
                '{\\fonttbl{\\f0\\froman Times New Roman;}}',
                '\\f0\\fs24', lines, '}'
            ), collapse='\n'), '\n'),
            author_year_references=TRUE
        ),
        tex=list(
            escape=.cdrgam_cli_methods_tex_escape,
            emphasis=function(text) paste0('\\emph{', text, '}'),
            cite=function(part, references) paste0(
                if (identical(part$type, 'narrative')) '\\citet{' else '\\citep{',
                paste(part$keys, collapse=','), '}'),
            heading=function(level, text) c('', paste0('\\',
                c('section', 'subsection', 'subsubsection')[[level]], '*{', text, '}')),
            paragraph=function(text) c('', text),
            # BibTeX formats the list from methods.bib.
            references=function(entries) character(),
            wrap=function(lines) paste0(paste(c(
                '% Methods section written by cdrgam methods. Cite with natbib and',
                '% the references in methods.bib.', lines[-1L]
            ), collapse='\n'), '\n'),
            author_year_references=FALSE
        )
    )
}

.cdrgam_cli_methods_formats <- names(.cdrgam_cli_methods_writers())

.cdrgam_cli_methods_write_report <- function(document, format) {
    writer <- .cdrgam_cli_methods_writers()[[format]]
    paragraph_text <- function(paragraph) {
        paste(vapply(.cdrgam_cli_methods_split_citations(paragraph), function(part) {
            if (identical(part$type, 'text')) writer$escape(part$text) else {
                writer$cite(part, document$references)
            }
        }, character(1)), collapse='')
    }
    lines <- writer$heading(1L, 'Methods')
    for (module in document$modules) {
        lines <- c(lines, writer$heading(2L, writer$escape(module$title)))
        for (section in module$sections) {
            # A section that repeats its module's title adds no heading.
            if (!identical(section$heading, module$title)) {
                lines <- c(lines, writer$heading(3L, writer$escape(section$heading)))
            }
            for (paragraph in section$paragraphs) {
                lines <- c(lines, writer$paragraph(paragraph_text(paragraph)))
            }
        }
    }
    if (length(document$modules)) lines <- c(lines, writer$paragraph(writer$escape(paste(
        'Analyses used R and the cdrgam and cdrgam.cli packages; their versions',
        'are listed in supplementary_software.tsv.'
    ))))
    if (!length(document$modules)) lines <- c(lines, writer$paragraph(writer$escape(
        'No section templates describe the selected work items.'
    )))
    if (length(document$references) && isTRUE(writer$author_year_references)) {
        entries <- vapply(document$references, .cdrgam_cli_methods_reference_entry,
            character(1), emphasis=writer$emphasis, escape=writer$escape)
        # Sort on the unescaped text so markup does not change the order.
        plain <- vapply(document$references, .cdrgam_cli_methods_reference_entry,
            character(1))
        lines <- c(lines, writer$references(entries[order(plain, method='radix')]))
    }
    writer$wrap(lines)
}

.cdrgam_cli_methods_write_markdown <- function(document) {
    .cdrgam_cli_methods_write_report(document, 'md')
}

# --- Software -----------------------------------------------------------------

# Versions recorded by the included artifacts, then the installation that
# wrote the report. Artifacts do not record mgcv, so it appears only for the
# report.
.cdrgam_cli_methods_software_table <- function(records) {
    included <- Filter(function(record) {
        isTRUE(record$included) && !is.null(record$manifest$execution)
    }, records)
    recorded <- function(field) vapply(included, function(record) {
        execution <- record$manifest$execution
        value <- switch(field, R=execution$R_version, cdrgam=execution$cdrgam$version,
            cdrgam.cli=execution$cdrgam_cli$version)
        .cdrgam_cli_null(value, NA_character_)
    }, character(1))
    rows <- lapply(c('R', 'cdrgam', 'cdrgam.cli'), function(software) {
        versions <- recorded(software)
        versions <- sort(unique(versions[!is.na(versions)]), method='radix')
        if (!length(versions)) return(NULL)
        counts <- vapply(versions, function(version) sum(recorded(software) == version,
            na.rm=TRUE), integer(1))
        data.frame(software=software, version=versions,
            source=paste0('artifacts (', counts, ')'), stringsAsFactors=FALSE)
    })
    report <- data.frame(
        software=c('R', 'cdrgam', 'cdrgam.cli', 'mgcv'),
        version=c(as.character(getRversion()), vapply(c('cdrgam', 'cdrgam.cli', 'mgcv'),
            .cdrgam_cli_package_version, character(1))),
        source='report', stringsAsFactors=FALSE
    )
    out <- do.call(rbind, c(Filter(Negate(is.null), rows), list(report)))
    rownames(out) <- NULL
    out
}

# --- Trace -------------------------------------------------------------------

.cdrgam_cli_methods_trace <- function(records, document, scope, format) {
    item_rows <- lapply(records, function(record) list(
        project=record$project, kind=record$kind, name=record$name,
        identity=record$identity, state=record$state, included=record$included
    ))
    omitted <- Filter(function(record) !isTRUE(record$included), records)
    states <- vapply(omitted, `[[`, character(1), 'state')
    list(
        schema=.cdrgam_cli_methods_json_schema,
        format=format,
        scope=scope,
        items=item_rows,
        omitted=as.list(table(factor(states, levels=c('stale', 'missing')))),
        undescribed=as.list(document$undescribed),
        tables=as.list(names(document$tables)),
        sections=unlist(lapply(document$modules, function(module) {
            lapply(module$sections, function(section) list(
                kind=module$kind, heading=section$heading,
                template=section$template, context=section$fields,
                citations=as.list(section$citations)
            ))
        }), recursive=FALSE),
        reporting_installation=list(
            R=as.character(getRversion()),
            cdrgam=.cdrgam_cli_package_version('cdrgam'),
            cdrgam_cli=.cdrgam_cli_package_version('cdrgam.cli')
        )
    )
}

.cdrgam_cli_methods_write_json <- function(value) {
    paste0(jsonlite::toJSON(
        value, auto_unbox=TRUE, pretty=TRUE, null='null', digits=NA
    ), '\n')
}

.cdrgam_cli_methods_write_tsv_text <- function(table) {
    clean <- function(value) gsub('[\t\r\n]+', ' ', as.character(value))
    lines <- c(
        paste(clean(names(table)), collapse='\t'),
        if (nrow(table)) do.call(paste, c(lapply(table, clean), sep='\t'))
    )
    paste0(paste(lines, collapse='\n'), '\n')
}

.cdrgam_cli_methods_write_text <- function(path, text) {
    .cdrgam_cli_atomic_write(path, function(temporary) {
        connection <- file(temporary, open='wb')
        on.exit(close(connection), add=TRUE)
        writeBin(charToRaw(enc2utf8(text)), connection)
    })
}

#' Write a methods section
#'
#' Describes the selected work items and their upstream lineage as a methods
#' section with citations, and writes a trace of the inputs behind it.
#'
#' Without `preview`, only work items whose current resolved identity has a
#' complete artifact contribute. Stale and missing work items are omitted,
#' counted on stderr, and listed in `methods.json`. With `preview = TRUE`,
#' the report describes resolved definitions alone and reads no artifacts.
#' Both modes resolve dataset identities, so dataset sources must be readable.
#'
#' @param projects,models,predictions,visualizations,comparisons,tests
#'   Selectors with the same meaning as in [cdrgam_cli_plan()]. Several
#'   projects are pooled into one report.
#' @param preview Describe resolved definitions without reading artifacts.
#' @param format Output format: `"rtf"` (the default), `"md"`, or `"tex"`.
#'   The tex format writes LaTeX with natbib citations and a `methods.bib`
#'   file of the cited references.
#' @param output Output directory. Defaults to `results/methods` in the
#'   selected project and is required when several projects are selected.
#'   Existing files of the same names are replaced.
#' @param cdrgam_root Configured CDR-GAM root.
#' @return The paths of the written files, invisibly.
#' @export
cdrgam_cli_methods <- function(
        projects=NULL, models=NULL, predictions=NULL, visualizations=NULL,
        comparisons=NULL, tests=NULL, preview=FALSE, format='rtf', output=NULL,
        cdrgam_root=NULL
) {
    format <- .cdrgam_cli_scalar_character(format, 'format')
    if (!(format %in% .cdrgam_cli_methods_formats)) .cdrgam_cli_abort(paste0(
        'Unsupported methods format ', sQuote(format), '; available: ',
        paste(.cdrgam_cli_methods_formats, collapse=', ')
    ))
    graph <- .cdrgam_cli_combined_graph(
        projects, models, predictions, visualizations, comparisons, cdrgam_root,
        tests=tests
    )
    selected <- names(graph$definitions)
    if (is.null(output)) {
        if (length(selected) != 1L) .cdrgam_cli_abort(paste(
            'Give an output directory with -o when several projects are selected'
        ))
        output <- .cdrgam_cli_path(graph$definitions[[1L]], 'methods')
    }
    records <- .cdrgam_cli_methods_collect(graph, preview=isTRUE(preview))
    omitted <- Filter(function(record) !isTRUE(record$included), records)
    if (length(omitted)) {
        states <- table(vapply(omitted, `[[`, character(1), 'state'))
        message(
            'Omitted ', length(omitted), ' work item',
            if (length(omitted) == 1L) '' else 's', ' without a complete artifact (',
            paste(paste(as.integer(states), names(states)), collapse=', '),
            '); see methods.json'
        )
    }
    if (!any(vapply(records, function(record) {
            isTRUE(record$included) && !identical(record$kind, 'dataset')
        }, logical(1)))) {
        .cdrgam_cli_abort(paste(
            'No selected work item has a complete artifact; run the selection',
            'or use --preview'
        ))
    }
    document <- .cdrgam_cli_methods_document(records, preview=isTRUE(preview))
    if (length(document$undescribed)) message(
        'No methods templates describe these artifact kinds yet: ',
        paste(document$undescribed, collapse=', ')
    )
    scope <- list(
        mode=if (isTRUE(preview)) 'preview' else 'realized',
        projects=as.list(selected),
        selectors=Filter(length, list(
            models=as.list(models), predictions=as.list(predictions),
            visualizations=as.list(visualizations),
            comparisons=as.list(comparisons), tests=as.list(tests)
        ))
    )
    document$tables[['supplementary_software.tsv']] <-
        .cdrgam_cli_methods_software_table(records)
    paths <- c(
        report=file.path(output, paste0('methods.', format)),
        if (identical(format, 'tex')) c(bibliography=file.path(output, 'methods.bib')),
        trace=file.path(output, 'methods.json'),
        stats::setNames(file.path(output, names(document$tables)), names(document$tables))
    )
    for (name in names(document$tables)) {
        .cdrgam_cli_methods_write_text(
            paths[[name]], .cdrgam_cli_methods_write_tsv_text(document$tables[[name]])
        )
    }
    .cdrgam_cli_methods_write_text(
        paths[['report']], .cdrgam_cli_methods_write_report(document, format)
    )
    if (identical(format, 'tex')) .cdrgam_cli_methods_write_text(
        paths[['bibliography']], .cdrgam_cli_methods_write_bib(document$references)
    )
    .cdrgam_cli_methods_write_text(paths[['trace']], .cdrgam_cli_methods_write_json(
        .cdrgam_cli_methods_trace(records, document, scope, format)
    ))
    message('Wrote ', .cdrgam_cli_methods_join(c(paths[['report']], basename(paths[-1L]))))
    invisible(paths)
}
