# Reads the curated BibTeX file shipped in inst/methods and formats author-year
# citations. The reader accepts the subset of BibTeX that file uses: one
# @type{key, field = {value}, ...} entry per record, with braced or quoted
# values and no string macros or concatenation.

.cdrgam_cli_methods_bibliography_path <- function() {
    system.file('methods', 'references.bib', package='cdrgam.cli')
}

.cdrgam_cli_methods_bib_value <- function(text, start, where) {
    opening <- substr(text, start, start)
    if (identical(opening, '"')) {
        end <- regexpr('"', substring(text, start + 1L), fixed=TRUE)
        if (end < 0L) .cdrgam_cli_abort(paste0(where, ' has an unterminated quoted value'))
        return(list(value=substr(text, start + 1L, start + end - 1L), end=start + end))
    }
    if (!identical(opening, '{')) {
        .cdrgam_cli_abort(paste0(where, ' values must be braced or quoted'))
    }
    depth <- 0L
    characters <- strsplit(substring(text, start), '')[[1L]]
    for (index in seq_along(characters)) {
        if (characters[[index]] == '{') depth <- depth + 1L
        if (characters[[index]] == '}') depth <- depth - 1L
        if (depth == 0L) return(list(
            value=substr(text, start + 1L, start + index - 2L), end=start + index - 1L
        ))
    }
    .cdrgam_cli_abort(paste0(where, ' has an unbalanced braced value'))
}

.cdrgam_cli_methods_read_bibliography <- function(path=.cdrgam_cli_methods_bibliography_path()) {
    text <- paste(readLines(path, warn=FALSE, encoding='UTF-8'), collapse='\n')
    starts <- gregexpr('@([A-Za-z]+)\\s*\\{\\s*([A-Za-z0-9_:-]+)\\s*,', text, perl=TRUE)[[1L]]
    if (starts[[1L]] == -1L) return(list())
    capture_starts <- attr(starts, 'capture.start')
    capture_lengths <- attr(starts, 'capture.length')
    entries <- list()
    for (index in seq_along(starts)) {
        type <- tolower(substr(text, capture_starts[index, 1L],
            capture_starts[index, 1L] + capture_lengths[index, 1L] - 1L))
        key <- substr(text, capture_starts[index, 2L],
            capture_starts[index, 2L] + capture_lengths[index, 2L] - 1L)
        where <- paste0('Reference ', key)
        if (!is.null(entries[[key]])) .cdrgam_cli_abort(paste0(where, ' is duplicated'))
        position <- starts[[index]] + attr(starts, 'match.length')[[index]]
        fields <- list(type=type, key=key)
        repeat {
            rest <- substring(text, position)
            field <- regexpr('^\\s*([A-Za-z]+)\\s*=\\s*', rest, perl=TRUE)
            if (field < 0L) break
            name <- tolower(substr(rest, attr(field, 'capture.start')[[1L]],
                attr(field, 'capture.start')[[1L]] + attr(field, 'capture.length')[[1L]] - 1L))
            value <- .cdrgam_cli_methods_bib_value(
                text, position + attr(field, 'match.length'), where
            )
            fields[[name]] <- gsub('\\s+', ' ', trimws(value$value), perl=TRUE)
            position <- value$end + 1L
            separator <- regexpr('^\\s*,', substring(text, position), perl=TRUE)
            if (separator < 0L) break
            position <- position + attr(separator, 'match.length')
        }
        if (!grepl('^\\s*\\}', substring(text, position), perl=TRUE)) {
            .cdrgam_cli_abort(paste0(where, ' is not terminated by }'))
        }
        for (required in c('author', 'title', 'year')) if (is.null(fields[[required]])) {
            .cdrgam_cli_abort(paste0(where, ' lacks ', required))
        }
        fields$authors <- .cdrgam_cli_methods_bib_authors(fields$author)
        entries[[key]] <- fields
    }
    entries
}

.cdrgam_cli_methods_bib_accents <- c(
    '\\"a'='\u00e4', '\\"o'='\u00f6', '\\"u'='\u00fc', '\\"A'='\u00c4', '\\"O'='\u00d6',
    '\\"U'='\u00dc', "\\'e"='\u00e9', "\\'a"='\u00e1', '\\`e'='\u00e8', '\\c{c}'='\u00e7'
)

.cdrgam_cli_methods_bib_plain <- function(text) {
    for (accent in names(.cdrgam_cli_methods_bib_accents)) {
        text <- gsub(accent, .cdrgam_cli_methods_bib_accents[[accent]], text, fixed=TRUE)
    }
    text <- gsub('\\&', '&', gsub('--', '\u2013', text, fixed=TRUE), fixed=TRUE)
    if (grepl('\\', text, fixed=TRUE)) .cdrgam_cli_abort(paste0(
        'Reference text contains an unsupported TeX command: ', text
    ))
    gsub('[{}]', '', text)
}

# Names are written "Family, Given" and separated by " and ".
.cdrgam_cli_methods_bib_authors <- function(text) {
    lapply(strsplit(text, '\\s+and\\s+', perl=TRUE)[[1L]], function(name) {
        parts <- trimws(strsplit(name, ',', fixed=TRUE)[[1L]])
        if (length(parts) != 2L) .cdrgam_cli_abort(paste0(
            'Reference author ', sQuote(name), ' must be written Family, Given'
        ))
        given <- strsplit(.cdrgam_cli_methods_bib_plain(parts[[2L]]), '\\s+', perl=TRUE)[[1L]]
        list(
            family=.cdrgam_cli_methods_bib_plain(parts[[1L]]),
            initials=paste(paste0(substr(given, 1L, 1L), '.'), collapse=' ')
        )
    })
}

.cdrgam_cli_methods_author_label <- function(entry, conjunction) {
    families <- vapply(entry$authors, `[[`, character(1), 'family')
    if (length(families) == 1L) return(families)
    if (length(families) == 2L) return(paste(families[[1L]], conjunction, families[[2L]]))
    paste(families[[1L]], 'et al.')
}

# Returns the cited entries with author (parenthetical form), narrative_author,
# and year fields. Entries that share an author label and year get a, b, ...
# suffixes in title order, as in APA style.
.cdrgam_cli_methods_cited_references <- function(bibliography, keys) {
    missing <- setdiff(keys, names(bibliography))
    if (length(missing)) .cdrgam_cli_abort(paste0(
        'Methods text cites unknown references: ', paste(sort(missing), collapse=', ')
    ))
    entries <- bibliography[keys]
    labels <- vapply(entries, .cdrgam_cli_methods_author_label, character(1),
        conjunction='&')
    groups <- paste(labels, vapply(entries, `[[`, character(1), 'year'))
    for (group in unique(groups[duplicated(groups)])) {
        members <- which(groups == group)
        members <- members[order(
            vapply(entries[members], `[[`, character(1), 'title'), method='radix'
        )]
        for (index in seq_along(members)) {
            entries[[members[[index]]]]$suffix <- letters[[index]]
        }
    }
    lapply(entries, function(entry) {
        entry$year_label <- paste0(entry$year, .cdrgam_cli_null(entry$suffix, ''))
        entry$author_label <- .cdrgam_cli_methods_author_label(entry, '&')
        entry$narrative_label <- .cdrgam_cli_methods_author_label(entry, 'and')
        entry
    })
}

.cdrgam_cli_methods_reference_authors <- function(entry) {
    names <- vapply(entry$authors, function(author) {
        paste0(author$family, ', ', author$initials)
    }, character(1))
    if (length(names) == 1L) return(names)
    paste0(paste(names[-length(names)], collapse=', '), ', & ', names[[length(names)]])
}

# Formats one reference-list entry. emphasis wraps text that the format sets in
# italics; the writer escapes the plain parts with escape.
.cdrgam_cli_methods_reference_entry <- function(
        entry, emphasis=function(text) text, escape=function(text) text
) {
    plain <- function(field) escape(.cdrgam_cli_methods_bib_plain(entry[[field]]))
    title <- plain('title')
    if (!grepl('[.?!]$', title)) title <- paste0(title, '.')
    parts <- c(
        paste0(escape(.cdrgam_cli_methods_reference_authors(entry)), ' (',
            entry$year_label, ').'),
        if (identical(entry$type, 'book')) {
            paste0(emphasis(sub('[.]$', '', title)),
                if (!is.null(entry$edition)) paste0(' (', plain('edition'), ' ed.)') else '',
                '.')
        } else title
    )
    if (identical(entry$type, 'article')) {
        venue <- emphasis(plain('journal'))
        if (!is.null(entry$volume)) venue <- paste0(venue, ', ', emphasis(plain('volume')))
        if (!is.null(entry$number)) venue <- paste0(venue, '(', plain('number'), ')')
        if (!is.null(entry$pages)) venue <- paste0(venue, ', ', plain('pages'))
        parts <- c(parts, paste0(venue, '.'))
    }
    if (!is.null(entry$publisher)) parts <- c(parts, paste0(plain('publisher'), '.'))
    if (!is.null(entry$doi)) parts <- c(parts, paste0('https://doi.org/', entry$doi))
    paste(parts, collapse=' ')
}
