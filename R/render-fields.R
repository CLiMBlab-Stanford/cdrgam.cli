# The fields of a visualization's `render` mapping. Validation, the viewer's
# appearance controls, and the test that keeps the two in step all read this
# table, so a field added here is checked everywhere it must be handled.
#
# Each entry has:
#   kind        choice, mappings, palette, formats, positive, text, or flag
#               (true or false).
#   choices     for choice fields, the accepted values.
#   requires    for choice fields, the geometries that some values need,
#               as a named list from value to geometries.
#   geometries  geometries for which the field may be set at all.
#   default     a value, or a function of the partly validated render,
#               filled in when the field is absent. Fields without a
#               default stay absent, so adding a field leaves the identity
#               of existing renders unchanged.
#   omit        a value that is normalized to absence, for the same reason.
#   required    whether the field must be given.
#   viewer      how the viewer handles the field: `control` (an input in its
#               appearance panel), `derived` (set from the viewer's own state),
#               or `file` (rendered files only).
#   reason      for derived and file fields, why there is no control.
#   label       for control fields, the input's label.
#
# Entries are listed in the order of the allowed-field message. Defaults are
# filled in table order; field order is part of a render's identity, so new
# fields with defaults must come after formats.

.cdrgam_cli_line_geometries <- c('line', 'point', 'bar')

# The native ggplot2 themes a render may name, each drawn with base size 10.
# `paper` is bw and `slides` is minimal at base size 16.
.cdrgam_cli_ggplot_theme_names <- c(
    'grey', 'gray', 'bw', 'linedraw', 'light', 'dark', 'minimal', 'classic', 'void',
    'test'
)
.cdrgam_cli_surface_geometries <- c('raster', 'contour', 'raster-contour')

.cdrgam_cli_render_fields <- list(
    geometry=list(
        kind='choice', required=TRUE,
        choices=c(.cdrgam_cli_line_geometries, .cdrgam_cli_surface_geometries),
        viewer='control', label='Geometry'
    ),
    mappings=list(
        kind='mappings', required=TRUE, viewer='derived',
        reason='x is each line\'s horizontal axis and color is the line'
    ),
    interval=list(
        kind='choice', choices=c('ribbon', 'lines', 'errorbar', 'companion', 'none'),
        requires=list(
            ribbon='line', lines='line', errorbar=c('point', 'bar'),
            companion=.cdrgam_cli_surface_geometries
        ),
        # Surfaces keep the historical ribbon default, which they reject, so
        # that a surface must state its interval.
        default=function(render) {
            if (render$geometry %in% c('point', 'bar')) 'errorbar' else 'ribbon'
        },
        viewer='control', label='Intervals'
    ),
    theme=list(
        kind='choice', choices=c('paper', 'slides', .cdrgam_cli_ggplot_theme_names),
        default='paper', viewer='control', label='Theme'
    ),
    formats=list(
        kind='formats', choices=c('pdf', 'png', 'svg'), default='pdf',
        viewer='file', reason='the viewer exports SVG from its own toolbar'
    ),
    width=list(kind='positive', viewer='file', reason='sets the size of rendered files'),
    height=list(kind='positive', viewer='file', reason='sets the size of rendered files'),
    dpi=list(kind='positive', viewer='file', reason='sets the resolution of rendered files'),
    title=list(kind='text', viewer='control', label='Title'),
    xlab=list(kind='text', viewer='control', label='Horizontal axis title'),
    ylab=list(kind='text', viewer='control', label='Vertical axis title'),
    palette=list(kind='palette', viewer='control', label='Palette'),
    scales=list(
        kind='choice', choices=c('fixed', 'free', 'free_x', 'free_y'),
        viewer='derived', reason='applies only to facets, which the viewer does not draw'
    ),
    peaks=list(
        kind='choice', choices=c('auto', 'always', 'never'),
        viewer='control', label='Peak timeline'
    ),
    spacing=list(
        kind='choice', choices=c('uniform', 'linear'),
        geometries=.cdrgam_cli_line_geometries,
        viewer='control', label='Tick spacing'
    ),
    statistic=list(
        kind='choice', choices=c('estimate', 'peak-lag'), omit='estimate',
        requires=list('peak-lag'=c('point', 'bar')),
        viewer='control', label='Statistic'
    ),
    y_zero=list(
        kind='flag', omit=FALSE, geometries=.cdrgam_cli_line_geometries,
        viewer='control', label='Include zero on the vertical axis'
    )
)

# "line", "point or bar", "line, point, or bar".
.cdrgam_cli_or_list <- function(values) {
    if (length(values) <= 2L) return(paste(values, collapse=' or '))
    paste0(paste(values[-length(values)], collapse=', '), ', or ', values[[length(values)]])
}

.cdrgam_cli_geometry_phrase <- function(geometries) {
    if (setequal(geometries, .cdrgam_cli_surface_geometries)) return('a surface geometry')
    paste(.cdrgam_cli_or_list(geometries), 'geometry')
}

.cdrgam_cli_validate_render_mappings <- function(value, field) {
    if (!is.list(value) || is.null(names(value))) {
        .cdrgam_cli_abort(paste0(field, ' must be a named mapping'))
    }
    value <- .cdrgam_cli_restore_yaml_keys(value, c('TRUE'='y'), field)
    .cdrgam_cli_check_keys(
        value, c('x', 'y', 'color', 'fill', 'linetype', 'group', 'facet'), field, 'x'
    )
    value <- lapply(value, .cdrgam_cli_scalar_character, field=paste0(field, ' value'))
    if (is.null(value$y)) .cdrgam_cli_abort(paste0(field, '.y is required'))
    value
}

.cdrgam_cli_validate_render_palette <- function(value, field) {
    palette <- unlist(value, use.names=FALSE)
    if (length(palette) <= 1L) return(.cdrgam_cli_palette_name(
        .cdrgam_cli_scalar_character(palette, field), field
    ))
    if (!is.character(palette) || anyNA(palette)) .cdrgam_cli_abort(paste0(
        field, ' must be a palette name or a list of colors'
    ))
    colors <- vapply(palette, .cdrgam_cli_color_hex, character(1), USE.NAMES=FALSE)
    if (anyNA(colors)) .cdrgam_cli_abort(paste0(
        field, ' has colors that are not hex (#RRGGBB), rgb(R, G, B), or R color names: ',
        paste(palette[is.na(colors)], collapse=', ')
    ))
    colors
}

# Validate and normalize one field's given value.
.cdrgam_cli_validate_render_field <- function(spec, value, field) {
    switch(
        spec$kind,
        choice={
            value <- .cdrgam_cli_scalar_character(value, field)
            if (!(value %in% spec$choices)) .cdrgam_cli_abort(paste0(
                field, ' must be one of ', paste(spec$choices, collapse=', ')
            ))
            value
        },
        mappings=.cdrgam_cli_validate_render_mappings(value, field),
        palette=.cdrgam_cli_validate_render_palette(value, field),
        formats={
            value <- unique(as.character(unlist(value, use.names=FALSE)))
            if (!length(value) || any(!(value %in% spec$choices))) .cdrgam_cli_abort(paste0(
                field, ' supports ', paste(spec$choices[-length(spec$choices)], collapse=', '),
                ', and ', spec$choices[[length(spec$choices)]]
            ))
            value
        },
        positive={
            if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value <= 0) {
                .cdrgam_cli_abort(paste0(field, ' must be positive'))
            }
            value
        },
        text=.cdrgam_cli_scalar_character(value, field),
        flag={
            if (!is.logical(value) || length(value) != 1L || is.na(value)) {
                .cdrgam_cli_abort(paste0(field, ' must be true or false'))
            }
            value
        }
    )
}

.cdrgam_cli_validate_render <- function(value, field) {
    if (!is.list(value)) .cdrgam_cli_abort(paste0(field, ' must be a mapping'))
    fields <- .cdrgam_cli_render_fields
    required <- names(fields)[vapply(fields, function(spec) isTRUE(spec$required), logical(1))]
    .cdrgam_cli_check_keys(value, names(fields), field, required)
    # Geometry first: other fields' defaults and constraints depend on it.
    value$geometry <- .cdrgam_cli_validate_render_field(
        fields$geometry, value$geometry, paste0(field, '.geometry')
    )
    for (name in setdiff(names(fields), 'geometry')) {
        spec <- fields[[name]]
        subfield <- paste0(field, '.', name)
        if (is.null(value[[name]])) {
            default <- spec$default
            if (is.function(default)) default <- default(value)
            if (is.null(default)) next
            value[[name]] <- default
        } else {
            value[[name]] <- .cdrgam_cli_validate_render_field(spec, value[[name]], subfield)
        }
        if (!is.null(spec$omit) && identical(value[[name]], spec$omit)) {
            value[[name]] <- NULL
            next
        }
        if (!is.null(spec$geometries) && !(value$geometry %in% spec$geometries)) {
            .cdrgam_cli_abort(paste0(
                subfield, ' requires ', .cdrgam_cli_geometry_phrase(spec$geometries)
            ))
        }
        needed <- if (is.character(value[[name]]) && length(value[[name]]) == 1L) {
            spec$requires[[value[[name]]]]
        } else NULL
        if (!is.null(needed) && !(value$geometry %in% needed)) .cdrgam_cli_abort(paste0(
            subfield, ' ', value[[name]], ' requires ', .cdrgam_cli_geometry_phrase(needed)
        ))
    }
    if (value$geometry %in% c('raster', 'raster-contour') && is.null(value$mappings$fill)) {
        .cdrgam_cli_abort(paste0(field, '.mappings.fill is required for raster geometry'))
    }
    value
}

# The interval values that suit `geometry`, for layer checks and viewer
# choices.
.cdrgam_cli_geometry_intervals <- function(geometry) {
    spec <- .cdrgam_cli_render_fields$interval
    Filter(function(choice) {
        needed <- spec$requires[[choice]]
        is.null(needed) || geometry %in% needed
    }, spec$choices)
}
