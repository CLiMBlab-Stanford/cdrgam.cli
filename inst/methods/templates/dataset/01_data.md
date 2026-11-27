---
section: Data
context_version: 1
requires: [several, datasets, series, series_present, filters, filters_present, preprocess, preprocess_present, interactions, interactions_present, factors, factors_present, differences, differences_present]
---
Each dataset pairs a table of impulses, the timed events whose predictors
enter IRFs, with a table of responses.{{#several}} This report uses
{{datasets}}.{{/several}}{{#series_present}} Impulses contributed only to
responses in the same series, defined by {{series}}.{{/series_present}}
{{#filters_present}}Responses were retained only when {{filters}}; impulses
were not filtered.{{/filters_present}}
{{#preprocess_present}}The filtered impulses and responses were then
transformed by {{preprocess}}.{{/preprocess_present}}
{{#interactions_present}}{{interactions}}{{/interactions_present}}
{{#factors_present}}The columns {{factors}} were treated as
factors.{{/factors_present}}

{{#differences_present}}
The datasets differed in their {{differences}}; the values for each dataset are
listed in supplementary_datasets.tsv.
{{/differences_present}}
