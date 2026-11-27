---
section: Tests of model terms
context_version: 2
when: has_terms
requires: [has_terms, term_counts, term_counts_present, term_methods]
---
Model terms were tested with the statistics that the summary of an mgcv model
reports: {{term_methods}}. Wald tests used an F reference distribution when
the scale parameter was estimated.{{#term_counts_present}} The tests covered
{{term_counts}}.{{/term_counts_present}}
