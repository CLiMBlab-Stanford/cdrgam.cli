---
section: Model comparison
context_version: 1
requires: [several, comparisons, metrics, metrics_present, evaluation, evaluation_present, differences, differences_present]
---
{{#metrics_present}}Models were compared by the {{metrics}} of their residuals,
averaged over the responses with defined residuals{{#evaluation_present}} in
{{evaluation}}{{/evaluation_present}}.{{/metrics_present}}{{^metrics_present}}Models
were compared by the accuracy of their predictions.{{/metrics_present}}
{{#several}}This report includes {{comparisons}}, listed in
supplementary_comparisons.tsv.{{/several}}

{{#differences_present}}
The comparisons differed in their {{differences}}.
{{/differences_present}}
