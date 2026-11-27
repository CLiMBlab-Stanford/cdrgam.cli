---
section: Estimation
context_version: 1
requires: [estimator, estimator_present, family_link, family_link_present, method, method_present, rank, rank_present]
---
{{#estimator_present}}Models were estimated {{estimator}}.{{/estimator_present}}
{{#family_link_present}}The response was modeled with {{family_link}}.{{/family_link_present}}
{{#method_present}}Smoothing parameters were selected by {{method}}.{{/method_present}}
{{#rank_present}}Rank-deficient designs were resolved {{rank}}.{{/rank_present}}
