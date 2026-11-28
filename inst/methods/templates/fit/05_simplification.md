---
section: Automatic simplification
context_version: 1
when: autosimplify_used
requires: [autosimplify_used, autosimplify, autosimplify_present, simplified, simplified_present, unsimplified_present]
---
A fit that did not converge was diagnosed and simplified automatically. The
candidate edits were ranked by their convergence evidence and discounted by
the estimated degrees of freedom each would remove; the selected edit was
applied and the model refitted from a fresh optimizer state, until a fit
converged.{{#autosimplify_present}} Simplification was limited to
{{autosimplify}}.{{/autosimplify_present}}
{{#simplified_present}}Automatic simplification {{simplified}}.{{/simplified_present}}
{{#unsimplified_present}}No model required simplification.{{/unsimplified_present}}
