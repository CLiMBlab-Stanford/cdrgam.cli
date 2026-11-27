---
section: Model framework
context_version: 1
requires: [several, models, differences, differences_present]
---
Responses were analyzed with CDR-GAM, an unpublished extension of
continuous-time deconvolutional regression [@shain2021cdr] that estimates its
components as generalized additive models, implemented in the cdrgam
package. Each impulse response function (IRF) term contributes, at each
response, the sum over preceding impulses within its lag window of a smooth
function of the lag between impulse and response. When a predictor enters an
IRF linearly, that function is scaled by the predictor's value at the impulse.
Smooth functions were represented by penalized regression splines
[@wood2017generalized].{{#several}} This report describes {{models}}.{{/several}}
Model formulas are given verbatim in supplementary_models.tsv.

{{#differences_present}}
The models differed in their {{differences}}; the values for each model are
listed in supplementary_models.tsv.
{{/differences_present}}
