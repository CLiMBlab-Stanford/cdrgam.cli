---
section: Figures
context_version: 1
requires: [effect_figures_present, intervals, intervals_present, unconditional_present, any_simultaneous, booklets_present, differences, differences_present]
---
{{#effect_figures_present}}{{#intervals_present}}Figures of fitted effects show
{{intervals}}{{#unconditional_present}}, computed from coefficient
covariances corrected for smoothing-parameter uncertainty
[@wood2016smoothing]{{/unconditional_present}}.{{/intervals_present}}{{#differences_present}}
Figures of fitted effects differed in their {{differences}}, as listed in
supplementary_visualizations.tsv.{{/differences_present}}{{/effect_figures_present}}
{{#any_simultaneous}}A simultaneous interval uses, as its critical value, the
corresponding quantile of the maximum absolute standardized deviation over the
evaluation points of its curve, estimated from draws from the Gaussian
approximation to the coefficient posterior.{{/any_simultaneous}}
{{#booklets_present}}Default plot booklets of fitted models were produced with
cdrgam.{{/booklets_present}}
