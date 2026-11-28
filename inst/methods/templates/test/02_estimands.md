---
section: Estimands of fitted effects
context_version: 2
when: has_estimands
requires: [has_estimands, has_linear, linear_forms, has_peak, peak_forms, draws, draws_present, has_wood, level, level_present, unconditional_present, null_value, null_value_present, own_null, differences, differences_present]
---
Summary statistics of fitted effects were estimated by evaluating each effect
on a grid and combining the effects of a test on one joint Gaussian
approximation to the posterior distribution of the model coefficients, so that
estimands sharing coefficients have the correct covariance.{{#has_linear}}
Linear estimands, namely {{linear_forms}}, received Wald inference, which is
exact under this approximation.{{/has_linear}}{{#has_peak}} Peak estimands,
namely {{peak_forms}}, were computed from {{#draws_present}}{{draws}}
{{/draws_present}}draws from that approximation: estimates are the statistic of
the estimated coefficients, intervals are quantiles of the draws, and p-values
are two-sided posterior tail areas beyond the null value.{{/has_peak}}{{#has_wood}}
Whole-function estimands tested that an effect is zero everywhere with the
statistic of @wood2013pvalues.{{/has_wood}}

{{#level_present}}Intervals have {{level}} coverage.{{/level_present}}
{{#unconditional_present}}Coefficient covariances were corrected for
smoothing-parameter uncertainty [@wood2016smoothing].{{/unconditional_present}}
{{#null_value_present}}Estimands were tested against a null value of
{{null_value}}{{#own_null}}, unless an estimand specified its own{{/own_null}}.{{/null_value_present}}
Simultaneous intervals and adjusted p-values cover the linear estimands of a
test as one family and its peak estimands as another. They use the
distribution of the maximum absolute standardized deviation over the family,
and the adjusted p-values are single-step max-modulus p-values. Effect
definitions are listed in supplementary_tests.tsv.

{{#differences_present}}
The tests differed in their {{differences}}.
{{/differences_present}}
