---
section: Predictor scaling
context_version: 1
requires: [rescale_present, rescale_varies, center_present, center_varies]
---
{{#rescale_present}}
Continuous predictors and lag coordinates were divided by their standard
deviations in the training data before fitting, and effects are reported in
the original units.
{{/rescale_present}}
{{^rescale_present}}{{^rescale_varies}}
Predictors and lags were fitted in their original units.
{{/rescale_varies}}{{/rescale_present}}
{{#center_present}}
Predictors that entered IRFs linearly, including binary predictors, were
centered at their training means, so an intercept IRF describes the response
to an impulse with average predictor values.
{{/center_present}}
{{^center_present}}{{^center_varies}}
Predictors that entered IRFs linearly were not centered.
{{/center_varies}}{{/center_present}}
