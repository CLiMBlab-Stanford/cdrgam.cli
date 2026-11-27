---
section: Basis functions
context_version: 1
requires: [window, window_present, lag_basis, lag_basis_present, time_basis, time_basis_present, predictor_basis, predictor_basis_present, predictor_basis_varies]
---
{{#window_present}}
Unless a term specified its own window, IRFs spanned lags from {{window}} in
the time units of the data.
{{/window_present}}
{{#lag_basis_present}}
Unless a term specified otherwise, the lag dimension of each IRF used
{{lag_basis}}.
{{/lag_basis_present}}
{{#time_basis_present}}
IRFs that varied with response time used {{time_basis}} for that dimension.
{{/time_basis_present}}
{{#predictor_basis_present}}
Unless a term specified otherwise, predictors entered IRFs nonlinearly through
{{predictor_basis}}.
{{/predictor_basis_present}}
{{^predictor_basis_present}}{{^predictor_basis_varies}}
Unless a term specified otherwise, predictors entered IRFs linearly.
{{/predictor_basis_varies}}{{/predictor_basis_present}}
