# Methods

## Data

Each dataset pairs a table of impulses, the timed events whose predictors enter IRFs, with a table of responses. This report uses 2 datasets.

The datasets differed in their response filters and preprocessing; the values for each dataset are listed in supplementary_datasets.tsv.

## Model specification and estimation

### Model framework

Responses were analyzed with CDR-GAM, an unpublished extension of continuous-time deconvolutional regression (Shain & Schuler, 2021) that estimates its components as generalized additive models, implemented in the cdrgam package. Each impulse response function (IRF) term contributes, at each response, the sum over preceding impulses within its lag window of a smooth function of the lag between impulse and response. When a predictor enters an IRF linearly, that function is scaled by the predictor's value at the impulse. Smooth functions were represented by penalized regression splines (Wood, 2017). This report describes 4 models. Model formulas are given verbatim in supplementary_models.tsv.

The models differed in their estimation software and lag basis; the values for each model are listed in supplementary_models.tsv.

### Basis functions

Unless a term specified its own window, IRFs spanned lags from 0 to 1 in the time units of the data. Unless a term specified otherwise, predictors entered IRFs linearly.

### Predictor scaling

Continuous predictors and lag coordinates were divided by their standard deviations in the training data before fitting, and effects are reported in the original units. Predictors that entered IRFs linearly, including binary predictors, were centered at their training means, so an intercept IRF describes the response to an impulse with average predictor values.

### Estimation

The response was modeled with a Gaussian distribution and an identity link. Smoothing parameters were selected by restricted maximum likelihood (Wood, 2011).

## Predictions

Fitted models generated 2 prediction sets for 1 dataset. Predictions are on the response scale, and residuals are the observed minus the predicted responses. The model and dataset of each prediction set are listed in supplementary_predictions.tsv.

## Model comparison

Models were compared by the mean squared error and mean absolute error of their residuals, averaged over the responses with defined residuals in the training dataset.

## Statistical tests

### Tests of model terms

Model terms were tested with the statistics that the summary of an mgcv model reports: Wald tests of parametric terms (Wood, 2017), tests of smooth terms based on a rank-truncated pseudo-inverse (Wood, 2013b), and tests of fully penalized terms, such as random effects (Wood, 2013a). Wald tests used an F reference distribution when the scale parameter was estimated.

### Estimands of fitted effects

Summary statistics of fitted effects were estimated by evaluating each effect on a grid and combining the effects of a test on one joint Gaussian approximation to the posterior distribution of the model coefficients, so that estimands sharing coefficients have the correct covariance. Linear estimands, namely trapezoidal areas under the effect over lag, received Wald inference, which is exact under this approximation. Peak estimands, namely peak lags, were computed from 500 draws from that approximation: estimates are the statistic of the estimated coefficients, intervals are quantiles of the draws, and p-values are two-sided posterior tail areas beyond the null value.

Intervals have 90% coverage. Coefficient covariances were corrected for smoothing-parameter uncertainty (Wood et al., 2016). Estimands were tested against a null value of 0, unless an estimand specified its own. Simultaneous intervals and adjusted p-values cover the linear estimands of a test as one family and its peak estimands as another. They use the distribution of the maximum absolute standardized deviation over the family, and the adjusted p-values are single-step max-modulus p-values. Effect definitions are listed in supplementary_tests.tsv.

## Figures

Figures of fitted effects show 90% simultaneous intervals, computed from coefficient covariances corrected for smoothing-parameter uncertainty (Wood et al., 2016). A simultaneous interval uses, as its critical value, the corresponding quantile of the maximum absolute standardized deviation over the evaluation points of its curve, estimated from draws from the Gaussian approximation to the coefficient posterior. Default plot booklets of fitted models were produced with cdrgam.

Analyses used R and the cdrgam and cdrgam.cli packages; their versions are listed in supplementary_software.tsv.

## References

- Shain, C., & Schuler, W. (2021). Continuous-time deconvolutional regression for psycholinguistic modeling. *Cognition*, *215*, 104735. https://doi.org/10.1016/j.cognition.2021.104735
- Wood, S. N. (2011). Fast stable restricted maximum likelihood and marginal likelihood estimation of semiparametric generalized linear models. *Journal of the Royal Statistical Society: Series B (Statistical Methodology)*, *73*(1), 3–36. https://doi.org/10.1111/j.1467-9868.2010.00749.x
- Wood, S. N. (2013a). A simple test for random effects in regression models. *Biometrika*, *100*(4), 1005–1010. https://doi.org/10.1093/biomet/ast038
- Wood, S. N. (2013b). On p-values for smooth components of an extended generalized additive model. *Biometrika*, *100*(1), 221–228. https://doi.org/10.1093/biomet/ass048
- Wood, S. N. (2017). *Generalized additive models: An introduction with R* (2nd ed.). Chapman and Hall/CRC. https://doi.org/10.1201/9781315370279
- Wood, S. N., Pya, N., & Säfken, B. (2016). Smoothing parameter and model selection for general smooth models. *Journal of the American Statistical Association*, *111*(516), 1548–1563. https://doi.org/10.1080/01621459.2016.1180986
