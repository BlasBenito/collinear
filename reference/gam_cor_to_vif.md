# GAM describing the relationship between correlation and VIF thresholds

A fitted generalized additive model describing `max_vif` as a function
of `max_cor` in
[experiment_cor_vs_vif](https://blasbenito.github.io/collinear/reference/experiment_cor_vs_vif.md).

## Usage

``` r
data(gam_cor_to_vif)
```

## Format

A [`gam`](https://rdrr.io/pkg/mgcv/man/gam.html) object.

## Source

Generated internally from
[experiment_cor_vs_vif](https://blasbenito.github.io/collinear/reference/experiment_cor_vs_vif.md).

## Details

The model parameters (basis dimension `k` and weight exponent) were
selected via optimization, filtering for models in the top 90\\

The final model uses squared Jaccard similarity as weights to emphasize
cases with high agreement between
[`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md)
and
[`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md).

Model performance:

- Adjusted R-squared: 0.834

- Deviance explained: 83.4\\

- Effective degrees of freedom for smooth: ~6

## See also

Other experiments:
[`experiment_adaptive_thresholds`](https://blasbenito.github.io/collinear/reference/experiment_adaptive_thresholds.md),
[`experiment_cor_vs_vif`](https://blasbenito.github.io/collinear/reference/experiment_cor_vs_vif.md),
[`prediction_cor_to_vif`](https://blasbenito.github.io/collinear/reference/prediction_cor_to_vif.md)

## Examples

``` r
data(gam_cor_to_vif)
plot(gam_cor_to_vif, shade = TRUE)
```
