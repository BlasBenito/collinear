# Convert categorical predictors to numeric via target encoding

Target encoding maps the values of categorical variables (of class
`character` or `factor`) to numeric using another numeric variable as
reference. The encoding methods implemented here are:

- "mean" (implemented in
  [`target_encoding_mean()`](https://blasbenito.github.io/collinear/reference/target_encoding_methods.md)):
  Maps each category to the average of reference numeric variable across
  the category cases. Variables encoded with this method are identified
  with the suffix "\_\_encoded_mean". It has a method to control
  overfitting implemented via the argument `smoothing`. The integer
  value of this argument indicates a threshold in number of rows.
  Categories sized above this threshold are encoded with the group mean,
  while groups below it are encoded with a weighted mean of the group's
  mean and the global mean. This method is named "mean smoothing" in the
  relevant literature.

- "rank" (implemented in
  [`target_encoding_rank()`](https://blasbenito.github.io/collinear/reference/target_encoding_methods.md)):
  Returns the rank of the group as a integer, being 1 he group with the
  lower mean of the reference variable. Variables encoded with this
  method are identified with the suffix "\_\_encoded_rank".

- "loo" (implemented in
  [`target_encoding_loo()`](https://blasbenito.github.io/collinear/reference/target_encoding_methods.md)):
  Known as the "leave-one-out method" in the literature, it encodes each
  categorical value with the mean of the response variable across all
  other group cases. This method controls overfitting better than
  "mean". Variables encoded with this method are identified with the
  suffix "\_\_encoded_loo".

Accepts a parallelization setup via
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
and a progress bar via
[`progressr::handlers()`](https://progressr.futureverse.org/reference/handlers.html)
(see examples).

## Usage

``` r
target_encoding_lab(
  df = NULL,
  response = NULL,
  predictors = NULL,
  encoding_method = "loo",
  smoothing = 0,
  overwrite = FALSE,
  quiet = FALSE,
  ...
)
```

## Arguments

- df:

  (required; dataframe, tibble, or sf) A dataframe with responses
  (optional) and predictors. Must have at least 10 rows for pairwise
  correlation analysis, and `10 * (length(predictors) - 1)` for VIF.
  Default: NULL.

- response:

  (optional, character string) Name of a numeric response variable in
  `df`. Default: NULL.

- predictors:

  (optional; character vector or NULL) Names of the predictors in `df`.
  If NULL, all columns except `responses` and
  constant/near-zero-variance columns are used. Default: NULL.

- encoding_method:

  (optional; character vector or NULL). Name of the target encoding
  methods. One or several of: "mean", "rank", "loo". If NULL, target
  encoding is ignored, and `df` is returned with no modification.
  Default: "loo"

- smoothing:

  (optional; integer vector) Argument of the method "mean". Groups
  smaller than this number have their means pulled towards the mean of
  the response across all cases. Default: 0

- overwrite:

  (optional; logical) If TRUE, the original predictors in `df` are
  overwritten with their encoded versions, but only one encoding method,
  smoothing, white noise, and seed are allowed. Otherwise, encoded
  predictors with their descriptive names are added to `df`. Default:
  FALSE

- quiet:

  (optional; logical) If FALSE, messages are printed. Default: FALSE.

- ...:

  (optional) Internal args (e.g. `function_name` for
  [`validate_arg_function_name`](https://blasbenito.github.io/collinear/reference/validate_arg_function_name.md),
  a precomputed correlation matrix `m`, or cross-validation args for
  [`preference_order`](https://blasbenito.github.io/collinear/reference/preference_order.md)).

## Value

dataframe

## References

- Micci-Barreca, D. (2001) A Preprocessing Scheme for High-Cardinality
  Categorical Attributes in Classification and Prediction Problems.
  SIGKDD Explor. Newsl. 3, 1, 27-32. doi: 10.1145/507533.507538

## See also

Other target_encoding:
[`target_encoding_loo()`](https://blasbenito.github.io/collinear/reference/target_encoding_methods.md)

## Author

Blas M. Benito, PhD

## Examples

``` r
data(vi_smol)

#applying all methods for a continuous response
df <- target_encoding_lab(
  df = vi_smol,
  response = "vi_numeric",
  predictors = "koppen_zone",
  encoding_method = c(
    "mean",
    "loo",
    "rank"
  )
)
#> 
#> collinear::target_encoding_lab()
#> └── collinear::validate_arg_df(): converted the following character columns to factor:
#>  - koppen_zone
#> 
#> collinear::target_encoding_lab(): using response 'vi_numeric' to encode these categorical predictors:
#>  - koppen_zone

#identify encoded predictors
predictors.encoded <- grep(
  pattern = "*__encoded*",
  x = colnames(df),
  value = TRUE
)

head(df[, predictors.encoded])
#>       koppen_zone__encoded_mean koppen_zone__encoded_loo
#> 17401                 0.3233333                0.3231250
#> 24388                 0.5233028                0.5225926
#> 4775                  0.6555814                0.6566667
#> 26753                 0.5233028                0.5237037
#> 13218                 0.6676000                0.6633333
#> 26109                 0.5233028                0.5225000
#>       koppen_zone__encoded_rank
#> 17401                         7
#> 24388                         7
#> 4775                          7
#> 26753                         7
#> 13218                         7
#> 26109                         7

```
