# Tidymodels recipe step for multicollinearity filtering

Adds a step to a recipe created by `recipes::recipe()]+` to apply
multicollinearity filtering via
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md).

This function requires the `recipes` package to be installed.

Unlike
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md),
this wrapper does not perform target encoding, and the default value for
the argument `quiet` is `TRUE`.

## Usage

``` r
step_collinear(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  options = list(),
  selected = NULL,
  skip = FALSE,
  keep_original_cols = FALSE,
  id = recipes::rand_id("collinear")
)

# S3 method for class 'step_collinear'
prep(x = NULL, training = NULL, info = NULL, ...)

# S3 method for class 'step_collinear'
bake(object = NULL, new_data = NULL, ...)
```

## Arguments

- recipe:

  (required, recipe) A recipe object to which this step will be added.

- ...:

  (optional) Additional arguments (currently ignored).

- role:

  (optional, character) Not used by this step since no new variables are
  created. Default: NA

- trained:

  (optional, logical) Indicates if the step has been trained. Default:
  FALSE

- options:

  (optional, list) Named list of arguments passed to
  [`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md).
  Common options include:

  - `max_cor`: Maximum correlation threshold. If NULL (default),
    automatically set based on median correlation of predictors.

  - `max_vif`: Maximum VIF threshold. If NULL (default), automatically
    set to match the auto-computed max_cor.

  - `preference_order`: Vector of predictor names in priority order.

  - `f`: Function to compute preference order (default: `f_auto`)

  - `quiet`: Suppress messages (default: TRUE)

  Note: `encoding_method` is not supported in this step. The automatic
  threshold selection adapts to each dataset's correlation structure.

- selected:

  (character vector) Predictors retained after filtering. Populated
  during training and used during baking. Default: NULL

- skip:

  (optional, logical) Trigger to skip this step when baking. Default:
  FALSE.

- keep_original_cols:

  (optional, logical) Whether to keep original columns. Default: FALSE.

- id:

  (optional, character) Unique identifier for this step.

- x:

  (required, step_collinear object) The step to be trained. Default:
  NULL

- training:

  (required, data.frame) The training dataset used to estimate
  quantities.

- info:

  (optional, data.frame) Preprocessed information about variables in
  `training`. Default: NULL

- object:

  (required, `step_collinear` object) The trained step. Default: NULL

- new_data:

  (required, data.frame) New data to apply the step to. Default: NULL

## Value

Updated recipe with new step.

## See also

Other multicollinearity_filtering:
[`collinear()`](https://blasbenito.github.io/collinear/reference/collinear.md),
[`collinear_select()`](https://blasbenito.github.io/collinear/reference/collinear_select.md),
[`cor_select()`](https://blasbenito.github.io/collinear/reference/cor_select.md),
[`vif_select()`](https://blasbenito.github.io/collinear/reference/vif_select.md)

## Examples

``` r
if (FALSE) { # \dontrun{
if(requireNamespace("recipes", quietly = TRUE) &&
    requireNamespace("parsnip", quietly = TRUE) &&
    requireNamespace("workflows", quietly = TRUE)
    ){

data(
  vi_smol,
  vi_predictors_numeric
  )

# model formula
vi_formula <- collinear::model_formula(
  df = vi_smol,
  response = "vi_numeric",
  predictors = vi_predictors_numeric
)

# recipe
vi_recipe <- recipes::recipe(
  formula = vi_formula,
  data = vi_smol
  ) |>
  #multicollinearity filtering
  collinear::step_collinear(
    recipes::all_predictors(),
    options = list(
      max_cor = 0.7,
      max_vif = 5,
      f = collinear::f_numeric_glm
      )
  ) |>
  #normalization
  recipes::step_normalize(
    recipes::all_predictors()
    )

# define linear regression model
vi_model <- parsnip::linear_reg() |>
  parsnip::set_engine("lm")

# create and fit workflow
vi_workflow <- workflows::workflow() |>
  workflows::add_recipe(vi_recipe) |>
  workflows::add_model(vi_model) |>
  workflows::fit(data = vi_smol)

vi_workflow

}
} # }
```
