data(vi_smol, vi_predictors)

# model formula
vi_formula <- collinear::model_formula(
  df = vi_smol,
  response = "vi_numeric",
  predictors = vi_predictors
)

# recipe
vi_recipe <- recipes::recipe(
  formula = vi_formula,
  data = vi_smol
  ) |>
  #multicollinearity filtering
  collinear::step_target_encoding(
    recipes::all_predictors(),
    options = list(
      encoding_method = "loo"
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
