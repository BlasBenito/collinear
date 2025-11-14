#' @title Tidymodels Integration of [collinear()]
#' @description
#' Adds a step to a [recipes::recipe()] that removes collinear predictors using
#' [collinear::collinear()]. Unlike [collinear::collinear()], this wrapper does not perform target encoding.
#'
#' This step requires the **recipes** package to be installed.
#'
#' @param recipe (required, recipe) A recipe object to which this step will be added.
#' @param ... (optional, selector functions) Variables to consider. Typically [recipes::all_predictors()] or [recipes::all_numeric_predictors()].
#' @param role (optional, character) Not used by this step since no new variables are created.
#' @param trained (optional, logical) Indicates if the step has been trained.
#' @param options (optional, list) Named list of arguments passed to
#'   [collinear::collinear()]. Common options include:
#'   \itemize{
#'     \item \code{max_cor}: Maximum correlation threshold (default: auto)
#'     \item \code{max_vif}: Maximum VIF threshold (default: auto)
#'     \item \code{preference_order}: Vector of predictor names in priority order
#'     \item \code{f}:
#'     \item \code{quiet}: Suppress messages (default: FALSE)
#'   }
#'   Note: \code{encoding_method} is not supported in this step.
#' @param selected (character, internal) Predictors retained after filtering.
#' @param skip (logical, optional) Should the step be skipped when baking? Defaults to FALSE.
#' @param keep_original_cols (logical, optional) Whether to keep original columns. Default: FALSE.
#' @param id (character, optional) Unique identifier for this step.
#'
#' @return A \code{step_collinear} object.
#'
#' @examples
#' \dontrun{
#' if(requireNamespace("recipes", quietly = TRUE) &&
#'     requireNamespace("parsnip", quietly = TRUE) &&
#'     requireNamespace("workflows", quietly = TRUE)
#'     ){
#'
#' data(
#'   vi_smol,
#'   vi_predictors_numeric
#'   )
#'
#' # model formula
#' vi_formula <- collinear::model_formula(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictors = vi_predictors_numeric
#' )
#'
#' # recipe
#' vi_recipe <- recipes::recipe(
#'   formula = vi_formula,
#'   data = vi_smol
#'   ) |>
#'   #multicollinearity filtering
#'   collinear::step_collinear(
#'     recipes::all_predictors(),
#'     options = list(
#'       max_cor = 0.7,
#'       max_vif = 5,
#'       f = collinear::f_numeric_glm
#'       )
#'   ) |>
#'   #normalization
#'   recipes::step_normalize(
#'     recipes::all_predictors()
#'     )
#'
#' # define linear regression model
#' vi_model <- parsnip::linear_reg() |>
#'   parsnip::set_engine("lm")
#'
#' # create and fit workflow
#' vi_workflow <- workflows::workflow() |>
#'   workflows::add_recipe(vi_recipe) |>
#'   workflows::add_model(vi_model) |>
#'   workflows::fit(data = vi_smol)
#'
#' vi_workflow
#'
#' }
#' }
#' @autoglobal
#' @export
step_collinear <- function(
    recipe, ...,
    role = NA,
    trained = FALSE,
    options = list(),
    selected = NULL,
    skip = FALSE,
    keep_original_cols = FALSE,
    id = recipes::rand_id("collinear")
    ){

  if (!requireNamespace("recipes", quietly = TRUE)) {
    stop(
      "collinear::step_collinear(): Package 'recipes' is not installed.",
      call. = FALSE
      )
  }

  recipes::add_step(
    recipe,
    step_collinear_new(
      terms = recipes::ellipse_check(...),
      role = role,
      trained = trained,
      options = options,
      selected = selected,
      skip = skip,
      keep_original_cols = keep_original_cols,
      id = id
    )
  )
}

step_collinear_new <- function(
    terms,
    role,
    trained,
    options,
    selected,
    skip,
    keep_original_cols,
    id
    ) {

  structure(
    list(
      terms = terms,
      role = role,
      trained = trained,
      options = options,
      selected = selected,
      skip = skip,
      keep_original_cols = keep_original_cols,
      id = id
    ),
    class = c(
      "step_collinear",
      "recipes_step"
      )
    )

}

#' @rdname step_collinear
#' @param x (step_collinear object) The step to be trained.
#' @param training (data.frame) The training dataset used to estimate quantities.
#' @param info (data.frame, optional) Preprocessed information about variables in `training`.
#' @param object (step_collinear object) The trained step.
#' @param new_data (data.frame) New data to apply the step to.
#' @param ... (optional) Additional arguments (currently ignored).
#' @importFrom recipes prep
#' @method prep step_collinear
#' @export
prep.step_collinear <- function(
    x,
    training,
    info = NULL,
    ...
    ) {

  if (!requireNamespace("recipes", quietly = TRUE)) {
    stop(
      "collinear::step_collinear(): Package 'recipes' is not installed.",
      call. = FALSE
    )
  }

  # outcomes from the recipe
  responses <- recipes::recipes_eval_select(
    rlang::quo(recipes::all_outcomes()),
    training,
    info
  )

  # predictors selected by the step
  predictors <- recipes::recipes_eval_select(
    x$terms,      # already a quosure created by ellipse_check()
    training,
    info
  )

  # remove outcomes if any selector accidentally included them
  predictors <- setdiff(predictors, responses)

  # allow overrides via options
  if (!is.null(x$options$responses)) {
    responses <- x$options$responses
  }

  if (!is.null(x$options$predictors)) {
    predictors <- x$options$predictors
  }

  #keep outcome
  df <- training[, c(responses, predictors), drop = FALSE]

  # df <- training[, predictors, drop = FALSE]

  #inject responses and predictors into options
  x$options$responses <- responses
  x$options$predictors <- predictors
  x$options$encoding_method <- NULL

  result <- do.call(
    what = collinear::collinear,
    args = c(
      list(df = df),
      x$options
      )
    )

  step_collinear_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    options = x$options,
    selected = c(
      result[[1]]$response,
      result[[1]]$selection
      ),
    skip = x$skip,
    keep_original_cols = x$keep_original_cols,
    id = x$id
  )

}

#' @rdname step_collinear
#' @param object (step_collinear object) The trained step.
#' @param new_data (data.frame) New data to apply the step to.
#' @param ... (optional) Additional arguments (currently ignored).
#' @importFrom recipes bake
#' @method bake step_collinear
#' @export
bake.step_collinear <- function(object, new_data, ...) {

  if (is.null(object$selected)) {
    warning(
      "collinear::bake.step_collinear(): function 'step_collinear()' was not properly trained, returning all variables.",
      call. = FALSE
    )
    return(new_data)
  }

  recipes::check_new_data(
    new_data = new_data,
    req = object$selected
  )

  new_data[, object$selected, drop = FALSE]
}
