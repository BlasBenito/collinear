#' @title Tidymodels Integration of [collinear()]
#'
#' @description
#'
#' Adds a step to a recipe created by [recipes::recipe()] to apply multicollinearity filtering via [collinear::collinear()].
#'
#' This function requires the \code{recipes} package to be installed.
#'
#'Unlike \code{collinear()}, this wrapper does not perform target encoding, and the default value for the argument \code{quiet} is \code{TRUE}.
#'
#' @param recipe (required, recipe) A recipe object to which this step will be added.
#' @param ... (optional, selector functions) Variables to consider. Typically [recipes::all_predictors()] or [recipes::all_numeric_predictors()].
#' @param role (optional, character) Not used by this step since no new variables are created. Default: NA
#' @param trained (optional, logical) Indicates if the step has been trained. Default: FALSE
#' @param options (optional, list) Named list of arguments passed to [collinear::collinear()]. Common options include:
#'   \itemize{
#'     \item \code{max_cor}: Maximum correlation threshold. If NULL (default), automatically set based on median correlation of predictors.
#'     \item \code{max_vif}: Maximum VIF threshold. If NULL (default), automatically set to match the auto-computed max_cor.
#'     \item \code{preference_order}: Vector of predictor names in priority order.
#'     \item \code{f}: Function to compute preference order (default: \code{f_auto})
#'     \item \code{quiet}: Suppress messages (default: TRUE)
#'   }
#'   Note: \code{encoding_method} is not supported in this step. The automatic threshold selection adapts to each dataset's correlation structure.
#' @param selected (character vector) Predictors retained after filtering. Populated during training and used during baking. Default: NULL
#' @param skip (optional, logical) Trigger to skip this step when baking. Default: FALSE.
#' @param keep_original_cols (optional, logical) Whether to keep original columns. Default: FALSE.
#' @param id (optional, character) Unique identifier for this step.
#'
#' @return Updated recipe with new step.
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
    recipe,
    ...,
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
      "collinear::step_collinear(): Package 'recipes' is required but not installed.",
      call. = FALSE
      )
  }

  # recipes::add_step(
  #   recipe,
  #   step_collinear_new(
  #     terms = recipes::ellipse_check(...),
  #     role = role,
  #     trained = trained,
  #     options = options,
  #     selected = selected,
  #     skip = skip,
  #     keep_original_cols = keep_original_cols,
  #     id = id
  #   )
  # )

  recipes::add_step(
    recipe,
    step_collinear_new(
      terms = rlang::enquos(...),  # ← Changed: store quosures
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
#' @param x (required, step_collinear object) The step to be trained. Default: NULL
#' @param training (required, data.frame) The training dataset used to estimate quantities.
#' @param info (optional, data.frame) Preprocessed information about variables in `training`. Default: NULL
#' @param ... (optional) Additional arguments (currently ignored).
#' @importFrom recipes prep
#' @method prep step_collinear
#' @export
prep.step_collinear <- function(
    x = NULL,
    training = NULL,
    info = NULL,
    ...
    ) {

  if (!requireNamespace("recipes", quietly = TRUE)) {
    stop(
      "collinear::step_collinear(): Package 'recipes' is required but not installed.",
      call. = FALSE
    )
  }

  # outcomes from the recipe
  responses <- recipes::recipes_eval_select(
    quos = list(rlang::quo(recipes::all_outcomes())),
    data = training,
    info = info
  )

  # predictors selected by the step
  predictors <- recipes::recipes_eval_select(
    quos = x$terms,
    data = training,
    info = info
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

  # Set quiet = TRUE as default unless user specified otherwise
  if (is.null(x$options$quiet)) {
    x$options$quiet <- TRUE
  }

  result <- do.call(
    what = collinear::collinear,
    args = c(
      list(df = df),
      x$options
      )
    )

  if(length(result) == 0 || is.null(result[[1]])) {
    stop("collinear::step_collinear(): multicollinearity filtering failed,  function collinear::collinear() returned empty results.", call. = FALSE)
  }

  # In prep.step_collinear, around line 129
  selected = if(is.null(result[[1]]$response)) {
    result[[1]]$selection
  } else {
    c(result[[1]]$response, result[[1]]$selection)
  }

  step_collinear_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    options = x$options,
    selected = selected,
    skip = x$skip,
    keep_original_cols = x$keep_original_cols,
    id = x$id
  )

}

#' @rdname step_collinear
#' @param object (required, \code{step_collinear} object) The trained step. Default: NULL
#' @param new_data (required, data.frame) New data to apply the step to. Default: NULL
#' @param ... (optional) Additional arguments (currently ignored).
#' @importFrom recipes bake
#' @method bake step_collinear
#' @export
bake.step_collinear <- function(
    object = NULL,
    new_data = NULL,
    ...
    ) {

  if (!requireNamespace("recipes", quietly = TRUE)) {
    stop(
      "collinear::step_collinear(): Package 'recipes' is required but not installed.",
      call. = FALSE
    )
  }

  if (is.null(object$selected)) {
    warning(
      "collinear::bake.step_collinear(): function 'step_collinear()' was not properly trained, returning all variables.",
      call. = FALSE
    )
    return(new_data)
  }

  recipes::check_new_data(
    req = object$selected,
    object = object,
    new_data = new_data
  )

  new_data[, object$selected, drop = FALSE]

}
