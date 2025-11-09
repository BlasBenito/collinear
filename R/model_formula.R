#' Generate Model Formulas
#'
#' @description
#' Generates model formulas from a dataframe, a response name, and a vector of predictors that can be the output of a multicollinearity management function such as [collinear_select()] and the likes. Intended to help fit exploratory models from the result of a multicollinearity analysis.
#'
#' The types of formulas it can generate are:
#' \itemize{
#'   \item additive: \code{y ~ x + z}
#'   \item polynomial: \code{y ~ poly(x, ...) + poly(z, ...)}
#'   \item GAM: \code{y ~ s(x) + s(z)}
#'   \item random effect: \code{y ~ x + (1 \ z)}
#' }
#'
#' @inheritParams collinear
#' @inheritParams f_auto
#'
#' @param term_f (optional; string). Name of function to apply to each term in the formula, such as "s" for [mgcv::s()] or any other smoothing function, "poly" for [stats::poly()]. Default: NULL
#'
#' @param term_args (optional; string). Arguments of the function applied to each term. For example, for "poly" it can be "degree = 2, raw = TRUE". Default: NULL
#'
#' @param random_effects (optional, string or character vector). Names of variables to be used as random effects. Each element is added to the final formula as \code{+(1 | random_effect_name)}. Default: NULL
#'
#' @return list if \code{predictors} is a list or length of \code{response} is higher than one, and character vector otherwise.
#' @export
#' @autoglobal
#' @examples
#' data(
#'   vi_smol,
#'   vi_predictors_numeric
#'   )
#'
#' #reduce collinearity
#' x <- collinear_select(
#'   df = vi_smol,
#'   predictors = vi_predictors_numeric
#' )
#'
#' #additive formula
#' y <- model_formula(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictors = x
#' )
#'
#' y
#'
#' #using a formula in a model
#' m <- stats::lm(
#'  formula = y,
#'  data = vi_smol
#'  )
#'
#' summary(m)
#'
#' #classification formula (character response)
#' y <- model_formula(
#'   df = vi_smol,
#'   response = "vi_categorical",
#'   predictors = x
#' )
#'
#' y
#'
#'
#' #polynomial formula (3rd degree)
#' y <- model_formula(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictors = x,
#'   term_f = "poly",
#'   term_args = "degree = 3, raw = TRUE"
#' )
#'
#' y
#'
#' #gam formula
#' y <- model_formula(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictors = x,
#'   term_f = "s"
#' )
#'
#' y
#'
#' #random effect
#' y <- model_formula(
#'   df = vi_smol,
#'   response = "vi_numeric",
#'   predictors = x,
#'   random_effects = "country_name" #from vi_smol$country_name
#' )
#'
#' y
#' @family modelling_tools
model_formula <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    term_f = NULL,
    term_args = NULL,
    random_effects = NULL,
    quiet = FALSE,
    ...
){

  function_name <- validate_arg_function_name(
    default_name = "collinear::model_formula()",
    ... = ...
  )

  df <- validate_arg_df_not_null(
    df = df,
    function_name = function_name
  )

  quiet <- validate_arg_quiet(
    quiet = quiet,
    function_name = function_name
  )

  response <- validate_arg_responses(
    df = df,
    responses = response,
    max_responses = 1,
    quiet = quiet,
    function_name = function_name
  )

  if(is.null(response)){

    stop(
      "\n",
      function_name,
      ": argument 'response' cannot be NULL.",
      call. = FALSE
    )

  }

  predictors <- validate_arg_predictors(
    df = df,
    responses = response,
    predictors = predictors,
    quiet = quiet,
    function_name = function_name
  )

  if(is.null(predictors)){
    stop(
      "\n",
      function_name,
      ": argument 'predictors' cannot be NULL.",
      call. = FALSE
    )
  }

  predictors_types <- identify_valid_variables(
    df = df,
    predictors = predictors
  )

  #prepare terms formula
  term_comma <- ","
  if(!is.null(term_f)){

    #remove (
    term_f <- gsub(
      pattern = "\\(",
      replacement = "",
      x = term_f
    )

    term_f <- paste0(term_f, "(")

    if(is.null(term_args)){

      term_args <- ")"
      term_comma <- NULL

    } else {

      term_args <- gsub(
        pattern = "\\)",
        replacement = "",
        x = term_args
      )

      term_args <- gsub(
        pattern = "^,",
        replacement = "",
        x = term_args
      )

      term_args <- paste0(term_args, ")")

    }

  } else {
    term_args <- NULL
    term_comma <- NULL
  }

  #random effects

  if(!is.null(random_effects)){

    if(!is.character(random_effects)){
      stop(
        "\n",
        function_name,
        ": argument 'random_effects' must be a character string or vector.",
        call. = FALSE
      )
    }

    random_effects <- setdiff(
      x = random_effects,
      y = unique(unlist(predictors))
    )

    if(length(random_effects) == 0){
      stop(
        "\n",
        function_name,
        ": argument 'random_effects' must name variables not in argument 'predictors'.",
        call. = FALSE
      )
    }

    random_effects <- gsub(
      pattern = "\\)",
      replacement = "",
      x = random_effects
    )

    random_effects <- gsub(
      pattern = "\\(",
      replacement = "",
      x = random_effects
    )

    random_effects <- gsub(
      pattern = "\\+",
      replacement = "",
      x = random_effects
    )

    random_effects <- gsub(
      pattern = "\\|",
      replacement = "",
      x = random_effects
    )

    random_effects <- gsub(
      pattern = "1",
      replacement = "",
      x = random_effects
    )

    random_effects <- gsub(
      pattern = "\\s",
      replacement = "",
      x = random_effects
    )

    random_effects <- paste(
      "+",
      paste(
        "(1 |",
        random_effects,
        ")",
        collapse = " + "
      )
    )

  }

  #out list
  terms <- vector()

  #iterate over responses
  for(predictor.i in predictors){

    if(predictor.i %in% predictors_types$numeric){

      predictor.i <- paste0(
        term_f,
        predictor.i,
        term_comma,
        term_args
      )

    }

    terms <- c(terms, predictor.i)

  }

  out <- paste0(
    response,
    " ~ ",
    paste0(
      terms,
      collapse = " + "
    ),
    random_effects
  ) |>
    stats::as.formula()

  environment(out) <- parent.frame()


  out

}



