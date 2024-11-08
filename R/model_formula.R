#' Generate Model Formulas
#'
#' @inheritParams collinear
#' @param df (optional; data frame, tibble, or sf). A data frame with responses and predictors. Required if `predictors = NULL`. Default: NULL.
#' @param predictors (optional, character vector, output of [collinear()]): predictors to include in the formula. Required if `df = NULL`.
#' @param term_f (optional; string). Name of function to apply to each term in the formula, such as "s" for [mgcv::s()] or any other smoothing function, "poly" for [stats::poly()]. Default: NULL
#' @param term_args (optional; string). Arguments of the function applied to each term. For example, for "poly" it can be "degree = 2, raw = TRUE". Default: NULL
#' @param random_effects (optional, string or character vector). Names of variables to be used as random effects. Each element is added to the final formula as ` +(1 | random_effect_name)`. Default: NULL
#'
#' @return list if `predictors` is a list or length of `response` is higher than one, and character vector otherwise.
#' @export
#' @autoglobal
#' @examples
#' #using df, response, and predictors
#' #----------------------------------
#' df <- vi[1:1000, ]
#'
#' #additive formulas
#' formulas_additive <- model_formula(
#'   df = df,
#'   response = c(
#'     "vi_numeric",
#'     "vi_categorical"
#'     ),
#'   predictors = vi_predictors_numeric[1:10]
#' )
#'
#' formulas_additive
#'
#' #using a formula in a model
#' #m <- stats::lm(
#' #  formula = formulas_additive[[1]],
#' #  data = df
#' #  )
#'
#' # using output of collinear()
#' #----------------------------------
#' selection <- collinear(
#'   df = df,
#'   response = c(
#'     "vi_numeric",
#'     "vi_binomial"
#'   ),
#'   predictors = vi_predictors_numeric[1:10],
#'   quiet = TRUE
#' )
#'
#' #polynomial formulas
#' formulas_poly <- model_formula(
#'   predictors = selection,
#'   term_f = "poly",
#'   term_args = "degree = 3, raw = TRUE"
#' )
#'
#' formulas_poly
#'
#' #gam formulas
#' formulas_gam <- model_formula(
#'   predictors = selection,
#'   term_f = "s"
#' )
#'
#' formulas_gam
#'
#' #adding a random effect
#' formulas_random_effect <- model_formula(
#'   predictors = selection,
#'   random_effects = "country_name"
#' )
#'
#' formulas_random_effect
#' @family modelling_tools
model_formula <- function(
    df = NULL,
    response = NULL,
    predictors = NULL,
    term_f = NULL,
    term_args = NULL,
    random_effects = NULL,
    quiet = FALSE
){

  #if no predictors, df and response are required
  #and predictors is generated from df colnames minus response
  if(is.null(predictors)){

    #df
    if(is.null(df)){
      stop(
        "collinear::model_formula(): arguments 'df' and 'predictors' cannot be NULL at the same time.",
        call. = FALSE
      )
    }

    df <- validate_df(
      df = df,
      quiet = quiet
    )

    #response
    if(is.null(response)){
      stop(
        "collinear::model_formula(): arguments 'response' and 'predictors' cannot be NULL at the same time.",
        call. = FALSE
      )
    }

    #generate predictors vector from df and response
    predictors <- validate_predictors(
      df = df,
      response = NULL,
      predictors = predictors,
      quiet = quiet
    )

    #remove response from predictors
    predictors <- setdiff(
      x = predictors,
      y = response
    )

  }

  #predictors is a character vector
  if(inherits(x = predictors, what = "character")){

    #set response from attributes if argument is NULL
    if(is.null(response)){
      response <- attributes(predictors)$response
    }

    #if still NULL, stop
    if(is.null(response)){
      stop(
        "collinear::model_formula(): argument 'predictors' must have a valid attribute 'response' if the argument 'response' is NULL.",
        call. = FALSE
      )
    }

    #convert to list
    predictors_list <- list()

    for(response.i in response){

      attr(
        x = predictors,
        which = "validated"
      ) <- TRUE

      attr(
        x = predictors,
        which = "response"
      ) <- response.i

      predictors_list[[response.i]] <- predictors

    }

    predictors <- predictors_list

  }

  #predictors is a list
  if(!inherits(x = predictors, what = "list")){
    stop(
      "collinear::model_formula(): argument 'predictors' must be a list.",
      call. = FALSE
    )
  }

  #list must have names
  if(any(is.null(names(predictors)))){
    names(predictors) <- sapply(
      X = predictors,
      FUN = function(x){
        attributes(x)$response
      }
    )
  }

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
        "collinear::model_formula(): argument 'random_effects' must be a character string or vector.",
        call. = FALSE
      )
    }

    random_effects <- setdiff(
      x = random_effects,
      y = unique(unlist(predictors))
    )

    if(length(random_effects) == 0){
      stop(
        "collinear::model_formula(): argument 'random_effects' must name variables not in argument 'predictors'.",
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
  out <- list()

  #iterate over predictors
  for(i in seq_len(length(predictors))){

    out[[names(predictors)[i]]] <- paste0(
      names(predictors)[i],
      " ~ ",
      paste0(
        paste0(
          term_f,
          as.vector(predictors[[i]]),
          term_comma,
          term_args
        ),
        collapse = " + "
      ),
      random_effects
    ) |>
      stats::as.formula()

  }

  if(length(out) == 1){
    out <- unlist(out)
  }

  out

}



