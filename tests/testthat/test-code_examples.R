testthat::test_that("All code examples work!", {

  # validate_arg_response ----
  data(vi)

  response <- validate_arg_response(
    df = vi,
    response = "vi_numeric"
  )

  attributes(response)$validated


  #validate_arg_quiet ----
  quiet <- validate_arg_quiet(
    function_name = "f()",
    quiet = TRUE
  )

  attributes(quiet)$validated

  #validate_arg_preference_order ----
  data(vi, vi_predictors)

  predictors <- validate_arg_predictors(
    df = vi,
    predictors = vi_predictors
  )

  my_preference_order <- c(
    "swi_max",
    "swi_min",
    "swi_deviance"
  )

  my_order <- validate_arg_preference_order(
    predictors = predictors,
    preference_order = my_preference_order,
    preference_order_auto = vi_predictors
  )

  my_order

  #validate_arg_predictors ----
  data(vi, vi_predictors)

  predictors <- validate_arg_predictors(
    df = vi,
    predictors = vi_predictors
  )

  attributes(predictors)$validated

  #validate_arg_predictors_vif ----
  data(vi, vi_predictors)

  predictors <- validate_arg_predictors_vif(
    df = vi,
    predictors = vi_predictors,
    function_name = "vif_df()"
  )

  attributes(predictors)$validated
  attributes(predictors)$validated_vif

  #validate_arg_predictors_cor ----
  data(vi, vi_predictors)

  predictors <- validate_arg_predictors_cor(
    df = vi,
    predictors = vi_predictors,
    function_name = "cor_df()"
  )

  attributes(predictors)$validated
  attributes(predictors)$validated_cor

  #validate_arg_max_vif ----
  max_vif <- validate_arg_max_vif(
    max_vif = 11, #wrong value
    function_name = "f()",
    quiet = FALSE
  )

  max_vif
  attributes(max_vif)$validated

  #validate_arg_max_cor ----
  max_cor <- validate_arg_max_cor(
    max_cor = 1.5, #wrong value
    function_name = "f()",
    quiet = FALSE
  )

  max_cor
  attributes(max_cor)$validated

  #validate_arg_df ----
  data(vi, vi_predictors)

  vi <- validate_arg_df(
    df = vi,
    response = "vi_numeric",
    predictors = vi_predictors,
    function_name = "f()",
    quiet = FALSE
  )

  attributes(vi)$validated

  #validate_arg_df_not_null ----
  data(vi)

  df <- validate_arg_df_not_null(
    df = vi,
    function_name = "f()"
  )

  #tolerance ----
  tolerance()

  #target_encoding_methods ----
  data(vi)

  vi <- vi[1:1000, ]

  df <- target_encoding_mean(
    df = vi,
    response = "vi_numeric",
    predictor = "soil_type",
    encoded_name = "soil_type_encoded"
  )

  plot(
    x = df$soil_type_encoded,
    y = df$vi_numeric,
    xlab = "encoded variable",
    ylab = "response"
  )

  df <- target_encoding_rank(
    df = vi,
    response = "vi_numeric",
    predictor = "soil_type",
    encoded_name = "soil_type_encoded"
  )

  plot(
    x = df$soil_type_encoded,
    y = df$vi_numeric,
    xlab = "encoded variable",
    ylab = "response"
  )

  df <- target_encoding_loo(
    df = vi,
    response = "vi_numeric",
    predictor = "soil_type",
    encoded_name = "soil_type_encoded"
  )

  plot(
    x = df$soil_type_encoded,
    y = df$vi_numeric,
    xlab = "encoded variable",
    ylab = "response"
  )

  #target_encoding_lab ----
  data(vi, vi_predictors)

  vi <- vi[1:1000, ]

  df <- target_encoding_lab(
    df = vi,
    response = "vi_numeric",
    predictors = "koppen_zone",
    methods = c(
      "mean",
      "loo",
      "rank"
    ),
    white_noise = c(0, 0.1, 0.2)
  )

  predictors.encoded <- grep(
    pattern = "*__encoded*",
    x = colnames(df),
    value = TRUE
  )

  #preference_order ----
  data(vi, vi_predictors, vi_predictors_numeric)

  df <- vi[1:1000, ]
  predictors <- vi_predictors[1:10]
  predictors_numeric <- vi_predictors_numeric[1:10]

  future::plan(
    future::multisession,
    workers = 2
  )

  df_preference <- preference_order(
    df = df,
    response = "vi_numeric",
    predictors = predictors_numeric,
    f = NULL
  )

  df_preference

  responses <- c(
    "vi_categorical",
    "vi_counts"
  )

  preference_list <- preference_order(
    df = df,
    response = responses,
    predictors = predictors
  )

  names(preference_list)
  preference_list[[1]]
  preference_list[[2]]

  x <- collinear(
    df = df,
    response = responses,
    predictors = predictors,
    preference_order = preference_list
  )

  df <- preference_order(
    df = vi,
    response = "vi_binomial",
    predictors = predictors_numeric,
    f = f_auc_glm_binomial
  )

  future::plan(future::sequential)

  #f_auto ----
  data(vi, vi_predictors_numeric)

  f <- f_auto(
    df = vi[1:1000, ],
    response = "vi_numeric",
    predictors = vi_predictors_numeric
  )


  #f_functions ----
  f_functions()


  #f_auto_rules ----
  f_auto_rules()


  #f_r2_...----
  data(vi)

  vi <- vi[1:1000, ]

  df <- data.frame(
    y = vi[["vi_numeric"]],
    x = vi[["swi_max"]]
  ) |>
    na.omit()

  f_r2_pearson(df = df)

  f_r2_spearman(df = df)

  f_r2_glm_gaussian(df = df)

  f_r2_glm_gaussian_poly2(df = df)

  f_r2_gam_gaussian(df = df)

  f_r2_rpart(df = df)

  f_r2_rf(df = df)

  #f_r2_counts ----
  data(vi)

  vi <- vi[1:1000, ]

  df <- data.frame(
    y = vi[["vi_counts"]],
    x = vi[["swi_max"]]
  ) |>
    na.omit()

  f_r2_glm_poisson(df = df)

  f_r2_glm_poisson_poly2(df = df)

  f_r2_gam_poisson(df = df)

  #f_auc_... ----
  data(vi)

  vi <- vi[1:1000, ]

  df <- data.frame(
    y = vi[["vi_binomial"]],
    x = vi[["swi_max"]]
  ) |>
    na.omit()

  f_auc_glm_binomial(df = df)

  f_auc_glm_binomial_poly2(df = df)

  f_auc_gam_binomial(df = df)

  f_auc_rpart(df = df)

  f_auc_rf(df = df)

  #f_v ----
  data(vi)

  vi <- vi[1:1000, ]

  df <- data.frame(
    y = vi[["vi_factor"]],
    x = vi[["soil_type"]]
  ) |>
    na.omit()

  f_v(df = df)

  #f_v_rf_categorical ----
  data(vi)

  vi <- vi[1:1000, ]

  df <- data.frame(
    y = vi[["vi_factor"]],
    x = vi[["soil_type"]]
  ) |>
    na.omit()

  f_v_rf_categorical(df = df)

  df <- data.frame(
    y = vi[["vi_factor"]],
    x = vi[["swi_mean"]]
  ) |>
    na.omit()

  f_v_rf_categorical(df = df)

  #performance_score_v ----
  performance_score_v(
    o = c("a", "a", "b", "c", "c"),
    p = c("a", "b", "b", "c", "c")
  )

  #performance_score_r2 ----
  performance_score_r2(
    o = c(1, 1, 1, 0.5, 0.5, 0, 0),
    p = c(1, 0.8, 0.7, 0.6, 0.5, 0.1, 0)
  )

  #performance_score_auc ----
  performance_score_auc(
    o = c(1, 1, 1, 1, 0, 0, 0),
    p = c(1, 0.8, 0.7, 0.6, 0.5, 0.6, 0.7)
  )

  #model_formula ----
  data(vi, vi_predictors_numeric)

  df <- vi[1:1000, ]

  formulas_additive <- model_formula(
    df = df,
    response = c(
      "vi_numeric",
      "vi_categorical"
    ),
    predictors = vi_predictors_numeric[1:10]
  )

  formulas_additive

  m <- stats::lm(
    formula = formulas_additive[[1]],
    data = df
  )

  selection <- collinear(
    df = df,
    response = c(
      "vi_numeric",
      "vi_binomial"
    ),
    predictors = vi_predictors_numeric[1:10],
    quiet = TRUE
  )

  formulas_poly <- model_formula(
    predictors = selection,
    term_f = "poly",
    term_args = "degree = 3, raw = TRUE"
  )

  formulas_poly

  formulas_gam <- model_formula(
    predictors = selection,
    term_f = "s"
  )

  formulas_gam

  formulas_random_effect <- model_formula(
    predictors = selection,
    random_effects = "country_name"
  )

  formulas_random_effect

  #identify_predictors ----
  data(
    vi,
    vi_predictors
  )

  predictors_names <- identify_predictors(
    df = vi,
    predictors = vi_predictors
  )

  predictors_names

  #identify_predictors_logical ----
  data(vi, vi_predictors)

  logical.predictors <- identify_predictors_logical(
    df = vi,
    predictors = vi_predictors
  )

  logical.predictors

  #identify_predictors_numeric ----
  data(vi, vi_predictors)

  numeric.predictors <- identify_predictors_numeric(
    df = vi,
    predictors = vi_predictors
  )

  numeric.predictors

  #identify_predictors_categorical ----
  data(vi, vi_predictors)

  non.numeric.predictors <- identify_predictors_categorical(
    df = vi,
    predictors = vi_predictors
  )

  non.numeric.predictors

  #identify_predictors_zero_variance ----
  data(vi, vi_predictors)

  vi$zv_1 <- 1
  vi$zv_2 <- runif(n = nrow(vi), min = 0, max = 0.0001)

  vi_predictors <- c(
    vi_predictors,
    "zv_1",
    "zv_2"
  )

  zero.variance.predictors <- identify_predictors_zero_variance(
    df = vi,
    predictors = vi_predictors
  )

  zero.variance.predictors

  #identify_response_type ----
  data(vi, vi_predictors)

  identify_response_type(
    df = vi,
    response = "vi_numeric"
  )

  identify_response_type(
    df = vi,
    response = "vi_counts"
  )

  identify_response_type(
    df = vi,
    response = "vi_binomial"
  )

  identify_response_type(
    df = vi,
    response = "vi_categorical"
  )

  identify_response_type(
    df = vi,
    response = "vi_factor"
  )

  #identify_predictors_type ----
  data(vi, vi_predictors)

  identify_predictors_type(
    df = vi,
    predictors = vi_predictors
  )

  identify_predictors_type(
    df = vi,
    predictors = vi_predictors_numeric
  )

  identify_predictors_type(
    df = vi,
    predictors = vi_predictors_categorical
  )

  #drop_geometry_column ----
  data(vi)

  vi$geometry <- NA
  attr(x = vi, which = "sf_column") <- "geometry"

  df <- drop_geometry_column(
    df = vi
  )

  "geometry" %in% colnames(df)

  #case_weights ----
  case_weights(
    x = c(0, 0, 0, 1, 1)
  )

  case_weights(
    x = c("a", "a", "b", "b", "c")
  )

  #vif ----
  data(vi, vi_predictors_numeric)

  m <- cor_matrix(
    df = vi[1:1000, ],
    predictors = vi_predictors_numeric[1:5]
  )

  vif(m)

  #vif_df ----
  data(vi, vi_predictors_numeric)

  v <- vif_df(
    df = vi[1:1000, ],
    predictors = vi_predictors_numeric[1:5]
  )

  v

  #vif_select ----
  data(vi, vi_predictors)

  df <- vi[1:1000, ]

  #predictors has mixed types
  sapply(
    X = df[, vi_predictors, drop = FALSE],
    FUN = class
  ) |>
    unique()

  #categorical predictors are ignored
  selected_predictors <- vif_select(
    df = df,
    predictors = vi_predictors,
    max_vif = 5,
    quiet = FALSE
  )

  #all these have a VIF lower than max_vif (2.5)
  vif_df(
    df = df,
    predictors = selected_predictors,
    quiet = TRUE
  )

  #custom preference order
  selected_predictors <- vif_select(
    df = df,
    predictors = vi_predictors,
    preference_order = c(
      "swi_mean",
      "soil_temperature_mean",
      "topo_elevation",
      "wrong_name" #ignored
    ),
    max_vif = 2.5,
    quiet = FALSE
  )

  #using automated preference order
  df_preference <- preference_order(
    df = df,
    response = "vi_numeric",
    predictors = vi_predictors[1:10]
  )

  selected_predictors <- vif_select(
    df = df,
    predictors = vi_predictors,
    preference_order = df_preference,
    max_vif = 5,
    quiet = FALSE
  )

  #cor_df ----
  data(vi)

  #subset to speed-up example
  vi <- vi[1:1000, ]

  #OPTIONAL: parallelization setup
  # future::plan(
  #   future::multisession,
  #   workers = 2
  # )

  #OPTIONAL: progress bar
  # progressr::handlers(global = TRUE)

  #correlation data frame
  df <- cor_df(
    df = vi,
    predictors = c(
      "koppen_zone", #character
      "soil_type", #factor
      "topo_elevation", #numeric
      "soil_temperature_mean" #numeric
    )
  )

  df

  #OPTIONAL: disable parallelization
  #future::plan(future::sequential)

  #cor_matrix ----

  data(vi)

  #subset to speed-up example
  vi <- vi[1:1000, ]

  predictors <- c(
    "koppen_zone", #character
    "soil_type", #factor
    "topo_elevation", #numeric
    "soil_temperature_mean" #numeric
  )

  #OPTIONAL: parallelization setup
  # future::plan(
  #   future::multisession,
  #   workers = 2
  # )

  #OPTIONAL: progress bar
  # progressr::handlers(global = TRUE)

  #correlation data frame to matrix
  m <- cor_df(
    df = vi,
    predictors = predictors
  ) |>
    cor_matrix()

  m

  #direct computation (uses cor_df() internally)
  m <- cor_matrix(
    df = vi,
    predictors = predictors
  )

  m

  #OPTIONAL: disable parallelization
  #future::plan(future::sequential)

  #cor_select ----

  data(vi)

  #subset to speed-up example
  vi <- vi[1:1000, ]

  #predictors
  predictors = c(
    "koppen_zone", #character
    "soil_type", #factor
    "topo_elevation", #numeric
    "soil_temperature_mean" #numeric
  )

  #OPTIONAL: parallelization setup
  # future::plan(
  #   future::multisession,
  #   workers = 2
  # )

  #OPTIONAL: progress bar
  # progressr::handlers(global = TRUE)

  #without preference order
  selected_predictors <- cor_select(
    df = vi,
    predictors = predictors,
    preference_order = NULL,
    max_cor = 0.75
  )


  #with custom preference order
  selected_predictors <- cor_select(
    df = vi,
    predictors = predictors,
    preference_order = c(
      "soil_temperature_mean",
      "soil_type"
    ),
    max_cor = 0.75
  )


  #with automated preference order
  df_preference <- preference_order(
    df = vi,
    response = "vi_numeric",
    predictors = predictors
  )

  selected_predictors <- cor_select(
    df = df,
    predictors = predictors,
    preference_order = df_preference,
    max_cor = 0.75
  )

  #OPTIONAL: disable parallelization
  #future::plan(future::sequential)

  #cor_cramer_v ----
  data(vi)

  #subset to speed-up example
  vi <- vi[1:1000, ]

  #Cramer's V for two categorical predictors
  v <- cor_cramer_v(
    x = vi$soil_type,
    y = vi$koppen_zone
    )

  v

  #cor_clusters ----
  data(vi)

  #subset to speed-up example
  vi <- vi[1:1000, ]

  #OPTIONAL: parallelization setup
  # future::plan(
  #   future::multisession,
  #   workers = 2
  # )

  #OPTIONAL: progress bar
  # progressr::handlers(global = TRUE)

  #group predictors using max_cor as clustering threshold
  df_clusters <- cor_clusters(
    df = vi,
    predictors = c(
      "koppen_zone", #character
      "soil_type", #factor
      "topo_elevation", #numeric
      "soil_temperature_mean" #numeric
    ),
    max_cor = 0.75,
    plot = FALSE #set to TRUE to plot result
  )

  #OPTIONAL: disable parallelization
  #future::plan(future::sequential)

  #collinear ----

  data(vi)

  #subset to speed-up example
  vi <- vi[1:1000, ]

  #OPTIONAL: parallelization setup
  # future::plan(
  #   future::multisession,
  #   workers = 2
  # )

  #OPTIONAL: progress bar
  # progressr::handlers(global = TRUE)

  #predictors has mixed types
  #small subset to speed example up
  predictors <- c(
    "koppen_zone", #character
    "soil_type", #factor
    "topo_elevation", #numeric
    "soil_temperature_mean" #numeric
  )


  #with numeric responses
  #--------------------------------
  #  target encoding
  #  automated preference order
  #  all predictors filtered by correlation and VIF
  x <- collinear(
    df = df,
    response = c(
      "vi_numeric",
      "vi_binomial"
      ),
    predictors = predictors
  )

  x


  #with custom preference order
  #--------------------------------
  x <- collinear(
    df = df,
    response = "vi_numeric",
    predictors = predictors,
    preference_order = c(
      "swi_mean",
      "soil_type"
    )
  )


  #pre-computed preference order
  #--------------------------------
  preference_df <- preference_order(
    df = df,
    response = "vi_numeric",
    predictors = predictors
  )

  x <- collinear(
    df = df,
    response = "vi_numeric",
    predictors = predictors,
    preference_order = preference_df
  )

  #resetting to sequential processing
  future::plan(future::sequential)


})
