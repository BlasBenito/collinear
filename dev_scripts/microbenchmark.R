library(microbenchmark)

# Option 1
option1 <- function() {

  #correlation matrix
  m <- cor_matrix(
    df = df,
    predictors = predictors
  ) |>
    abs()

  #auto preference order
  #variables with lower sum of correlation with others go higher
  preference_order_auto <- m |>
    colSums() |>
    sort() |>
    names()

  #validate preference order
  preference_order <- validate_preference_order(
    predictors = predictors,
    preference_order = preference_order,
    preference_order_auto = preference_order_auto
  )

  preference_order_rev <- rev(preference_order)

  m <- m[
    preference_order_rev,
    preference_order_rev
  ]

  #set diag to 0
  diag(m) <- 0

  selected <- preference_order_rev

  for(i in preference_order_rev){

    if(max(m[selected, i]) > cor_max){
      selected <- selected[selected != i]
    }

  }
}

# Option 2
option2 <- function() {

  #correlation matrix
  m <- cor_matrix(
    df = df,
    predictors = predictors
  ) |>
    abs()

  #auto preference order
  #variables with lower sum of correlation with others go higher
  preference_order_auto <- m |>
    colSums() |>
    sort() |>
    names()

  #validate preference order
  preference_order <- validate_preference_order(
    predictors = predictors,
    preference_order = preference_order,
    preference_order_auto = preference_order_auto
  )

  #organize the correlation matrix according to preference_order
  m <- m[
    preference_order,
    preference_order
  ]

  #set diag to 0
  diag(m) <- 0

  #i for first iteration
  i <- 1

  #iterate over i values
  while(i < ncol(m)){

    i <- i + 1

    #remove i column and row if max > cor_max
    if(max(m[1:i, i]) > cor_max){

      #remove column
      m <- m[-i, -i, drop = FALSE]

      #break condition
      if(ncol(m) == 1){break}

      #go back one column
      i <- i - 1

    }

  }
}

# Benchmark both options
benchmark_result <- microbenchmark(
  option1(),
  option2(),
  times = 10
)

print(benchmark_result)
