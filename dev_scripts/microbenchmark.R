library(microbenchmark)

# Option 1
option1 <- function() {
  #rank of interest
  df.rank <- data.frame(
    predictor = preference_order,
    rank = seq_len(ncol(df))
  )

  #recursive VIF filtering
  for(i in seq(from = nrow(df.rank), to = 2)){

    #generate VIF data frame
    vif.i.df <- vif_df(
      df = df,
      predictors = df.rank[["predictor"]]
    )

    #extract relevant vif value
    vif.i <- vif.i.df[["vif"]][
      vif.i.df[["predictor"]] == df.rank[["predictor"]][i]
    ]

    #removing rank row if vif higher than max_vif
    if(vif.i > max_vif){
      df.rank <- df.rank[-i, ]
    }

  }

  #selected variables
  out <- df.rank[["predictor"]]
}

# Option 2
option2 <- function() {
  #rank of interest
  df.rank <- data.frame(
    predictor = preference_order,
    rank = seq_len(ncol(df)),
    selected = TRUE
  )

  #recursive VIF filtering
  for(i in seq(from = nrow(df.rank), to = 2)){

    #generate VIF data frame
    vif.i.df <- vif_df(
      df = df,
      predictors = df.rank[df.rank[["selected"]] == TRUE, "predictor"]
    )

    #extract relevant vif value
    vif.i <- vif.i.df[
      vif.i.df[["predictor"]] == df.rank[["predictor"]][i],
      "vif"
      ]

    #removing rank row if vif higher than max_vif
    if(vif.i > max_vif){
      df.rank[["selected"]][i] <- FALSE
    }

  }

  #selected variables
  out <- df.rank[df.rank[["selected"]] == TRUE, "predictor"]
}

# Benchmark both options
benchmark_result <- microbenchmark(
  option1(),
  option2(),
  times = 50
)

print(benchmark_result)
