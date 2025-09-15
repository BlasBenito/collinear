library(collinear)
library(future)
library(tictoc)
library(ggplot2)
library(distantia)
library(progressr)
library(mgcv)

future::plan(
  strategy = future::multisession,
  workers = future::availableCores() - 1
)

#data to use
data(vi, vi_predictors_numeric)
df <- vi[, vi_predictors_numeric]
rm(vi)

#add synthetic data

#with seasons
x <- distantia::zoo_simulate(
  name = "sim",
  cols = 100,
  rows = nrow(df),
  seasons = floor(nrow(df)/1000),
  seed = 1
) |>
  as.data.frame()

#without seasons
y <- distantia::zoo_simulate(
  name = "sim",
  cols = 100,
  rows = nrow(df),
  seed = 2
) |>
  as.data.frame()

df <- cbind(df, x, y)
rm(x)

#max dimensions
min_cols <- 10
max_cols <- 100
max_rows <- 10000


#candidate values
max_vif_candidates <- seq(from = 1, to = 10, by = 0.1)
max_cor_candidates <- seq(from = 0.1, to = 0.99, by = 0.01)
n_cols_candidates <- seq(from = min_cols, to = max_cols)

#random seed
set.seed(1)

#iterations
n <- 10000
iterations <- seq_len(n)

iterations_df <- data.frame(
  max_cor = sample(x = max_cor_candidates, size = n, replace = TRUE),
  n_cols = sample(x = n_cols_candidates, size = n, replace = TRUE),
  out_max_vif = rep(NA, n),
  out_similarity = rep(NA, n),
  out_length = rep(NA, n)
)

#compute n_rows (minimum of 30 per column)
iterations_df$n_rows <- vapply(
  iterations_df$n_cols,
  function(nc) sample(seq(nc * 30, nrow(df)), 1),
  numeric(1)
)


tic()

#run experiment
progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")
progressr::with_progress({

  p <- progressor(along = iterations)

  experiment_list <- future.apply::future_lapply(
    X = iterations,
    FUN = function(i){

      p()

      #get params
      iterations.df.i <- iterations_df[i, ]

      #subset data frame
      df.i <- df[
        sample(x = seq_len(nrow(df)), size = iterations.df.i$n_rows),
        sample(x = seq_len(ncol(df)), size = iterations.df.i$n_cols)
      ]

      #cor
      out.cor <- collinear::cor_select(
        df = df.i,
        predictors = colnames(df.i),
        preference_order = colnames(df.i),
        max_cor = iterations.df.i$max_cor,
        quiet = TRUE
      )

      #if out.cor selects nothing, cancel this run
      if(length(out.cor) == length(colnames(df.i))){
        return(NULL)
      }

      #vif
      for(j in seq_len(length(max_vif_candidates))){

        out.vif <- collinear::vif_select(
          df = df.i,
          predictors = colnames(df.i),
          preference_order = colnames(df.i),
          max_vif = max_vif_candidates[j],
          quiet = TRUE
        )

        if(length(out.vif) >= length(out.cor)){
          break
        }

      }

      #fill results
      iterations.df.i$out_max_vif <- max_vif_candidates[j]
      iterations.df.i$out_similarity <- length(intersect(out.vif, out.cor)) / length(union(out.vif, out.cor))
      iterations.df.i$out_length <- length(out.vif)

      return(iterations.df.i)

    },
    future.seed = TRUE

  )

})

toc()

#reset future backend (also kills hanging processes)
future::plan(sequential)

#join rows
experiment_df <- dplyr::bind_rows(experiment_list)
rm(experiment_list)

save(experiment_df, file = "dev_scripts/equivalency_max_vif_max_cor/experiment_results.RData")

#transform weights

#gam model
m <- mgcv::gam(
  formula = out_max_vif ~ s(max_cor, k = 9),
  weights = experiment_df$out_similarity^3,
  data = experiment_df,
  select = TRUE
)

AIC(m)

plotmo::plotmo(
  m,
  type = "response",                # fitted values on response scale
  pch  = 16,                        # filled points
  col.response = "turbo",           # optional viridis turbo palette
  pt.col = "grey40",                # training data color
  pt.cex = 0.6,                     # point size
  main = "mgcv::gam: out_max_vif ~ s(max_cor)"
)

experiment_df <- experiment_df |>
  dplyr::arrange(
    out_similarity
  )

ggplot(
  data = experiment_df
  ) +
  aes(
    x = max_cor,
    y = out_max_vif,
    color = out_similarity,
    weight = out_similarity^3
  ) +
  geom_point(alpha = 0.5) +
  geom_smooth(
    method = mgcv::gam,
    formula = y ~ s(x, k = 9),
    color = "gray40",
    size = 1.5
    ) +
  # scale_color_viridis_c(option = "C") +
  scale_color_gradientn(
    colours = hcl.colors(100, "Zissou1")   # HCL Zissou1 palette
  ) +
  labs(
    title = "Equivalence Between Correlation and VIF\n In Multicollinearity Filtering",
    x = "Max Pearson Correlation",
    y = "Max Variance Inflation Factor",
    color = "Variable\nSelection\nSimilarity\n(Jaccard)"
  ) +
  scale_x_continuous(
    breaks = seq(0, 1, by = 0.1)
  ) +
  scale_y_continuous(
    breaks = seq(1, 10, by = 1)
  ) +
  theme_bw(base_size = 18) +   # <- bigger text everywhere
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 14)
  )


ggplot(
  data = experiment_df
) +
  aes(
    x = max_cor,
    y = out_length/n_cols,
    color = n_rows,
  ) +
  geom_point()

