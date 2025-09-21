set.seed(1)

library(collinear)
data(vi)

vi_smol <- vi[sample(x = 1:nrow(vi), size = length(vi_predictors) * 10), ]

identify_predictors(df = vi_smol)

nrow(vi_smol)

usethis::use_data(vi_smol, overwrite = TRUE)
