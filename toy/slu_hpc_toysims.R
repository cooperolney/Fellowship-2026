# library(tidyverse)
# n <- 50000000
# 
# calc_mean <- function(n) {
#   seed <- sample(1e9, size = 1)
#   set.seed(seed)
#   zs <- rnorm(n, 0, 1)
#   mean_df <- tibble(sample_mean = mean(zs)) |>
#     mutate(seed = seed)
#   return(mean_df)
# }
# 
# out_df <- calc_mean(n = n)


out_df <- data.frame(x = c(1, 2))
write.csv(out_df, "sims_test.csv", append = TRUE)


