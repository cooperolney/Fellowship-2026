library(dplyr)
library(tidyr)
library(brms)
library(ggeffects)

final_data <- readRDS("final_data.rds")

# Data Subset
set.seed(123)

unique_games <- final_data |> distinct(game_id) |> pull(game_id)

selected_games <- sample(unique_games, 1534)

# subset of 50 games
subset_games <- final_data |> 
  filter(game_id %in% selected_games) #|>
  #filter(time_elapsed %% 24 == 0)

# scale time and score so algorithm runs better
subset_games <- subset_games |>
  mutate(
    time_scaled = time_elapsed / 2400,
    lead_scaled = lead_diff / 50,
    ratings_scaled = rating_diff / 50
  )


# Model
brm_mod_rank <- brm(home_win ~ lead_diff + time_elapsed + rating_diff + time_elapsed:lead_diff + time_elapsed:rating_diff,
                    data = subset_games,
                    family = bernoulli(),
                    prior = c(prior(normal(0, 2), class = "Intercept"))
)


# Plot
# model predictions
preds_brm_mod_rank <- ggpredict(brm_mod_rank, terms = c("time_elapsed [0:2400 by=10]", 
                                                        "lead_diff [-15, -10, -5, 0, 5, 10, 15]")) |>
  as.data.frame(terms_to_colnames = TRUE) |>
  as_tibble()

##  -- Save rds file --
saveRDS(preds_brm_mod_rank, file = "bayesian_logistic_quarter.rds")






