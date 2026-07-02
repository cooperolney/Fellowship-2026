library(dplyr)
library(tidyr)
library(brms)
library(ggeffects)

final_data <- readRDS("final_data.rds")

# Data Subset
set.seed(123)

unique_games <- final_data |> distinct(game_id) |> pull(game_id)

selected_games <- sample(unique_games, 6136)

# subset of 50 games
subset_games <- final_data |> 
  filter(game_id %in% selected_games) #|>
  #filter(time_elapsed %% 24 == 0)

# scale time and score so algorithm runs better

min_lead_diff <- min(subset_games$lead_diff)
max_lead_diff <- max(subset_games$lead_diff)

min_rating_diff <- min(subset_games$rating_diff)
max_rating_diff <- max(subset_games$rating_diff)

## puts all predictors on the [0, 1] scale
subset_games <- subset_games |>
  mutate(
    time_scaled = (time_elapsed - 0) / (2400 - 0),
    lead_scaled = (lead_diff - min_lead_diff) / (max_lead_diff - min_lead_diff),
    rating_scaled = (rating_diff - min_rating_diff) / (max_rating_diff - min_rating_diff)
  )


# Model
brm_mod_rank <- brm(home_win ~ lead_scaled + time_scaled + rating_scaled + time_scaled:lead_scaled + time_scaled:rating_scaled,
                    data = subset_games,
                    family = bernoulli(),
                    prior = c(prior(normal(0, 4), class = "Intercept"))
)


# Plot
# model predictions: will need to change the values to correspond
# with scaled versions of the predictors.
# preds_brm_mod_rank <- ggpredict(brm_mod_rank, terms = c("time_elapsed [0:2400 by=10]", 
#                                                         "lead_diff [-15, -10, -5, 0, 5, 10, 15]")) |>
#   as.data.frame(terms_to_colnames = TRUE) |>
#   as_tibble()

##  -- Save rds file --
saveRDS(brm_mod_rank, file = "brm_full.rds")






