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

log_mod_rank <- glm(home_win ~ lead_diff + time_elapsed + rating_diff + 
                        time_elapsed:lead_diff + time_elapsed:rating_diff,
                      data = final_data, family = "binomial")

saveRDS(log_mod_rank, file = "log_full_rank.rds")


