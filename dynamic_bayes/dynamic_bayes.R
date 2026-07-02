library(dplyr)
library(tidyr)

# Load in 2025 season pbp data
mbb_pbp <-  hoopR::load_mbb_pbp(seasons = 2025)


# -- 2025 Season Data -- 


# Clean data
# create new vars: time_elapsed and lead_diff
mbb_pbp_2025 <- mbb_pbp |>
  mutate(time_elapsed = 2400 - end_game_seconds_remaining,
         lead_diff = home_score - away_score)

# create new var: home_win
mbb_pbp_2025 <- mbb_pbp_2025 |> group_by(game_id) |>
  mutate(
    home_win = if_else(
      is.na(end_game_seconds_remaining) & home_score > away_score, 1, 0
    )) |>
  mutate(home_win = max(home_win)) |>
  ungroup(game_id)

# fill in the NAs
mbb_pbp_2025 <- mbb_pbp_2025 |> mutate(
  time_elapsed = if_else(is.na(time_elapsed), 2400, time_elapsed),
  end_game_seconds_remaining = if_else(is.na(end_game_seconds_remaining), 0, end_game_seconds_remaining)
)

# only need these columns
mbb_pbp_2025 <- mbb_pbp_2025 |>
  select(game_id, time_elapsed, lead_diff, home_win)

# 1 row for 1 second, no duplicates, 14 million rows
mbb_pbp_2025_clean <- mbb_pbp_2025 |> 
  group_by(game_id) |> 
  complete(time_elapsed = 0:2400) |> 
  fill(lead_diff, home_win, .direction = "down") |> 
  ungroup() |> 
  arrange(game_id, time_elapsed, desc(row_number())) |>
  distinct(game_id, time_elapsed, .keep_all = TRUE)


# -- Dynamic Prior Logic --


# Based on table 1 and figure 1
# Figure 1 time intervals: [0, 1200), [1200, 1800), [1800, 2100), [2100, 2340), [2340, 2400]

# Function assigns alpha and beta values based on time and lead diff
get_dynamic_prior <- function(t, l) {
  
  if (t < 1200) {
    case_when(
      l >= 30  ~ list(alpha = 19, beta = 1),  # Red
      l >= 20  ~ list(alpha = 9,  beta = 1),  # Orange
      l >= 15  ~ list(alpha = 4,  beta = 1),  # Yellow
      l > -15  ~ list(alpha = 1,  beta = 1),  # White
      l > -20  ~ list(alpha = 1,  beta = 4),  # Green
      l > -30  ~ list(alpha = 1,  beta = 9),  # Light Blue
      TRUE     ~ list(alpha = 1,  beta = 19)  # Blue
    )
  } else if (t < 1800) {
    case_when(
      l >= 25  ~ list(alpha = 19, beta = 1),
      l >= 20  ~ list(alpha = 9,  beta = 1),
      l >= 10  ~ list(alpha = 4,  beta = 1),
      l > -10  ~ list(alpha = 1,  beta = 1),
      l > -20  ~ list(alpha = 1,  beta = 4),
      l > -25  ~ list(alpha = 1,  beta = 9),
      TRUE     ~ list(alpha = 1,  beta = 19)
    )
  } else if (t < 2100) {
    case_when(
      l >= 20  ~ list(alpha = 19, beta = 1),
      l >= 15  ~ list(alpha = 9,  beta = 1),
      l >= 10  ~ list(alpha = 4,  beta = 1),
      l > -5   ~ list(alpha = 1,  beta = 1),
      l > -15  ~ list(alpha = 1,  beta = 4),
      l > -20  ~ list(alpha = 1,  beta = 9),
      TRUE     ~ list(alpha = 1,  beta = 19)
    )
  } else if (t < 2340) {
    case_when(
      l >= 15  ~ list(alpha = 19, beta = 1),
      l >= 10  ~ list(alpha = 9,  beta = 1),
      l >= 5   ~ list(alpha = 4,  beta = 1),
      l > -5   ~ list(alpha = 1,  beta = 1),
      l > -10  ~ list(alpha = 1,  beta = 4),
      l > -15  ~ list(alpha = 1,  beta = 9),
      TRUE     ~ list(alpha = 1,  beta = 19)
    )
  } else {
    case_when(
      l >= 10  ~ list(alpha = 19, beta = 1),
      l >= 5   ~ list(alpha = 9,  beta = 1),
      l >= 3   ~ list(alpha = 4,  beta = 1),
      l > -3   ~ list(alpha = 1,  beta = 1),
      l > -5   ~ list(alpha = 1,  beta = 4),
      l > -10  ~ list(alpha = 1,  beta = 9),
      TRUE     ~ list(alpha = 1,  beta = 19)
    )
  }
}


#  -- Window Calculation --


# Subset data first as it won't run with all of the data
# 4 min for 50 games
set.seed(123)

unique_games <- mbb_pbp_2025_clean |> distinct(game_id) |> pull(game_id)

selected_games <- sample(unique_games, 6136)

mbb_pbp_subset <- mbb_pbp_2025_clean |> 
  filter(game_id %in% selected_games)


# Start by generating a unique base grid of cells present in the subset
base_grid <- mbb_pbp_subset |>
  distinct(time_elapsed, lead_diff)

# filter base grid
#base_grid <- base_grid |>
# filter(time_elapsed %% 10 == 0)

dynamic_bayes_heatmap <- base_grid |>
  rowwise() |>
  mutate(
    window_data = list({
      # Capture the grid cell coordinates
      current_t <- time_elapsed
      current_l <- lead_diff
      
      # Search the subset for matches within the window boundaries
      mbb_pbp_subset |>
        filter(
          time_elapsed >= (current_t - 3) & time_elapsed <= (current_t + 3),
          lead_diff >= (current_l - 2) & lead_diff <= (current_l + 2)
        ) |>
        summarise(
          N_window = n(), 
          n_window = sum(home_win)
        )
    })
  ) |>
  unnest(window_data)


# -- Conjugate Update --


dynamic_bayes_heatmap <- dynamic_bayes_heatmap |>
  rowwise() |>
  mutate(
    # Pull prior parameters
    prior = list(get_dynamic_prior(time_elapsed, lead_diff)),
    alpha = prior$alpha,
    beta = prior$beta,
    
    # Equation 2: posterior mean probability calculation
    win_prob = (n_window + alpha) / (N_window + alpha + beta)
  ) |>
  ungroup()


##  -- Save rds file --
saveRDS(dynamic_bayes_heatmap, file = "dynamic_bayes_full.rds")


