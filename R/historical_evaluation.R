#### Packages ####
library(tidyverse)
library(scales)
library(cmdstanr)
library(posterior)
library(metill)
library(geomtextpath)
library(ggtext)
library(ggh4x)
library(glue)
library(here)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

fit_historical_model <- function(start_date, end_date, sex = "male") {
  # Read data
  d <- read_csv(here("results", sex, "d.csv"))
  teams <- read_csv(here("results", sex, "teams.csv"))

  # Filter data for training period
  d_train <- d |>
    filter(
      date >= start_date,
      date <= end_date
    )

  # Get games to predict (up to one month after end_date)
  next_games <- d |>
    filter(
      date > end_date,
      date <= end_date + 30
    )

  # Calculate time differences between matches
  timediffs <- d_train |>
    pivot_longer(c(home, away)) |>
    select(
      game_nr,
      season,
      date,
      name,
      value
    ) |>
    mutate(
      time_diff = as.numeric(date - lag(date)),
      .by = value
    ) |>
    mutate(
      time_diff = if_else(is.na(time_diff), 7, time_diff),
      time_diff = pmin(time_diff, 100)
    ) |>
    select(-value, -season) |>
    pivot_wider(names_from = name, values_from = time_diff) |>
    rename(
      home_timediff = home,
      away_timediff = away
    )

  # Calculate round numbers for each team
  rounds <- d_train |>
    pivot_longer(c(home, away)) |>
    select(
      game_nr,
      season,
      date,
      name,
      value
    ) |>
    mutate(
      round = row_number(),
      .by = value
    ) |>
    select(-value, -season) |>
    pivot_wider(names_from = name, values_from = round) |>
    rename(
      home_round = home,
      away_round = away
    )

  # Calculate round numbers for each team and season
  season_rounds <- d_train |>
    pivot_longer(c(home, away)) |>
    select(
      game_nr,
      date,
      season,
      name,
      value
    ) |>
    mutate(
      season_round = row_number(),
      first_of_season = 1 * (season_round == 1),
      .by = c(season, value)
    ) |>
    select(-value, -season_round, -season) |>
    pivot_wider(names_from = name, values_from = first_of_season) |>
    rename(
      season_first = home
    ) |>
    select(-away)

  # Prepare model data
  model_d <- d_train |>
    inner_join(timediffs) |>
    inner_join(rounds) |>
    inner_join(season_rounds) |>
    inner_join(
      teams |> rename(home_nr = team_nr),
      by = join_by(home == team)
    ) |>
    inner_join(
      teams |> rename(away_nr = team_nr),
      by = join_by(away == team)
    )

  # Create time between matches matrix
  n_rounds <- max(c(model_d$home_round, model_d$away_round))
  time_between_matches <- matrix(
    0,
    nrow = nrow(teams),
    ncol = n_rounds
  )
  for (i in 1:nrow(model_d)) {
    time_between_matches[
      model_d$home_nr[i],
      model_d$home_round[i]
    ] <- model_d$home_timediff[i]
    time_between_matches[
      model_d$away_nr[i],
      model_d$away_round[i]
    ] <- model_d$away_timediff[i]
  }

  # Get latest game dates for each team
  latest_game_dates <- model_d |>
    pivot_longer(c(home, away)) |>
    select(date, team = value) |>
    filter(
      date == max(date),
      .by = team
    ) |>
    rename(latest_date = date)

  # Calculate time to next games
  next_game_dates <- next_games |>
    filter(division == 1) |>
    pivot_longer(c(home, away)) |>
    mutate(
      game_nr = row_number(),
      .by = value
    ) |>
    filter(
      game_nr == 1,
      .by = value
    ) |>
    select(next_date = date, team = value)

  time_to_next_games <- next_game_dates |>
    inner_join(latest_game_dates) |>
    mutate(
      timediff = as.numeric(next_date - latest_date)
    ) |>
    pull(timediff)

  # Get top teams
  top_teams <- next_games |>
    filter(division == 1) |>
    pivot_longer(c(home, away)) |>
    distinct(value) |>
    rename(team = value) |>
    inner_join(teams)

  # Prepare prediction data
  pred_d <- next_games |>
    inner_join(
      teams |> rename(home_nr = team_nr),
      by = join_by(home == team)
    ) |>
    inner_join(
      teams |> rename(away_nr = team_nr),
      by = join_by(away == team)
    )

  # Prepare Stan data
  stan_data <- list(
    K = nrow(teams),
    N = nrow(model_d),
    N_pred = nrow(pred_d),
    N_rounds = n_rounds,
    season_first = model_d$season_first,
    team1 = model_d$home_nr,
    team2 = model_d$away_nr,
    round1 = model_d$home_round,
    round2 = model_d$away_round,
    time_between_matches = time_between_matches,
    goals1 = model_d$home_goals,
    goals2 = model_d$away_goals,
    team1_pred = pred_d$home_nr,
    team2_pred = pred_d$away_nr,
    pred_timediff1 = pred_d$home_timediff,
    pred_timediff2 = pred_d$away_timediff,
    time_to_next_games = time_to_next_games,
    top_teams = top_teams$team_nr,
    N_top_teams = nrow(top_teams)
  )

  # Compile and fit model
  model <- cmdstan_model(
    here(
      "Stan",
      "model_poisson_attack_defense_correlation_rwmatches.stan"
    )
  )

  # Fit model
  results <- model$sample(
    data = stan_data,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    init = 0
  )

  # Get predictions
  posterior_goals <- results$draws(c("goals1_pred", "goals2_pred")) |>
    as_draws_df() |>
    as_tibble() |>
    pivot_longer(
      -c(.draw, .chain, .iteration),
      names_to = "parameter",
      values_to = "value"
    ) |>
    mutate(
      type = if_else(
        str_detect(parameter, "goals1"),
        "home_goals",
        "away_goals"
      ),
      game_nr = str_match(parameter, "d\\[(.*)\\]$")[, 2] |> as.numeric()
    ) |>
    select(.draw, type, game_nr, value) |>
    pivot_wider(names_from = type, values_from = value) |>
    inner_join(next_games, by = "game_nr") |>
    select(
      iteration = .draw,
      game_nr,
      division,
      date,
      home,
      away,
      home_goals,
      away_goals
    )

  # Save predictions
  dir.create(
    here("results", sex, "historical"),
    showWarnings = FALSE,
    recursive = TRUE
  )

  posterior_goals |>
    write_csv(
      here(
        "results",
        sex,
        "historical",
        paste0(
          format(end_date, "%Y-%m-%d"),
          ".csv"
        )
      )
    )

  # Return results for potential further analysis
  return(list(
    results = results,
    predictions = posterior_goals,
    training_data = d_train,
    prediction_data = next_games
  ))
}

# Example usage:
# fit_historical_model(
#   start_date = as.Date("2023-01-01"),
#   end_date = as.Date("2023-06-30"),
#   sex = "male"
# )
