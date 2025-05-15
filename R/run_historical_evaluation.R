#### Packages ####
library(tidyverse)
library(lubridate)
library(glue)
library(here)

# Source the historical evaluation function
source(here("R", "historical_evaluation.R"))

run_historical_evaluation <- function(sex = "male", window_years = 3) {
  # Read data
  d <- read_csv(here("data", sex, "data.csv"))

  # Get all unique game dates
  game_dates <- d |>
    filter(dags < max(dags)) |>
    arrange(dags) |>
    pull(dags) |>
    unique() |>
    rev()

  # Create results directory
  if (!dir.exists(here("results", sex, "historical"))) {
    dir.create(
      here("results", sex, "historical"),
      showWarnings = FALSE,
      recursive = TRUE
    )
  }

  # Loop over dates in reverse
  for (i in seq_along(game_dates)) {
    end_date <- game_dates[i]
    start_date <- end_date - years(window_years)

    # Skip if we don't have enough historical data
    if (start_date < min(game_dates)) {
      next
    }

    # Skip if we've already processed this date
    if (
      file.exists(
        here(
          "results",
          sex,
          "historical",
          paste0(format(end_date, "%Y-%m-%d"), ".csv")
        )
      )
    ) {
      next
    }

    # Fit model and save predictions
    tryCatch(
      {
        message(glue("Processing {format(end_date, '%Y-%m-%d')}"))
        fit_historical_model(
          start_date = start_date,
          end_date = end_date,
          sex = sex
        )
      },
      error = function(e) {
        message(glue(
          "Error processing {format(end_date, '%Y-%m-%d')}: {e$message}"
        ))
      }
    )
  }
}

# Example usage:
run_historical_evaluation(sex = "male", window_years = 3)
