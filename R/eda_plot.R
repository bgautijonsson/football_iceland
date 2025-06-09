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

sex <- "male"

from_season <- 2021

#### Data Prep ####
results <- read_rds(here("results", sex, "fit.rds"))

d <- read_csv(here("results", sex, "d.csv"))
teams <- read_csv(here("results", sex, "teams.csv"))
next_games <- read_csv(here("results", sex, "next_games.csv"))
top_teams <- read_csv(here("results", sex, "top_teams.csv"))
pred_d <- read_csv(here("results", sex, "pred_d.csv"))
model_d <- read_csv(here("results", sex, "model_d.csv"))

offense <- results$summary("offense")
defense <- results$summary("defense")

plot_dat <- offense |>
  mutate(
    round = str_match(variable, "\\[([0-9]+)")[, 2] |> as.numeric(),
    team_nr = str_match(variable, "([0-9]+)\\]")[, 2] |> as.numeric()
  ) |>
  select(round, team_nr, median, q5, q95) |>
  inner_join(
    teams
  ) |>
  mutate(
    variable = "Sóknarstyrkur"
  ) |>
  bind_rows(
    defense |>
      mutate(
        round = str_match(variable, "\\[([0-9]+)")[, 2] |> as.numeric(),
        team_nr = str_match(variable, "([0-9]+)\\]")[, 2] |> as.numeric()
      ) |>
      select(round, team_nr, median, q5, q95) |>
      inner_join(
        teams
      ) |>
      mutate(
        variable = "Varnarstyrkur"
      )
  )

plot_dat |>
  filter(
    team %in%
      c(
        "KR",
        # "Víkingur R.",
        "Vestri"
      )
  ) |>
  inner_join(
    d |>
      pivot_longer(c(home, away)) |>
      select(
        season,
        game_nr,
        date,
        name,
        value
      ) |>
      filter(
        season >= from_season
      ) |>
      mutate(
        round = row_number(),
        .by = value
      ) |>
      select(
        season,
        round,
        team = value,
        date
      )
  ) |>
  ggplot(aes(date, median)) +
  geom_hline(
    yintercept = 0,
    lty = 2,
    alpha = 0.3
  ) +
  geom_ribbon(
    aes(ymin = q5, ymax = q95, fill = team, group = paste(team, season)),
    alpha = 0.1
  ) +
  geom_line(
    aes(
      col = team,
      group = paste(team, season)
    ),
    linewidth = 1
  ) +
  scale_x_date(
    guide = guide_axis_truncated(),
    breaks = breaks_width("4 month"),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = guide_axis_truncated()
  ) +
  scale_colour_brewer(
    palette = "Set1"
  ) +
  scale_fill_brewer(
    palette = "Set1"
  ) +
  facet_wrap("variable", ncol = 1) +
  labs(
    title = "Þróun styrks nokkurra félagsliða í Bestu Deild karla",
    x = NULL,
    y = NULL,
    col = NULL,
    fill = NULL
  )

ggsave(
  filename = "temp_male.png",
  width = 8,
  height = 0.5 * 8,
  scale = 1.4
)

