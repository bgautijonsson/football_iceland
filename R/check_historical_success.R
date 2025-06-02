library(tidyverse)
library(googlesheets4)
library(metill)
library(purrr)
library(here)
library(gt)
library(gtExtras)
library(geomtextpath)
theme_set(theme_metill())
gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))

bets <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1cJJo8SVJsMQvdzkM3h_pltjS55gbxvuSu_4ID_f8RUM/edit?gid=508088312#gid=508088312", 
  sheet = "Bets"
) |> 
  filter(
    deild == "iceland",
    dags_bet >= clock::date_build(2025, 4, 16)
  ) |> 
  mutate(
    dags_leikur = as_date(dags_leikur)
  ) |> 
  drop_na(prob)


#### Overall ####

bets |> 
  drop_na(prob) |> 
  # drop_na(win) |>
  arrange(dags_leikur) |> 
  crossing(
    iter = 1:4000
  ) |> 
  mutate(
    win = rbinom(n(), size = 1, prob = prob),
    change = win * payout - amount
  ) |> 
  mutate(
    cumul_change = cumsum(change),
    .by = iter
  ) |> 
  summarise(
    cumul_change = tail(cumul_change, 1),
    .by = c(dags_leikur, iter)
  ) |> 
  summarise(
    lower = quantile(cumul_change, 0.025),
    upper = quantile(cumul_change, 0.975),
    .by = c(dags_leikur)
  ) |> 
  inner_join(
    bets |> 
      drop_na(prob, ev) |> 
      arrange(dags_leikur) |> 
      mutate(
        cumul_ev = cumsum(ev * amount),
        change = na_if(change, 0),
        cumul_change = cumsum(change),
        type = if_else(
          is.na(change),
          "Prediction",
          "Observed"
        )
      ) |> 
      summarise(
        cumul_ev = tail(na.omit(cumul_ev), 1),
        cumul_change = tail(na.omit(cumul_change), 1),
        type = unique(type),
        .by = c(dags_leikur)
      )
  ) |> 
  ggplot(aes(dags_leikur, cumul_ev)) +
  geom_hline(
    yintercept = 0,
    lty = 2
  ) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.1
  ) +
  geom_line(
    data = ~filter(
      .x, 
      dags_leikur >= max(dags_leikur - 4)
    ),
    lty = 3,
    col = "#969696",
    # vjust = 1.5,
    linewidth = 1
  ) +
  geom_textline(
    data = ~filter(.x, type == "Observed"),
    aes(label = "Expected"),
    col = "#969696",
    size = 5,
    hjust = 0.85,
    # vjust = 1.5,
    linewidth = 1,
    text_smoothing = 40
  ) +
  geom_textline(
    aes(y = cumul_change, label = "Observed"),
    col = "#252525",
    size = 5,
    hjust = 0.1,
    # vjust = -0.5,
    linewidth = 1,
    text_smoothing = 30
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_currency(prefix = "", suffix = "€")
  ) +
  theme(
    plot.margin = margin(5, 15, 5, 15)
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparing cumulative expected profit to cumulative observed profit",
    subtitle = "Cumulative expected profit = cumsum(bet_amount * ev)"
  )

bets |> 
  drop_na(prob) |> 
  drop_na(win) |>
  arrange(dags_leikur) |> 
  crossing(
    iter = 1:10000
  ) |> 
  mutate(
    win = rbinom(n(), size = 1, prob = prob),
    change = win * payout - amount
  ) |> 
  summarise(
    change = sum(change),
    .by = c(dags_leikur, iter)
  ) |> 
  summarise(
    lower = quantile(change, 0.025),
    upper = quantile(change, 0.975),
    .by = c(dags_leikur)
  ) |> 
  inner_join(
    bets |> 
      drop_na(prob) |> 
      # drop_na(win) |>
      filter(
        !is.na(ev),
        # (!is.na(ev) | !is.na(p_push))
      ) |> 
      arrange(dags_leikur) |> 
      mutate(
        ev = ev * amount,
        change = na_if(change, 0),
        type = if_else(
          is.na(change),
          "Prediction",
          "Observed"
        )
      ) |> 
      summarise(
        ev = sum(ev, na.rm = TRUE),
        change = sum(change, na.rm = TRUE),
        type = unique(type),
        .by = c(dags_leikur)
      )
  ) |> 
  ggplot(aes(dags_leikur, ev)) +
  geom_hline(
    yintercept = 0,
    lty = 2
  ) +
  geom_linerange(
    aes(ymin = lower, ymax = upper),
    alpha = 0.3
  ) +
  # geom_point(
  #   data = ~filter(.x, type == "Observed"),
  #   aes(col = "Expected"),
  #   size = 5,
  # ) +
  geom_point(
    aes(y = change, col = "Observed"),
    size = 5,
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(20),
    labels = label_date_short()
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated(),
    breaks = breaks_pretty(10),
    labels = label_currency(prefix = "", suffix = "€")
  ) +
  theme(
    plot.margin = margin(5, 15, 5, 15),
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Comparing observed profit to 95% prediction intervals of expected profit for each day"
  )


