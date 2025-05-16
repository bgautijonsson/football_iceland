library(tidyverse)
library(here)
library(metill)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

#### League Placement ####
d <- here("results", "male", "historical") |> 
  list.files(full.names = TRUE, pattern = "^[0-9].*") |> 
  here("placement.csv") |> 
  map(
    \(x) {
      read_csv(x) |> 
        mutate(
          fit_date = x
        )
    }
  ) |> 
  list_rbind() |> 
  mutate(
    fit_date = as_date(fit_date)
  ) |> 
  # filter(
  #   !fit_date %in% clock::date_build(
  #     2025, 
  #     c(4, 5, 5), 
  #     c(27, 4, 10)
  #   )
  # ) |> 
  mutate(
    ordered_mean = unique(mean[fit_date == max(fit_date)]),
    .by = team
  )

date_range <- d$fit_date |> range()



d |> 
  arrange(
    fit_date, team, placement
  ) |> 
  mutate(
    team = fct_reorder(team, ordered_mean, .na_rm = TRUE)
  ) |> 
  inner_join(
    d |> 
      distinct(fit_date) |> 
      mutate(
        from_date = fit_date,
        to_date = lead(fit_date, default = max(fit_date) + 1)
      )
  ) |> 
  ggplot(aes(fit_date, placement)) +
  geom_rect(
    aes(
      xmin = from_date, xmax = to_date,
      ymin = placement - 0.5,
      ymax = placement + 0.5,
      fill = p,
      alpha = p
    )
  ) +
  geom_step(
    data = d |> 
      filter(
        p >= 0.95,
        .by = c(fit_date, team)
      ) |> 
      summarise(
        mean = mean(placement),
        .by = c(fit_date, team, ordered_mean)
      ) |> 
      mutate(
        team = fct_reorder(team, ordered_mean, .na_rm = TRUE)
      ),
    aes(y = mean),
    col = "red",
    linewidth = 1,
    position = position_nudge(x = 0.5)
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    labels = label_date_short(),
    breaks = breaks_pretty(8)
  ) +
  scale_y_reverse(
    guide = ggh4x::guide_axis_truncated(),
    breaks = 1:12
  ) +
  scale_fill_gradient(
    low = "#fdfcfc",
    high = "black"
  ) +
  facet_wrap("team") +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Þróun spár fótboltalíkans metils fyrir Bestu deild karla",
    subtitle = str_c(
      "Líkindadreifing yfir sæti liða í deildinni",
      " | ",
      "Líklegri niðurstaða er dekkri",
      " | ",
      "Rauð lína er líklegasta niðurstaða hvers liðs"
    )
  )

p <- crossing(
  fit_date = seq.Date(
    from = date_range[1], 
    to = date_range[2], 
    by = "day"
  ),
  team = unique(d$team),
  placement = 1:12
) |> 
  left_join(d) |>
  arrange(
    fit_date, team, placement
  ) |> 
  mutate(
    p_raw = if_else(
      fit_date %in% date_range,
      coalesce(p_raw, 0),
      p_raw
    ),
    p_raw = zoo::na.approx(p_raw),
    p = if_else(
      fit_date %in% date_range,
      coalesce(p, 0),
      p
    ),
    p = zoo::na.approx(p),
    .by = c(team, placement)
  ) |> 
  complete(
    team, placement, fit_date,
    fill = list(
      n = 0,
      p_raw = 0,
      p = 0
    )
  ) |> 
  mutate(
    team = fct_reorder(team, ordered_mean, .na_rm = TRUE)
  ) |> 
  ggplot(aes(fit_date, placement)) +
  geom_raster(aes(fill = p)) +
  geom_smooth(
    data = crossing(
      fit_date = seq.Date(
        from = date_range[1], 
        to = date_range[2], 
        by = "day"
      ),
      team = unique(d$team)
    ) |> 
      left_join(
        d |> 
          filter(p == 1) |> 
          mutate(
            mean = placement
          ) |> 
          distinct(
            fit_date, team, mean, ordered_mean
          ) |> 
          mutate(
            team = fct_reorder(team, ordered_mean, .na_rm = TRUE)
          )
      ) |> 
      arrange(fit_date) |> 
      mutate(
        mean = zoo::na.approx(mean),
        .by = team
      ) |> 
      mutate(
        team = fct_reorder(team, -ordered_mean, .na_rm = TRUE)
      ),
    aes(y = mean),
    col = "red",
    se = 0,
    span = 0.6
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    labels = label_date_short(),
    breaks = breaks_pretty(8)
  ) +
  scale_y_reverse(
    breaks = 1:12,
    guide = ggh4x::guide_axis_truncated()
  ) +
  # scale_fill_distiller(
  #   palette = "Greys",
  #   direction = 1
  # ) +
  scale_fill_gradient(
    low = "#fdfcfc",
    high = "black"
  ) +
  facet_wrap("team") +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Þróun spár fótboltalíkans metils fyrir Bestu deild karla",
    subtitle = str_c(
      "Líkindadreifing yfir sæti liða í deildinni",
      " | ",
      "Líklegri niðurstaða er dekkri",
      " | ",
      "Rauð lína er líklegasta niðurstaða hvers liðs"
    )
  )

p

ggsave(
  plot = p,
  filename = here("results", "male", "historical", "placement.png"),
  width = 8,
  height = 0.7 * 8,
  scale = 1.3
)

p <- crossing(
  fit_date = seq.Date(
    from = date_range[1], 
    to = date_range[2], 
    by = "day"
  ),
  team = unique(d$team),
  placement = 1:12
) |> 
  left_join(
    d |> 
      mutate(
        p_raw = n / sum(n),
        .by = c(fit_date, placement)
      ) |> 
      mutate(
        p = p_raw / max(p_raw),
        .by = c(placement, fit_date)
      )
  ) |>
  arrange(
    fit_date
  ) |> 
  mutate(
    p_raw = if_else(
      fit_date %in% date_range,
      coalesce(p_raw, 0),
      p_raw
    ),
    p_raw = zoo::na.approx(p_raw),
    p = if_else(
      fit_date %in% date_range,
      coalesce(p, 0),
      p
    ),
    p = zoo::na.approx(p),
    .by = c(team, placement)
  ) |> 
  complete(
    team, placement, fit_date,
    fill = list(
      n = 0,
      p_raw = 0,
      p = 0
    )
  ) |> 
  mutate(
    team = fct_reorder(team, -ordered_mean, .na_rm = TRUE)
  ) |> 
  ggplot(aes(fit_date, team, fill = p)) +
  geom_raster() +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    labels = label_date_short()
  ) +
  scale_y_discrete() +
  scale_fill_gradient(
    low = "#fdfcfc",
    high = "black"
  ) +
  facet_wrap("placement") +
  theme(legend.position = "none") +
  labs(
    x = NULL,
    y = NULL,
    title = "Þróun spár fótboltalíkans metils fyrir Bestu deild karla",
    subtitle = "Líkindadreifing yfir hvaða lið eru líklegust til að lenda í tilteknu sæti"
  )


ggsave(
  plot = p,
  filename = here("results", "male", "historical", "placement_byplacement.png"),
  width = 8,
  height = 0.7 * 8,
  scale = 1.3
)


#### League Points ####
d <- here("results", "male", "historical") |> 
  list.files(full.names = TRUE, pattern = "^[0-9].*") |> 
  here("league_points.csv") |> 
  map(
    \(x) {
      read_csv(x) |> 
        mutate(
          fit_date = x
        )
    }
  ) |> 
  list_rbind() |> 
  mutate(
    fit_date = as_date(fit_date),
    team = str_replace(team, " \\([0-9]+\\%\\)", "")
  ) |> 
  filter(
    !fit_date %in% clock::date_build(
      2025,
      c(4, 5, 5),
      c(27, 4, 10)
    )
  ) |>
  mutate(
    ordered_mean = unique(mean[fit_date == max(fit_date)]),
    .by = team
  )

d |> 
  arrange(
    fit_date, team, points
  ) |> 
  mutate(
    team = fct_reorder(team, ordered_mean, .na_rm = TRUE)
  ) |> 
  inner_join(
    d |> 
      distinct(fit_date) |> 
      mutate(
        from_date = fit_date,
        to_date = lead(fit_date, default = max(fit_date) + 1)
      )
  ) |> 
  ggplot(aes(fit_date, points)) +
  geom_rect(
    aes(
      xmin = from_date, xmax = to_date,
      ymin = points - 0.5,
      ymax = points + 0.5,
      fill = p,
      alpha = p
    )
  ) +
  geom_step(
    data = d |> 
      distinct(fit_date, team, mean, ordered_mean) |> 
      mutate(
        team = fct_reorder(team, ordered_mean, .na_rm = TRUE)
      ),
    aes(y = mean),
    col = "red",
    linewidth = 1,
    position = position_nudge(x = 0.5)
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    labels = label_date_short(),
    breaks = breaks_pretty(8)
  ) +
  scale_y_reverse(
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_fill_gradient(
    low = "#fdfcfc",
    high = "black"
  ) +
  facet_wrap("team") +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Þróun spár fótboltalíkans metils fyrir Bestu deild karla",
    subtitle = str_c(
      "Líkindadreifing yfir sæti liða í deildinni",
      " | ",
      "Líklegri niðurstaða er dekkri",
      " | ",
      "Rauð lína er líklegasta niðurstaða hvers liðs"
    )
  )

date_range <- d$fit_date |> range()

p <- crossing(
  fit_date = seq.Date(
    from = date_range[1], 
    to = date_range[2], 
    by = "day"
  ),
  team = unique(d$team),
  points = seq(0, max(d$points))
) |> 
  left_join(d) |>
  arrange(
    fit_date, team, points
  ) |> 
  mutate(
    p_raw = if_else(
      fit_date %in% date_range,
      coalesce(p_raw, 0),
      p_raw
    ),
    p_raw = zoo::na.approx(p_raw),
    p = if_else(
      fit_date %in% date_range,
      coalesce(p, 0),
      p
    ),
    p = zoo::na.approx(p),
    .by = c(team, points)
  ) |> 
  complete(
    team, points, fit_date,
    fill = list(
      n = 0,
      p_raw = 0,
      p = 0
    )
  ) |>
  mutate(
    team = fct_reorder(team, -ordered_mean, .na_rm = TRUE)
  ) |> 
  ggplot(aes(fit_date, points)) +
  geom_raster(aes(fill = p)) +
  geom_smooth(
    data = crossing(
      fit_date = seq.Date(
        from = date_range[1], 
        to = date_range[2], 
        by = "day"
      ),
      team = unique(d$team)
    ) |> 
      left_join(
        d |> 
          distinct(fit_date, team, mean, ordered_mean)
      ) |> 
      arrange(fit_date) |> 
      mutate(
        mean = zoo::na.approx(mean),
        .by = team
      ) |> 
      mutate(
        team = fct_reorder(team, -ordered_mean, .na_rm = TRUE)
      ),
    aes(y = mean),
    col = "red",
    se = 0,
    span = 0.5
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    labels = label_date_short(),
    breaks = breaks_pretty(8)
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated()
  ) +
  # scale_fill_distiller(
  #   palette = "Greys",
  #   direction = 1
  # ) +
  scale_fill_gradient(
    low = "#fdfcfc",
    high = "black"
  ) +
  facet_wrap("team") +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Þróun spár fótboltalíkans metils fyrir Bestu deild karla",
    subtitle = str_c(
      "Líkindadreifing yfir stigafjölda liða",
      " | ",
      "Líklegri niðurstaða er dekkri",
      " | ",
      "Rauð lína er meðalspá hvers liðs"
    )
  )

p


ggsave(
  plot = p,
  filename = here("results", "male", "historical", "points.png"),
  width = 8,
  height = 0.7 * 8,
  scale = 1.3
)


d |> 
  arrange(
    fit_date, team, points
  ) |> 
  mutate(
    team = fct_reorder(team, -ordered_mean, .na_rm = TRUE)
  ) |> 
  ggplot(aes(fit_date, points)) +
  geom_step(
    aes(col = p, group = points),
    linewidth = 1
  ) +
  geom_step(
    data = d |> 
      distinct(fit_date, team, mean, ordered_mean) |> 
      mutate(
        team = fct_reorder(team, -ordered_mean, .na_rm = TRUE)
      ),
    aes(y = mean),
    col = "red",
    linewidth = 1
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    labels = label_date_short(),
    breaks = breaks_pretty(8)
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated()
  ) +
  # scale_fill_distiller(
  #   palette = "Greys",
  #   direction = 1
  # ) +
  scale_colour_gradient(
    low = "#fdfcfc",
    high = "black"
  ) +
  facet_wrap("team") +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Þróun spár fótboltalíkans metils fyrir Bestu deild karla",
    subtitle = str_c(
      "Líkindadreifing yfir stigafjölda liða",
      " | ",
      "Líklegri niðurstaða er dekkri",
      " | ",
      "Rauð lína er meðalspá hvers liðs"
    )
  )

d |> 
  arrange(
    fit_date, team, points
  ) |> 
  mutate(
    team = fct_reorder(team, -ordered_mean, .na_rm = TRUE)
  ) |> 
  inner_join(
    d |> 
      distinct(fit_date) |> 
      mutate(
        from_date = fit_date,
        to_date = lead(fit_date, default = max(fit_date) + 1)
      )
  ) |> 
  ggplot(aes(fit_date, points)) +
  geom_rect(
    aes(
      xmin = from_date, xmax = to_date,
      ymin = points - 0.5,
      ymax = points + 0.5,
      fill = p
    )
  ) +
  geom_step(
    data = d |> 
      distinct(fit_date, team, mean, ordered_mean) |> 
      mutate(
        team = fct_reorder(team, -ordered_mean, .na_rm = TRUE)
      ),
    aes(y = mean),
    col = "red",
    linewidth = 1,
    position = position_nudge(x = 0.5)
  ) +
  scale_x_date(
    guide = ggh4x::guide_axis_truncated(),
    labels = label_date_short(),
    breaks = breaks_pretty(8)
  ) +
  scale_y_continuous(
    guide = ggh4x::guide_axis_truncated()
  ) +
  scale_fill_gradient(
    low = "#fdfcfc",
    high = "black"
  ) +
  facet_wrap("team") +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Þróun spár fótboltalíkans metils fyrir Bestu deild karla",
    subtitle = str_c(
      "Líkindadreifing yfir stigafjölda liða",
      " | ",
      "Líklegri niðurstaða er dekkri",
      " | ",
      "Rauð lína er meðalspá hvers liðs"
    )
  )

ggsave(
  filename = here("results", "male", "historical", "points.png"),
  width = 8,
  height = 0.7 * 8,
  scale = 1.3
)
