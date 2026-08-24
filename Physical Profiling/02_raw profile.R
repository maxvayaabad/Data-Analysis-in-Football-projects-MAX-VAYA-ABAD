# ============================================================
# RAW / DESCRIPTIVE PLAYER PHYSICAL PROFILE
#
# Input:
#   data/player_session_profile_tidy_final.csv
#
# Unit of observation:
#   one row = one player in one training session
#
# This script creates raw descriptive player profiles only.
# It does not fit mixed models, adjust for context or impute data.
# ============================================================


# ------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(patchwork)


# ------------------------------------------------------------
# 1. CONFIGURATION
# ------------------------------------------------------------

external_metrics <- c(
  "distance_per_min",
  "sprint_dist_per_min",
  "LSA_per_min",
  "MSR_per_min",
  "HSR_per_min",
  "HSR_sprint_per_min",
  "no_sprints_per_min",
  "accelerations_per_min",
  "decelerations_per_min"
)

external_metric_labels <- c(
  distance_per_min = "Distance / min",
  sprint_dist_per_min = "Sprint dist. / min",
  LSA_per_min = "LSA / min",
  MSR_per_min = "MSR / min",
  HSR_per_min = "HSR / min",
  HSR_sprint_per_min = "HSR + Sprint / min",
  no_sprints_per_min = "Sprints / min",
  accelerations_per_min = "Accelerations / min",
  decelerations_per_min = "Decelerations / min"
)

external_metric_blocks <- c(
  distance_per_min = "Volume",
  sprint_dist_per_min = "Volume",
  LSA_per_min = "Running types",
  MSR_per_min = "Running types",
  HSR_per_min = "Running types",
  HSR_sprint_per_min = "Intensity",
  no_sprints_per_min = "Intensity",
  accelerations_per_min = "Mechanical load",
  decelerations_per_min = "Mechanical load"
)

block_colours <- c(
  "Volume" = "#2C7FB8",
  "Running types" = "#41AB5D",
  "Intensity" = "#D95F0E",
  "Mechanical load" = "#756BB1"
)

internal_metrics <- c("RPE", "TRIMP_per_min", "High_HR_pct")

internal_metric_labels <- c(
  RPE = "RPE",
  TRIMP_per_min = "TRIMP / min",
  High_HR_pct = "High HR %"
)

profile_colour <- "#1F4E79"
radar_rings <- c(20, 40, 60, 80, 100)


# ------------------------------------------------------------
# 2. LOAD DATA AND CHECK REQUIRED VARIABLES
# ------------------------------------------------------------

data <- read_csv("player_session_profile_tidy_final.csv")%>%
  mutate(Date = ymd(Date))

required_variables <- c(
  "PlayerName",
  "PLAYING.POSITION",
  "Team",
  "hr_duration",
  "RPE",
  "TRIMPmod",
  "HRzone4",
  "HRzone5",
  external_metrics
)

missing_variables <- setdiff(required_variables, names(data))

if (length(missing_variables) > 0) {
  stop(
    "Missing required variables: ",
    paste(missing_variables, collapse = ", "),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 3. HELPER FUNCTIONS
# ------------------------------------------------------------

percentile_0_100 <- function(x) {
  if (sum(!is.na(x)) == 0) {
    return(rep(NA_real_, length(x)))
  }

  percent_rank(x) * 100
}

evidence_category <- function(n_sessions) {
  case_when(
    is.na(n_sessions) ~ NA_character_,
    n_sessions >= 45 ~ "Very high",
    n_sessions >= 35 ~ "High",
    n_sessions >= 25 ~ "Moderate",
    n_sessions >= 20 ~ "Low",
    TRUE ~ "Very low"
  )
}

most_common_value <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(NA_character_)
  }

  counts <- table(x)
  names(counts)[counts == max(counts)] %>%
    sort() %>%
    first()
}

check_player_exists <- function(player_name) {
  if (!player_name %in% player_profile_metadata$PlayerName) {
    stop(
      "Player not found: ", player_name,
      ". Check spelling against player_profile_metadata$PlayerName.",
      call. = FALSE
    )
  }
}

reference_column <- function(reference) {
  reference <- match.arg(reference, c("overall", "position"))

  if (reference == "overall") {
    "overall_percentile"
  } else {
    "position_percentile"
  }
}

make_density_data <- function(data, x_var, group_var, n_points = 180) {
  data %>%
    filter(!is.na(.data[[x_var]])) %>%
    group_by(across(all_of(group_var))) %>%
    group_modify(~ {
      values <- .x[[x_var]]

      if (length(unique(values)) < 2) {
        return(tibble(raw_value = values[1], density = 0))
      }

      density_values <- density(values, n = n_points, na.rm = TRUE)

      tibble(
        raw_value = density_values$x,
        density = density_values$y
      )
    }) %>%
    ungroup()
}


# ------------------------------------------------------------
# 4. PLAYER-LEVEL EXTERNAL LOAD PROFILE
# ------------------------------------------------------------

# Team is a player attribute. If a player has multiple Team values, the modal
# Team is used, with alphabetical tie-breaking for deterministic output.
player_external_summary <- data %>%
  group_by(PlayerName, PLAYING.POSITION) %>%
  summarise(
    Team = most_common_value(Team),
    n_external = sum(if_all(all_of(external_metrics), ~ !is.na(.x))),
    across(
      all_of(external_metrics),
      ~ median(.x, na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      all_of(external_metrics),
      ~ if_else(is.nan(.x), NA_real_, .x)
    )
  )

position_reference <- player_external_summary %>%
  count(PLAYING.POSITION, name = "n_position_players")

external_percentiles_long <- player_external_summary %>%
  left_join(position_reference, by = "PLAYING.POSITION") %>%
  pivot_longer(
    all_of(external_metrics),
    names_to = "metric",
    values_to = "raw_median"
  ) %>%
  mutate(
    metric = factor(metric, levels = external_metrics),
    block = external_metric_blocks[as.character(metric)],
    metric_label = external_metric_labels[as.character(metric)]
  ) %>%
  group_by(metric) %>%
  mutate(overall_percentile = percentile_0_100(raw_median)) %>%
  ungroup() %>%
  group_by(PLAYING.POSITION, metric) %>%
  mutate(position_percentile = percentile_0_100(raw_median)) %>%
  ungroup()


# ------------------------------------------------------------
# 5. PLAYER-LEVEL INTERNAL RESPONSE PROFILE
# ------------------------------------------------------------

# HRzone4 and HRzone5 appear to be expressed in minutes because their zone
# totals align with hr_duration in the source rows inspected. High_HR_pct is
# therefore calculated as high-HR minutes divided by HR duration.
data_internal <- data %>%
  mutate(
    TRIMP_per_min = if_else(
      !is.na(TRIMPmod) & !is.na(hr_duration) & hr_duration > 0,
      TRIMPmod / hr_duration,
      NA_real_
    ),
    High_HR_pct = if_else(
      !is.na(HRzone4) & !is.na(HRzone5) & !is.na(hr_duration) & hr_duration > 0,
      100 * (HRzone4 + HRzone5) / hr_duration,
      NA_real_
    )
  )

player_internal_summary <- data_internal %>%
  group_by(PlayerName, PLAYING.POSITION) %>%
  summarise(
    Team = most_common_value(Team),
    n_HR = sum(!is.na(TRIMP_per_min)),
    n_RPE = sum(!is.na(RPE)),
    RPE = median(RPE, na.rm = TRUE),
    TRIMP_per_min = median(TRIMP_per_min, na.rm = TRUE),
    High_HR_pct = median(High_HR_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(all_of(internal_metrics), ~ if_else(is.nan(.x), NA_real_, .x))
  )

internal_percentiles_long <- player_internal_summary %>%
  left_join(position_reference, by = "PLAYING.POSITION") %>%
  pivot_longer(
    all_of(internal_metrics),
    names_to = "metric",
    values_to = "raw_median"
  ) %>%
  mutate(
    metric = factor(metric, levels = internal_metrics),
    metric_label = internal_metric_labels[as.character(metric)]
  ) %>%
  group_by(metric) %>%
  mutate(overall_percentile = percentile_0_100(raw_median)) %>%
  ungroup() %>%
  group_by(PLAYING.POSITION, metric) %>%
  mutate(position_percentile = percentile_0_100(raw_median)) %>%
  ungroup()


# ------------------------------------------------------------
# 6. PLAYER PROFILE METADATA
# ------------------------------------------------------------

player_profile_metadata <- player_external_summary %>%
  select(PlayerName, PLAYING.POSITION, Team, n_external) %>%
  left_join(
    player_internal_summary %>%
      select(PlayerName, n_HR, n_RPE),
    by = "PlayerName"
  ) %>%
  left_join(position_reference, by = "PLAYING.POSITION") %>%
  mutate(
    external_evidence = evidence_category(n_external),
    HR_evidence = evidence_category(n_HR),
    RPE_evidence = evidence_category(n_RPE)
  )


# ------------------------------------------------------------
# 7. PLOTTING FUNCTIONS
# ------------------------------------------------------------

plot_external_radar <- function(player_name, reference = c("overall", "position")) {
  reference <- match.arg(reference)
  percentile_col <- reference_column(reference)
  check_player_exists(player_name)

  player_data <- external_percentiles_long %>%
    filter(PlayerName == player_name) %>%
    arrange(metric) %>%
    mutate(
      percentile = .data[[percentile_col]],
      angle = 2 * pi * (row_number() - 1) / n(),
      x = percentile * sin(angle),
      y = percentile * cos(angle),
      label_x = 116 * sin(angle),
      label_y = 116 * cos(angle),
      hjust_label = case_when(
        label_x > 10 ~ 0,
        label_x < -10 ~ 1,
        TRUE ~ 0.5
      )
    )

  ring_data <- crossing(
    ring = radar_rings,
    angle = seq(0, 2 * pi, length.out = 361)
  ) %>%
    mutate(
      x = ring * sin(angle),
      y = ring * cos(angle)
    )

  axis_data <- player_data %>%
    transmute(
      metric,
      metric_label,
      block,
      x = 0,
      y = 0,
      xend = 100 * sin(angle),
      yend = 100 * cos(angle),
      label_x,
      label_y,
      hjust_label
    )

  polygon_data <- bind_rows(player_data, slice(player_data, 1))
  block_legend_data <- axis_data %>%
    distinct(block) %>%
    mutate(x = 0, y = 0, xend = 0, yend = 0)

  ggplot() +
    geom_path(
      data = ring_data,
      aes(x, y, group = ring),
      colour = "grey88",
      linewidth = 0.35
    ) +
    geom_text(
      data = tibble(ring = radar_rings, x = 0, y = radar_rings),
      aes(x, y, label = ring),
      colour = "grey58",
      size = 2.8,
      vjust = -0.4
    ) +
    geom_segment(
      data = axis_data,
      aes(x = x, y = y, xend = xend, yend = yend),
      colour = "grey82",
      linewidth = 0.35
    ) +
    geom_segment(
      data = axis_data,
      aes(x = 103 * sin(atan2(xend, yend)),
          y = 103 * cos(atan2(xend, yend)),
          xend = 109 * sin(atan2(xend, yend)),
          yend = 109 * cos(atan2(xend, yend)),
          colour = block),
      linewidth = 1.8,
      lineend = "round"
    ) +
    geom_polygon(
      data = player_data,
      aes(x, y),
      fill = profile_colour,
      colour = profile_colour,
      alpha = 0.16,
      linewidth = 0.8
    ) +
    geom_path(
      data = polygon_data,
      aes(x, y),
      colour = profile_colour,
      linewidth = 1.2,
      linejoin = "round"
    ) +
    geom_point(
      data = player_data,
      aes(x, y),
      colour = profile_colour,
      fill = "white",
      shape = 21,
      stroke = 1,
      size = 2.7
    ) +
    geom_text(
      data = axis_data,
      aes(label_x, label_y, label = metric_label, colour = block, hjust = hjust_label),
      size = 3.4,
      fontface = "bold",
      lineheight = 0.95,
      show.legend = FALSE
    ) +
    geom_segment(
      data = block_legend_data,
      aes(x = x, y = y, xend = xend, yend = yend, colour = block),
      linewidth = 1.8,
      lineend = "round"
    ) +
    scale_colour_manual(values = block_colours) +
    coord_equal(xlim = c(-135, 135), ylim = c(-130, 135), clip = "off") +
    labs(
      title = "External Load Radar",
      subtitle = paste(str_to_title(reference), "percentile profile"),
      colour = "Block"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 15, colour = "#111111"),
      plot.subtitle = element_text(size = 10.5, colour = "grey35"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 9.5),
      legend.key.width = grid::unit(18, "pt"),
      plot.margin = margin(8, 18, 8, 18)
    )
}

plot_internal_response <- function(player_name, reference = c("overall", "position")) {
  reference <- match.arg(reference)
  percentile_col <- reference_column(reference)
  check_player_exists(player_name)

  player_metadata <- player_profile_metadata %>%
    filter(PlayerName == player_name) %>%
    slice(1)

  reference_data <- internal_percentiles_long

  if (reference == "position") {
    reference_data <- reference_data %>%
      filter(PLAYING.POSITION == player_metadata$PLAYING.POSITION)
  }

  player_data <- internal_percentiles_long %>%
    filter(PlayerName == player_name) %>%
    mutate(
      percentile = .data[[percentile_col]],
      metric_label = factor(
        metric_label,
        levels = unname(internal_metric_labels[internal_metrics])
      ),
      value_label = case_when(
        metric == "RPE" ~ sprintf("%.1f", raw_median),
        metric == "TRIMP_per_min" ~ sprintf("%.2f", raw_median),
        metric == "High_HR_pct" ~ sprintf("%.0f%%", raw_median),
        TRUE ~ sprintf("%.2f", raw_median)
      ),
      percentile_label = if_else(
        is.na(percentile),
        "P NA",
        paste0("P", round(percentile))
      )
    )

  density_data <- reference_data %>%
    mutate(
      metric_label = factor(
        metric_label,
        levels = unname(internal_metric_labels[internal_metrics])
      )
    ) %>%
    make_density_data("raw_median", "metric_label") %>%
    group_by(metric_label) %>%
    mutate(
      density_scaled = {
        max_density <- max(density, na.rm = TRUE)

        if (max_density > 0) {
          density / max_density
        } else {
          rep(0, n())
        }
      }
    ) %>%
    ungroup()

  highlighted_density <- density_data %>%
    left_join(
      player_data %>%
        select(metric_label, player_value = raw_median),
      by = "metric_label"
    ) %>%
    filter(raw_value <= player_value)

  ggplot(density_data, aes(raw_value, density_scaled)) +
    geom_area(fill = "grey86", colour = NA) +
    geom_area(
      data = highlighted_density,
      fill = profile_colour,
      alpha = 0.92,
      colour = NA
    ) +
    geom_point(
      data = player_data,
      aes(x = raw_median, y = 0),
      inherit.aes = FALSE,
      shape = 24,
      fill = "#B22222",
      colour = "#B22222",
      size = 2.4
    ) +
    geom_text(
      data = player_data,
      aes(
        x = Inf,
        y = 0.72,
        label = paste0(value_label, "  ", percentile_label)
      ),
      inherit.aes = FALSE,
      hjust = -0.03,
      fontface = "bold",
      colour = "#9E1B1B",
      size = 3.8
    ) +
    facet_wrap(
      vars(metric_label),
      ncol = 1,
      scales = "free_x",
      strip.position = "left"
    ) +
    labs(
      title = "Internal Response",
      subtitle = paste("Player value against", reference, "reference distribution"),
      x = NULL,
      y = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(
        size = 7.5,
        colour = "grey45"
      ),
      axis.ticks.x = element_line(
        colour = "grey70",
        linewidth = 0.3
      ),
      strip.placement = "outside",
      strip.text.y.left = element_text(
        angle = 0,
        hjust = 1,
        face = "bold",
        colour = "grey20",
        size = 9.5
      ),
      panel.spacing.y = grid::unit(0.8, "lines"),
      plot.title = element_text(face = "bold", size = 14, colour = "#111111"),
      plot.subtitle = element_text(size = 9.8, colour = "grey35"),
      plot.margin = margin(8, 52, 8, 8)
    )
}

plot_player_card <- function(player_name, reference = c("overall", "position")) {
  reference <- match.arg(reference)
  check_player_exists(player_name)

  metadata <- player_profile_metadata %>%
    filter(PlayerName == player_name) %>%
    slice(1)

  reference_title <- str_to_title(reference)

  header <- ggplot() +
    annotate(
      "text",
      x = 0,
      y = 1.05,
      label = metadata$PlayerName,
      hjust = 0,
      fontface = "bold",
      size = 7,
      colour = "#111111"
    ) +
    annotate(
      "text",
      x = 0,
      y = 0.35,
      label = paste(metadata$PLAYING.POSITION, "|", metadata$Team),
      hjust = 0,
      size = 3.8,
      colour = "grey30"
    ) +
    annotate(
      "text",
      x = 1,
      y = 1.05,
      label = paste(reference_title, "Reference"),
      hjust = 1,
      fontface = "bold.italic",
      size = 4.6,
      colour = "grey45"
    ) +
    xlim(0, 1) +
    ylim(0, 1.25) +
    theme_void()

  footer_text <- paste0(
    "External profile: ", metadata$external_evidence, " - ",
    metadata$n_external, " sessions\n",
    "HR response: ", metadata$HR_evidence, " - ",
    metadata$n_HR, " sessions\n",
    "RPE response: ", metadata$RPE_evidence, " - ",
    metadata$n_RPE, " sessions\n",
    "Position reference: ", metadata$PLAYING.POSITION, ", n = ",
    metadata$n_position_players, " players"
  )

  footer <- ggplot() +
    annotate(
      "text",
      x = 0,
      y = 1,
      label = footer_text,
      hjust = 0,
      vjust = 1,
      size = 3.4,
      lineheight = 1.05,
      colour = "grey25"
    ) +
    xlim(0, 1) +
    ylim(0, 1) +
    theme_void()

  player_card <- header /
    (plot_external_radar(player_name, reference) + plot_internal_response(player_name, reference) +
       plot_layout(widths = c(1.55, 1))) /
    footer +
    plot_layout(heights = c(0.14, 1, 0.18))

  player_card +
    plot_annotation(
      theme = theme(plot.background = element_rect(fill = "white", colour = NA))
    )
}

save(
  external_percentiles_long,
  internal_percentiles_long,
  player_profile_metadata,
  
  external_metric_labels,
  external_metric_blocks,
  block_colours,
  
  internal_metric_labels,
  internal_metrics,
  
  profile_colour,
  radar_rings,
  
  check_player_exists,
  reference_column,
  make_density_data,
  
  plot_external_radar,
  plot_internal_response,
  plot_player_card,
  
  file = "raw_profile_tool.RData"
  )


# ------------------------------------------------------------
# 8. EXAMPLE CALLS
# ------------------------------------------------------------

example_player <- player_profile_metadata$PlayerName[5]

plot_player_card(example_player, reference = "overall")
plot_player_card(example_player, reference = "position")
