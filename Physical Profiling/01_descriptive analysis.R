# ============================================================
# DYNAMIC PHYSICAL PROFILING: EDA, analytical sample and profiles
# Input:  data/player_session_profile.csv
# Output: data/player_session_profile_tidy_final.csv
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

load_metrics <- c("distance_per_min", "sprint_dist_per_min",
                  "LSA_per_min", "MSR_per_min", "HSR_per_min",
                  "HSR_sprint_per_min", "no_sprints_per_min",
                  "accelerations_per_min", "decelerations_per_min")
absolute_metrics <- c("Total_distance", "HSR_sprinting", "sprinting",
                      "no_sprints", "acc2", "dec2")

data <- read_csv(here("data", "player_session_profile.csv"),
                 show_col_types = FALSE) %>%
  mutate(Date = ymd(Date))


# ============================================================
# 1. INITIAL EDA
# ============================================================

# ------------------------------------------------------------
# 1.1 Dataset structure and session identification
# ------------------------------------------------------------
dim(data)
glimpse(data)
summary(data)

data %>% summarise(
  n_player_sessions = n(), n_players = n_distinct(PlayerName),
  first_date = min(Date, na.rm = TRUE), last_date = max(Date, na.rm = TRUE)
)

# Duplicate player-session records and players with multiple sessions per day.
data %>% count(PlayerName, Date, TeamTrained, Starttime) %>% filter(n > 1)
data %>% count(PlayerName, Date) %>% filter(n > 1) %>% arrange(Date, PlayerName)


# ------------------------------------------------------------
# 1.2 Missing data by variable and by player
# ------------------------------------------------------------
variables_eda <- c(
  "PLAYING.POSITION", "Team", "TeamTrained", "TYPE", "gps_duration",
  "hr_duration", "gps_missing_pct", "hr_missing_pct", absolute_metrics,
  load_metrics, "RPE", "TRIMPmod"
)

missing_summary <- data %>%
  summarise(across(all_of(variables_eda),
                   list(n_missing = ~ sum(is.na(.x)),
                        pct_missing = ~ mean(is.na(.x)) * 100))) %>%
  pivot_longer(everything(), names_to = c("variable", ".value"),
               names_pattern = "(.*)_(n_missing|pct_missing)") %>%
  arrange(desc(pct_missing))

ggplot(missing_summary, aes(reorder(variable, pct_missing), pct_missing)) +
  geom_col() + coord_flip() +
  labs(title = "Missing data by variable", x = NULL, y = "% missing") +
  theme_minimal()

## Almost 35% missings in RPE. GPS metrics around 20% missings. 5% missings in "TYPE".

missing_by_player <- data %>%
  group_by(PlayerName, PLAYING.POSITION) %>%
  summarise(
    n_sessions = n(),
    across(all_of(load_metrics), ~ sum(!is.na(.x)), .names = "n_{.col}"),
    pct_any_external_missing = mean(!if_all(all_of(load_metrics), ~ !is.na(.x))) * 100,
    pct_gps_missingness_unknown = mean(is.na(gps_missing_pct)) * 100,
    .groups = "drop"
  ) %>% arrange(pct_any_external_missing, n_sessions)
missing_by_player

## Detectable missing pattern in GPS variables (in block).

# ------------------------------------------------------------
# 1.3 Session durations and GPS / HR recording quality
# ------------------------------------------------------------
duration_long <- data %>%
  select(gps_duration, hr_duration, real_training_duration) %>%
  pivot_longer(everything(), names_to = "duration_type", values_to = "minutes")

ggplot(duration_long, aes(minutes)) + geom_histogram(bins = 30) +
  facet_wrap(~ duration_type, scales = "free") +
  labs(title = "Session duration distributions", x = "Minutes", y = "Sessions") +
  theme_minimal()


ggplot(data, aes(real_training_duration, gps_duration)) +
  geom_point(alpha = .4) + geom_abline(slope = 1, linetype = "dashed") +
  labs(title = "GPS duration versus training duration",
       x = "Training duration", y = "GPS duration") + theme_minimal()
## Duration recorded (GPS and hr) almost the same as real training duration.

recording_quality <- data %>% summarise(
  gps_duration_missing = sum(is.na(gps_duration)),
  hr_duration_missing = sum(is.na(hr_duration)),
  gps_missingness_unknown = sum(is.na(gps_missing_pct)),
  hr_missingness_unknown = sum(is.na(hr_missing_pct)),
  gps_missingness_p50 = median(gps_missing_pct, na.rm = TRUE),
  gps_missingness_p90 = quantile(gps_missing_pct, .90, na.rm = TRUE),
  hr_missingness_p50 = median(hr_missing_pct, na.rm = TRUE),
  hr_missingness_p90 = quantile(hr_missing_pct, .90, na.rm = TRUE)
)
recording_quality

## 509-427 = 82 sessions have no calculation of gps missings pct

quality_long <- data %>% select(gps_missing_pct, hr_missing_pct) %>%
  pivot_longer(everything(), names_to = "recording", values_to = "missing_pct")
ggplot(quality_long, aes(missing_pct)) + geom_histogram(bins = 30) +
  facet_wrap(~ recording, scales = "free_y") +
  labs(title = "GPS and HR recording quality", x = "% missing", y = "Player-sessions") +
  theme_minimal()

## Most of sessions have no missings.

# ------------------------------------------------------------
# 1.4 External-load distributions and suspicious sessions
# ------------------------------------------------------------
external_absolute <- data %>% select(all_of(absolute_metrics)) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value")
ggplot(external_absolute, aes(value)) + geom_histogram(bins = 30) +
  facet_wrap(~ metric, scales = "free") +
  labs(title = "Absolute external-load distributions", x = NULL, y = "Player-sessions") +
  theme_minimal()

## Total distance, accelerations and decelerations show relatively unimodal
## distributions, although with some extreme values at both tails.
## High-speed running and sprint-related variables are strongly right-skewed:
## most player-sessions contain relatively little high-speed exposure, while
## a small number of sessions accumulate much larger HSR/sprint volumes.

external_relative <- data %>% select(all_of(load_metrics)) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value")
ggplot(external_relative, aes(value)) + geom_histogram(bins = 30) +
  facet_wrap(~ metric, scales = "free") +
  labs(title = "Relative external-load distributions", x = NULL, y = "Player-sessions") +
  theme_minimal()

## Expressing load relative to GPS duration makes distance, accelerations and
## decelerations per minute approximately symmetric and more comparable across
## sessions of different duration.
## HSR+sprint per minute remains clearly right-skewed, indicating that
## high-speed exposure is much more session-dependent and concentrated in a
## smaller number of high-intensity sessions.
## A few extreme observations are present, but these should be inspected rather
## than automatically removed, as they may represent genuine football demands.


# These thresholds flag sessions for review; they are not additional exclusions.
suspicious_sessions <- data %>%
  filter(gps_duration < 10 |
           (!is.na(gps_missing_pct) & gps_missing_pct > 10) |
           distance_per_min > 130 | HSR_sprint_per_min > 12 |
           accelerations_per_min > 3 | decelerations_per_min > 2.5) %>%
  select(PlayerName, Date, TYPE, gps_duration, gps_missing_pct, all_of(load_metrics)) %>%
  arrange(gps_duration, desc(gps_missing_pct))
suspicious_sessions

## some sessions with very short duration (not on MD) or others with anormal values

speed_zone_check <- data %>%
  mutate(distance_from_zones = LSA + MSR + HSR + sprinting,
         difference = Total_distance - distance_from_zones) %>%
  summarise(across(difference, list(mean = ~ mean(.x, na.rm = TRUE),
                                    median = ~ median(.x, na.rm = TRUE),
                                    min = ~ min(.x, na.rm = TRUE),
                                    max = ~ max(.x, na.rm = TRUE))))
speed_zone_check

## check in addition of run types 

# ------------------------------------------------------------
# 1.5 Correlations and external load by context
# ------------------------------------------------------------
external_cor <- data %>% select(all_of(load_metrics)) %>%
  cor(use = "pairwise.complete.obs")
external_cor_long <- external_cor %>% as.data.frame() %>%
  rownames_to_column("metric_1") %>%
  pivot_longer(-metric_1, names_to = "metric_2", values_to = "correlation")

ggplot(external_cor_long, aes(metric_1, metric_2, fill = correlation)) +
  geom_tile() + geom_text(aes(label = round(correlation, 2)), size = 3) +
  scale_fill_gradient2(midpoint = 0, limits = c(-1, 1)) + coord_equal() +
  labs(title = "Correlation between relative external-load metrics", x = NULL, y = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

## external load metrics (per minute) are mainly positively correlated linearly.

context_long <- data %>% select(PLAYING.POSITION, TYPE, all_of(load_metrics)) %>%
  pivot_longer(all_of(load_metrics), names_to = "metric", values_to = "value")

ggplot(context_long, aes(PLAYING.POSITION, value)) + geom_boxplot() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "External load by playing position", x = NULL, y = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

## playing position can have an effect on the distribution of external load metrics.

context_long %>%
  filter(!is.na(TYPE)) %>%
  ggplot(aes(TYPE, value)) + geom_boxplot() +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "External load by training-session type", x = NULL, y = NULL) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Obviously session type have different external load metrics

sessions_per_player <- data %>%
  count(PlayerName, PLAYING.POSITION, name = "n_sessions") %>% arrange(n_sessions)
sessions_per_player


# ============================================================
# 2. DEFINE ANALYTICAL INCLUSION CRITERIA
# ============================================================
# Valid sessions have: (1) external-load data available, (2) GPS duration >=10
# min, and (3) GPS missingness <=10% when a missingness value is available.
# Players require a minimum of 20 valid sessions.
# ============================================================
data_with_eligibility <- data %>% mutate(
  external_load_eligible = external_load_available %in% TRUE,
  gps_duration_eligible = !is.na(gps_duration) & gps_duration >= 10,
  gps_missingness_eligible = is.na(gps_missing_pct) | gps_missing_pct <= 10,
  valid_session = external_load_eligible & gps_duration_eligible & gps_missingness_eligible
)

inclusion_flow <- data_with_eligibility %>% summarise(
  input_player_sessions = n(),
  external_load_available = sum(external_load_eligible),
  gps_duration_at_least_10_min = sum(external_load_eligible & gps_duration_eligible),
  gps_quality_eligible = sum(valid_session)
)
inclusion_flow


# ============================================================
# 3. APPLY FILTERS AND WRITE THE FINAL TIDY DATASET
# ============================================================
data_tidy_final <- data_with_eligibility %>% filter(valid_session) %>%
  add_count(PlayerName, name = "n_valid_sessions") %>%
  filter(n_valid_sessions >= 20) %>%
  select(-external_load_eligible, -gps_duration_eligible,
         -gps_missingness_eligible, -valid_session)

write_csv(data_tidy_final, here("data", "player_session_profile_tidy_final.csv"))


# ============================================================
# 4. SHORT POST-FILTER EDA / SANITY CHECK
# ============================================================
data_tidy_final %>% summarise(n_player_sessions = n(), n_players = n_distinct(PlayerName))

valid_sessions_per_player <- data_tidy_final %>%
  count(PlayerName, PLAYING.POSITION, name = "n_valid_sessions") %>%
  arrange(n_valid_sessions)

ggplot(
  valid_sessions_per_player,
  aes(
    x = n_valid_sessions,
    y = reorder(PlayerName, n_valid_sessions)
  )
) +
  geom_col() +
  geom_vline(
    xintercept = 20,
    linetype = "dashed"
  ) +
  labs(
    title = "Number of valid sessions per player",
    subtitle = "Dashed line indicates the minimum inclusion criterion (20 sessions)",
    x = "Number of valid sessions",
    y = "Player"
  ) +
  theme_minimal()

## most players with at least 30 sessions after filter. Maximum in less than 60.

data_tidy_final %>% count(PLAYING.POSITION, sort = TRUE)
data_tidy_final %>% count(TYPE, sort = TRUE)

final_relative_long <- data_tidy_final %>% select(all_of(load_metrics)) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value")
ggplot(final_relative_long, aes(value)) + geom_histogram(bins = 30) +
  facet_wrap(~ metric, scales = "free") +
  labs(title = "Final relative external-load distributions", x = NULL, y = "Player-sessions") +
  theme_minimal()

## Final distributions remain stable after filtering.
## Distance, accelerations and decelerations per minute are approximately
## unimodal, while HSR+sprint per minute remains clearly right-skewed.
## No major distributional distortions appear to have been introduced.









