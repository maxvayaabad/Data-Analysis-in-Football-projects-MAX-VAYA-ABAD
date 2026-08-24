# ============================================================
# DYNAMIC PHYSICAL PROFILING
# 01 - PREPROCESSING
#
# Unit of observation:
#   one row = one player in one training session
#
# This script only:
#   - reads the original combined database
#   - adds player position and training-session type
#   - creates GPS / HR quality variables
#   - creates external-load relative metrics
#   - creates internal-load variables
#   - selects the variables needed for the project
#
# No imputation, EDA or player filtering is performed here.
# ============================================================


# ------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)


# ------------------------------------------------------------
# 1. LOAD MAIN DATA
# ------------------------------------------------------------

data_raw <- read_csv(
  here("data", "PAS_MF_TL_full_data.csv"),
  show_col_types = FALSE
)

data_raw <- data_raw %>%
  mutate(
    Date = ymd(Date)
  )


# ------------------------------------------------------------
# 2. PLAYER POSITION
# ------------------------------------------------------------

player_position <- read_csv(
  here("data", "PlayerPosition_O16_O18.csv"),
  show_col_types = FALSE
) %>%
  distinct(Pseudonym, .keep_all = TRUE) %>%
  mutate(
    PLAYING.POSITION = recode(
      PLAYING.POSITION,
      "Centrale verdediger" = "Central defender",
      "Middenvelder" = "Midfielder",
      "Spits" = "Striker",
      "Vleugelaanvaller" = "Winger",
      "Vleugelverdediger" = "Fullback"
    )
  ) %>%
  select(
    Pseudonym,
    PLAYING.POSITION
  )


data_profile <- data_raw %>%
  left_join(
    player_position,
    by = c("PlayerName" = "Pseudonym")
  )


# ------------------------------------------------------------
# 3. TRAINING SESSION TYPE
# ------------------------------------------------------------

# U16
training_type_u16 <- read_csv(
  here("data", "Training_type_O16.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    DATUM = ymd(DATUM),
    Team = "U16"
  ) %>%
  select(
    Date = DATUM,
    Team,
    TYPE
  )


# U18
training_type_u18 <- read_csv(
  here("data", "Training_type_O18.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    DATUM = ymd(DATUM),
    Team = "U18"
  ) %>%
  select(
    Date = DATUM,
    Team,
    TYPE
  )


training_type <- bind_rows(
  training_type_u16,
  training_type_u18
) %>%
  mutate(
    TYPE = if_else(
      TYPE == "MD+2-1",
      "MD+2",
      TYPE
    )
  )


data_profile <- data_profile %>%
  left_join(
    training_type,
    by = c("Date", "Team")
  )


# ------------------------------------------------------------
# 4. ADD TRAINING TYPES THAT WERE MISSING
# ------------------------------------------------------------

training_type_missing <- read_csv2(
  here("data", "Missing_training_type.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    Date = dmy(Date)
  ) %>%
  select(
    Date,
    Team,
    Type
  ) %>%
  distinct()


# Check whether Date + Team still identifies multiple
# different training types
training_type_missing %>%
  count(Date, Team) %>%
  filter(n > 1)

data_profile <- data_profile %>%
  left_join(
    training_type_missing,
    by = c("Date", "Team"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    TYPE = coalesce(TYPE, Type)
  ) %>%
  select(-Type)

# ------------------------------------------------------------
# 5. SESSION DURATION INFORMATION
# ------------------------------------------------------------

# Keep the different durations separated.
#
# GPS duration will be used for external-load relative metrics.
# HR duration is retained for internal-load metrics.
#
# real_training_duration is retained as contextual / quality
# information.

data_profile <- data_profile %>%
  mutate(
    gps_duration = Training_duration_file_GPS,
    hr_duration  = Training_duration_file_HR
  )


# ------------------------------------------------------------
# 6. GPS DATA QUALITY
# ------------------------------------------------------------

# Original preprocessing notes that total_GPS_missing also
# includes periods where a player started later or finished
# earlier.
#
# Following the logic of the original project, estimate GPS
# missingness occurring DURING the player's actual session.

data_profile <- data_profile %>%
  group_by(Date, TeamTrained) %>%
  mutate(
    mode_session_duration =
      DescTools::Mode(real_training_duration, na.rm = TRUE)[1],
    
    training_duration_deviation =
      mode_session_duration - real_training_duration
  ) %>%
  ungroup() %>%
  
  mutate(
    gps_missing_during_session =
      total_GPS_missing - training_duration_deviation,
    
    gps_missing_during_session =
      case_when(
        is.na(gps_missing_during_session) ~ NA_real_,
        gps_missing_during_session < 0 ~ 0,
        TRUE ~ gps_missing_during_session
      ),
    
    gps_missing_pct =
      100 * gps_missing_during_session / real_training_duration
  )


# ------------------------------------------------------------
# 7. HR DATA QUALITY
# ------------------------------------------------------------

data_profile <- data_profile %>%
  mutate(
    hr_missing_during_session =
      HRerror - training_duration_deviation,
    
    hr_missing_during_session =
      case_when(
        is.na(hr_missing_during_session) ~ NA_real_,
        hr_missing_during_session < 0 ~ 0,
        TRUE ~ hr_missing_during_session
      ),
    
    hr_missing_pct =
      100 * hr_missing_during_session / real_training_duration
  )


# ------------------------------------------------------------
# 8. COMBINED HIGH-SPEED RUNNING VARIABLE
# ------------------------------------------------------------

# Original authors combine HSR and sprinting because sprint
# exposure is relatively low in adolescent players.

data_profile <- data_profile %>%
  mutate(
    HSR_sprinting = HSR + sprinting
  )


# ------------------------------------------------------------
# 9. EXTERNAL-LOAD RELATIVE METRICS
# ------------------------------------------------------------

# Use the actual available GPS duration, rather than the
# scheduled session duration.

data_profile <- data_profile %>%
  mutate(
    distance_per_min =
      Total_distance / gps_duration,
    
    LSA_per_min =
      LSA / gps_duration,
    
    MSR_per_min =
      MSR / gps_duration,
    
    HSR_per_min =
      HSR / gps_duration,
    
    sprint_dist_per_min =
      sprinting / gps_duration,
    
    HSR_sprint_per_min =
      HSR_sprinting / gps_duration,
    
    accelerations_per_min =
      acc2 / gps_duration,
    
    decelerations_per_min =
      dec2 / gps_duration,
    
    no_sprints_per_min =
      no_sprints / gps_duration
  )


# ------------------------------------------------------------
# 10. INTERNAL LOAD / RESPONSE
# ------------------------------------------------------------

# Keep internal response separate from the external-load
# physical profile.
#
# sRPE is recalculated using the available HR-session
# duration, following the logic used by the original authors.

data_profile <- data_profile %>%
  mutate(
    sRPE_profile =
      RPE * hr_duration
  )


# ------------------------------------------------------------
# 11. DATA AVAILABILITY FLAGS
# ------------------------------------------------------------

# These are descriptive flags only.
# No observations are removed here.

data_profile <- data_profile %>%
  mutate(
    
    external_load_available =
      !is.na(Total_distance),
    
    speed_load_available =
      !is.na(HSR) | !is.na(sprinting),
    
    accdec_available =
      !is.na(acc2) & !is.na(dec2),
    
    HR_available =
      !is.na(TRIMPmod),
    
    RPE_available =
      !is.na(RPE)
  )


# ------------------------------------------------------------
# 12. FINAL PLAYER-SESSION DATAFRAME
# ------------------------------------------------------------

player_session_profile <- data_profile %>%
  
  select(
    
    # -------------------------
    # IDENTIFICATION
    # -------------------------
    PlayerName,
    Date,
    
    # -------------------------
    # PLAYER / SESSION CONTEXT
    # -------------------------
    PLAYING.POSITION,
    Team,
    TeamTrained,
    TYPE,
    weekday,
    Starttime,
    
    # -------------------------
    # DURATION
    # -------------------------
    Duration_minutes,
    real_training_duration,
    gps_duration,
    hr_duration,
    
    # -------------------------
    # DATA QUALITY
    # -------------------------
    Hz_file,
    
    total_GPS_missing,
    gps_missing_during_session,
    gps_missing_pct,
    
    HRerror,
    HRaboveMAX,
    hr_missing_during_session,
    hr_missing_pct,
    
    # -------------------------
    # EXTERNAL LOAD - ABSOLUTE
    # -------------------------
    Total_distance,
    
    LSA,
    MSR,
    
    HSR,
    sprinting,
    HSR_sprinting,
    
    no_sprints,
    avg_sprint_distance,
    
    acc2,
    dec2,
    
    # -------------------------
    # EXTERNAL LOAD - RELATIVE
    # -------------------------
    distance_per_min,
    
    LSA_per_min,
    MSR_per_min,
    
    HSR_per_min,
    sprint_dist_per_min,
    HSR_sprint_per_min,
    
    no_sprints_per_min,
    
    accelerations_per_min,
    decelerations_per_min,
    
    # -------------------------
    # INTERNAL RESPONSE
    # -------------------------
    RPE,
    sRPE_profile,
    
    HRzone0,
    HRzone1,
    HRzone2,
    HRzone3,
    HRzone4,
    HRzone5,
    
    TRIMPmod,
    
    # -------------------------
    # DATA AVAILABILITY FLAGS
    # -------------------------
    external_load_available,
    speed_load_available,
    accdec_available,
    HR_available,
    RPE_available
  ) %>%
  
  arrange(
    PlayerName,
    Date
  )

# Remove exact duplicated rows only
player_session_profile <- player_session_profile %>%
  distinct()

player_session_profile <- player_session_profile %>%
  mutate(
    session_id = paste(
      Date,
      TeamTrained,
      Starttime,
      sep = "_"
    )
  ) %>%
  relocate(session_id, .after = Date)

# ------------------------------------------------------------
# 13. OPTIONAL: SAVE PREPROCESSED DATA
# ------------------------------------------------------------

write_csv(
  player_session_profile,
  here("data", "player_session_profile.csv")
)

