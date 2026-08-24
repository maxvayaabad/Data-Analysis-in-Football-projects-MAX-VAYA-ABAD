# ============================================================
# DYNAMIC PHYSICAL PROFILING
# 02.1 - RAW PROFILE GENERATOR
#
# Purpose:
#   Load the profile objects created in 02_raw_profile.R
#   and generate a raw physical profile for any available player.
#
# Required file:
#   raw_profile_tool.RData
#
# Workflow:
#   1. Run 02_raw_profile.R once to create raw_profile_tool.RData
#   2. Open this script
#   3. Change only PLAYER_NAME and REFERENCE below
#   4. Run the script to display the player profile
# ============================================================


# ------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(here)


# ------------------------------------------------------------
# 1. LOAD SAVED PROFILE TOOL
# ------------------------------------------------------------

load(
  here("raw_profile_tool.RData")
)


# ------------------------------------------------------------
# 2. AVAILABLE PLAYERS
# ------------------------------------------------------------

available_players <- sort(
  unique(player_profile_metadata$PlayerName)
)

available_players


# ------------------------------------------------------------
# 3. SELECT PLAYER AND REFERENCE
# ------------------------------------------------------------

# Change only these two values when generating a new profile.

PLAYER_NAME <- "Chad Kersey"

# Options:
#   "overall"  = compare the player with all players
#   "position" = compare the player with players in the same position

REFERENCE <- "overall"


# ------------------------------------------------------------
# 4. CHECK PLAYER SELECTION
# ------------------------------------------------------------

if (!PLAYER_NAME %in% available_players) {
  stop(
    "Player not available. Choose one of:\n",
    paste(available_players, collapse = "\n")
  )
}


# ------------------------------------------------------------
# 5. GENERATE PLAYER PHYSICAL PROFILE
# ------------------------------------------------------------

physical_profile <- plot_player_card(
  player_name = PLAYER_NAME,
  reference = REFERENCE
)

physical_profile


# ============================================================
# OPTIONAL EXAMPLES
# ============================================================

# Example 1: Same player against the full squad
#
# plot_player_card(
#   player_name = "Alton Chamberlain",
#   reference = "overall"
# )


# Example 2: Same player against positional reference
#
# plot_player_card(
#   player_name = "Alton Chamberlain",
#   reference = "position"
# )


# Example 3: Quick player selector by index
#
# available_players
#
# PLAYER_NAME <- available_players[1]
#
# plot_player_card(
#   player_name = PLAYER_NAME,
#   reference = "position"
# )
