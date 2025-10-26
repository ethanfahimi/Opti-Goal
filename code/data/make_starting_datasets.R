## -----------------------------------------------------------------------------
##
## Script Name: make_dataset.R
##
## Purpose: Cleanse Raw FPL Data
##
## Author: Dilan SriDaran & Ethan Fahimi
##
## Date Created: 2023-09-28
##
## -----------------------------------------------------------------------------

# 1. Preliminaries -------------------------------------------------------------

# load packages and functions
source("load_libraries.R")
source("functions.R")

# import raw data
fpl_data <- fread("../../data/raw/fpl-form-predicted-points.csv")

# 2. Clean Data ----------------------------------------------------------------

clean_data <- fpl_data %>%
  # extract relevant features
  select(ID, Name, Team, Pos, Price, contains("_with_prob")) %>%
  select(-contains("tba_")) %>%
  # reset price feature to original value
  mutate(Price = 0.5 * round(Price / 0.5)) %>%
  # create missing features for first four gameweeks
  rowwise() %>% 
  mutate(
    `1_with_prob` = mean(`5_with_prob`:`38_with_prob`),
    `2_with_prob` = mean(`5_with_prob`:`38_with_prob`),
    `3_with_prob` = mean(`5_with_prob`:`38_with_prob`),
    `4_with_prob` = mean(`5_with_prob`:`38_with_prob`)
  ) %>%
  ungroup() %>%
  as.data.frame() %>%
  # create total expected points
  mutate(total_pts = rowSums(select_if(., is.numeric)) - ID - Price) %>%
  # remove players with poor performance for price bracket
  group_by(Price, Pos) %>%
  mutate(Rank = dense_rank(desc(total_pts))) %>%
  filter((Rank <= 3 & Pos == "FWD") | (Rank <= 5 & Pos == "MID") | (Rank <= 5 & Pos == "DEF") | (Rank <= 2 & Pos == "GK")) %>%
  # remove any players with 0 expected points (unless at price floor for position)
  group_by(Pos) %>%
  filter(! (total_pts < 1 & Price != min(Price))) %>%
  ungroup()

# 3. Create Team Data ----------------------------------------------------------

team_data <- clean_data %>%
  select(Name) %>%
  cbind(one_hot_encode_variable(clean_data, "Team"))

fwrite(team_data, "../../data/processed/team.csv")

# 4. Create Position Data ------------------------------------------------------

position_data <- clean_data %>%
  select(Name) %>%
  cbind(one_hot_encode_variable(clean_data, "Pos"))

fwrite(position_data, "../../data/processed/position.csv")

# 5. Create Price Data ---------------------------------------------------------

price_data <- clean_data %>%
  select(Name, Price)

fwrite(price_data, "../../data/processed/price.csv")

# 6. Create Points Data --------------------------------------------------------

points_data <- clean_data %>%
  select(Name, glue("{1:38}_with_prob"))

fwrite(points_data, "../../data/processed/points.csv")
