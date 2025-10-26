# 1. Define Functions ----------------------------------------------------------

extract_last_word <- function(text) {
  words <- unlist(strsplit(text, " "))
  last_word <- tail(words, 1)
  return(last_word)
}

clean_points_data <- function(input_path, output_path) {
  
  # read data and convert team names
  actual_data <- fread(input_path) %>%
    select(name, position, team, total_points) %>%
    mutate(team = case_when(
      team == "Arsenal" ~ "ARS",
      team == "Nott'm Forest" ~ "NFO",
      team == "Aston Villa" ~ "AVL",
      team == "Sheffield Utd" ~ "SHU",
      team == "Luton" ~ "LUT",
      team == "Bournemouth" ~ "BOU",
      team == "Brentford" ~ "BRE",
      team == "Brighton" ~ "BHA",
      team == "Chelsea" ~ "CHE",
      team == "Burnley" ~ "BUR",
      team == "Man Utd" ~ "MUN",
      team == "Crystal Palace" ~ "CRY",
      team == "Everton" ~ "EVE",
      team == "Fulham" ~ "FUL",
      team == "Liverpool" ~ "LIV",
      team == "Man City" ~ "MCI",
      team == "Newcastle" ~ "NEW",
      team == "Spurs" ~ "TOT",
      team == "West Ham" ~ "WHU",
      team == "Wolves" ~ "WOL"
    )) %>%
    rename(Team = team, Pos = position, TP = total_points)
  
  # match to naming convention of expected points data
  actual_data$Name <- sapply(actual_data$name, extract_last_word)
  
  # join data sets
  points_data <- clean_data %>%
    select(ID, Name, Team, Pos, Price) %>%
    left_join(actual_data, by = c("Name", "Team", "Pos"))
  
  # apply name mapping for name differences
  mapping <- fread("../../data/processed/mapping_files/player_name_mapping.csv")
  
  # finalize data format
  points_data <- points_data %>% 
    filter(is.na(TP)) %>%
    select(-name, -TP) %>%
    left_join(mapping, by = "Name") %>%
    rename(name = Name_New) %>%
    left_join(actual_data, by = c("name", "Team", "Pos")) %>%
    transmute(ID, Name = Name.x, Team, Pos, Price, TP) %>%
    bind_rows(points_data %>% filter(!is.na(TP))) %>%
    select(-name) %>%
    arrange(ID)
  
  # summarize data
  points_data <- points_data %>%
    group_by(ID, Name, Team, Pos, Price) %>%
    summarize(TP = mean(TP, na.rm = T)) %>%
    ungroup()
  
  fwrite(points_data, output_path)
}

# 2. Execute Functions ---------------------------------------------------------

for (i in 1:9) {
  clean_points_data(
    glue("../../data/raw/actual_points/gw{i}.csv"),
    glue("../../data/processed/actual_points/gw{i}.csv")
  )
}

