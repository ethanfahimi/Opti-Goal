# read static predictions from season start
static_predictions <- fread("../../data/processed/points.csv")

# loop through future weeks to update predictions based on form
for (start_week in 2:9) {

  # determine weeks remaining in season
  weeks_left <- (start_week - 1):38
    
  if (start_week %in% 4:9) {

    # read actual points scored for three prior weeks
    actual_points_1 <- fread(glue("../../data/processed/actual_points/gw{start_week - 1}.csv"))
    actual_points_2 <- fread(glue("../../data/processed/actual_points/gw{start_week - 2}.csv"))
    actual_points_3 <- fread(glue("../../data/processed/actual_points/gw{start_week - 3}.csv"))
    
    # determine average points scored over prior three weeks (form)
    base_predictions <- static_predictions %>%
      select(Name, glue("{weeks_left}_with_prob")) %>%
      mutate(form = (actual_points_1$TP + actual_points_2$TP + actual_points_3$TP) / 3)
    
    # initialize updated predictions
    new_predictions <- base_predictions
    
    # compute updated predictions for future weeks, based 50% on form and 50% on initial expectations
    for (w in weeks_left) {
      new_predictions <- new_predictions %>%
        mutate(!!sym(glue("{w}_with_prob")) := ifelse(
          !is.na(form), 
          0.5 * (!!sym(glue("{w}_with_prob")) + form),
          !!sym(glue("{w}_with_prob"))
        ))
    }
    
    # clean result
    new_predictions <- new_predictions %>% select(-form)
  
  } else {
    new_predictions <- static_predictions %>% select(Name, glue("{weeks_left}_with_prob"))
  }
  
  # write output
  fwrite(new_predictions, glue("../../data/processed/predicted_points/without_injuries/points_{start_week}.csv"))
}

# start_week <- 4
# injury_data <- fread("../../data/raw/fpl-injuries.csv") %>%
#   select(ID, Name, Team, contains(glue("GW{weeks_left}"))) %>%
#   pivot_longer(cols = GW9:GW18) %>%
#   filter(!is.na(value))
