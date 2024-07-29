
library(tidyverse)
library(collegebaseball)

calculate_d3_league_wide <- function(team_names_csv, year = 2024) {
  # Read team names from CSV
  team_names <- read_csv(team_names_csv)$team_name
  
  # Initialize empty dataframe for team IDs
  team_ids <- data.frame(team_name = character(), team_id = numeric(), stringsAsFactors = FALSE)
  
  # Loop through each team to get their ID
  for (team in team_names) {
    tryCatch({
      id_result <- ncaa_school_id_lookup(team_name = team, season = year)
      if (nrow(id_result) > 0) {
        team_ids <- rbind(team_ids, data.frame(team_name = team, team_id = id_result$team_id[1]))
      }
    }, error = function(e) {
      message(sprintf("Error looking up ID for team %s: %s", team, e$message))
    })
  }
  
  # Fetch batting statistics for each team
  all_stats <- map_dfr(team_ids$team_id, function(id) {
    tryCatch({
      ncaa_stats(team_id = id, year = year, type = "pitching")
    }, error = function(e) {
      warning(paste("Error fetching data for team ID:", id))
      return(NULL)
    })
  })
  
  # Return results
  return(all_stats)
}
