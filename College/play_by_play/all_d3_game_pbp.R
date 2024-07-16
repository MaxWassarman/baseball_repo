library(tidyverse)
library(collegebaseball)

# Function 1: Get team IDs and schedules
get_d3_schedules <- function(team_names_csv, year = 2024) {
  # Read team names from CSV file
  team_names <- read_csv(team_names_csv)$team_name
  
  # Initialize empty dataframes for team IDs and schedules
  team_ids <- data.frame(team_name = character(), team_id = numeric(), stringsAsFactors = FALSE)
  all_schedules <- data.frame()
  
  # Loop through each team to get their ID
  for (team in team_names) {
    tryCatch({
      # Look up the school ID for the current team
      id_result <- ncaa_school_id_lookup(team_name = team, season = year)
      if (nrow(id_result) > 0) {
        # If ID is found, add it to the team_ids dataframe
        team_ids <- rbind(team_ids, data.frame(team_name = team, team_id = id_result$team_id[1]))
      }
    }, error = function(e) {
      # If an error occurs, print a message and continue to the next team
      message(sprintf("Error looking up ID for team %s: %s", team, e$message))
    })
  }
  
  # Loop through each team ID to get their schedule
  for (id in team_ids$team_id) {
    tryCatch({
      # Get the schedule for the current team ID
      schedule <- ncaa_schedule(team_id = id, year = year)
      # Add the schedule to the all_schedules dataframe
      all_schedules <- rbind(all_schedules, schedule)
    }, error = function(e) {
      # If an error occurs, print a message and continue to the next team
      message(sprintf("Error getting schedule for team ID %s: %s", id, e$message))
    })
  }
  
  # Remove duplicate games (each game appears twice, once for each team)
  unique_schedules <- all_schedules %>% distinct(contest_id, .keep_all = TRUE)
  return(unique_schedules)
}

# Function 2: Get play-by-play data
get_d3_pbp_data <- function(schedules) {
  all_games_pbp <- data.frame()
  
  # Loop through each game in the schedules
  for (i in 1:nrow(schedules)) {
    game_id <- schedules$contest_id[i]
    
    tryCatch({
      # Get play-by-play data for the current game
      pbp_data <- pbp_testing(game_id = game_id)
      
      # Check if the data is valid
      if (is.data.frame(pbp_data) && nrow(pbp_data) > 0) {
        if (all(c("away_text", "home_text") %in% names(pbp_data))) {
          # Filter out rows where both away_text and home_text are just scores
          pbp_data <- pbp_data %>% 
            filter(!(grepl("^\\d+-\\d+$", away_text) & grepl("^\\d+-\\d+$", home_text)))
          
          if (nrow(pbp_data) > 0) {
            # If valid data remains after filtering, add it to all_games_pbp
            all_games_pbp <- rbind(all_games_pbp, pbp_data)
          } else {
            message(sprintf("Game ID %s has no valid play-by-play data after filtering", game_id))
          }
        } else {
          message(sprintf("Game ID %s data does not contain expected 'away_text' and 'home_text' columns", game_id))
        }
      } else {
        message(sprintf("Game ID %s returned no data or invalid data structure", game_id))
      }
    }, error = function(e) {
      message(sprintf("Error processing game ID %s: %s", game_id, e$message))
    })
    
    # Optional: Add a small delay to avoid overwhelming the server
    Sys.sleep(0.5)
  }
  
  return(all_games_pbp)
}

# Function 3: Parse play-by-play data
parse_ncaa_pbp_data <- function(pbp_data) {
  # Get unique game IDs
  unique_games <- unique(pbp_data$game_id)
  total_games <- length(unique_games)
  
  # Initialize a list to store parsed data for each game
  parsed_data <- vector("list", total_games)
  
  # Loop through each unique game
  for (i in seq_along(unique_games)) {
    game_id <- unique_games[i]
    # Filter data for the current game
    game_data <- pbp_data %>% filter(game_id == !!game_id)
    
    tryCatch({
      # Parse the play-by-play data for the current game
      parsed_game <- ncaa_parse(game_data)
      parsed_data[[i]] <- parsed_game
    }, error = function(e) {
      cat(sprintf("Error processing game %d (ID: %s): %s\n", i, game_id, e$message))
      # Return NULL for games that couldn't be parsed
      parsed_data[[i]] <- NULL
    })
  }
  
  # Remove NULL entries (games that couldn't be parsed) and combine results
  bind_rows(parsed_data[!sapply(parsed_data, is.null)])
}