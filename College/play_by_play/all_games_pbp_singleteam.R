library(tidyverse)

bulk_parse <- function(schedule) {
  game_ids <- schedule$contest_id
  
  process_game <- function(game_id) {
    tryCatch({
      pbp_data <- pbp_testing(game_id = game_id)
      
      parsed_data <- ncaa_parse(pbp_data)
      
      return(parsed_data)
    }, 
    error = function(e) {
      message(sprintf("Error processing game ID %s: %s", game_id, e$message))
      return(NULL)
      }
    )
  }

  all_games_data <- map(game_ids, process_game)
  
  all_games_data <- all_games_data[!sapply(all_games_data, is.null)]

  combined_data <- bind_rows(all_games_data)
  
  return(combined_data)
}