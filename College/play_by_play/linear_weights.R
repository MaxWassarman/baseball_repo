library(tidyverse)

calculate_college_linear_weights <- function(pbp_data, re24_matrix) {
  get_re <- function(base_cd, outs) {
    if (is.na(base_cd) || is.na(outs)) return(0)
    re24_matrix[min(max(base_cd + 1, 1), 8), min(max(outs + 1, 1), 3)]
  }
  
  event_data <- tibble(
    event = character(),
    re24 = numeric(),
    count = integer()
  )
  
  for (i in 1:nrow(pbp_data)) {
    row <- pbp_data[i, ]
    
    re_start <- get_re(row$base_cd_before, row$outs_before)
    re_end <- if (i < nrow(pbp_data)) {
      get_re(pbp_data$base_cd_before[i+1], pbp_data$outs_before[i+1])
    } else {
      0
    }
    
    if (row$inn_end == 1) re_end <- 0
    
    re24 <- re_end - re_start + row$runs_on_play
    
    event <- case_when(
      row$event_cd %in% c(2, 3, 6) ~ "out",
      row$event_cd == 14 ~ "walk",
      row$event_cd == 16 ~ "hit_by_pitch",
      row$event_cd == 20 ~ "single",
      row$event_cd == 21 ~ "double",
      row$event_cd == 22 ~ "triple",
      row$event_cd == 23 ~ "home_run"
    )
    
    event_data <- event_data %>%
      add_row(event = event, re24 = re24, count = 1)
  }
  
  linear_weights <- event_data %>%
    group_by(event) %>%
    summarise(
      count = sum(count),
      total_re24 = sum(re24),
      linear_weights = round(total_re24 / count, 3)
    ) %>%
    mutate(
      linear_weights_above_average = linear_weights - weighted.mean(linear_weights, count),
      linear_weights_above_outs = linear_weights - linear_weights[event == "out"]
    ) %>%
    arrange(desc(linear_weights)) 
  
  return(linear_weights)
}


calculate_college_linear_weights <- function(pbp_data, re24_matrix) {
  # Function to get RE value from the matrix
  get_re <- function(base_cd, outs) {
    if (is.na(base_cd) || is.na(outs)) return(0)
    re24_matrix[min(max(base_cd + 1, 1), 8), min(max(outs + 1, 1), 3)]
  }
  
  # Initialize counters
  event_counts <- list("walk" = 0, "hit_by_pitch" = 0, "single" = 0, 
                       "double" = 0, "triple" = 0, "home_run" = 0, "out" = 0, "other" = 0)
  event_re24_sums <- list("walk" = 0, "hit_by_pitch" = 0, "single" = 0, 
                          "double" = 0, "triple" = 0, "home_run" = 0, "out" = 0, "other" = 0)
  
  # Process data row by row
  for (i in 1:nrow(pbp_data)) {
    row <- pbp_data[i, ]
    
    re_start <- get_re(row$base_cd_before, row$outs_before)
    re_end <- if (i < nrow(pbp_data)) {
      get_re(pbp_data$base_cd_before[i+1], pbp_data$outs_before[i+1])
    } else {
      0
    }
    
    if (row$inn_end == 1) re_end <- 0
    
    re24 <- re_end - re_start + row$runs_on_play
    
    event <- switch(as.character(row$event_cd),
                    "2" = "out",
                    "3" = "out",
                    "6" = "out",
                    "14" = "walk",
                    "16" = "hit_by_pitch",
                    "20" = "single",
                    "21" = "double",
                    "22" = "triple",
                    "23" = "home_run",
                    "other")  # Default case
    
    if (is.null(event)) {
      warning(paste("Unexpected event_cd:", row$event_cd, "at row", i))
      event <- "other"
    }
    
    event_counts[[event]] <- event_counts[[event]] + 1
    event_re24_sums[[event]] <- event_re24_sums[[event]] + re24
  }
  
  # Calculate linear weights
  linear_weights <- tibble(
    events = names(event_counts),
    count = unlist(event_counts),
    linear_weights_above_average = unlist(event_re24_sums) / count
  ) |>
    filter(events != "other") |>
    mutate(
      linear_weights_above_average = round(linear_weights_above_average, 3),
      linear_weights_above_outs = linear_weights_above_average - 
        linear_weights_above_average[events == "out"]
    ) |>
    arrange(desc(linear_weights_above_average))
  
  return(linear_weights)
}


calculate_normalized_linear_weights <- function(linear_weights, stats) {
  # Ensure we have the necessary columns in linear_weights
  required_columns <- c("events", "linear_weights_above_outs", "count")
  if (!all(required_columns %in% names(linear_weights))) {
    stop("linear_weights must contain columns: events, linear_weights_above_outs, and count")
  }
  
  # Calculate total value produced by all events
  total_value <- sum(linear_weights$linear_weights_above_outs * linear_weights$count)
  
  # Calculate total plate appearances by summing the counts
  total_pa <- sum(linear_weights$count)
  
  # Calculate the denominator
  denominator <- total_value / total_pa
  
  #denominator <- total_value / 538062 #This seems better think my PA calc is wrong so estimating based on (AB,BB,HBP,SF,SH)
  
  league_obp <- (sum(stats$H) + sum(stats$BB) + sum(stats$HBP)) / (sum(stats$AB) + sum(stats$BB) + sum(stats$HBP) + sum(stats$SF))
  
  woba_scale <- league_obp / denominator
  
  # Normalize the weights
  normalized_weights <- linear_weights %>%
    mutate(
      normalized_weight = linear_weights_above_outs * woba_scale,
      normalized_weight = round(normalized_weight, 3)
    )
  
  return(normalized_weights)
}