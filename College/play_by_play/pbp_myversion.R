library(baseballr)
library(collegebaseball)
library(tidyverse)
library(httr)
library(rvest)

pbp_testing <- function(game_id = NA_real_, game_pbp_url = NA_character_, ...) {
  if (is.na(game_pbp_url) && is.na(game_id)) {
    cli::cli_abort(glue::glue("{Sys.time()}: No game_info_url or game_id provided"))
  }
  if (!is.na(game_id) & is.na(game_pbp_url)) {
    url <- paste0("https://stats.ncaa.org/contests/",game_id,"/play_by_play")
  } else {
    url <- game_pbp_url
  }
  pbp_payload <- url |>
    xml2::read_html()
  table_list <- (pbp_payload |>
                   rvest::html_elements("table"))[-c(1,2,3)] |>
    rvest::html_table()
  date_slug <- pbp_payload |>
    rvest::html_elements("tr:nth-child(4) .grey_text") |>
    rvest::html_text(trim=T)
  loc_slug <- pbp_payload |>
    rvest::html_elements("tr:nth-child(5) .grey_text") |>
    rvest::html_text(trim=T)
  att <- pbp_payload |>
    rvest::html_elements("tr:nth-child(6) .grey_text") |>
    rvest::html_text(trim=T)
  add_inning_column <- function(df, inning) {
    df$inning <- inning
    return(df)
  }
  mapped_table <- lapply(seq_along(table_list), function(i) add_inning_column(table_list[[i]], i))
  mapped_table <- dplyr::bind_rows(mapped_table)
  
  col_names <- names(mapped_table)
  away_team <- col_names[1]
  home_team <- col_names[3]
  
  mapped_table <- mapped_table |>
    dplyr::rename(away_des = 1, home_des = 3) |>
    dplyr::mutate(
      away_team = away_team,
      home_team = home_team,
      game_id = as.numeric(gsub("\\D", "", url)),
      date = substr(date_slug, start = 1, stop = 10),
      year = as.integer(format(as.Date(date, "%m/%d/%Y"), "%Y")),
      away_text = ifelse(away_des != "", away_des, ""),
      home_text = ifelse(home_des != "", home_des, ""),
      away_score = gsub("-.*", "", Score),
      home_score = gsub(".*-", "", Score)
    ) |>
    dplyr::filter(!grepl("LOB:", away_des) & !grepl("LOB:", home_des))
  
  mapped_table <- mapped_table |>
    dplyr::select(
      year,
      date,
      game_id,
      inning,
      away_team,
      home_team,
      away_score,
      home_score,
      away_text,
      home_text
    )
  return(mapped_table)
}