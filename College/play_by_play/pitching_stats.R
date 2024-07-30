library(tidyverse)
library(gt)

calculate_pitching_stats <- function(stats) {
  stats |>
    mutate(
      AB = BF - BB - HB - SHA - SFA,
      lgERA = (sum(ER)/sum(IP)) * 9,
      lgHR = sum(`HR-A`),
      lgFO = sum(FO),
      lgBB = sum(BB),
      lgHBP = sum(HB),
      lgK = sum(SO),
      lgIP = sum(IP),
      cFIP = lgERA - (((13*lgHR)+(3*(lgBB+lgHBP))-(2*lgK))/lgIP), #4.8
      FIP = ((13*`HR-A`)+(3*(BB+HB))-(2*SO))/IP + cFIP,
      `lgHR/FB%` = (lgHR/lgFO),
      xFIP = ((13*(FO * `lgHR/FB%`))+(3*(BB+HB))-(2*SO))/IP + cFIP, 
      `HR/FB%` = (`HR-A`/FO) * 100,
      `P/IP` = Pitches/IP,
      `K%` = (SO/BF) * 100,
      `BB%` = (BB/BF) * 100,
      `K-BB%` = `K%`-`BB%`,
      WHIP = (BB+H)/IP,
      BABIP = (H-`HR-A`)/(AB-SO-`HR-A`+SFA)
    ) |>
    select(-c(lgERA,lgHR,lgBB,lgHBP,lgK,lgIP,cFIP))
}

calculate_pitching_stats(d3_pitching) |>
  select(player_name, team_name, ERA, FIP, xFIP, IP,WHIP, `K%`, `BB%`, `K-BB%`, `P/IP`) |>
  mutate(across(where(is.numeric), ~round(., 2))) |>
  arrange(WHIP) |>
  filter(team_name == "Oberlin")

create_pitching_table <- function(d3_pitching) {
  calculate_pitching_stats(d3_pitching) |>
    select(player_name, team_name, ERA, FIP, xFIP, IP, WHIP, `K%`, `BB%`, `K-BB%`, BABIP, `P/IP`) |>
    mutate(across(where(is.numeric), ~round(., 3))) |>  # Round to 3 decimals initially
    arrange(FIP) |>
    filter(team_name == "Oberlin") |>
    gt() |>
    tab_header(
      title = "Oberlin Pitching Statistics 2024",
      subtitle = "Sorted by FIP (lgERA 6.27)"
    ) |>
    fmt_number(
      columns = c(ERA, FIP, xFIP, IP, WHIP, `K%`, `BB%`, `K-BB%`, `P/IP`),
      decimals = 2
    ) |>
    fmt_number(
      columns = BABIP,
      decimals = 3
    ) |>
    cols_label(
      player_name = "Player",
      team_name = "Team",
      ERA = "ERA",
      FIP = "FIP",
      xFIP = "xFIP",
      IP = "IP",
      WHIP = "WHIP",
      `K%` = "K%",
      `BB%` = "BB%",
      `K-BB%` = "K-BB%",
      BABIP = "BABIP",
      `P/IP` = "P/IP"
    ) |>
    tab_style(
      style = cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(2)
      ),
      locations = cells_column_labels()
    ) |>
    tab_options(
      table.border.top.color = "black",
      table.border.top.width = px(2),
      table.border.bottom.color = "black",
      table.border.bottom.width = px(2),
      column_labels.background.color = "lightgrey"
    )
}