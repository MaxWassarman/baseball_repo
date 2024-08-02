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
      BABIP = (H-`HR-A`)/(AB-SO-`HR-A`+SFA),
      `IRS%` = (`Inh Run Score`/`Inh Run`) * 100,
      `GO/FO` = (GO/FO),
      EBHR = (`2B-A`+`3B-A`+`HR-A`) / H
    ) |>
    select(-c(lgERA,lgHR,lgBB,lgHBP,lgK,lgIP,cFIP))
}

create_pitching_table <- function(d3_pitching, team) {
  calculate_pitching_stats(d3_pitching) |>
    select(player_name, team_name, ERA, FIP, xFIP, IP, WHIP, `K%`, `BB%`, `K-BB%`, BABIP, `P/IP`, `IRS%`, `GO/FO`, EBHR) |>
    mutate(across(where(is.numeric), ~round(., 3))) |>
    arrange(FIP) |>
    filter(team_name == team) |>
    gt() |>
    tab_header(
      title = sprintf("Pitching Statistics 2024 - %s", team),
      subtitle = "Sorted by FIP"
    ) |>
    fmt_number(
      columns = c(ERA, FIP, xFIP, IP, WHIP, `K%`, `BB%`, `K-BB%`, `P/IP`, `IRS%`, `GO/FO`, EBHR),
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
      `P/IP` = "P/IP",
      `IRS%` = "IRS%",
      `GO/FO` = "GO/FO",
      EBHR = "EBHR"
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
    ) |>
    tab_footnote(
      footnote = "FIP = Run prevention independent of defense (same scale as ERA)",
      locations = cells_column_labels(columns = FIP)
    ) |>
    tab_footnote(
      footnote = "xFIP = FIP assuming league average home run rate",
      locations = cells_column_labels(columns = xFIP)
    ) |>
    tab_footnote(
      footnote = "BABIP = Batting average allowed on balls in play",
      locations = cells_column_labels(columns = BABIP)
    ) |>
    tab_footnote(
      footnote = "P/IP = Pitches per inning pitched",
      locations = cells_column_labels(columns = `P/IP`)
    ) |>
    tab_footnote(
      footnote = "IRS% = Percentage of inherited runers that score",
      locations = cells_column_labels(columns = `IRS%`)
    ) |>
    tab_footnote(
      footnote = "GO/FO = Ground out to fly out Ratio",
      locations = cells_column_labels(columns = `GO/FO`)
    ) |>
    tab_footnote(
      footnote = "EBHR = Ratio of hits allowed that go for extra bases",
      locations = cells_column_labels(columns = EBHR)
    ) |>
    tab_footnote(
      footnote = "Averages: ERA(6.26), FIP(6.25), xFIP(6.25), WHIP(1.72), K%(16.9), BB%(10.9), K-BB%(6.0), BABIP(0.345), IRS%(48.0), GO/FO(0.89), EBHR(0.27)"
    )
}

create_pitching_table(d3_pitching, "Oberlin")