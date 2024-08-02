library(tidyverse)
library(gt)

calculate_hitting_stats <- function(stats, weights) {
  
  weights <- weights |>
    select(events, normalized_weight) |>
    deframe()
  
  woba_scale <- weights["wOBA scale"]

  league_totals <- stats |>
    summarise(
      BB = sum(BB),
      HBP = sum(HBP),
      `1B` = sum(H) - sum(`2B`) - sum(`3B`) - sum(HR),
      `2B` = sum(`2B`),
      `3B` = sum(`3B`),
      HR = sum(HR),
      AB = sum(AB),
      SF = sum(SF),
      SH = sum(SH)
    )
  
  league_woba <- with(league_totals,
                      (weights["walk"] * BB +
                         weights["hit_by_pitch"] * HBP +
                         weights["single"] * `1B` +
                         weights["double"] * `2B` +
                         weights["triple"] * `3B` +
                         weights["home_run"] * HR) /
                        (AB + BB + SF + SH + HBP)
  )
  
  stats |>
    mutate(
      `1B` = H-`2B`-`3B`-HR,
      wOBA_numerator = (weights["walk"] * BB +
                          weights["hit_by_pitch"] * HBP +
                          weights["single"] * `1B` +
                          weights["double"] * `2B` +
                          weights["triple"] * `3B` +
                          weights["home_run"] * HR),
      wOBA_denominator = (AB + BB + SF - SH + HBP),
      wOBA = wOBA_numerator / wOBA_denominator,
      wRAA = ((wOBA - league_woba) / woba_scale) * (AB + BB + SF + SH + HBP),
      `K%` = K/(AB + BB + SF + SH + HBP)*100,
      `BB%` = BB/(AB + BB + SF + SH + HBP)*100,
      BABIP = (H-HR)/(AB-K-HR+SF+SH),
      OPS = OBPct + SlgPct
    )
}

create_hitting_table <- function(d3_batting, d3_weights, team) {
  calculate_hitting_stats(d3_batting, d3_weights) |>
    filter(AB > 0) |>  # Filter out players with 0 AB
    select(player_name, team_name, BA, OPS, wOBA, wRAA, `K%`, `BB%`, BABIP, AB) |>
    mutate(across(where(is.numeric), ~round(., 3))) |>  # Round to 3 decimals initially
    arrange(desc(wRAA)) |>
    filter(team_name == team) |>
    gt() |>
    tab_header(
      title = sprintf("Hitting Statistics 2024 - %s", team),
      subtitle = "Sorted by wRAA (Weighted Runs Above Average)"
    ) |>
    fmt_number(
      columns = c(BA, OPS, wOBA, BABIP),
      decimals = 3
    ) |>
    fmt_number(
      columns = c(wRAA, `K%`, `BB%`),
      decimals = 2
    ) |>
    cols_label(
      player_name = "Player",
      team_name = "Team",
      BA = "AVG",
      OPS = "OPS",
      wOBA = "wOBA",
      wRAA = "wRAA",
      `K%` = "K%",
      `BB%` = "BB%",
      BABIP = "BABIP",
      AB = "AB"
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
      footnote = "wOBA = version of on base percentage that accounts for how a player reached base",
      locations = cells_column_labels(columns = wOBA)
    ) |>
    tab_footnote(
      footnote = "wRAA = Runs contributed by a hitter compared to league average",
      locations = cells_column_labels(columns = wRAA)
    ) |>
    tab_footnote(
      footnote = "BABIP = Batting average on balls in play",
      locations = cells_column_labels(columns = BABIP)
    ) |>
    tab_footnote(
      footnote = "Averages: AVG(.292), OPS(.806), wOBA(.389), wRAA(0), K%(17.1), BB%(11.0), BABIP(0.345)"
    )
}

create_hitting_table(d3_batting, d3_weights, "UMass Boston")