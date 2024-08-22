library(tidyverse)
library(tidymodels)
library(arrow)
library(baseballr)

set.seed(12345)


df <- read_csv("/Users/maxwassarman/baseball_work/python/baseball_repo/Stuff/Statcast_2020_to_2023.csv")
sampled_df <- df |> 
  sample_frac(0.2)


data <- sampled_df |>
  mutate(
    pfx_x = -12 * pfx_x,
    pfx_z = 12 * pfx_z,
    vy_f = -1 * sqrt((vy0^2) - (2 * ay * (50 - (17/12)))),
    t = (vy_f - vy0) / ay,
    vz_f = vz0 + (az * t),
    vx_f = vx0 + (ax * t),
    VAA = -1 * atan(vz_f / vy_f) * (180 / pi),
    HAA = -1 * atan(vx_f / vy_f) * (180 / pi),
    VAA = round(VAA, 2),
    HAA = round(HAA, 2)
  )


vaa_model_data <- data |>
  select(VAA, release_speed, release_extension, release_pos_z, pfx_z, plate_z) |>
  drop_na()


vaa_split <- initial_split(vaa_model_data, prop = 0.75)
vaa_train <- training(vaa_split)
vaa_test <- testing(vaa_split)


vaa_rf <- rand_forest(mode = "regression", trees = 50) |>
  set_engine("randomForest") |>
  fit(VAA ~ release_speed + release_extension + release_pos_z + pfx_z + plate_z, data = vaa_train)


vaa_preds <- predict(vaa_rf, new_data = vaa_test)

vaa_results <- vaa_test |>
  select(VAA) |>
  bind_cols(vaa_preds)

ggplot(vaa_results, aes(x = VAA, y = .pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "pink", linetype = "dashed") +
  theme_bw() +
  labs(title = "Actual vs Predicted VAA",
       x = "Actual VAA",
       y = "Predicted VAA")

vaa_metrics <- metric_set(mae)

vaa_performance <- vaa_metrics(vaa_results, truth = VAA, estimate = .pred)
print(vaa_performance)



haa_model_data <- data |>
  select(HAA, release_pos_x, plate_x, pfx_x) |>
  drop_na()

haa_split <- initial_split(haa_model_data, prop = 0.75)
haa_train <- training(haa_split)
haa_test <- testing(haa_split)


haa_rf <- rand_forest(mode = "regression", trees = 50) |>
  set_engine("randomForest") |>
  fit(HAA ~ release_pos_x + plate_x + pfx_x, data = haa_train)


haa_preds <- predict(haa_rf, new_data = haa_test)

haa_results <- haa_test |>
  select(HAA) |>
  bind_cols(haa_preds)

ggplot(haa_results, aes(x = HAA, y = .pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "pink", linetype = "dashed") +
  theme_bw() +
  labs(title = "Actual vs Predicted HAA",
       x = "Actual HAA",
       y = "Predicted HAA")

haa_metrics <- metric_set(mae)

haa_performance <- haa_metrics(haa_results, truth = HAA, estimate = .pred)
print(haa_performance)

saveRDS(vaa_rf, file = "/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/vaa_rf_model.rds")
saveRDS(haa_rf, file = "/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/haa_rf_model.rds")

new_data <- df |>
  mutate(
    pfx_x = -12 * pfx_x,
    pfx_z = 12 * pfx_z,
    vy_f = -1 * sqrt((vy0^2) - (2 * ay * (50 - (17/12)))),
    t = (vy_f - vy0) / ay,
    vz_f = vz0 + (az * t),
    vx_f = vx0 + (ax * t),
    VAA = -1 * atan(vz_f / vy_f) * (180 / pi),
    HAA = -1 * atan(vx_f / vy_f) * (180 / pi),
    VAA = round(VAA, 2),
    HAA = round(HAA, 2),
    plate_z = 2.5,
    plate_x = 0
  )



new_data_preds <- new_data |>
  drop_na(VAA, release_speed, release_extension, release_pos_z, pfx_z, plate_z,
          HAA, release_pos_x, pfx_x, plate_x) |>
  mutate(adj_vaa = predict(vaa_rf, new_data = pick(VAA, release_speed, release_extension, release_pos_z, pfx_z, plate_z))$.pred,
         adj_haa = predict(haa_rf, new_data = pick(HAA, release_pos_x, pfx_x, plate_x))$.pred,
         adj_haa = adj_haa*-1) |> 
  left_join(new_data, by = names(new_data))

write_parquet(new_data_preds, "/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/sc_20_23.parquet")  

vaa_rf <- readRDS("/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/vaa_rf_model.rds")
haa_rf <- readRDS("/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/haa_rf_model.rds")

new_data_preds <- read_parquet("/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/sc_20_23.parquet")

###

new_data_preds |>
  filter(player_name == "Snell, Blake",
         game_year == 2023) |>
  group_by(pitch_type) |>
  ggplot(aes(x = adj_haa, y = adj_vaa, color = pitch_type)) +
  geom_point() +
  scale_y_continuous(limits = c(-12.5, -3.5), breaks = seq(-12.5, -3.5, 1)) +
  scale_x_continuous(limits = c(-6, 6), breaks = seq(-6, 6, 1)) +
  geom_hline(yintercept = seq(-12.5, -3.5, 1), linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_vline(xintercept = seq(-6, 6, 1), linetype = "dashed", color = "gray", alpha = 0.5) +
  labs(x = "Adj HAA", 
       y = "Adj VAA",
       title = "Blake Snell: Pitch Approach Angles",
       color = "Pitch Type") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_color_manual(values = c("FF" = "#D22D49",
                                "SI" = "#FE9D00",
                                "SL" = "#EEE716", 
                                "CH" = "#1DBE3A",
                                "FC" = "#933F2C",
                                "KC" = "#6236CD",
                                "ST" = "#DDB33A",
                                "SV" = "#93AFD4",
                                "CU" = "#00D1ED",
                                "FS" = "#3BACAC",
                                "FO" = "#55CCAB"))


#ggsave("adj_angle_ex_player_plot.png")

###

whiff_events <- c(
  "swinging_strike", "foul_tip", "foul_bunt", 
  "missed_bunt", "swinging_strike_blocked"
)

swing_events <- c(
  "foul_tip", "swinging_strike", "swinging_strike_blocked", 
  "missed_bunt", "foul", "hit_into_play", "foul_bunt", "bunt_foul_tip"
)


vaa_whiff <- new_data_preds |>
  mutate(
    is_whiff = if_else(description %in% whiff_events, 1, 0),
    is_swing = if_else(description %in% swing_events, 1, 0),
  ) |>
  group_by(adj_vaa = round(adj_vaa, 1)) |>
  summarise(
    whiff_perc = sum(is_whiff, na.rm = TRUE) / sum(is_swing, na.rm = TRUE),
  ) |>
  ggplot(aes(x = adj_vaa, y = whiff_perc)) +
  geom_point() +
  geom_smooth(span = .4) +
  scale_x_continuous(limits = c(-12.5, 1), breaks = seq(-12.5, 1, 2.5)) +
  geom_vline(xintercept = -6.79, color="red") +
  geom_vline(xintercept = -2.7, color="red") +
  geom_hline(yintercept = 0.288, color="red") +
  labs(
    title = "Whiff Percentage by Adjusted Vertical Approach Angle",
    x = "Adjusted Vertical Approach Angle (degrees)",
    y = "Whiff Percentage"
  ) +
  theme_bw()

print(vaa_whiff)

#ggsave("/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/vaa_whiff.png")

###

vaa_gb <- new_data_preds |>
  mutate(
    is_gb = if_else(bb_type == "ground_ball", 1, 0),
    in_play = if_else(description == "hit_into_play", 1, 0)
  ) |>
  group_by(adj_vaa = round(adj_vaa, 1)) |>
  summarise(
    gb_perc = sum(is_gb, na.rm = TRUE) / sum(in_play, na.rm = TRUE)
  ) |>
  ggplot(aes(x = adj_vaa, y = gb_perc)) +
  geom_point() +
  geom_smooth(span = 0.3) +
  scale_x_continuous(limits = c(-12.5, 1), breaks = seq(-12.5, 1, 2.5)) +
  geom_vline(xintercept = -5.55, color="red") +
  geom_vline(xintercept = -10.85, color="red") +
  geom_hline(yintercept = 0.425, color="red") +
  labs(
    title = "Ground Ball Percentage by Adjusted Vertical Approach Angle",
    x = "Adjusted Vertical Approach Angle (degrees)",
    y = "Ground Ball Percentage"
  ) +
  theme_bw()

print(vaa_gb)

#ggsave("/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/vaa_gb.png")

###

haa_whiff <- new_data_preds |>
  mutate(
    is_whiff = if_else(description %in% whiff_events, 1, 0),
    is_swing = if_else(description %in% swing_events, 1, 0),
  ) |>
  group_by(adj_haa = round(adj_haa, 1)) |>
  summarise(
    whiff_perc = sum(is_whiff, na.rm = TRUE) / sum(is_swing, na.rm = TRUE),
  ) |>
  ggplot(aes(x = adj_haa, y = whiff_perc)) +
  geom_point() +
  geom_smooth(span = .4) +
  scale_x_continuous(limits = c(-6, 1), breaks = seq(-6, 1, 2)) +
  labs(
    title = "Whiff Percentage by Adjusted Horizontal Approach Angle",
    x = "Adjusted Horizontal Approach Angle (degrees)",
    y = "Whiff Percentage"
  ) +
  theme_bw()

print(haa_whiff)

#ggsave("/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/haa_whiff.png")

###

haa_gb <- new_data_preds |>
  mutate(
    is_gb = if_else(bb_type == "ground_ball", 1, 0),
    in_play = if_else(description == "hit_into_play", 1, 0)
  ) |>
  group_by(adj_haa = round(adj_haa, 1)) |>
  summarise(
    gb_perc = sum(is_gb, na.rm = TRUE) / sum(in_play, na.rm = TRUE)
  ) |>
  ggplot(aes(x = adj_haa, y = gb_perc)) +
  geom_point() +
  geom_smooth(span = 0.3) +
  scale_x_continuous(limits = c(-6, 1), breaks = seq(-6, 1, 2)) +
  labs(
    title = "Ground Ball Percentage by Adjusted Horizontal Approach Angle",
    x = "Adjusted Horizontal Approach Angle (degrees)",
    y = "Ground Ball Percentage"
  ) +
  theme_bw()

print(haa_gb)

#ggsave("/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/haa_gb.png")

###


pitch_types <- c("FF", "SI", "FC", "SL", "ST", "CU", "CH", "FS")
pitch_names <- c("Fastball", "Sinker", "Cutter", "Slider", "Sweeper", "Curveball", "Changeup", "Splitter")

new_data_preds |>
  filter(game_year == 2022, 
         p_throws == "R", 
         pitch_type %in% pitch_types) |>
  mutate(pitch_type = factor(pitch_type, levels = pitch_types)) |>
  ggplot(aes(x = adj_haa, y = adj_vaa, color = pitch_type, fill = pitch_type)) +
  stat_ellipse(geom = "polygon", alpha = 0.2, level = 0.6) +
  stat_ellipse(geom = "path", alpha = 1, level = 0.6) +
  scale_y_continuous(limits = c(-12, -3), breaks = seq(-12, -3, 1)) +
  scale_x_continuous(limits = c(-6, 6), breaks = seq(-6, 6, 1)) +
  geom_hline(yintercept = seq(-12, -3, 1), linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_vline(xintercept = seq(-6, 6, 1), linetype = "dashed", color = "gray", alpha = 0.5) +
  labs(x = "Adj HAA", 
       y = "Adj VAA",
       title = "Pitch Approach Angles by Type (Right-Handed Pitchers)",
       color = "Pitch Type",
       fill = "Pitch Type") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right") +
  scale_color_manual(values = c("FF" = "#D22D49",
                                "CH" = "#1DBE3A",
                                "CU" = "#00D1ED",
                                "FC" = "#933F2C",
                                "SI" = "#FE9D00",
                                "SL" = "#EEE716",
                                "ST" = "#DDB33A",
                                "FS" = "#3BACAC"),
                     labels = setNames(pitch_names, pitch_types)) +
  scale_fill_manual(values = c("FF" = "#D22D49",
                               "CH" = "#1DBE3A",
                               "CU" = "#00D1ED",
                               "FC" = "#933F2C",
                               "SI" = "#FE9D00",
                               "SL" = "#EEE716",
                               "ST" = "#DDB33A",
                               "FS" = "#3BACAC"),
                    labels = setNames(pitch_names, pitch_types)) +
  coord_fixed(ratio = 1)


###

pitch_types <- c("FF", "CH", "CU", "FC", "SI", "SL", "ST", "FS")
pitch_names <- c("Fastball", "Changeup", "Curveball", "Cutter", "Sinker", "Slider", "Sweeper", "Splitter")

new_data_preds |>
  filter(game_year == 2023, 
         p_throws %in% c("R", "L"), 
         pitch_type %in% pitch_types) |>
  mutate(pitch_type = factor(pitch_type, levels = pitch_types),
         p_throws = factor(p_throws, levels = c("R", "L"), labels = c("Right-Handed", "Left-Handed"))) |>
  ggplot(aes(x = adj_haa, y = adj_vaa, color = pitch_type, fill = pitch_type)) +
  stat_ellipse(geom = "polygon", alpha = 0.2, level = 0.6) +
  stat_ellipse(geom = "path", alpha = 1, level = 0.6) +
  scale_y_continuous(limits = c(-12, -3), breaks = seq(-12, -3, 1)) +
  scale_x_continuous(limits = c(-6, 6), breaks = seq(-6, 6, 1)) +
  geom_hline(yintercept = seq(-12, -3, 1), linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_vline(xintercept = seq(-6, 6, 1), linetype = "dashed", color = "gray", alpha = 0.5) +
  labs(x = "Adjusted HAA", 
       y = "Adjusted VAA",
       title = "Pitch Approach Angles by Type and Pitcher Handedness (2022)",
       color = "Pitch Type",
       fill = "Pitch Type") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
        ) +
  scale_color_manual(values = c("FF" = "#D22D49",
                                "CH" = "#1DBE3A",
                                "CU" = "#00D1ED",
                                "FC" = "#933F2C",
                                "SI" = "#FE9D00",
                                "SL" = "#EEE716",
                                "ST" = "#DDB33A",
                                "FS" = "#3BACAC"),
                     labels = setNames(pitch_names, pitch_types)) +
  scale_fill_manual(values = c("FF" = "#D22D49",
                               "CH" = "#1DBE3A",
                               "CU" = "#00D1ED",
                               "FC" = "#933F2C",
                               "SI" = "#FE9D00",
                               "SL" = "#EEE716",
                               "ST" = "#DDB33A",
                               "FS" = "#3BACAC"),
                    labels = setNames(pitch_names, pitch_types)) +
  coord_fixed(ratio = 1) +
  facet_wrap(~ p_throws, ncol = 2)

#ggsave("/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/league_avg_angle.png")

###



haa_breaks <- seq(-6, 6, length.out = 25)
vaa_breaks <- seq(-12, -3, length.out = 25)


haa_width <- diff(haa_breaks)[1]
vaa_width <- diff(vaa_breaks)[1]


whiff_data <- new_data_preds |>
  filter(game_year == 2023, 
         p_throws %in% c("R", "L"),
         adj_haa >= -6 & adj_haa <= 6,
         adj_vaa >= -12 & adj_vaa <= -3) |>
  mutate(p_throws = factor(p_throws, levels = c("R", "L"), labels = c("Right-Handed", "Left-Handed")),
         is_whiff = if_else(description %in% whiff_events, 1, 0),
         is_swing = if_else(description %in% swing_events, 1, 0),
         adj_haa_bin = cut(adj_haa, breaks = haa_breaks, include.lowest = TRUE, labels = FALSE),
         adj_vaa_bin = cut(adj_vaa, breaks = vaa_breaks, include.lowest = TRUE, labels = FALSE)) |>
  group_by(p_throws, adj_haa_bin, adj_vaa_bin) |>
  summarise(whiff_pct = sum(is_whiff) / sum(is_swing),
            adj_haa = haa_breaks[adj_haa_bin] + haa_width/2,
            adj_vaa = vaa_breaks[adj_vaa_bin] + vaa_width/2,
            n_pitches = n(),
            .groups = "drop") |>
  filter(!is.na(whiff_pct), n_pitches >= 10)

pitch_data <- new_data_preds |>
  filter(game_year == 2023, 
         p_throws %in% c("R", "L"), 
         pitch_type %in% pitch_types,
         adj_haa >= -6 & adj_haa <= 6,
         adj_vaa >= -12 & adj_vaa <= -3) |>
  mutate(pitch_type = factor(pitch_type, levels = pitch_types),
         p_throws = factor(p_throws, levels = c("R", "L"), labels = c("Right-Handed", "Left-Handed")))


ggplot() +
  geom_tile(data = whiff_data, aes(x = adj_haa, y = adj_vaa, fill = whiff_pct), 
            width = haa_width, height = vaa_width) +
  stat_ellipse(data = pitch_data, aes(x = adj_haa, y = adj_vaa, color = pitch_type), 
               geom = "path", alpha = 1, level = 0.7) +
  scale_fill_gradientn(colors = c("blue", "white", "red"),
                       values = scales::rescale(c(0, 0.25, 0.4)),
                       limits = c(0, 0.4),
                       oob = scales::squish,
                       name = "Whiff %", 
                       labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("FF" = "#D22D49", "CH" = "#1DBE3A", "CU" = "#00D1ED",
                                "FC" = "#933F2C", "SI" = "#FE9D00", "SL" = "#EEE716",
                                "ST" = "#DDB33A", "FS" = "#3BACAC"),
                     labels = setNames(pitch_names, pitch_types),
                     name = "Pitch Type") +
  scale_y_continuous(limits = c(-12, -3), breaks = seq(-12, -3, 1)) +
  scale_x_continuous(limits = c(-6, 6), breaks = seq(-6, 6, 1)) +
  geom_hline(yintercept = seq(-12, -3, 1), linetype = "dashed", color = "gray", alpha = 0.5) +
  geom_vline(xintercept = seq(-6, 6, 1), linetype = "dashed", color = "gray", alpha = 0.5) +
  labs(x = "Adjusted Horizontal Approach Angle", 
       y = "Adjusted Vertical Approach Angle",
       title = "Whiff Percentage and Pitch Approach Angles (2023)",
       subtitle = "Minimum 10 pitches per bin | Ellipses show 70% of pitches") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)) +
  coord_fixed(ratio = 1) +
  facet_wrap(~ p_throws, ncol = 2)

#ggsave("/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/league_avg_angles_whiff.png")


###


create_combined_pitch_plot <- function(data, pitcher_name, pitcher_year, league_year) {
  
  pitch_types <- c("FF", "CH", "CU", "FC", "SI", "SL", "ST", "FS")
  pitch_names <- c("Fastball", "Changeup", "Curveball", "Cutter", "Sinker", "Slider", "Sweeper", "Splitter")
  
  pitcher_hand <- data |>
    filter(player_name == pitcher_name, game_year == pitcher_year) |>
    pull(p_throws) |>
    unique()
  
  if(length(pitcher_hand) == 0) {
    stop("Pitcher not found in the dataset for the specified year.")
  }
  
  pitcher_means <- data |>
    filter(player_name == pitcher_name, game_year == pitcher_year) |>
    group_by(pitch_type) |>
    summarise(mean_vaa = mean(adj_vaa, na.rm = TRUE),
              mean_haa = mean(adj_haa, na.rm = TRUE))
  
  data |>
    filter(game_year == league_year, 
           p_throws == pitcher_hand, 
           pitch_type %in% pitch_types) |>
    mutate(pitch_type = factor(pitch_type, levels = pitch_types),
           p_throws = factor(p_throws, levels = c("R", "L"), 
                             labels = c("Right-Handed", "Left-Handed"))) |>
    ggplot(aes(x = adj_haa, y = adj_vaa, color = pitch_type, fill = pitch_type)) +git 
    stat_ellipse(geom = "polygon", alpha = 0.2, level = 0.6) +
    stat_ellipse(geom = "path", alpha = 1, level = 0.6) +
    geom_point(data = pitcher_means, aes(x = mean_haa, y = mean_vaa), size = 4, shape = 16, fill = "white") +
    scale_y_continuous(limits = c(-12, -3), breaks = seq(-12, -3, 1)) +
    scale_x_continuous(limits = c(-6, 6), breaks = seq(-6, 6, 1)) +
    geom_hline(yintercept = seq(-12, -3, 1), linetype = "dashed", color = "gray", alpha = 0.5) +
    geom_vline(xintercept = seq(-6, 6, 1), linetype = "dashed", color = "gray", alpha = 0.5) +
    labs(x = "Adjusted HAA", 
         y = "Adjusted VAA",
         title = paste("Pitch Approach Angles: League-wide vs.", pitcher_name),
         subtitle = paste("League-wide distributions (", league_year, ") with pitchers average pitch angles (", pitcher_year, ")"),
         color = "Pitch Type",
         fill = "Pitch Type") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "right",
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)) +
    scale_color_manual(values = c("FF" = "#D22D49", "CH" = "#1DBE3A", "CU" = "#00D1ED",
                                  "FC" = "#933F2C", "SI" = "#FE9D00", "SL" = "#EEE716",
                                  "ST" = "#DDB33A", "FS" = "#3BACAC"),
                       labels = setNames(pitch_names, pitch_types)) +
    scale_fill_manual(values = c("FF" = "#D22D49", "CH" = "#1DBE3A", "CU" = "#00D1ED",
                                 "FC" = "#933F2C", "SI" = "#FE9D00", "SL" = "#EEE716",
                                 "ST" = "#DDB33A", "FS" = "#3BACAC"),
                      labels = setNames(pitch_names, pitch_types)) +
    coord_fixed(ratio = 1)
}


create_combined_pitch_plot(new_data_preds, "Snell, Blake", 2023, 2023)

#ggsave("/Users/maxwassarman/baseball_work/python/baseball_repo/Pitching/league_avg_anlges_snell.png")
