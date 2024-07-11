library(shiny)
library(bslib)
library(tidyverse)
library(baseballr)
library(patchwork)
library(ggrepel)
library(rsconnect)


# Plot function
plot_pitch_movement_year <- function(data, selected_zones = NULL) {
  
  # Helper functions
  time_to_plate <- function(vy0, ay, y, y0 = 50) {
    vy = -sqrt(vy0^2 + 2*ay*(y-y0))
    t = (vy - vy0) / ay
    return(t)
  }
  
  initial_velocity_mph <- function(vx0, vy0, vz0) {
    v0 = sqrt(vx0^2 + vy0^2 + vz0^2)
    return(v0 * 3600 / 5280)
  }
  
  horizontal_movement <- function(vx0, ax, t) {
    return((vx0 * t + 0.5 * ax * t^2) * 12)
  }
  
  vertical_movement <- function(vz0, az, t) {
    return((vz0 * t + 0.5 * az * t^2) * 12)
  }
  
  # Filter data by selected zones if provided
  if (!is.null(selected_zones)) {
    data <- data |>
      filter(pitch_type != "PO") |>
      group_by(pitch_type, zone) |>
      summarize(
        n_pitches = n(),
        mean_vx0 = mean(vx0, na.rm = TRUE),
        mean_vy0 = mean(vy0, na.rm = TRUE),
        mean_vz0 = mean(vz0, na.rm = TRUE),
        mean_ax = mean(ax, na.rm = TRUE),
        mean_ay = mean(ay, na.rm = TRUE),
        mean_az = mean(az, na.rm = TRUE),
        .groups = "drop"
      )
    data <- data |> 
      filter(zone %in% selected_zones)
  } else {
    data <- data |>
      filter(pitch_type != "PO") |>
      group_by(pitch_type) |>
      summarize(
        n_pitches = n(),
        mean_vx0 = mean(vx0, na.rm = TRUE),
        mean_vy0 = mean(vy0, na.rm = TRUE),
        mean_vz0 = mean(vz0, na.rm = TRUE),
        mean_ax = mean(ax, na.rm = TRUE),
        mean_ay = mean(ay, na.rm = TRUE),
        mean_az = mean(az, na.rm = TRUE)
      )
  }
  
  # Calculate time to plate and initial velocity for each pitch type using mean values
  data <- data |>
    group_by(pitch_type) |>
    summarize(
      across(
        c(mean_vx0, mean_vy0, mean_vz0, mean_ax, mean_ay, mean_az), mean, na.rm = TRUE)) |>
    mutate(
      time_to_plate = time_to_plate(mean_vy0, mean_ay, y = 1.417),
      v0_mph = initial_velocity_mph(mean_vx0, mean_vy0, mean_vz0)
    )
  
  # Remove any rows with NA or infinite time_to_plate
  data <- data |>
    filter(!is.na(time_to_plate) & is.finite(time_to_plate))
  
  # Create sequence of time points
  max_time <- max(data$time_to_plate, na.rm = TRUE)
  time_seq <- seq(0, max_time, by = 0.01)
  
  # Create plot data
  plot_data <- data |>
    crossing(time = time_seq) |>
    mutate(
      horizontal_movement = horizontal_movement(mean_vx0, mean_ax, time),
      vertical_movement = vertical_movement(mean_vz0, mean_az, time)
    ) |>
    group_by(pitch_type) |>
    filter(time <= time_to_plate) |>
    ungroup()
  
  last_points <- plot_data |>
    group_by(pitch_type) |>
    slice_max(time, n = 1) |>
    ungroup()
  
  # Calculate points for perception lines
  perp_points <- last_points %>%
    mutate(
      perp_time = time - 0.150,
      perp_horizontal = horizontal_movement(mean_vx0, mean_ax, perp_time),
      perp_vertical = vertical_movement(mean_vz0, mean_az, perp_time)
    )
  
  # Create the horizontal plot
  p1 <- ggplot(plot_data, aes(x = time, y = horizontal_movement, color = pitch_type)) +
    geom_line() +
    geom_segment(data = perp_points, 
                 aes(x = perp_time, xend = perp_time, 
                     y = perp_horizontal - 0.75, yend = perp_horizontal + 0.75),
                 size = 1) +
    labs(title = "Horizontal Movement",
         x = "Time (seconds)",
         y = "Horizontal Movement (inches)",
         color = "Pitch Type") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, max_time, by = 0.02)) +
    scale_y_continuous(expand = c(0.1, 0)) +
    geom_text_repel(
      aes(label = paste0(pitch_type, " (",round(v0_mph, 1), " mph)")),
      data = last_points,
      size = 5
    ) +
    theme(legend.position = "none")
  
  # Create the vertical plot
  p2 <- ggplot(plot_data, aes(x = time, y = vertical_movement, color = pitch_type)) +
    geom_line() +
    geom_segment(data = perp_points, 
                 aes(x = perp_time, xend = perp_time, 
                     y = perp_vertical - 1.5, yend = perp_vertical + 1.5),
                 size = 1) +
    labs(title = "Vertical Movement",
         x = "Time (seconds)",
         y = "Vertical Movement (inches)",
         color = "Pitch Type") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(0, max_time, by = 0.02)) +
    scale_y_continuous(expand = c(0.1, 0)) +
    geom_text_repel(
      aes(label = paste0(pitch_type, " (",round(v0_mph, 1), " mph)")),
      data = last_points,
      size = 5
    ) +
    theme(legend.position = "none") 
  
  # Combine the plots side by side
  subtitle <- if (is.null(selected_zones)) "All Zones" else paste("Zone(s):", paste(selected_zones, collapse = ", "))
  combined_plot <- p1 + p2 +
    plot_layout(ncol = 1)
  
  return(combined_plot)
}

# Load the full dataset
temp_data1 <- read.csv("/Users/maxwassarman/baseball_work/python/baseball_repo/Stuff/Statcast_2020_to_2022.csv")
temp_data2 <- read.csv("/Users/maxwassarman/baseball_work/python/baseball_repo/Stuff/Statcast_2023.csv")
temp_data3 <- read.csv("/Users/maxwassarman/baseball_work/python/baseball_repo/Stuff/Statcast_2024_upto7-8.csv")
data <- rbind(temp_data1,temp_data2,temp_data3)

player_data <- data |>
  filter(player_name == "Brash, Matt") |>
  filter(substr(game_date, 1, 4) == "2022")

# UI
ui <- page_sidebar(
  title = div(
    style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
    span("Pitch Synergy App"),
    span(
      style = "font-size: 0.8em;",
      "Credit to Eli Ben-Porat for the original plots ",
      a("(X)", href = "https://x.com/EliBenPorat", target = "_blank")
    )
  ),
  sidebar = sidebar(
    textInput("player_name", "Player Name (Last, First)", "deGrom, Jacob"),
    numericInput("year", "Year", 2023, min = 2020, max = 2023),
    checkboxGroupInput("zones", "Select Zones", 
                       choices = c(1:9, 11:14), 
                       selected = c(1:14)),
    actionButton("go", "Generate Plot")
  ),
  card(
    plotOutput("movement_plot")
  )
)

# Server
server <- function(input, output, session) {
  filtered_data <- eventReactive(input$go, {
    req(input$player_name, input$year)
    
    # Check if the player exists in the dataset
    player_exists <- any(data$player_name == input$player_name)
    
    if (!player_exists) {
      return(list(error = "Player not found in the dataset."))
    }
    
    data <- data %>%
      filter(player_name == input$player_name,
             substr(game_date, 1, 4) == as.character(input$year))
    
    if (nrow(data) == 0) {
      return(list(error = paste("No data available for", input$player_name, "in", input$year)))
    }
    
    if (!is.null(input$zones) && length(input$zones) > 0) {
      data <- data %>% filter(zone %in% input$zones)
    }
    
    if (nrow(data) == 0) {
      return(list(error = "No data available for the selected zones."))
    }
    
    data
  })
  
  output$movement_plot <- renderPlot({
    data <- filtered_data()
    
    if (!is.null(data$error)) {
      # Display error message on the plot
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, data$error, cex = 1.5)
    } else {
      plot_pitch_movement_year(data)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)