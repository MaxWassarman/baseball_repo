import streamlit as st
import subprocess
import os
from PIL import Image
import pandas as pd

r_script = """
library(tidyverse)

read_process_csv <- function(file_path) {
  df <- read_csv(file_path, skip = 4, col_names = TRUE)
  
  df$Date <- as.Date(df$Date, format = "%a %b %d %Y")
  df$HB <- as.numeric(df$`HB (spin)`)
  df$VB <- as.numeric(df$`VB (spin)`)
  df$Velocity <- as.numeric(df$Velocity)
  
  return(df)
}

plot_pitches <- function(data, date = NULL, output_file) {
  filtered_data <- data %>%
    filter(!grepl("-", `Pitch Type`))
  
  if (!is.null(date)) {
    filtered_data <- filtered_data %>%
      filter(Date == as.Date(date))
    title <- paste("Pitch Movement on", date)
  } else {
    title <- "Pitch Movement (All Dates)"
  }
  
  filtered_data <- filtered_data %>%
    group_by(`Pitch Type`) %>%
    mutate(
      mean_HB = mean(HB, na.rm = TRUE),
      mean_VB = mean(VB, na.rm = TRUE),
      mean_Velo = round(mean(Velocity, na.rm = TRUE), 2)
    )
  
  p <- ggplot(filtered_data, aes(x=HB, y=VB, color = `Pitch Type`)) +
    geom_point(alpha = 0.8, shape = 16, size = 3) +
    scale_color_manual(values = c("Fastball" = "#D22D49",
                                  "Sinker" = "#FE9D00",
                                  "Slider" = "#EEE716", 
                                  "ChangeUp" = "#1DBE3A",
                                  "Cutter" = "#933F2C",
                                  "CurveBall" = "#00D1ED",
                                  "Splitter" = "#3BACAC")) +
    scale_fill_manual(values = c("Fastball" = "#D22D49",
                                 "Sinker" = "#FE9D00",
                                 "Slider" = "#EEE716", 
                                 "ChangeUp" = "#1DBE3A",
                                 "Cutter" = "#933F2C",
                                 "CurveBall" = "#00D1ED",
                                 "Splitter" = "#3BACAC")) +
    geom_point(aes(x=mean_HB, y = mean_VB, fill = `Pitch Type`), 
               size = 5, shape = 21, color = "black") +
    geom_text(aes(x=mean_HB, y=mean_VB, label=paste0(mean_Velo, " mph")), 
              color="black", vjust=-1.5, size=5) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(limits = c(-25,25)) +
    scale_y_continuous(limits = c(-25,25)) +
    theme_bw() +
    labs(
      x = "Horizontal Movement (in)",
      y = "Induced Vertical Movement (in)",
      title = title
    )
  
  ggsave(output_file, p, width = 12, height = 8, dpi = 300)
}

args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
date <- if(args[2] == "All Dates") NULL else args[2]
output_file <- args[3]

df <- read_process_csv(input_file)
plot_pitches(df, date, output_file)

# Write unique dates to a file
write.csv(format(unique(df$Date), "%Y-%m-%d"), "unique_dates.csv", row.names = FALSE)
"""


with open("plot_script.R", "w") as f:
    f.write(r_script)

st.title("Baseball Pitch Visualization")

uploaded_file = st.file_uploader("Choose a CSV file", type="csv")

if uploaded_file is not None:
    with open("temp.csv", "wb") as f:
        f.write(uploaded_file.getbuffer())
    
    # Get Dates
    subprocess.run(["Rscript", "plot_script.R", "temp.csv", "All Dates", "temp_plot.png"])
    
    dates = pd.read_csv("unique_dates.csv")['x'].tolist()
    
    date_option = st.selectbox(
        'Select a date (optional):',
        ['All Dates'] + dates
    )
    
    # Get Plot
    subprocess.run(["Rscript", "plot_script.R", "temp.csv", date_option, "temp_plot.png"])
    
    # Display the plot
    image = Image.open("temp_plot.png")
    st.image(image, use_column_width=True)

    # Clean up temporary files
    os.remove("temp.csv")
    os.remove("temp_plot.png")
    os.remove("unique_dates.csv")

# Clean up R script file
os.remove("plot_script.R")