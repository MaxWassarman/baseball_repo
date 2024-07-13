# College pbp example workflow

```r
library(baseballr)
library(collegebaseball) #Robert Frey Github
library(tidyverse)
library(httr)
library(rvest)

id <- collegebaseball::ncaa_school_id_lookup(team_name = "Oberlin", season = 2024) |>
  select(team_id)
  
schedule <- collegebaseball::ncaa_schedule(team_id = id$team_id, year = 2024)
  
#Choose a contest id from the schedule data

#From pbp_myversion.r (Slight updates to Robert Frey's version from package)
test <- pbp_testing(game_id = 4527024)  

#From NCAA_base_pbp_parser.R (Dave Miller Github (slight updates))
test_parsed <- ncaa_parse(test)
```
