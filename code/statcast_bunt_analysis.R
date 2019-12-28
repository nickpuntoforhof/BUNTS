### Retrosheet_parse_bunts.R ###
### Noah Stafford ###
### This file parses retroshseet data and ###
### finds all plate appearances that ended with a bunt ###

### Libraries
library(tidyverse)

### Note: If I don't end up adding the full retrosheet dataset to the git repository,
### this code cannot be run with the download.  Use the clean code as a starting point.

### Set paths
retrosheet_dir <- "C:/Users/woody/OneDrive/Documents/retrosheet/"
statcast_dir <- "C:/Users/woody/OneDrive/Documents/githubrepos/BUNTS/data/"
output_dir <- "C:/Users/woody/OneDrive/Documents/githubrepos/BUNTS/output/"

### Load in Statcast Bunt Data
bunts_foul_raw <- read.csv(paste0(statcast_dir,"statcast_bunts_fouled.csv"), stringsAsFactors = FALSE)
bunts_in_play_raw <- read.csv(paste0(statcast_dir,"statcast_bunts_in_play.csv"), stringsAsFactors = FALSE)
bunts_missed_raw <- read.csv(paste0(statcast_dir,"statcast_bunts_missed.csv"), stringsAsFactors = FALSE)

### Create dummy variables to differentiate in play/foul/missed
bunts_foul_raw$foul <- 1
bunts_foul_raw$in_play <- 0
bunts_foul_raw$missed <- 0
bunts_in_play_raw$foul <- 0
bunts_in_play_raw$in_play <- 1
bunts_in_play_raw$missed <- 0
bunts_missed_raw$foul <- 0
bunts_missed_raw$in_play <- 0
bunts_missed_raw$missed <- 1

### Combine datasets into dataset of all bunt events (foul bunts, in play bunts, missed bunts)
bunts <- rbind(bunts_foul_raw, bunts_in_play_raw, bunts_missed_raw)
bunts$game_date <- as.Date(bunts$game_date, "%Y-%m-%d")

# Just to check there is no overlap in the datasets (check if missed, foul, and in_play datasets
# have mutually exclusive rows)
nrow(bunts)
nrow(unique(bunts))

# Note: Research if any QC filtering is standard for Statcast Data.  I do not believe there is, however.
# Proably, if I'm going to look into the pitchFX data there should be a filtering step for that.

### Now load-in parsed retrosheet data.  
# What we want is a dataset with the statcast bunt info, and a dataset with retrosheet info
# on each PA where a bunt event occurred, along with the bunt event into aggregated for each PA,
# in such cases where there were multiple bunt events in a PA

### Aggregate data to get rates
player_bunts <- bunts %>% group_by(player_name) %>% summarize(bunt_attempts = n(),
                                                              foul = sum(foul), 
                                                              in_play = sum(in_play), 
                                                              whiff = sum(missed),
                                                              fair_rate = in_play / bunt_attempts,
                                                              foul_rate = foul / bunt_attempts,
                                                              whiff_rate = whiff / bunt_attempts)
player_bunts_filtered <- player_bunts %>% filter(bunt_attempts >= 10)
