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
# Derive some statistics
bunts <- rbind(bunts_foul_raw, bunts_in_play_raw, bunts_missed_raw)
bunts$game_date <- as.Date(bunts$game_date, "%Y-%m-%d")
bunts$player_team <- ifelse(bunts$inning_topbot == "Bot",bunts$home_team, bunts$away_team)
bunts$hit <- ifelse(bunts$events %in% c("single","double","triple","home_run"), 1,0)
bunts$sac <- ifelse(bunts$events %in% c("sac_bunt"), 1, 0)
bunts$dp <- ifelse(bunts$events %in% c("grounded_into_double_play", "double_play"), 1, 0)
bunts$error <- ifelse(bunts$events %in% c("field_error"), 1, 0)
bunts$k <- ifelse(bunts$events %in% c("strikeout"), 1, 0)
bunts$inf_align_shift <- ifelse(bunts$if_fielding_alignment %in% c("Infield shift"), 1, 0)
bunts$inf_align_unkn <- ifelse(bunts$if_fielding_alignment %in% c("null"), 1, 0)
bunts$inf_align_standard <- ifelse(bunts$if_fielding_alignment %in% c("Standard"), 1, 0)
bunts$inf_align_strategic <- ifelse(bunts$if_fielding_alignment %in% c("Strategic"), 1, 0)
bunts$launch_speed <- as.numeric(bunts$launch_speed)
bunts$runner_1b <- ifelse(bunts$on_1b == "null", 0, 1)
bunts$runner_2b <- ifelse(bunts$on_2b == "null", 0, 1)
bunts$runner_3b <- ifelse(bunts$on_3b == "null", 0, 1)

# Just to check there is no overlap in the datasets (check if missed, foul, and in_play datasets
# have mutually exclusive rows)
#nrow(bunts)
#nrow(unique(bunts))

# Note: Research if any QC filtering is standard for Statcast Data.  I do not believe there is, however.
# Proably, if I'm going to look into the pitchFX data there should be a filtering step for that.

### Now load-in parsed retrosheet data.  
positions <- read.csv("./data/retrosheet_player_positions_per_game_08_19.csv", stringsAsFactors = FALSE)
positions$gameDate <- as.Date(positions$gameDate, fomat = "%Y-%m-%d")
id_key_all <- read.csv("./data/chadwick_bureau_people.csv", stringsAsFactors = FALSE)
id_key <- id_key_all %>% select(key_retro, key_mlbam)

# Merge the retrosheet data with the id_key to get MLBAM keys into the data
positions <- merge(positions, id_key, by.x = "retroID", by.y = "key_retro", all.x = TRUE)

# Get the most frequently played position for each player
player_pos <- positions %>% group_by(retroID,playerName ,key_mlbam, pos) %>% summarize(n = n()) %>% filter(n==max(n))

# Merge positions with bunts to get the fielding position of each observation in the bunts dataset
bunts <- merge(bunts, player_pos, by.x = "batter", by.y = "key_mlbam", all.x = TRUE)

### Aggregate data to get rates
player_bunts <- bunts %>% group_by(player_name, batter, pos) %>% summarize(bunt_attempts = n(),
                                                              hits = sum(hit),
                                                              sacrifices = sum(sac),
                                                              foul = sum(foul), 
                                                              in_play = sum(in_play), 
                                                              whiff = sum(missed),
                                                              fair_rate = in_play / bunt_attempts,
                                                              foul_rate = foul / bunt_attempts,
                                                              whiff_rate = whiff / bunt_attempts,
                                                              BUH_pct = hits / in_play,
                                                              double_plays = sum(dp),
                                                              errors = sum(error),
                                                              k = sum(k), 
                                                              H_SAC_pct = (hits + sacrifices + errors)/ in_play,
                                                              mean_exit_velo = mean(launch_speed, na.rm = TRUE),
                                                              max_speed = max(launch_speed, na.rm = TRUE))
# Add pitcher indicator variable
player_bunts$pitcher_pos <- factor(ifelse(player_bunts$pos == 1, "Pitcher", "Position Player"))

### ANALYSIS

# Set a minimum # of bunt attempts at min_bunt_thresh
min_bunt_thresh = 20
d <- player_bunts %>% filter(bunt_attempts >= min_bunt_thresh)


ggplot(d, aes(x= fair_rate, y= 1-H_SAC_pct)) + geom_point() + 
  labs(x = "Fair Bunt %", y = "Out %") + ylim(0,1) + xlim(0,1) + ggtitle("") + 
  facet_grid(cols = vars(pitcher_pos)) +
  geom_text(aes(label=ifelse((H_SAC_pct<0.3) | (fair_rate < 0.24),player_name,'')), 
            col='black',hjust=1.2,vjust=0) + 
  ggtitle("Out % and Fair Bunt % for all hitters in Statcast Era min. 30 bunt attempts")

bad_bunters <- d %>% filter(player_name %in% c("Rich Hill", "Bobby Abreu", 
                                               "Gabe Kapler", "Chris Archer", 
                                               "Carlos Frias",
                                               "James Jone", "Dan Uggla"))
# 75% of Rich Hill's bunts


### Modeling bunt probability
# Calculate the expected value of a bunt in each base-out state

# Perhaps calculate these values by calculating win probability given that a
# player is trying to bunt.
hit_weight = 1.5
sac_weight = 1
error_weight = 1.5
dp_weight = -1
out_weight = 0

bunt_counts <- bunts %>% group_by(outs_when_up, runner_1b, runner_2b, runner_3b, .drop = FALSE) %>% 
  summarize(in_play = sum(in_play),
            hit = sum(hit),
            sac = sum(sac),
            error = sum(error),
            dp = sum(dp),
            out = in_play - (hit + sac + error + dp),
            expected_runs = (hit*hit_weight + sac*sac_weight + error*error_weight + dp*dp_weight + out*out_weight) / in_play)
