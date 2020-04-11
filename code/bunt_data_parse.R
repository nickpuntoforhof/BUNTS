### bunt_data_parse.R ###
### Noah Stafford ###
### This file parses retroshseet data and ###
### finds all plate appearances that ended with a bunt ###

### Libraries
library(tidyverse)
library(stringr)

### Note: If I don't end up adding the full retrosheet dataset to the git repository,
### this code cannot be run directly.  If you would like to replicate the analysis,
### look at the script bunt_analysis.R.  If you would like to run this script, you need to have

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
positions <- read.csv(paste0(statcast_dir,"retrosheet_player_positions_per_game_08_19.csv"), stringsAsFactors = FALSE)
positions$gameDate <- as.Date(positions$gameDate, fomat = "%Y-%m-%d")
id_key_all <- read.csv(paste0(statcast_dir,"chadwick_bureau_people.csv"), stringsAsFactors = FALSE)
id_key <- id_key_all %>% select(key_retro, key_mlbam)

# Merge the retrosheet data with the id_key to get MLBAM keys into the data
positions <- merge(positions, id_key, by.x = "retroID", by.y = "key_retro", all.x = TRUE)

# Get the most frequently played position for each player
player_pos <- positions %>% group_by(retroID,key_mlbam, pos) %>% summarize(n = n()) %>% filter(n==max(n)) %>% 
  distinct(retroID, key_mlbam, .keep_all = TRUE)

# Merge positions with bunts to get the fielding position of each observation in the bunts dataset
bunts <- merge(bunts, player_pos, by.x = "batter", by.y = "key_mlbam", all.x = TRUE)
# Merge positions with bunts to get the retrosheet id for the pitchers
bunts <- merge(bunts, player_pos, by.x = "pitcher", by.y = "key_mlbam", all.x = TRUE)
bunts <- bunts %>% rename("batterRetroID" = "retroID.x", "pos" = "pos.x", "n" = "n.x",
                 "pitcherRetroID" = "retroID.y", "pos_pitch" = "pos.y", "n_pitch" = "n.y")

### Add in retrosheet play-by-play data to get base-out state before and after each PA the bunt attempt occurred
# Set working directory pointing to parsed retrosheet files for the specified dates 
setwd(paste0(statcast_dir, "bevent_2008_2019/"))

# Get all filenames
file_names <- list.files()

# Load in each file in a loop and assign to an element in a list
# Also define the column names
bev_list <- list()
column_labels <- c("game_id", "vis_team", "inning", "batting_team", "outs","balls","strikes","pitch_sequence", "vis_score",
                   "home_score","batter","pitcher","batter_dest","runner_on_1st_dest","runner_on_2nd_dest",
                   "runner_on_3rd_dest","event_num")
i = 1
for(file in file_names){
  print(file)
  loop_list <- read.csv(file, stringsAsFactors = FALSE)
  colnames(loop_list) <- column_labels
  bev_list[[i]] <- loop_list[-c(length(column_labels) + 1, ncol(loop_list))]
  i = i + 1
}

bev_all <- bind_rows(bev_list, .id = "column_label")
bev_all$home_team <- substr(bev_all$game_id, 0, 3)
bev_all$game_date <- as.Date(substr(bev_all$game_id,4,11), format = "%Y%m%d")
bev_all$dblhdr <- substr(bev_all$game_id,12,12)

# Change team names to mlbam format
retro_team_id <- c("ANA","ARI","ATL","BAL","BOS","CHA","CHN","CIN","CLE","COL","DET","FLO","HOU","KCA","LAN","MIL",
                   "MIN","NYA","NYN","OAK","PHI","PIT","SDN","SEA","SFN","SLN","TBA","TEX","TOR","WAS","MIA")
mlbam_team_id <- c("LAA","ARI","ATL","BAL","BOS","CWS","CHC","CIN","CLE","COL",'DET',"FLA",'HOU','KC','LAD','MIL',
                   'MIN','NYY','NYM','OAK','PHI','PIT','SD','SEA','SF','STL','TB','TEX','TOR','WSH','MIA')
team_name_translator <- function(x) {
  team_rosetta <- as.list(setNames(mlbam_team_id, retro_team_id))
  return (team_rosetta[[x]])
}

bev_all$inning_topbot <- ifelse(bev_all$batting_team == 0, "Top", "Bot")
bev_all$vis_team <- sapply(bev_all$vis_team, team_name_translator)
bev_all$home_team <- sapply(bev_all$home_team, team_name_translator)

# Get one entry per PA -- this means we have to filter out rows that represent stolen bases and the like.
# To get the last entry for each PA, simply filter by the entry with the longest pitch_sequence entry.
bev_all <- bev_all %>% group_by(game_date, home_team, vis_team, vis_score, home_score,
  inning, inning_topbot, outs, batter, pitcher) %>% filter(nchar(pitch_sequence) == max(nchar(pitch_sequence)))

# For remaining PAs with more than one observation, take the last observation for each PA
bev_all <- bev_all %>% map_df(rev) %>% distinct(game_date, home_team, vis_team, vis_score, home_score,
                                                inning, inning_topbot, outs, batter, pitcher, .keep_all = TRUE)

# Merge bunts with bev_all to get the before play and after play base-out states for each play
bunts_situation <- merge(bunts, bev_all, by.x = c("game_date","home_team","away_team","home_score", "away_score", "inning","inning_topbot",
                                                  "outs_when_up","batterRetroID","pitcherRetroID"), 
      by.y = c("game_date","home_team","vis_team","home_score", "vis_score", "inning",
               "inning_topbot","outs","batter","pitcher"), all.x = TRUE)

# Bunts_situation is the data we need for analysis! Write bunts_situation to an R data object.
save(bunts_situation, file = paste0(statcast_dir, "bunts.RDATA"))
write.csv(bunts_situation, file = paste0(statcast_dir, "bunts.csv"), row.names = FALSE)
