### Retrosheet_parse_bunts.R ###
### Noah Stafford ###
### This file parses retroshseet data and ###
### finds all plate appearances that ended with a bunt ###

### Libraries
library(tidyverse)
library(stringr)
library(zoo)

# Set working directory
setwd("~/githubrepos/BUNTS")

# Load in data
load("./data/bunts.RDATA")

table(bunts_situation$launch_speed)

hard_bunts <- bunts_situation %>% filter(launch_speed > 95)

### ANALYSIS

### Aggregate data by date to get rolling means
bunts_date <- bunts_situation %>% arrange(game_date) %>% group_by(game_date) %>% summarize(bunt_attempts = n(),
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
                                                                                     max_speed = max(launch_speed, na.rm = TRUE)) %>%
  mutate(year = as.numeric(format(game_date, "%Y")))

### 
bunts_rolling <- bunts_date %>% arrange(game_date) %>% group_by(year) %>% mutate(bunt_attempts = rollsum(bunt_attempts, 30, align = 'left', fill = 'NA'),
                                                                                    hits = rollsum(hits, 30, align = 'left', fill = 'NA'),
                                                                                    sacrifices = rollsum(sacrifices, 30, align = 'left', fill = 'NA'),
                                                                                    foul = rollsum(foul, 30, align = 'left', fill = 'NA'), 
                                                                                    in_play = rollsum(in_play, 30, align = 'left', fill = 'NA'),
                                                                                    whiff = rollsum(whiff, 30, align = 'left', fill = 'NA'),
                                                                                    fair_rate = in_play / bunt_attempts,
                                                                                    foul_rate = foul / bunt_attempts,
                                                                                    whiff_rate = whiff / bunt_attempts,
                                                                                    BUH_pct = hits / in_play,
                                                                                    double_plays = rollsum(double_plays, 30, align = 'left', fill = 'NA'),
                                                                                    errors = rollsum(errors, 30, align = 'left', fill = 'NA'),
                                                                                    k = rollsum(k, 30, align = 'left', fill = 'NA'), 
                                                                                    H_SAC_pct = (hits + sacrifices + errors)/ in_play)
                                                                                    #mean_exit_velo = mean(launch_speed, na.rm = TRUE),
                                                                                    #max_speed = max(launch_speed, na.rm = TRUE))

ggplot(bunts_rolling, aes(x = game_date, y = bunt_attempts)) + geom_bar(stat='identity')
ggplot(bunts_rolling, aes(x = game_date, y = fair_rate)) + geom_line(group='identity')

### Aggregate data by player to get rates
player_bunts <- bunts_situation %>% group_by(player_name, batter, pos) %>% summarize(bunt_attempts = n(),
                                                                           hits = rollsum(hit, ),
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
player_bunts$pitcher_pos <- factor(ifelse(player_bunts$pos == 1, "Pitchers", "Position Players"))


# Set a minimum # of bunt attempts at min_bunt_thresh
min_bunt_thresh = 30
d <- player_bunts %>% filter(bunt_attempts >= min_bunt_thresh)


ggplot(d, aes(x= fair_rate, y= 1-H_SAC_pct)) + geom_point() + 
  labs(x = "Fair Bunts per Attempt", y = "Outs per Bunt In Play") + ylim(0,1) + xlim(0,1) + ggtitle("") + 
  facet_grid(cols = vars(pitcher_pos)) +
  geom_text(aes(label=ifelse((H_SAC_pct<0.3) | (fair_rate < 0.24),player_name,'')), 
            col='black',hjust=1.2,vjust=0) + geom_text(aes(label=ifelse(fair_rate > 0.75,player_name,'')), 
                                                       col='black',hjust=-0.2,vjust=0) +
  theme_light() +
  ggtitle("Outs per Bunt in Play and Fair Bunt % for all hitters in Statcast Era min. 30 bunt attempts")

bad_bunters <- d %>% filter(player_name %in% c("Rich Hill", "Bobby Abreu", 
                                               "Gabe Kapler", "Chris Archer", 
                                               "Carlos Frias",
                                               "James Jones", "Dan Uggla"))
# 75% of Rich Hill's bunts


### Modeling bunt probability
# Calculate the expected value of a bunt in each base-out state
re <- read.csv("./data/re_table_2010_2015.csv")
re <- re %>% rename("runner_1b" = "ï..runner_1b")

re_lst <- list()
# Convert to a list to more easily populate dataframe columns with these values
for (row in 1:nrow(re)) {
  key <- paste0(re[row, "runner_1b"], re[row, "runner_2b"], re[row, "runner_3b"], re[row, "outs"])
  re_lst[key] <- re[row, "re"]
}