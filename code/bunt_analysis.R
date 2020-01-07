### Retrosheet_parse_bunts.R ###
### Noah Stafford ###
### This file parses retroshseet data and ###
### finds all plate appearances that ended with a bunt ###

### Libraries
library(tidyverse)
library(stringr)

# Set working directory
setwd("~/githubrepos/BUNTS")

# Load in data
load("./data/bunts.RDATA")

### ANALYSIS
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
re <- read.csv("./data/re_table_2010_2015.csv")
re <- re %>% rename("runner_1b" = "ï..runner_1b")

re_lst <- list()
# Convert to a list to more easily populate dataframe columns with these values
for (row in 1:nrow(re)) {
  key <- paste0(re[row, "runner_1b"], re[row, "runner_2b"], re[row, "runner_3b"], re[row, "outs"])
  re_lst[key] <- re[row, "re"]
}