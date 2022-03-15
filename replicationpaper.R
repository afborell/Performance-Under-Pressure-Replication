library(rjson)
library(stringr)
library(tidyverse)
library(hoopR)

tictoc::tic()
progressr::with_progress({
  nba_pbp10 <- hoopR::load_nba_pbp(2010)
})
tictoc::toc()
#2002
nba_pbp02 <- nba_pbp02 %>% 
  mutate(ft_a = ifelse(type_text == 'Free Throw - 1 of 2' | type_text == 'Free Throw - 2 of 2', 1, 0))
ft02 <- nba_pbp02 %>% 
  filter(ft_a == 1)
ft02 <- ft02 %>% 
  mutate(ft_made = ifelse(scoring_play == TRUE,1,0))
ft02 %>% mutate(name = gsub("\\smakes.|\\smisses.|\\smade.|\\smissed.|\\Free.|\\Throw.|\\free.|\\throw.|\\`1`.|\\of.|\\`2`.", "", text)) -> ft02
ft02$name <- str_extract(ft02$name, "^\\D+")
ft02 <- ft02 %>%
  group_by(name) %>%
  mutate(running.ft_perc = cummean(ft_made))
ft02 <- ft02 %>% 
  mutate(home_ft = ifelse(team_id == home_team_id,1,0))
ft02 <- ft02 %>% 
  mutate(score_dif = home_score - away_score)
ft02 %>%
  mutate(down1_4 = case_when(
    home_ft == 1 & score_dif < 0 & score_dif >= -4 ~ 1,
    home_ft == 0 & score_dif > 0 & score_dif <= 4 ~ 1,
    home_ft == 1 & score_dif >= 0 | score_dif < -4 ~ 0,
    home_ft == 0 & score_dif <= 0 | score_dif > 4 ~ 0
  )) -> ft02
ft02 %>%
  mutate(up0_4 = case_when(
    home_ft == 1 & score_dif >= 0 & score_dif < 5 ~ 1,
    home_ft == 0 & score_dif <= 0 & score_dif > -5 ~ 1,
    home_ft == 1 & score_dif < 0 | score_dif >= 5 ~ 0,
    home_ft == 0 & score_dif > 0 | score_dif <= -5 ~ 0
  )) -> ft02
ft02 <- ft02 %>%
  mutate(other = ifelse(down1_4 == 0 & up0_4 == 0,1,0))
ft02 <- ft02 %>%
  mutate(playoff = ifelse(season_type == 3,1,0))
ft02 <- 
  ft02 %>%
  group_by(name) %>%
  mutate(lag.ft_made = dplyr::lag(ft_made, n = 1, default = NA))
ft02 <- ft02 %>%
  group_by(name) %>%
  mutate(experience_fts = cumsum(ft_a))

#2003
nba_pbp03 <- nba_pbp03 %>% 
  mutate(ft_a = ifelse(type_text == 'Free Throw - 1 of 2' | type_text == 'Free Throw - 2 of 2', 1, 0))
ft03 <- nba_pbp03 %>% 
  filter(ft_a == 1)
ft03 <- ft03 %>% 
  mutate(ft_made = ifelse(scoring_play == TRUE,1,0))
ft03 %>% mutate(name = gsub("\\smakes.|\\smisses.|\\smade.|\\smissed.|\\Free.|\\Throw.|\\free.|\\throw.|\\`1`.|\\of.|\\`2`.", "", text)) -> ft03
ft03$name <- str_extract(ft03$name, "^\\D+")
ft03 <- ft03 %>%
  group_by(name) %>%
  mutate(running.ft_perc = cummean(ft_made))
ft03 <- ft03 %>% 
  mutate(home_ft = ifelse(team_id == home_team_id,1,0))
ft03 <- ft03 %>% 
  mutate(score_dif = home_score - away_score)
ft03 %>%
  mutate(down1_4 = case_when(
    home_ft == 1 & score_dif < 0 & score_dif >= -4 ~ 1,
    home_ft == 0 & score_dif > 0 & score_dif <= 4 ~ 1,
    home_ft == 1 & score_dif >= 0 | score_dif < -4 ~ 0,
    home_ft == 0 & score_dif <= 0 | score_dif > 4 ~ 0
  )) -> ft03
ft03 %>%
  mutate(up0_4 = case_when(
    home_ft == 1 & score_dif >= 0 & score_dif < 5 ~ 1,
    home_ft == 0 & score_dif <= 0 & score_dif > -5 ~ 1,
    home_ft == 1 & score_dif < 0 | score_dif >= 5 ~ 0,
    home_ft == 0 & score_dif > 0 | score_dif <= -5 ~ 0
  )) -> ft03
ft03 <- ft03 %>%
  mutate(other = ifelse(down1_4 == 0 & up0_4 == 0,1,0))
ft03 <- ft03 %>%
  mutate(playoff = ifelse(season_type == 3,1,0))
ft03 <- 
  ft03 %>%
  group_by(name) %>%
  mutate(lag.ft_made = dplyr::lag(ft_made, n = 1, default = NA))
ft03 <- ft03 %>%
  group_by(name) %>%
  mutate(experience_fts = cumsum(ft_a))

#2004
nba_pbp04 <- nba_pbp04 %>% 
  mutate(ft_a = ifelse(type_text == 'Free Throw - 1 of 2' | type_text == 'Free Throw - 2 of 2', 1, 0))
ft04 <- nba_pbp04 %>% 
  filter(ft_a == 1)
ft04 <- ft04 %>% 
  mutate(ft_made = ifelse(scoring_play == TRUE,1,0))
ft04 %>% mutate(name = gsub("\\smakes.|\\smisses.|\\smade.|\\smissed.|\\Free.|\\Throw.|\\free.|\\throw.|\\`1`.|\\of.|\\`2`.", "", text)) -> ft04
ft04$name <- str_extract(ft04$name, "^\\D+")
ft04 <- ft04 %>%
  group_by(name) %>%
  mutate(running.ft_perc = cummean(ft_made))
ft04 <- ft04 %>% 
  mutate(home_ft = ifelse(team_id == home_team_id,1,0))
ft04 <- ft04 %>% 
  mutate(score_dif = home_score - away_score)
ft04 %>%
  mutate(down1_4 = case_when(
    home_ft == 1 & score_dif < 0 & score_dif >= -4 ~ 1,
    home_ft == 0 & score_dif > 0 & score_dif <= 4 ~ 1,
    home_ft == 1 & score_dif >= 0 | score_dif < -4 ~ 0,
    home_ft == 0 & score_dif <= 0 | score_dif > 4 ~ 0
  )) -> ft04
ft04 %>%
  mutate(up0_4 = case_when(
    home_ft == 1 & score_dif >= 0 & score_dif < 5 ~ 1,
    home_ft == 0 & score_dif <= 0 & score_dif > -5 ~ 1,
    home_ft == 1 & score_dif < 0 | score_dif >= 5 ~ 0,
    home_ft == 0 & score_dif > 0 | score_dif <= -5 ~ 0
  )) -> ft04
ft04 <- ft04 %>%
  mutate(other = ifelse(down1_4 == 0 & up0_4 == 0,1,0))
ft04 <- ft04 %>%
  mutate(playoff = ifelse(season_type == 3,1,0))
ft04 <- 
  ft04 %>%
  group_by(name) %>%
  mutate(lag.ft_made = dplyr::lag(ft_made, n = 1, default = NA))
ft04 <- ft04 %>%
  group_by(name) %>%
  mutate(experience_fts = cumsum(ft_a))


#2005
nba_pbp05 <- nba_pbp05 %>% 
  mutate(ft_a = ifelse(type_text == 'Free Throw - 1 of 2' | type_text == 'Free Throw - 2 of 2', 1, 0))
ft05 <- nba_pbp05 %>% 
  filter(ft_a == 1)
ft05 <- ft05 %>% 
  mutate(ft_made = ifelse(scoring_play == TRUE,1,0))
ft05 %>% mutate(name = gsub("\\smakes.|\\smisses.|\\smade.|\\smissed.|\\Free.|\\Throw.|\\free.|\\throw.|\\`1`.|\\of.|\\`2`.", "", text)) -> ft05
ft05$name <- str_extract(ft05$name, "^\\D+")
ft05 <- ft05 %>%
  group_by(name) %>%
  mutate(running.ft_perc = cummean(ft_made))
ft05 <- ft05 %>% 
  mutate(home_ft = ifelse(team_id == home_team_id,1,0))
ft05 <- ft05 %>% 
  mutate(score_dif = home_score - away_score)
ft05 %>%
  mutate(down1_4 = case_when(
    home_ft == 1 & score_dif < 0 & score_dif >= -4 ~ 1,
    home_ft == 0 & score_dif > 0 & score_dif <= 4 ~ 1,
    home_ft == 1 & score_dif >= 0 | score_dif < -4 ~ 0,
    home_ft == 0 & score_dif <= 0 | score_dif > 4 ~ 0
  )) -> ft05
ft05 %>%
  mutate(up0_4 = case_when(
    home_ft == 1 & score_dif >= 0 & score_dif < 5 ~ 1,
    home_ft == 0 & score_dif <= 0 & score_dif > -5 ~ 1,
    home_ft == 1 & score_dif < 0 | score_dif >= 5 ~ 0,
    home_ft == 0 & score_dif > 0 | score_dif <= -5 ~ 0
  )) -> ft05
ft05 <- ft05 %>%
  mutate(other = ifelse(down1_4 == 0 & up0_4 == 0,1,0))
ft05 <- ft05 %>%
  mutate(playoff = ifelse(season_type == 3,1,0))
ft05 <- 
  ft05 %>%
  group_by(name) %>%
  mutate(lag.ft_made = dplyr::lag(ft_made, n = 1, default = NA))
ft05 <- ft05 %>%
  group_by(name) %>%
  mutate(experience_fts = cumsum(ft_a))

#2006
nba_pbp06 <- nba_pbp06 %>% 
  mutate(ft_a = ifelse(type_text == 'Free Throw - 1 of 2' | type_text == 'Free Throw - 2 of 2', 1, 0))
ft06 <- nba_pbp06 %>% 
  filter(ft_a == 1)
ft06 <- ft06 %>% 
  mutate(ft_made = ifelse(scoring_play == TRUE,1,0))
ft06 %>% mutate(name = gsub("\\smakes.|\\smisses.|\\smade.|\\smissed.|\\Free.|\\Throw.|\\free.|\\throw.|\\`1`.|\\of.|\\`2`.", "", text)) -> ft06
ft06$name <- str_extract(ft06$name, "^\\D+")
ft06$name <- sub("free throw", "", ft06$name)
library(janitor)
trim.trailing <- function (x) sub("\\s+$", "", x)
ft06$name <- trim.trailing(ft06$name)

ft06 <- ft06 %>%
  group_by(name) %>%
  mutate(running.ft_perc = cummean(ft_made))
ft06 <- ft06 %>% 
  mutate(home_ft = ifelse(team_id == home_team_id,1,0))
ft06 <- ft06 %>% 
  mutate(score_dif = home_score - away_score)
ft06 %>%
  mutate(down1_4 = case_when(
    home_ft == 1 & score_dif < 0 & score_dif >= -4 ~ 1,
    home_ft == 0 & score_dif > 0 & score_dif <= 4 ~ 1,
    home_ft == 1 & score_dif >= 0 | score_dif < -4 ~ 0,
    home_ft == 0 & score_dif <= 0 | score_dif > 4 ~ 0
  )) -> ft06
ft06 %>%
  mutate(up0_4 = case_when(
    home_ft == 1 & score_dif >= 0 & score_dif < 5 ~ 1,
    home_ft == 0 & score_dif <= 0 & score_dif > -5 ~ 1,
    home_ft == 1 & score_dif < 0 | score_dif >= 5 ~ 0,
    home_ft == 0 & score_dif > 0 | score_dif <= -5 ~ 0
  )) -> ft06
ft06 <- ft06 %>%
  mutate(other = ifelse(down1_4 == 0 & up0_4 == 0,1,0))
ft06 <- ft06 %>%
  mutate(playoff = ifelse(season_type == 3,1,0))
ft06 <- 
  ft06 %>%
  group_by(name) %>%
  mutate(lag.ft_made = dplyr::lag(ft_made, n = 1, default = NA))
ft06 <- ft06 %>%
  group_by(name) %>%
  mutate(experience_fts = cumsum(ft_a))

#2007
nba_pbp07 <- nba_pbp07 %>% 
  mutate(ft_a = ifelse(type_text == 'Free Throw - 1 of 2' | type_text == 'Free Throw - 2 of 2', 1, 0))
ft07 <- nba_pbp07 %>% 
  filter(ft_a == 1)
ft07 <- ft07 %>% 
  mutate(ft_made = ifelse(scoring_play == TRUE,1,0))
ft07 %>% mutate(name = gsub("\\smakes.|\\smisses.|\\smade.|\\smissed.|\\Free.|\\Throw.|\\free.|\\throw.|\\`1`.|\\of.|\\`2`.", "", text)) -> ft07
ft07$name <- str_extract(ft07$name, "^\\D+")
ft07$name <- sub("free throw", "", ft07$name)
ft07$name <- trim.trailing(ft07$name)
ft07 <- ft07 %>%
  group_by(name) %>%
  mutate(running.ft_perc = cummean(ft_made))
ft07 <- ft07 %>% 
  mutate(home_ft = ifelse(team_id == home_team_id,1,0))
ft07 <- ft07 %>% 
  mutate(score_dif = home_score - away_score)
ft07 %>%
  mutate(down1_4 = case_when(
    home_ft == 1 & score_dif < 0 & score_dif >= -4 ~ 1,
    home_ft == 0 & score_dif > 0 & score_dif <= 4 ~ 1,
    home_ft == 1 & score_dif >= 0 | score_dif < -4 ~ 0,
    home_ft == 0 & score_dif <= 0 | score_dif > 4 ~ 0
  )) -> ft07
ft07 %>%
  mutate(up0_4 = case_when(
    home_ft == 1 & score_dif >= 0 & score_dif < 5 ~ 1,
    home_ft == 0 & score_dif <= 0 & score_dif > -5 ~ 1,
    home_ft == 1 & score_dif < 0 | score_dif >= 5 ~ 0,
    home_ft == 0 & score_dif > 0 | score_dif <= -5 ~ 0
  )) -> ft07
ft07 <- ft07 %>%
  mutate(other = ifelse(down1_4 == 0 & up0_4 == 0,1,0))
ft07 <- ft07 %>%
  mutate(playoff = ifelse(season_type == 3,1,0))
ft07 <- 
  ft07 %>%
  group_by(name) %>%
  mutate(lag.ft_made = dplyr::lag(ft_made, n = 1, default = NA))
ft07 <- ft07 %>%
  group_by(name) %>%
  mutate(experience_fts = cumsum(ft_a))

#2008
nba_pbp08 <- nba_pbp08 %>% 
  mutate(ft_a = ifelse(type_text == 'Free Throw - 1 of 2' | type_text == 'Free Throw - 2 of 2', 1, 0))
ft08 <- nba_pbp08 %>% 
  filter(ft_a == 1)
ft08 <- ft08 %>% 
  mutate(ft_made = ifelse(scoring_play == TRUE,1,0))
ft08 %>% mutate(name = gsub("\\smakes.|\\smisses.|\\smade.|\\smissed.|\\Free.|\\Throw.|\\free.|\\throw.|\\`1`.|\\of.|\\`2`.", "", text)) -> ft08
ft08$name <- str_extract(ft08$name, "^\\D+")
ft08$name <- sub("free throw", "", ft08$name)
ft08$name <- trim.trailing(ft08$name)
ft08 <- ft08 %>%
  group_by(name) %>%
  mutate(running.ft_perc = cummean(ft_made))
ft08 <- ft08 %>% 
  mutate(home_ft = ifelse(team_id == home_team_id,1,0))
ft08 <- ft08 %>% 
  mutate(score_dif = home_score - away_score)
ft08 %>%
  mutate(down1_4 = case_when(
    home_ft == 1 & score_dif < 0 & score_dif >= -4 ~ 1,
    home_ft == 0 & score_dif > 0 & score_dif <= 4 ~ 1,
    home_ft == 1 & score_dif >= 0 | score_dif < -4 ~ 0,
    home_ft == 0 & score_dif <= 0 | score_dif > 4 ~ 0
  )) -> ft08
ft08 %>%
  mutate(up0_4 = case_when(
    home_ft == 1 & score_dif >= 0 & score_dif < 5 ~ 1,
    home_ft == 0 & score_dif <= 0 & score_dif > -5 ~ 1,
    home_ft == 1 & score_dif < 0 | score_dif >= 5 ~ 0,
    home_ft == 0 & score_dif > 0 | score_dif <= -5 ~ 0
  )) -> ft08
ft08 <- ft08 %>%
  mutate(other = ifelse(down1_4 == 0 & up0_4 == 0,1,0))
ft08 <- ft08 %>%
  mutate(playoff = ifelse(season_type == 3,1,0))
ft08 <- 
  ft08 %>%
  group_by(name) %>%
  mutate(lag.ft_made = dplyr::lag(ft_made, n = 1, default = NA))
ft08 <- ft08 %>%
  group_by(name) %>%
  mutate(experience_fts = cumsum(ft_a))

#2009
nba_pbp09 <- nba_pbp09 %>% 
  mutate(ft_a = ifelse(type_text == 'Free Throw - 1 of 2' | type_text == 'Free Throw - 2 of 2', 1, 0))
ft09 <- nba_pbp09 %>% 
  filter(ft_a == 1)
ft09 <- ft09 %>% 
  mutate(ft_made = ifelse(scoring_play == TRUE,1,0))
ft09 %>% mutate(name = gsub("\\smakes.|\\smisses.|\\smade.|\\smissed.|\\Free.|\\Throw.|\\free.|\\throw.|\\`1`.|\\of.|\\`2`.", "", text)) -> ft09
ft09$name <- str_extract(ft09$name, "^\\D+")
ft09$name <- sub("free throw", "", ft09$name)
ft09$name <- trim.trailing(ft09$name)
ft09 <- ft09 %>%
  group_by(name) %>%
  mutate(running.ft_perc = cummean(ft_made))
ft09 <- ft09 %>% 
  mutate(home_ft = ifelse(team_id == home_team_id,1,0))
ft09 <- ft09 %>% 
  mutate(score_dif = home_score - away_score)
ft09 %>%
  mutate(down1_4 = case_when(
    home_ft == 1 & score_dif < 0 & score_dif >= -4 ~ 1,
    home_ft == 0 & score_dif > 0 & score_dif <= 4 ~ 1,
    home_ft == 1 & score_dif >= 0 | score_dif < -4 ~ 0,
    home_ft == 0 & score_dif <= 0 | score_dif > 4 ~ 0
  )) -> ft09
ft09 %>%
  mutate(up0_4 = case_when(
    home_ft == 1 & score_dif >= 0 & score_dif < 5 ~ 1,
    home_ft == 0 & score_dif <= 0 & score_dif > -5 ~ 1,
    home_ft == 1 & score_dif < 0 | score_dif >= 5 ~ 0,
    home_ft == 0 & score_dif > 0 | score_dif <= -5 ~ 0
  )) -> ft09
ft09 <- ft09 %>%
  mutate(other = ifelse(down1_4 == 0 & up0_4 == 0,1,0))
ft09 <- ft09 %>%
  mutate(playoff = ifelse(season_type == 3,1,0))
ft09 <- 
  ft09 %>%
  group_by(name) %>%
  mutate(lag.ft_made = dplyr::lag(ft_made, n = 1, default = NA))
ft09 <- ft09 %>%
  group_by(name) %>%
  mutate(experience_fts = cumsum(ft_a))

#2010
nba_pbp10 <- nba_pbp10 %>% 
  mutate(ft_a = ifelse(type_text == 'Free Throw - 1 of 2' | type_text == 'Free Throw - 2 of 2', 1, 0))
ft10 <- nba_pbp10 %>% 
  filter(ft_a == 1)
ft10 <- ft10 %>% 
  mutate(ft_made = ifelse(scoring_play == TRUE,1,0))
ft10 %>% mutate(name = gsub("\\smakes.|\\smisses.|\\smade.|\\smissed.|\\Free.|\\Throw.|\\free.|\\throw.|\\`1`.|\\of.|\\`2`.", "", text)) -> ft10
ft10$name <- str_extract(ft10$name, "^\\D+")
ft10$name <- sub("free throw", "", ft10$name)
ft10$name <- trim.trailing(ft10$name)
ft10 <- ft10 %>%
  group_by(name) %>%
  mutate(running.ft_perc = cummean(ft_made))
ft10 <- ft10 %>% 
  mutate(home_ft = ifelse(team_id == home_team_id,1,0))
ft10 <- ft10 %>% 
  mutate(score_dif = home_score - away_score)
ft10 %>%
  mutate(down1_4 = case_when(
    home_ft == 1 & score_dif < 0 & score_dif >= -4 ~ 1,
    home_ft == 0 & score_dif > 0 & score_dif <= 4 ~ 1,
    home_ft == 1 & score_dif >= 0 | score_dif < -4 ~ 0,
    home_ft == 0 & score_dif <= 0 | score_dif > 4 ~ 0
  )) -> ft10
ft10 %>%
  mutate(up0_4 = case_when(
    home_ft == 1 & score_dif >= 0 & score_dif < 5 ~ 1,
    home_ft == 0 & score_dif <= 0 & score_dif > -5 ~ 1,
    home_ft == 1 & score_dif < 0 | score_dif >= 5 ~ 0,
    home_ft == 0 & score_dif > 0 | score_dif <= -5 ~ 0
  )) -> ft10
ft10 <- ft10 %>%
  mutate(other = ifelse(down1_4 == 0 & up0_4 == 0,1,0))
ft10 <- ft10 %>%
  mutate(playoff = ifelse(season_type == 3,1,0))
ft10 <- 
  ft10 %>%
  group_by(name) %>%
  mutate(lag.ft_made = dplyr::lag(ft_made, n = 1, default = NA))
ft10 <- ft10 %>%
  group_by(name) %>%
  mutate(experience_fts = cumsum(ft_a))

#player experience
tictoc::tic()
progressr::with_progress({
  nba_pbp01 <- hoopR::load_nba_pbp(2001)
})
tictoc::toc()


players02 <- data_frame(c(1:384))
players02$player_name <- unique(ft02$name)
players02$season <- 2002

players03 <- data_frame(c(1:634))
players03$player_name <- unique(ft03$name)
players03$season <- 2003

players04 <- data_frame(c(1:482))
players04$player_name <- unique(ft04$name)
players04$season <- 2004

players05 <- data_frame(c(1:509))
players05$player_name <- unique(ft05$name)
players05$season <- 2005

players06 <- data_frame(c(1:481))
players06$player_name <- unique(ft06$name)
players06$season <- 2006

players07 <- data_frame(c(1:504))
players07$player_name <- unique(ft07$name)
players07$season <- 2007

players08 <- data_frame(c(1:499))
players08$player_name <- unique(ft08$name)
players08$season <- 2008

players09 <- data_frame(c(1:515))
players09$player_name <- unique(ft09$name)
players09$season <- 2009

players10 <- data_frame(c(1:477))
players10$player_name <- unique(ft10$name)
players10$season <- 2010

#attendance
library(rvest)
library(lubridate)
tms <- read_csv('tms.csv')
#02
urlstats <- 'https://www.basketball-reference.com/leagues/NBA_2002.html'
webpage <- read_html(urlstats)
webpage %>%  
  html_nodes("table") %>% 
  .[[9]] %>% 
  html_table(fill=T) -> att02
colnames(att02) <- NULL
colnames(att02) <- att02[1,]  
att02 <- att02[-1,]
att02$Team <- stringr::str_replace(att02$Team, '\\*', '')

att02 <- att02 %>% 
  select(c(Team, `Attend./G`))
ft02 <- ft02 %>% 
  left_join(tms, by = c('home_team_name_alt' = 'teamab'))
ft02 <- ft02 %>% 
  left_join(att02, by = c('teamname' = 'Team'))
#03
urlstats <- 'https://www.basketball-reference.com/leagues/NBA_2003.html'
webpage <- read_html(urlstats)
webpage %>%  
  html_nodes("table") %>% 
  .[[9]] %>% 
  html_table(fill=T) -> att03
colnames(att03) <- NULL
colnames(att03) <- att03[1,]  
att03 <- att03[-1,]
att03$Team <- stringr::str_replace(att03$Team, '\\*', '')

att03 <- att03 %>% 
  select(c(Team, `Attend./G`))
ft03 <- ft03 %>% 
  left_join(tms, by = c('home_team_name_alt' = 'teamab'))
ft03 <- ft03 %>% 
  left_join(att03, by = c('teamname' = 'Team'))
ft03$teamname[is.na(ft03$teamname)] <- 'New Orleans Hornets'
ft03$`Attend./G`[is.na(ft03$`Attend./G`)] <- 15651

#04
urlstats <- 'https://www.basketball-reference.com/leagues/NBA_2004.html'
webpage <- read_html(urlstats)
webpage %>%  
  html_nodes("table") %>% 
  .[[9]] %>% 
  html_table(fill=T) -> att04
colnames(att04) <- NULL
colnames(att04) <- att04[1,]  
att04 <- att04[-1,]
att04$Team <- stringr::str_replace(att04$Team, '\\*', '')

att04 <- att04 %>% 
  select(c(Team, `Attend./G`))
ft04 <- ft04 %>% 
  left_join(tms, by = c('home_team_name_alt' = 'teamab'))
ft04 <- ft04 %>% 
  left_join(att04, by = c('teamname' = 'Team'))
ft04$teamname[is.na(ft04$teamname)] <- 'New Orleans Hornets'
ft04$`Attend./G`[is.na(ft04$`Attend./G`)] <- 14332

#05
urlstats <- 'https://www.basketball-reference.com/leagues/NBA_2005.html'
webpage <- read_html(urlstats)
webpage %>%  
  html_nodes("table") %>% 
  .[[9]] %>% 
  html_table(fill=T) -> att05
colnames(att05) <- NULL
colnames(att05) <- att05[1,]  
att05 <- att05[-1,]
att05$Team <- stringr::str_replace(att05$Team, '\\*', '')

att05 <- att05 %>% 
  select(c(Team, `Attend./G`))
ft05 <- ft05 %>% 
  left_join(tms, by = c('home_team_name_alt' = 'teamab'))
ft05 <- ft05 %>% 
  left_join(att05, by = c('teamname' = 'Team'))
ft05$`Attend./G`[is.na(ft05$`Attend./G`)] <- 14432

#06
urlstats <- 'https://www.basketball-reference.com/leagues/NBA_2006.html'
webpage <- read_html(urlstats)
webpage %>%  
  html_nodes("table") %>% 
  .[[9]] %>% 
  html_table(fill=T) -> att06
colnames(att06) <- NULL
colnames(att06) <- att06[1,]  
att06 <- att06[-1,]
att06$Team <- stringr::str_replace(att06$Team, '\\*', '')

att06 <- att06 %>% 
  select(c(Team, `Attend./G`))
ft06 <- ft06 %>% 
  left_join(tms, by = c('home_team_name_alt' = 'teamab'))
ft06 <- ft06 %>% 
  left_join(att06, by = c('teamname' = 'Team'))
ft06$teamname[is.na(ft06$teamname)] <- 'NO Hornets'
ft06$`Attend./G`[is.na(ft06$`Attend./G`)] <- 18717

#07
urlstats <- 'https://www.basketball-reference.com/leagues/NBA_2007.html'
webpage <- read_html(urlstats)
webpage %>%  
  html_nodes("table") %>% 
  .[[9]] %>% 
  html_table(fill=T) -> att07
colnames(att07) <- NULL
colnames(att07) <- att07[1,]  
att07 <- att07[-1,]
att07$Team <- stringr::str_replace(att07$Team, '\\*', '')

att07 <- att07 %>% 
  select(c(Team, `Attend./G`))
ft07 <- ft07 %>% 
  left_join(tms, by = c('home_team_name_alt' = 'teamab'))
ft07 <- ft07 %>% 
  left_join(att07, by = c('teamname' = 'Team'))
ft07$teamname[is.na(ft07$teamname)] <- 'New Orleans/Oklahoma City Hornets'
ft07$`Attend./G`[is.na(ft07$`Attend./G`)] <- 17954
#08
urlstats <- 'https://www.basketball-reference.com/leagues/NBA_2008.html'
webpage <- read_html(urlstats)
webpage %>%  
  html_nodes("table") %>% 
  .[[9]] %>% 
  html_table(fill=T) -> att08
colnames(att08) <- NULL
colnames(att08) <- att08[1,]  
att08 <- att08[-1,]
att08$Team <- stringr::str_replace(att08$Team, '\\*', '')

tms2 <- read_csv('tms2.csv')

att08 <- att08 %>% 
  select(c(Team, `Attend./G`))
ft08 <- ft08 %>% 
  left_join(tms2, by = c('home_team_name_alt' = 'teamab'))
ft08 <- ft08 %>% 
  left_join(att08, by = c('teamname' = 'Team'))
ft07$teamname[is.na(ft07$teamname)] <- 'New Orleans/Oklahoma City Hornets'
ft07$`Attend./G`[is.na(ft07$`Attend./G`)] <- 17954


#09
urlstats <- 'https://www.basketball-reference.com/leagues/NBA_2009.html'
webpage <- read_html(urlstats)
webpage %>%  
  html_nodes("table") %>% 
  .[[9]] %>% 
  html_table(fill=T) -> att09
colnames(att09) <- NULL
colnames(att09) <- att09[1,]  
att09 <- att09[-1,]
att09$Team <- stringr::str_replace(att09$Team, '\\*', '')

att09 <- att09 %>% 
  select(c(Team, `Attend./G`))
ft09 <- ft09 %>% 
  left_join(tms2, by = c('home_team_name_alt' = 'teamab'))
ft09 <- ft09 %>% 
  left_join(att09, by = c('teamname' = 'Team'))

#10
urlstats <- 'https://www.basketball-reference.com/leagues/NBA_2010.html'
webpage <- read_html(urlstats)
webpage %>%  
  html_nodes("table") %>% 
  .[[9]] %>% 
  html_table(fill=T) -> att10
colnames(att10) <- NULL
colnames(att10) <- att10[1,]  
att10 <- att10[-1,]
att10$Team <- stringr::str_replace(att10$Team, '\\*', '')

att10 <- att10 %>% 
  select(c(Team, `Attend./G`))
ft10 <- ft10 %>% 
  left_join(tms2, by = c('home_team_name_alt' = 'teamab'))
ft10 <- ft10 %>% 
  left_join(att10, by = c('teamname' = 'Team'))

#FINAL BOSS
final_ft.data <- rbind(ft03,ft04,ft05,ft06,ft07,ft08,ft09,ft10)
write_csv(final_ft.data, 'final_ft.data.csv')
write_csv(ft02, 'ft02.csv')
write_csv(ft03, 'ft03.csv')
write_csv(ft04, 'ft04.csv')
write_csv(ft05, 'ft05.csv')
write_csv(ft06, 'ft06.csv')
write_csv(ft07, 'ft07.csv')
write_csv(ft08, 'ft08.csv')
write_csv(ft09, 'ft09.csv')
write_csv(ft10, 'ft10.csv')

ft02 <- read_csv('ft02.csv')
ft09 <- read_csv('ft09.csv')

#Models
finalft <- read_csv('final_ft.data.csv')


finalft <- finalft %>% 
  mutate(
    finalshot = if_else(lag(name) == name, 1,0)
  )
#how many attempts that game
finalft <- finalft %>% 
  group_by(name, game_id) %>%
  mutate(atts_game = cumsum(ft_a))

#oneshot
finalft <- finalft %>% 
  mutate(
    oneshot = if_else(finalshot == 0 & lead(name) != name,1,0)
  )

finalft <- finalft %>% 
  mutate(quarters_1_3 = ifelse(period_number == 1 | period_number == 2 | period_number == 3,1,0))
quarter1 <- finalft %>% 
  filter(period_number == 1)
quarter2 <- finalft %>% 
  filter(period_number == 2)
quarter3 <- finalft %>% 
  filter(period_number == 3)
quarter1_3 <- finalft %>% 
  filter(quarters_1_3 == 1)

quarter1model <- glm(ft_made ~ oneshot + lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + Attend + home_ft*Attend + playoff, data = quarter1, family = "binomial")
summary(quarter1model)

quarter2model <- glm(ft_made ~ oneshot + lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + Attend + home_ft*Attend + playoff, data = quarter2, family = "binomial")
summary(quarter2model)

quarter3model <- glm(ft_made ~ oneshot + lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + Attend + home_ft*Attend + playoff, data = quarter3, family = "binomial")
summary(quarter3model)

quarter1_3model <- glm(ft_made ~ oneshot + lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + Attend + home_ft*Attend + playoff, data = quarter1_3, family = "binomial")
summary(quarter1_3model)

#More variables
quarter1model2 <- glm(ft_made ~ oneshot + lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + home_ft*running.ft_perc + home_ft*experience_fts + home_ft*atts_game + Attend + Attend*running.ft_perc + Attend*experience_fts + Attend*atts_game + home_ft*Attend + home_ft*Attend*running.ft_perc + home_ft*Attend*experience_fts + home_ft*Attend*atts_game + playoff + playoff*running.ft_perc + playoff*experience_fts + playoff*atts_game, data = quarter1, family = "binomial")
summary(quarter1model2)

quarter2model2 <- glm(ft_made ~ oneshot + lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + home_ft*running.ft_perc + home_ft*experience_fts + home_ft*atts_game + Attend + Attend*running.ft_perc + Attend*experience_fts + Attend*atts_game + home_ft*Attend + home_ft*Attend*running.ft_perc + home_ft*Attend*experience_fts + home_ft*Attend*atts_game + playoff + playoff*running.ft_perc + playoff*experience_fts + playoff*atts_game, data = quarter2, family = "binomial")
summary(quarter2model2)

quarter3model2 <- glm(ft_made ~ oneshot + lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + home_ft*running.ft_perc + home_ft*experience_fts + home_ft*atts_game + Attend + Attend*running.ft_perc + Attend*experience_fts + Attend*atts_game + home_ft*Attend + home_ft*Attend*running.ft_perc + home_ft*Attend*experience_fts + home_ft*Attend*atts_game + playoff + playoff*running.ft_perc + playoff*experience_fts + playoff*atts_game, data = quarter3, family = "binomial")
summary(quarter3model2)

quarter1_3model2 <- glm(ft_made ~ oneshot + lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + home_ft*running.ft_perc + home_ft*experience_fts + home_ft*atts_game + Attend + Attend*running.ft_perc + Attend*experience_fts + Attend*atts_game + home_ft*Attend + home_ft*Attend*running.ft_perc + home_ft*Attend*experience_fts + home_ft*Attend*atts_game + playoff + playoff*running.ft_perc + playoff*experience_fts + playoff*atts_game, data = quarter1_3, family = "binomial")
summary(quarter1_3model2)

stargazer(quarter1model, quarter2model, quarter3model, quarter1_3model, quarter1model2, quarter2model2, quarter3model2, quarter1_3model2, title = 'Table 2: Analysis of Quarters 1-3', column.labels = c("Quarter 1","Quarter 2","Quarter 3","Quarters 1-3", "Quarter 1","Quarter 2","Quarter 3","Quarters 1-3"),
          dep.var.labels = "Made FT", keep = c('home_ft', 'Attend', 'playoff', 'atts_game:', 'experience_fts:','running.ft_perc:'), omit = c("Constant", 'running.ft_perc', 'experience_fts', 'atts_game', 'oneshot', 'lag.ft_made', 'up5_10', 'up4', 'up3', 'up2', 'up1', 'tied', 'down1', 'down2', 'down3', 'down4', 'down5_10'), 
          covariate.labels = c('Home', 'Attend', 'Playoff', 'Home x FT Perc', 'Home x Exper FT', 'Home x Atts', 'FT Perc x Attend', 'Exper FT x Attend', 'Atts x Attend', 'Home x Attend', 'FT Perc x Playoff', 'Exper FT x Playoff', 'Atts x Playoff', 'Home x FT Perc x Attend', 'Home x Exper FT x Attend', 'Home x Atts x Attend'),
          notes = 'All models include OneShot, PrevMade, PrevMiss, Up5_10, Up4, Up3, Up2, Up1, Tied, Down1, Down2, Down3, Down4, and Down5_10', model.numbers = TRUE, model.names = FALSE,
          type = "html", font.size = 'tiny', out = "table2.html")


#time
finalft %>%
  mutate(up1 = case_when(
    home_ft == 1 & score_dif == 1 ~ 1,
    home_ft == 0 & score_dif == -1 ~ 1,
    score_dif != -1 | score_dif != 1 ~ 0
  )) -> finalft
finalft %>%
  mutate(down1 = case_when(
    home_ft == 1 & score_dif == -1 ~ 1,
    home_ft == 0 & score_dif == 1 ~ 1,
    score_dif != -1 | score_dif != 1 ~ 0
  )) -> finalft
finalft %>%
  mutate(up2 = case_when(
    home_ft == 1 & score_dif == 2 ~ 1,
    home_ft == 0 & score_dif == -2 ~ 1,
    score_dif != -2 | score_dif != 2 ~ 0
  )) -> finalft
finalft %>%
  mutate(down2 = case_when(
    home_ft == 1 & score_dif == -2 ~ 1,
    home_ft == 0 & score_dif == 2 ~ 1,
    score_dif != -2 | score_dif != 2 ~ 0
  )) -> finalft
finalft %>%
  mutate(up3 = case_when(
    home_ft == 1 & score_dif == 3 ~ 1,
    home_ft == 0 & score_dif == -3 ~ 1,
    score_dif != -3 | score_dif != 3 ~ 0
  )) -> finalft
finalft %>%
  mutate(down3 = case_when(
    home_ft == 1 & score_dif == -3 ~ 1,
    home_ft == 0 & score_dif == 3 ~ 1,
    score_dif != -3 | score_dif != 3 ~ 0
  )) -> finalft
finalft %>%
  mutate(up4 = case_when(
    home_ft == 1 & score_dif == 4 ~ 1,
    home_ft == 0 & score_dif == -4 ~ 1,
    score_dif != -4 | score_dif != 4 ~ 0
  )) -> finalft
finalft %>%
  mutate(down4 = case_when(
    home_ft == 1 & score_dif == -4 ~ 1,
    home_ft == 0 & score_dif == 4 ~ 1,
    score_dif != -4 | score_dif != 4 ~ 0
  )) -> finalft
finalft %>%
  mutate(up5_10 = case_when(
    home_ft == 1 & score_dif >= 5 & score_dif <= 10 ~ 1,
    home_ft == 0 & score_dif <= -5 & score_dif >= -10 ~ 1,
    home_ft == 1 & score_dif < 5 | score_dif > 10 ~ 0,
    home_ft == 0 & score_dif > -5 | score_dif < -10 ~ 0
  )) -> finalft
finalft %>%
  mutate(down5_10 = case_when(
    home_ft == 1 & score_dif <= -5 & score_dif >= -10 ~ 1,
    home_ft == 0 & score_dif >= 5 & score_dif <= 10 ~ 1,
    home_ft == 1 & score_dif > -5 | score_dif < -10 ~ 0,
    home_ft == 0 & score_dif < 5 | score_dif > 10 ~ 0
  )) -> finalft
finalft <- finalft %>% 
  mutate(tied = if_else(score_dif == 0,1,0))
secs60 <- finalft %>% 
  filter(end_game_seconds_remaining <= 60)
secs30 <- finalft %>% 
  filter(end_game_seconds_remaining <= 30)
secs15 <- finalft %>% 
  filter(end_game_seconds_remaining <= 15)
finalft <- finalft %>% 
  mutate(last60 = if_else(end_game_seconds_remaining <= 60,1,0))
finalft <- finalft %>% 
  mutate(last30 = if_else(end_game_seconds_remaining <= 30,1,0))
finalft <- finalft %>% 
  mutate(last15 = if_else(end_game_seconds_remaining <= 15,1,0))
finalftTab3 <- finalft %>% 
  filter(finalshot != 1 & end_game_seconds_remaining > 6)
sec60model <- glm(ft_made ~ lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + playoff + last60*down5_10 + last60*down4 + last60*down3 + last60*down2 + last60*down1 + last60*up5_10 + last60*up4 + last60*up3 + last60*up2 + last60*up1, data = finalftTab3, family = 'binomial')
summary(sec60model)
sec30model <- glm(ft_made ~ lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + playoff + last30*down5_10 + last30*down4 + last30*down3 + last30*down2 + last30*down1 + last30*up5_10 + last30*up4 + last30*up3 + last30*up2 + last30*up1, data = finalftTab3, family = 'binomial')
summary(sec30model)
sec15model <- glm(ft_made ~ lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + playoff + last15*down5_10 + last15*down4 + last15*down3 + last15*down2 + last15*down1 + last15*up5_10 + last15*up4 + last15*up3 + last15*up2 + last15*up1, data = finalftTab3, family = 'binomial')
summary(sec15model)
sec60model2 <- glm(ft_made ~ lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + playoff + last60*down5_10 + last60*down4 + last60*down3 + last60*down2 + last60*down1 + last60*up5_10 + last60*up4 + last60*up3 + last60*up2 + last60*up1 + finalshot*last60*down2 + finalshot*last60*down1 + finalshot*last60*tied + finalshot*last60*up1 + finalshot*last60*up2, data = finalft, family = 'binomial')
summary(sec60model2)
sec30model2 <- glm(ft_made ~ lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + playoff + last30*down5_10 + last30*down4 + last30*down3 + last30*down2 + last30*down1 + last30*up5_10 + last30*up4 + last30*up3 + last30*up2 + last30*up1 + finalshot*last30*down2 + finalshot*last30*down1 + finalshot*last30*tied + finalshot*last30*up1 + finalshot*last30*up2, data = finalft, family = 'binomial')
summary(sec30model2)
sec15model2 <- glm(ft_made ~ lag.ft_made + up5_10 + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down5_10 + home_ft + playoff + last15*down5_10 + last15*down4 + last15*down3 + last15*down2 + last15*down1 + last15*up5_10 + last15*up4 + last15*up3 + last15*up2 + last15*up1 + finalshot*last15*down2 + finalshot*last15*down1 + finalshot*last15*tied + finalshot*last15*up1 + finalshot*last15*up2, data = finalft, family = 'binomial')
summary(sec15model2)

#individual
stargazer(sec60model, sec60model2, title = 'Table 3: Analysis of Last Minute Pressure Effects. 11+ Baseline',  column.labels = c("Last 60 Sec.","Last 60 Sec."),
          dep.var.labels = "Made FT", keep = c('last60:', 'down2:', 'down1:', 'up2:', 'up1:', 'finalshot:', 'tied:'), omit = c("up5_10", 'up4', 'up3', 'down5_10', 'down4', 'down3',  'Constant', ":up5_10", ':up4', ':up3', ':down5_10', ':down4', ':down3', ':tied', 'playoff', 'home_ft', 'lag.ft_made'),
          model.numbers = TRUE, model.names = FALSE,
          covariate.labels = c('Down 2 x Last 60', 'Down 1 x Last 60', 'Up 2 x Last 60', 'Up 1 x Last 60', 'Last 60 x Final Shot', 'Down 2 x Final Shot', 'Down 1 x Final Shot', 'Tied x Final Shot', 'Tied x Last 60', 'Up 1 x Final Shot', 'Up 2 x Final Shot', 'Down 2 x Last 60 x Final Shot', 'Down 1 x Last 60 x Final Shot', 'Tied x Last 60 x Final Shot', 'Up 1 x Last 60 x Final Shot', 'Up 2 x Last 60 x Final Shot'),
          type = "html", font.size = 'tiny', out = "table3sec60.html")
stargazer(sec30model, sec30model2, title = 'Table 3: Analysis of Last Minute Pressure Effects. 11+ Baseline', column.labels = c("Last 30 Sec.","Last 30 Sec."),
          dep.var.labels = "Made FT", keep = c('last30:', 'down2:', 'down1:', 'up2:', 'up1:', 'finalshot:', 'tied:'), omit = c("up5_10", 'up4', 'up3', 'down5_10', 'down4', 'down3',  'Constant', ":up5_10", ':up4', ':up3', ':down5_10', ':down4', ':down3', ':tied', 'playoff', 'home_ft', 'lag.ft_made'),
          model.numbers = TRUE, model.names = FALSE,
          covariate.labels = c('Down 2 x Last 30', 'Down 1 x Last 30', 'Up 2 x Last 30', 'Up 1 x Last 30', 'Last 30 x Final Shot', 'Down 2 x Final Shot', 'Down 1 x Final Shot', 'Tied x Final Shot', 'Tied x Last 30', 'Up 1 x Final Shot', 'Up 2 x Final Shot', 'Down 2 x Last 30 x Final Shot', 'Down 1 x Last 30 x Final Shot', 'Tied x Last 30 x Final Shot', 'Up 1 x Last 30 x Final Shot', 'Up 2 x Last 30 x Final Shot'),
          type = "html", font.size = 'tiny', out = "table3sec30.html")
stargazer(sec15model, sec15model2, title = 'Table 3: Analysis of Last Minute Pressure Effects. 11+ Baseline', column.labels = c("Last 15 Sec.","Last 15 Sec."),
          dep.var.labels = "Made FT", keep = c('last15:', 'down2:', 'down1:', 'up2:', 'up1:', 'finalshot:', 'tied:'), omit = c("up5_10", 'up4', 'up3', 'down5_10', 'down4', 'down3',  'Constant', ":up5_10", ':up4', ':up3', ':down5_10', ':down4', ':down3', ':tied', 'playoff', 'home_ft', 'lag.ft_made'),
          model.numbers = TRUE, model.names = FALSE,
          covariate.labels = c('Down 2 x Last 15', 'Down 1 x Last 15', 'Up 2 x Last 15', 'Up 1 x Last 15', 'Last 15 x Final Shot', 'Down 2 x Final Shot', 'Down 1 x Final Shot', 'Tied x Final Shot', 'Tied x Last 15', 'Up 1 x Final Shot', 'Up 2 x Final Shot', 'Down 2 x Last 15 x Final Shot', 'Down 1 x Last 15 x Final Shot', 'Tied x Last 15 x Final Shot', 'Up 1 x Last 15 x Final Shot', 'Up 2 x Last 15 x Final Shot'),
          type = "html", font.size = 'tiny', out = "table3sec15.html")


#table 4 - 11+ baseline
finalft <- finalft %>% 
  mutate(plus11 = if_else(score_dif >= 11 | score_dif <= -11,1,0))
finalft %>%
  mutate(up11plus = case_when(
    home_ft == 1 & score_dif >= 11 ~ 1,
    home_ft == 0 & score_dif <= -11 ~ 1,
    home_ft == 1 & score_dif < 11 ~ 0,
    home_ft == 0 & score_dif >-11 ~ 0
  )) -> finalft
finalft %>%
  mutate(down11plus = case_when(
    home_ft == 1 & score_dif <= -11 ~ 1,
    home_ft == 0 & score_dif >= 11 ~ 1,
    home_ft == 1 & score_dif > -11 ~ 0,
    home_ft == 0 & score_dif < 11 ~ 0
  )) -> finalft
sec60model4_1 <- glm(ft_made ~ lag.ft_made + up11plus + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down11plus + home_ft + playoff + last60*down11plus + last60*down4 + last60*down3 + last60*down2 + last60*down1 + last60*up11plus + last60*up4 + last60*up3 + last60*up2 + last60*up1, data = finalft, family = 'binomial')
summary(sec60model4_1)
sec30model4_1 <- glm(ft_made ~ lag.ft_made + up11plus + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down11plus + home_ft + playoff + last30*down11plus + last30*down4 + last30*down3 + last30*down2 + last30*down1 + last30*up11plus + last30*up4 + last30*up3 + last30*up2 + last30*up1, data = finalft, family = 'binomial')
summary(sec30model4_1)
sec15model4_1 <- glm(ft_made ~ lag.ft_made + up11plus + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down11plus + home_ft + playoff + last15*down11plus + last15*down4 + last15*down3 + last15*down2 + last15*down1 + last15*up11plus + last15*up4 + last15*up3 + last15*up2 + last15*up1, data = finalft, family = 'binomial')
summary(sec15model4_1)
sec60model4 <- glm(ft_made ~ lag.ft_made + up11plus + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down11plus + home_ft + playoff + last60*down11plus + last60*down4 + last60*down3 + last60*down2 + last60*down1 + last60*up11plus + last60*up4 + last60*up3 + last60*up2 + last60*up1 + finalshot*last60*down2 + finalshot*last60*down1 + finalshot*last60*tied + finalshot*last60*up1 + finalshot*last60*up2, data = finalft, family = 'binomial')
summary(sec60model4)
sec30model4 <- glm(ft_made ~ lag.ft_made + up11plus + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down11plus + home_ft + playoff + last30*down11plus + last30*down4 + last30*down3 + last30*down2 + last30*down1 + last30*up11plus + last30*up4 + last30*up3 + last30*up2 + last30*up1 + finalshot*last30*down2 + finalshot*last30*down1 + finalshot*last30*tied + finalshot*last30*up1 + finalshot*last30*up2, data = finalft, family = 'binomial')
summary(sec30model4)
sec15model4 <- glm(ft_made ~ lag.ft_made + up11plus + up4 + up3+ up2 + up1 + tied + down1 + down2 + down3 + down4 + down11plus + home_ft + playoff + last15*down11plus + last15*down4 + last15*down3 + last15*down2 + last15*down1 + last15*up11plus + last15*up4 + last15*up3 + last15*up2 + last15*up1 + finalshot*last15*down2 + finalshot*last15*down1 + finalshot*last15*tied + finalshot*last15*up1 + finalshot*last15*up2, data = finalft, family = 'binomial')
summary(sec15model4)

stargazer(sec60model4_1, sec60model4, title = 'Table 4: Analysis of Last Minute Pressure Effects. 5-10 Baseline',  column.labels = c("Last 60 Sec.","Last 60 Sec."),
          dep.var.labels = "Made FT", keep = c('last60:', 'down2:', 'down1:', 'up2:', 'up1:', 'finalshot:', 'tied:'), omit = c("up11plus", 'up4', 'up3', 'down11plus', 'down4', 'down3',  'Constant', ":up11plus", ':up4', ':up3', ':down11plus', ':down4', ':down3', ':tied', 'playoff', 'home_ft', 'lag.ft_made'),
          model.numbers = TRUE, model.names = FALSE,
          covariate.labels = c('Down 2 x Last 60', 'Down 1 x Last 60', 'Up 2 x Last 60', 'Up 1 x Last 60', 'Last 60 x Final Shot', 'Down 2 x Final Shot', 'Down 1 x Final Shot', 'Tied x Final Shot', 'Tied x Last 60', 'Up 1 x Final Shot', 'Up 2 x Final Shot', 'Down 2 x Last 60 x Final Shot', 'Down 1 x Last 60 x Final Shot', 'Tied x Last 60 x Final Shot', 'Up 1 x Last 60 x Final Shot', 'Up 2 x Last 60 x Final Shot'),
          type = "html", font.size = 'tiny', out = "table4sec60.html")
stargazer(sec30model4_1, sec30model4, title = 'Table 4: Analysis of Last Minute Pressure Effects. 5-10 Baseline', column.labels = c("Last 30 Sec.","Last 30 Sec."),
          dep.var.labels = "Made FT", keep = c('last30:', 'down2:', 'down1:', 'up2:', 'up1:', 'finalshot:', 'tied:'), omit = c("up11plus", 'up4', 'up3', 'down11plus', 'down4', 'down3',  'Constant', ":up11plus", ':up4', ':up3', ':down11plus', ':down4', ':down3', ':tied', 'playoff', 'home_ft', 'lag.ft_made'),
          model.numbers = TRUE, model.names = FALSE,
          covariate.labels = c('Down 2 x Last 30', 'Down 1 x Last 30', 'Up 2 x Last 30', 'Up 1 x Last 30', 'Last 30 x Final Shot', 'Down 2 x Final Shot', 'Down 1 x Final Shot', 'Tied x Final Shot', 'Tied x Last 30', 'Up 1 x Final Shot', 'Up 2 x Final Shot', 'Down 2 x Last 30 x Final Shot', 'Down 1 x Last 30 x Final Shot', 'Tied x Last 30 x Final Shot', 'Up 1 x Last 30 x Final Shot', 'Up 2 x Last 30 x Final Shot'),
          type = "html", font.size = 'tiny', out = "table4sec30.html")
stargazer(sec15model4_1, sec15model4, title = 'Table 4: Analysis of Last Minute Pressure Effects. 5-10 Baseline', column.labels = c("Last 15 Sec.","Last 15 Sec."),
          dep.var.labels = "Made FT", keep = c('last15:', 'down2:', 'down1:', 'up2:', 'up1:', 'finalshot:', 'tied:'), omit = c("up11plus", 'up4', 'up3', 'down11plus', 'down4', 'down3',  'Constant', ":up11plus", ':up4', ':up3', ':down11plus', ':down4', ':down3', ':tied', 'playoff', 'home_ft', 'lag.ft_made'),
          model.numbers = TRUE, model.names = FALSE,
          covariate.labels = c('Down 2 x Last 15', 'Down 1 x Last 15', 'Up 2 x Last 15', 'Up 1 x Last 15', 'Last 15 x Final Shot', 'Down 2 x Final Shot', 'Down 1 x Final Shot', 'Tied x Final Shot', 'Tied x Last 15', 'Up 1 x Final Shot', 'Up 2 x Final Shot', 'Down 2 x Last 15 x Final Shot', 'Down 1 x Last 15 x Final Shot', 'Tied x Last 15 x Final Shot', 'Up 1 x Last 15 x Final Shot', 'Up 2 x Last 15 x Final Shot'),
          type = "html", font.size = 'tiny', out = "table4sec15.html")

#table5
finalft <- finalft %>%
  mutate(PrAtt = if_else(end_game_seconds_remaining <=60 & down1_4 == 1 | end_game_seconds_remaining <=60 & tied == 1,1,0))
finalft <- finalft %>%
  group_by(name) %>%
  mutate(PrAtts = cumsum(PrAtt))
tab30_5_1 <- glm(ft_made ~ last30*down1_4 + up5_10 + up0_4 + down5_10 + lag.ft_made + home_ft + playoff, data = finalft, family = 'binomial')
tab30_5_2 <- glm(ft_made ~ last30*down1_4 + running.ft_perc*last30*down1_4 + up5_10 + up0_4 + down5_10 + lag.ft_made + home_ft + playoff, data = finalft, family = 'binomial')
tab30_5_3 <- glm(ft_made ~ last30*down1_4 + atts_game*last30*down1_4 + up5_10 + up0_4 + down5_10 + lag.ft_made + home_ft + playoff, data = finalft, family = 'binomial')
tab30_5_4 <- glm(ft_made ~ last30*down1_4 + experience_fts*last30*down1_4 + up5_10 + up0_4 + down5_10 + lag.ft_made + home_ft + playoff, data = finalft, family = 'binomial')
tab30_5_5 <- glm(ft_made ~ last30*down1_4 + PrAtts*last30*down1_4 + up5_10 + up0_4 + down5_10 + lag.ft_made + home_ft + playoff, data = finalft, family = 'binomial')
tab30_5_6 <- glm(ft_made ~ last30*down1_4 + running.ft_perc*last30*down1_4 + atts_game*last30*down1_4 + experience_fts*last30*down1_4 + PrAtts*last30*down1_4 + up5_10 + up0_4 + down5_10 + lag.ft_made + home_ft + playoff, data = finalft, family = 'binomial')

stargazer(tab30_5_1, tab30_5_2, tab30_5_3, tab30_5_4, tab30_5_5, tab30_5_6, title = 'Table 5: Player Characteristic Interactions; Last 30 Second Interaction',
          dep.var.labels = "Made FT", keep = c('last30:', 'down1_4:', 'running.ft_perc:', 'atts_game:', 'experience_fts:', 'PrAtts:'), omit = c('up5_10','up0_4','down5_10','lag.ft_made','home_ft','playoff'),
          covariate.labels = c('Last 30 x Down 1 to 4', 'Last 30 x FT Perc', 'Down 1 to 4 x FT Perc', 'Last 30 x Down 1 to 4 x FT Perc', 'Last 30 x Atts', 'Down 1 to 4 x Atts', 'Last 30 x Down 1 to 4 x Atts', 'Last 30 x Experience FTs', 'Down 1 to 4 x Experience FTs', 'Last 30 x Down 1 to 4 x Experience FTs', 'Last 30 x PrAtts', 'Down 1 to 4 x PrAtts', 'Last 30 x Down 1 to 4 x PrAtts'),
          model.numbers = TRUE, model.names = FALSE,
          type = "html", font.size = 'tiny', out = "table5sec30.html")

tab15_5_1 <- glm(ft_made ~ last15*down1_4 + up5_10 + up0_4 + down5_10 + lag.ft_made + home_ft + playoff, data = finalft, family = 'binomial')
tab15_5_2 <- glm(ft_made ~ last15*down1_4 + running.ft_perc*last15*down1_4 + up5_10 + up0_4 + down5_10 + lag.ft_made + home_ft + playoff, data = finalft, family = 'binomial')
tab15_5_3 <- glm(ft_made ~ last15*down1_4 + atts_game*last15*down1_4 + up5_10 + up0_4 + down5_10 + lag.ft_made + home_ft + playoff, data = finalft, family = 'binomial')
tab15_5_4 <- glm(ft_made ~ last15*down1_4 + experience_fts*last15*down1_4 + up5_10 + up0_4 + down5_10 + lag.ft_made + home_ft + playoff, data = finalft, family = 'binomial')
tab15_5_5 <- glm(ft_made ~ last15*down1_4 + PrAtts*last15*down1_4 + up5_10 + up0_4 + down5_10 + lag.ft_made + home_ft + playoff, data = finalft, family = 'binomial')
tab15_5_6 <- glm(ft_made ~ last15*down1_4 + running.ft_perc*last15*down1_4 + atts_game*last15*down1_4 + experience_fts*last15*down1_4 + PrAtts*last15*down1_4 + up5_10 + up0_4 + down5_10 + lag.ft_made + home_ft + playoff, data = finalft, family = 'binomial')

stargazer(tab15_5_1, tab15_5_2, tab15_5_3, tab15_5_4, tab15_5_5, tab15_5_6,title = 'Table 5: Player Characteristic Interactions; Last 15 Second Interaction',
          dep.var.labels = "Made FT", keep = c('last15:', 'down1_4:', 'running.ft_perc:', 'atts_game:', 'experience_fts:', 'PrAtts:'), omit = c('up5_10','up0_4','down5_10','lag.ft_made','home_ft','playoff'),
          covariate.labels = c('Last 15 x Down 1 to 4', 'Last 15 x FT Perc', 'Down 1 to 4 x FT Perc', 'Last 15 x Down 1 to 4 x FT Perc', 'Last 15 x Atts', 'Down 1 to 4 x Atts', 'Last 15 x Down 1 to 4 x Atts', 'Last 15 x Experience FTs', 'Down 1 to 4 x Experience FTs', 'Last 15 x Down 1 to 4 x Experience FTs', 'Last 15 x PrAtts', 'Down 1 to 4 x PrAtts', 'Last 15 x Down 1 to 4 x PrAtts'),
          model.numbers = TRUE, model.names = FALSE,
          type = "html", font.size = 'tiny', out = "table5sec15.html")

#table 6
sec30model6_1 <- glm(ft_made ~ lag.ft_made*last30*down1_4 + home_ft*last30*down1_4 + playoff*last30*down1_4 + Attend*last30*down1_4 + home_ft*Attend*last30*down1_4 + up5_10 + up0_4 + down5_10, data = finalft, family = 'binomial')
summary(sec30model6_1)
sec15model6_1 <- glm(ft_made ~ lag.ft_made*last15*down1_4 + home_ft*last15*down1_4 + playoff*last15*down1_4 + Attend*last15*down1_4 + home_ft*Attend*last15*down1_4 +  up5_10 + up0_4 + down5_10, data = finalft, family = 'binomial')

sec30model6_2 <- glm(ft_made ~ lag.ft_made*last30*down1_4 + home_ft*last30*down1_4 + playoff*last30*down1_4 +  up5_10 + up0_4 + down5_10, data = finalft, family = 'binomial')
summary(sec30model6_2)
sec15model6_2 <- glm(ft_made ~ lag.ft_made*last15*down1_4 + home_ft*last15*down1_4 + playoff*last15*down1_4 +  up5_10 + up0_4 + down5_10, data = finalft, family = 'binomial')

stargazer(sec30model6_1, sec30model6_2, column.labels = c("Last 30 Sec.","Last 30 Sec."), title = 'Table 6: Other Interactions; Last 30 Seconds',
          dep.var.labels = "Made FT", keep = c('lag.ft_made:', 'last30:', 'down1_4:', 'home_ft:', 'playoff:', 'Attend:', 'down1_4:'), omit = c('Constant', 'lag.ft_made', 'last30', 'down1_4', 'home_ft', 'playoff', 'Attend', 'up5_10', 'up0_4','down5_10'),
          covariate.labels = c('Prev Made x Last 30', 'Prev Made x Down 1 to 4', 'Last 30 x Down 1 to 4', 'Last 30 x Home', 'Down 1 to 4 x Home FT', 'Last 30 x Playoff', 'Down 1 to 4 x Playoff', 'Last 30 x Attend', 'Down 1 to 4 x Attend', 'Home x Attend', 'Prev Made x Last 30 x Down 1 to 4', 'Last 30 x Down 1 to 4 x Home', 'Last 30 x Down 1 to 4 x Playoff', 'Last 30 x Down 1 to 4 x Attend', 'Last 30 x Home x Attend', 'Down 1 to 4 x Home x Attend', 'Last 30 x Down 1 to 4 x Home x Attend'),
          model.numbers = TRUE, model.names = FALSE,
          type = "html", font.size = 'tiny', out = "table6sec30.html")
stargazer(sec15model6_1, sec15model6_2, column.labels = c("Last 15 Sec.","Last 15 Sec."), title = 'Table 6: Other Interactions; Last 15 Seconds',
          dep.var.labels = "Made FT", keep = c('lag.ft_made:', 'last15:', 'down1_4:', 'home_ft:', 'playoff:', 'Attend:', 'down1_4:'), omit = c('Constant', 'lag.ft_made', 'last15', 'down1_4', 'home_ft', 'playoff', 'Attend', 'up5_10', 'up0_4','down5_10'),
          covariate.labels = c('Prev Made x Last 15', 'Prev Made x Down 1 to 4', 'Last 15 x Down 1 to 4', 'Last 15 x Home', 'Down 1 to 4 x Home FT', 'Last 15 x Playoff', 'Down 1 to 4 x Playoff', 'Last 15 x Attend', 'Down 1 to 4 x Attend', 'Home x Attend', 'Prev Made x Last 15 x Down 1 to 4', 'Last 15 x Down 1 to 4 x Home', 'Last 15 x Down 1 to 4 x Playoff', 'Last 15 x Down 1 to 4 x Attend', 'Last 15 x Home x Attend', 'Down 1 to 4 x Home x Attend', 'Last 15 x Down 1 to 4 x Home x Attend'),
          model.numbers = TRUE, model.names = FALSE,
          type = "html", font.size = 'tiny', out = "table6sec15.html")
write_csv(finalft, 'final_ft.data.csv')

#summary statistics
time60 <- finalft %>% 
  filter(end_game_seconds_remaining > 60)
time60 <- time60 %>% 
  select(c('home_ft', 'down1_4','up0_4', 'other', 'playoff', 'running.ft_perc', 'experience_fts', 'atts_game', 'PrAtts'))
time30 <- finalft %>% 
  filter(end_game_seconds_remaining <= 60 & end_game_seconds_remaining >30)
time30 <- time30 %>% 
  select(c('home_ft', 'down1_4','up0_4', 'other', 'playoff', 'running.ft_perc', 'experience_fts', 'atts_game', 'PrAtts'))
time15 <- finalft %>% 
  filter(end_game_seconds_remaining <= 30 & end_game_seconds_remaining >15)
time15 <- time15 %>% 
  select(c('home_ft', 'down1_4','up0_4', 'other', 'playoff', 'running.ft_perc', 'experience_fts', 'atts_game', 'PrAtts'))
time0 <- finalft %>% 
  filter(end_game_seconds_remaining <= 15)
time0 <- time0 %>% 
  select(c('home_ft', 'down1_4','up0_4', 'other', 'playoff', 'running.ft_perc', 'experience_fts', 'atts_game', 'PrAtts'))
sum_home <- finalft %>% 
  filter(home_ft == 1)
sum_home_not <- finalft %>% 
  filter(home_ft == 0)
sum_playoff <- finalft %>% 
  filter(playoff == 1)
stargazer(as.data.frame(sum_home[c('home','down1_4','up0_4', 'other', 'playoff', 'running.ft_perc', 'experience_fts', 'atts_game', 'PrAtts')]), type = "html", title="Descriptive statistics", digits=1, out="table1.html")
stargazer(as.data.frame(sum_home_not[c('home','down1_4','up0_4', 'other', 'playoff', 'running.ft_perc', 'experience_fts', 'atts_game', 'PrAtts')]), type = "html", title="Descriptive statistics", digits=1, out="table1.html")
stargazer(as.data.frame(sum_playoff[c('home','down1_4','up0_4', 'other', 'playoff', 'running.ft_perc', 'experience_fts', 'atts_game', 'PrAtts')]), type = "html", title="Descriptive statistics", digits=1, out="table1.html")

stargazer(as.data.frame(time60[c('home_ft','down1_4','up0_4', 'other', 'playoff', 'running.ft_perc', 'atts_game', 'PrAtts')]), dep.var.labels = "Time > 60 Secs.",
          covariate.labels = c('Home', 'Down 1 to 4', 'Up 1 to 4', 'Other', 'Playoff', 'FT Perc', 'Atts', 'PrAtts'), type = "html", title="Table 1: Descriptive statistics; Time > 60 Secs. ", digits=2, out="table1.html")
stargazer(as.data.frame(time30[c('home_ft','down1_4','up0_4', 'other', 'playoff', 'running.ft_perc', 'atts_game', 'PrAtts')]), dep.var.labels = "30 Secs. < Time <= 60 Secs.",
          covariate.labels = c('Home', 'Down 1 to 4', 'Up 1 to 4', 'Other', 'Playoff', 'FT Perc', 'Atts', 'PrAtts'), type = "html", title="Table 1: Descriptive statistics; 30 Secs. < Time <= 60 Secs.", digits=2, out="table2.html")
stargazer(as.data.frame(time15[c('home_ft','down1_4','up0_4', 'other', 'playoff', 'running.ft_perc', 'atts_game', 'PrAtts')]), dep.var.labels = "15 Secs. < Time <= 30 Secs",
          covariate.labels = c('Home', 'Down 1 to 4', 'Up 1 to 4', 'Other', 'Playoff', 'FT Perc', 'Atts', 'PrAtts'), type = "html", title="Table 1: Descriptive statistics; 15 Secs. < Time <= 30 Secs", digits=2, out="table3.html")
stargazer(as.data.frame(time0[c('home_ft','down1_4','up0_4', 'other', 'playoff', 'running.ft_perc', 'atts_game', 'PrAtts')]), dep.var.labels = "Time <= 15 Secs.",covariate.labels = c('Home', 'Down 1 to 4', 'Up 1 to 4', 'Other', 'Playoff', 'FT Perc', 'Experience FTs', 'Atts', 'PrAtts'), type = "html", title="Table 1: Descriptive statistics; Time <= 15 Secs.", digits=2, out="table4.html")



#figure 1 graph
finalft <- read_csv('final_ft.data.csv')
finalft <- finalft %>% 
  mutate(time_cat = if_else(end_game_seconds_remaining >60 & end_game_seconds_remaining<=120,'60<t<=120', if_else(end_game_seconds_remaining >30 & end_game_seconds_remaining <=60,'30<t<=60',if_else(end_game_seconds_remaining >15 & end_game_seconds_remaining <=30,'15<t<=30',if_else(end_game_seconds_remaining <=15,'t<=15',"Other")))))
finalft <- finalft %>% 
  filter(score_dif < 90)
finalftgraph <- finalft %>% 
  filter(score_dif >= -10 & score_dif <= 10)
finalftgraph1 <- finalftgraph %>% 
  select(c('score_dif', 'running.ft_perc', 'PrAtt', 'time_cat')) %>% 
  group_by(finalftgraph$score_dif, PrAtt, time_cat) %>% 
  summarize(Mean = mean(running.ft_perc))
finalftgraph2 <- finalftgraph %>% 
  filter(PrAtt ==1)
finalftgraph2 <- finalftgraph2 %>% 
  select(c('score_dif', 'running.ft_perc')) %>% 
  group_by(finalftgraph$score_dif) %>% 
  summarize(Mean = mean(running.ft_perc))
finalftgraph1 <- finalftgraph1 %>% 
  filter(time_cat != 'Other')

p <- ggplot(finalftgraph1, aes(x=finalftgraph1$`finalftgraph$score_dif`,y=finalftgraph1$Mean, colour = finalftgraph1$PrAtt)) + geom_line() + facet_wrap(~time_cat, nrow = 2) + theme_classic() + theme(legend.position = 'none') + xlab('Score Difference') + ylab('Avg. FT %') + labs(title = 'Figure 1', subtitle = 'Comparison of free throw percentage in Normal (Black) and High Pressure (Blue) Situations')

