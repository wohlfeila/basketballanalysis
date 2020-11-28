library(tidyverse)

# get file names
traditional_files <- list.files("data/team_boxscore/team_boxscore_traditional/", full.names = TRUE)
advance_files <- list.files("data/team_boxscore/team_boxscore_advance/", full.names = TRUE)

# reading in files
traditional_boxscore_files <- lapply(traditional_files, function(x) read_rds(x))
advance_boxscore_files <- lapply(advance_files, function(x) read_rds(x))

# put data into data.frame
traditional_team_boxscore <- do.call(rbind, traditional_boxscore_files)
advance_team_boxscore <- do.call(rbind, advance_boxscore_files)

colnames(advance_team_boxscore) <- c("team","match_up","game_date","season",
                               "w_l","min","off_rtg","def_rtg","net_rtg","ast_pct",
                               "ast_to_ratio","ast_ratio","oreb_pct","dreb_pct",
                               "red_pct","tov_pct","efg_pct","ts_pct","pace","pie", "Season")

advance_team_boxscore <- advance_team_boxscore %>% 
  select(-season) %>% 
  rename(season = Season)

traditional_team_boxscore_clean <- traditional_team_boxscore %>% 
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"),
         home_team = if_else(grepl("@", match_up) == TRUE, str_sub(match_up, -3, -1), team),
         away_team = if_else(grepl("vs.", match_up) == TRUE, str_sub(match_up, -3, -1), team),
         home = if_else(team == home_team, 1, 0)) %>% 
  select(-match_up) %>%
  arrange(team, game_date) %>% 
  select(team, game_date, season, home_team, away_team, home, everything()) %>% 
  mutate_at(vars(min:plus_minus), function(x){as.numeric(x)}) %>% 
  group_by(team, season) %>% 
  mutate(pts_mean = lag(cummean(pts), n = 0),
         fgm_mean = lag(cummean(fgm), n = 0),
         fga_mean = lag(cummean(fga), n = 0),
         fg_pct_mean = lag(cummean(fg_pct), n = 0),
         three_pm_mean = lag(cummean(three_pm), n = 0),
         three_pa_mean = lag(cummean(three_pa), n = 0),
         three_pct_mean = lag(cummean(three_pct), n = 0),
         ftm_mean = lag(cummean(ftm), n = 0),
         fta_mean = lag(cummean(fta), n = 0),
         ft_pct_mean = lag(cummean(ft_pct), n = 0),
         oreb_mean = lag(cummean(oreb), n = 0),
         dreb_mean = lag(cummean(dreb), n = 0),
         reb_mean = lag(cummean(reb), n = 0),
         ast_mean = lag(cummean(ast), n = 0),
         stl_mean = lag(cummean(stl), n = 0),
         blk_mean = lag(cummean(blk), n = 0),
         tov_mean = lag(cummean(tov), n = 0),
         pf_mean = lag(cummean(pf), n = 0),
         plus_minus_mean = lag(cummean(plus_minus), n = 0),
         w_l = if_else(w_l == "W", 1, 0)) %>% 
  ungroup() %>% 
  mutate_at(vars(pts_mean:plus_minus_mean), function(x){round(x, digits = 1)}) %>% 
  select(-pts, -fgm, -fga, -fg_pct, -three_pm, -three_pa, -three_pct, 
         -ftm, -fta, -ft_pct, -oreb, -dreb, -reb, -ast, -stl, -blk, 
         -tov, -pf, -plus_minus)

advance_team_boxscore_clean <- advance_team_boxscore %>% 
  mutate(game_date = as.Date(game_date, format = "%m/%d/%Y"),
         home_team = if_else(grepl("@", match_up) == TRUE, str_sub(match_up, -3, -1), team),
         away_team = if_else(grepl("vs.", match_up) == TRUE, str_sub(match_up, -3, -1), team),
         home = if_else(team == home_team, 1, 0)) %>% 
  select(-match_up) %>%
  arrange(team, game_date) %>% 
  select(team, game_date, season, home_team, away_team, home, everything()) %>% 
  mutate_at(vars(min:pie), function(x){as.numeric(x)}) %>% 
  group_by(team, season) %>% 
  mutate(off_rtg_mean = lag(cummean(off_rtg), n = 0),
         def_rtg_mean = lag(cummean(def_rtg), n = 0),
         net_rtg_mean = lag(cummean(net_rtg), n = 0),
         ast_pct_mean = lag(cummean(ast_pct), n = 0),
         ast_to_ratio_mean = lag(cummean(ast_to_ratio), n = 0),
         ast_ratio_mean = lag(cummean(ast_ratio), n = 0),
         oreb_pct_mean = lag(cummean(oreb_pct), n = 0),
         dreb_pct_mean = lag(cummean(dreb_pct), n = 0),
         reb_pct_mean = lag(cummean(red_pct), n = 0),
         tov_pct_mean = lag(cummean(tov_pct), n = 0),
         efg_pct_mean = lag(cummean(efg_pct), n = 0),
         ts_pct_mean = lag(cummean(ts_pct), n = 0),
         pace_mean = lag(cummean(pace), n = 0),
         pie_mean = lag(cummean(pie), n = 0)) %>% 
  ungroup() %>% 
  mutate_at(vars(off_rtg_mean:pie_mean), function(x){round(x, digits = 1)}) %>% 
  select(-off_rtg,-def_rtg,-net_rtg,-ast_pct,-ast_to_ratio,-ast_ratio,-oreb_pct,-dreb_pct,
         -red_pct,-tov_pct,-efg_pct,-ts_pct,-pace,-pie, -w_l)

final_df <- traditional_team_boxscore_clean %>% 
  inner_join(advance_team_boxscore_clean, by = c("team","game_date","season","home_team","away_team","home", "min"))

home_teams <- final_df %>% 
  filter(home == 1)

colnames(home_teams) <- c("team","game_date","season","home_team","away_team","away","w_l",
                          "home_min","home_pts_mean","home_fgm_mean","home_fga_mean","home_fg_pct_mean","home_three_pm_mean",
                          "home_three_pa_mean","home_three_pct_mean","home_ftm_mean","home_fta_mean","home_ft_pct_mean",
                          "home_oreb_mean","home_dreb_mean","home_reb_mean","home_ast_mean","home_stl_mean","home_blk_mean",
                          "home_tov_mean","home_pf_mean","home_plus_minus_mean","home_off_rtg_mean","home_def_rtg_mean",
                          "home_net_rtg_mean","home_ast_pct_mean","home_ast_to_ratio_mean","home_ast_ratio_mean",
                          "home_oreb_pct_mean","home_dreb_pct_mean","home_reb_pct_mean","home_tov_pct_mean","home_efg_pct_mean",
                          "home_ts_pct_mean","home_pace_mean","home_pie_mean")

away_teams <- final_df %>% 
  filter(home == 0)

colnames(away_teams) <- c("team","game_date","season","home_team","away_team","away","w_l",
                          "away_min","away_pts_mean","away_fgm_mean","away_fga_mean","away_fg_pct_mean","away_three_pm_mean",
                          "away_three_pa_mean","away_three_pct_mean","away_ftm_mean","away_fta_mean","away_ft_pct_mean",
                          "away_oreb_mean","away_dreb_mean","away_reb_mean","away_ast_mean","away_stl_mean","away_blk_mean",
                          "away_tov_mean","away_pf_mean","away_plus_minus_mean","away_off_rtg_mean","away_def_rtg_mean",
                          "away_net_rtg_mean","away_ast_pct_mean","away_ast_to_ratio_mean","away_ast_ratio_mean",
                          "away_oreb_pct_mean","away_dreb_pct_mean","away_reb_pct_mean","away_tov_pct_mean","away_efg_pct_mean",
                          "away_ts_pct_mean","away_pace_mean","away_pie_mean")

# recreating standings for 2018-19
standings <- traditional_team_boxscore_clean %>% 
  filter(season == "2018-2019") %>% 
  group_by(team) %>% 
  summarise(W = sum(w_l)) %>% 
  ungroup() %>% 
  mutate(L = 82 - W)

# appending oppenent data
schedule_df <- read_rds("data/schedule_final.rds")

# changing names to join in boxscore data
schedule_clean <- schedule_df %>% 
  mutate(Away = case_when(Away =="Milwaukee Bucks"~"MIL",
                          Away =="Golden State Warriors"~"GSW", 
                          Away =="Toronto Raptors"~"TOR",
                          Away =="Utah Jazz"~"UTA",
                          Away =="Denver Nuggets"~"DEN", 
                          Away =="Portland Trail Blazers"~"POR", 
                          Away =="Houston Rockets"~"HOU", 
                          Away =="Boston Celtics"~"BOS",
                          Away =="Indiana Pacers"~"IND", 
                          Away =="Oklahoma City Thunder"~"OKC", 
                          Away =="Philadelphia 76ers"~"PHI", 
                          Away =="San Antonio Spurs"~"SAS",
                          Away =="Los Angeles Clippers"~"LAC", 
                          Away =="Detroit Pistons"~"DET", 
                          Away =="Orlando Magic"~"ORL", 
                          Away =="Miami Heat"~"MIA",
                          Away =="Sacramento Kings"~"SAC", 
                          Away =="New Orleans Pelicans"~"NOP", 
                          Away =="Minnesota Timberwolves"~"MIN", 
                          Away =="Brooklyn Nets"~"BKN",
                          Away =="Dallas Mavericks"~"DAL", 
                          Away =="Charlotte Hornets"~"CHA", 
                          Away =="Los Angeles Lakers"~"LAL", 
                          Away =="Memphis Grizzlies"~"MEM",
                          Away =="Washington Wizards"~"WAS", 
                          Away =="Atlanta Hawks"~"ATL", 
                          Away =="Chicago Bulls"~"CHI", 
                          Away =="Phoenix Suns"~"PHX",
                          Away =="New York Knicks"~"NYK", 
                          Away =="Cleveland Cavaliers"~"CLE"),
         Home = case_when(Home =="Milwaukee Bucks"~"MIL",
                          Home =="Golden State Warriors"~"GSW", 
                          Home =="Toronto Raptors"~"TOR",
                          Home =="Utah Jazz"~"UTA",
                          Home =="Denver Nuggets"~"DEN", 
                          Home =="Portland Trail Blazers"~"POR", 
                          Home =="Houston Rockets"~"HOU", 
                          Home =="Boston Celtics"~"BOS",
                          Home =="Indiana Pacers"~"IND", 
                          Home =="Oklahoma City Thunder"~"OKC", 
                          Home =="Philadelphia 76ers"~"PHI", 
                          Home =="San Antonio Spurs"~"SAS",
                          Home =="Los Angeles Clippers"~"LAC", 
                          Home =="Detroit Pistons"~"DET", 
                          Home =="Orlando Magic"~"ORL", 
                          Home =="Miami Heat"~"MIA",
                          Home =="Sacramento Kings"~"SAC", 
                          Home =="New Orleans Pelicans"~"NOP", 
                          Home =="Minnesota Timberwolves"~"MIN", 
                          Home =="Brooklyn Nets"~"BKN",
                          Home =="Dallas Mavericks"~"DAL", 
                          Home =="Charlotte Hornets"~"CHA", 
                          Home =="Los Angeles Lakers"~"LAL", 
                          Home =="Memphis Grizzlies"~"MEM",
                          Home =="Washington Wizards"~"WAS", 
                          Home =="Atlanta Hawks"~"ATL", 
                          Home =="Chicago Bulls"~"CHI", 
                          Home =="Phoenix Suns"~"PHX",
                          Home =="New York Knicks"~"NYK", 
                          Home =="Cleveland Cavaliers"~"CLE"),
          Win = case_when(Win =="Milwaukee Bucks"~"MIL",
                          Win =="Golden State Warriors"~"GSW", 
                          Win =="Toronto Raptors"~"TOR",
                          Win =="Utah Jazz"~"UTA",
                          Win =="Denver Nuggets"~"DEN", 
                          Win =="Portland Trail Blazers"~"POR", 
                          Win =="Houston Rockets"~"HOU", 
                          Win =="Boston Celtics"~"BOS",
                          Win =="Indiana Pacers"~"IND", 
                          Win =="Oklahoma City Thunder"~"OKC", 
                          Win =="Philadelphia 76ers"~"PHI", 
                          Win =="San Antonio Spurs"~"SAS",
                          Win =="Los Angeles Clippers"~"LAC", 
                          Win =="Detroit Pistons"~"DET", 
                          Win =="Orlando Magic"~"ORL", 
                          Win =="Miami Heat"~"MIA",
                          Win =="Sacramento Kings"~"SAC", 
                          Win =="New Orleans Pelicans"~"NOP", 
                          Win =="Minnesota Timberwolves"~"MIN", 
                          Win =="Brooklyn Nets"~"BKN",
                          Win =="Dallas Mavericks"~"DAL", 
                          Win =="Charlotte Hornets"~"CHA", 
                          Win =="Los Angeles Lakers"~"LAL", 
                          Win =="Memphis Grizzlies"~"MEM",
                          Win =="Washington Wizards"~"WAS", 
                          Win =="Atlanta Hawks"~"ATL", 
                          Win =="Chicago Bulls"~"CHI", 
                          Win =="Phoenix Suns"~"PHX",
                          Win =="New York Knicks"~"NYK", 
                          Win =="Cleveland Cavaliers"~"CLE"),
         home_win = if_else(Win == Home, 1, 0)) %>% 
  select(date, Away, Home, home_win) 

# final modeling data.frame
model_df <- schedule_clean %>% 
  left_join(home_teams %>% 
              select(-season, -home_team, -away_team, -away, -w_l), by = c("date" = "game_date", "Home" = "team")) %>% 
  left_join(away_teams %>% 
              select(-season, -home_team, -away_team, -away, -w_l), by = c("date" = "game_date", "Away" = "team"))

write_csv(model_df, "data/final_model_df.csv")
