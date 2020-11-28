library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(readr)


StandingsFunction <- function(x){
  # loading data
  path <- paste0("/Volumes/TOSHIBA EXT/NBA_data/Games/",x,"/")
  temp = list.files(path = path , pattern="*.csv")
  path_2 <- paste0(path, temp)
  myfiles = lapply(path_2, read.delim, stringsAsFactors = FALSE, sep=",")

  # combining data
  boxscores <- do.call(rbind, myfiles)
  box.clean <- boxscores

  # extracting teams for future use
  teams <- unique(box.clean$Team)
  col <- colnames(box.clean)
  num_games <- 82

  # creating empty list for future use
  standings.list <- list()

  # putting together team stats data frame
  for (i in teams) {
    timberwolves <- box.clean %>% 
      filter(Team == i) %>%
      filter(MP != 0) %>% 
      group_by(Date) %>% 
      mutate(team_pts = ifelse(W == i, W_PTS, L_PTS),
             opp_pts = ifelse(L == i, W_PTS, L_PTS),
             opp = ifelse(W == i, L, W))
  
    timberwolves.team.stats <- timberwolves %>% 
      group_by(Date) %>% 
      summarise(team_score = sum(mean(team_pts)),
                opp_score = sum(mean(opp_pts))) %>% 
      mutate(W = ifelse(team_score > opp_score, 1, 0),
             Team = i) %>% 
      summarise(Win = sum(W)) %>% 
      mutate(Loss = num_games - Win,
             Team = i,
             Season = x,
             t.MP = sum(timberwolves$MP),
             t.FG = sum(timberwolves$FG) / num_games,
             t.FGA = sum(timberwolves$FGA) / num_games,
             t.FG_perc = t.FG / t.FGA,
             t.3P = sum(timberwolves$X3P) / num_games,       
             t.3PA = sum(timberwolves$X3PA) / num_games,     
             t.3P_perc = t.3P / t.3PA, 
             t.FT = sum(timberwolves$FT) / num_games,       
             t.FTA = sum(timberwolves$FTA) / num_games,      
             t.FT_perc = t.FT / t.FTA,
             t.ORB = sum(timberwolves$ORB) / num_games,       
             t.DRB = sum(timberwolves$DRB) / num_games,
             t.TRB = sum(timberwolves$TRB) / num_games,
             t.AST = sum(timberwolves$AST) / num_games,
             t.STL = sum(timberwolves$STL) / num_games,
             t.BLK = sum(timberwolves$BLK) / num_games,
             t.TOV = sum(timberwolves$TOV) / num_games,
             t.PF = sum(timberwolves$PF) / num_games,    
             t.PTS = sum(timberwolves$PTS) / num_games,
             t.ORtg = mean(timberwolves$ORtg * timberwolves$MP) / mean(timberwolves$MP),
             t.DRtg = mean(timberwolves$DRtg * timberwolves$MP) / mean(timberwolves$MP))
    
    final <- timberwolves.team.stats %>% 
      mutate(t.TS_perc = t.PTS / (2 * (t.FGA + 0.44 * t.FTA)),
             t.eFG_perc = (t.FG + 0.5 * t.3P) / t.FGA,
             t.3PAr = t.3PA / t.FGA,
             t.FTr = t.FTA / t.FGA)
    
    
    standings.list[[i]] <- timberwolves.team.stats 
    }
  standings <- do.call(rbind, standings.list)
  }

year13.14 <- StandingsFunction("2013-14")
year14.15 <- StandingsFunction("2014-15")
year15.16 <- StandingsFunction("2015-16")
year16.17 <- StandingsFunction("2016-17")
year17.18 <- StandingsFunction("2017-18")
year18.19 <- StandingsFunction("2018-19")

team.standings <- rbind(year13.14,
                        year14.15,
                        year15.16,
                        year16.17,
                        year17.18,
                        year18.19)

team.standings[team.standings == "CHA"] <- "CHO"

# pulling in schedule data
results.final.df <- read_csv("/Volumes/TOSHIBA EXT/NBA_data/results_final_df.csv", 
                             col_types = cols(date = col_date(format = "%Y-%m-%d")))
# re-naming bobcats
results.final.df$Away[results.final.df$Away == "Charlotte Bobcats"] <- "Charlotte Hornets"
results.final.df$Home[results.final.df$Home == "Charlotte Bobcats"] <- "Charlotte Hornets"
results.final.df$Win[results.final.df$Win == "Charlotte Bobcats"] <- "Charlotte Hornets"

team.names <- unique(team.standings$Team)
team.names.full <- unique(results.final.df$Away)

team.df <- data_frame(team_names = sort(team.names), team_names_full = sort(team.names.full))
team.df[team.df=="CHI"] <- "CHO1"
team.df[team.df=="CHO"] <- "CHI"
team.df[team.df=="CHO1"] <- "CHO"

team.standings.clean <- team.standings %>% 
  left_join(team.df, by = c("Team" = "team_names"))

results.final.clean <- results.final.df %>% 
  select(-X1) %>% 
  mutate(season = ifelse(str_detect(date, "2013"), "2013-14",
                         ifelse(str_detect(date,"2014"), "2014-15",
                                ifelse(str_detect(date,"2015"), "2015-16",
                                       ifelse(str_detect(date,"2016"), "2016-17",
                                              ifelse(str_detect(date,"2017"), "2017-18", "2018-19"))))),
         home_win = ifelse(Win == Home, 1, 0))
         
str(team.standings.clean)
str(results.final.clean)
           
model_df <- results.final.clean %>% 
  left_join(team.standings.clean, by = c("Home" = "team_names_full", "season" = "Season")) %>%
  mutate(t.nrtg = t.ORtg - t.DRtg) %>% 
  left_join(team.standings.clean, by = c("Away" = "team_names_full", "season" = "Season")) %>% 
  select(-Loss.x, -Team.x, -Win.y, -Win, -Loss.y, -Team.y, -t.MP.x, -t.MP.y) %>% 
  mutate(o.nrtg = t.ORtg.y - t.DRtg.y)

colnames(model_df) <-  c("Away", "Away_PTS", "Home", "Home_PTS", "OTflag", "date", "Win",
                         "season", "home_win", "t.FG", "t.FGA", "t.FG_perc", "t.3P", 
                         "t.3PA", "t.3P_perc", "t.FT", "t.FTA", "t.FT_perc", "t.ORB", "t.DRB", 
                         "t.TRB", "t.AST", "t.STL", "t.BLK", "t.TOV", "t.PF", "t.PTS", "t.ORtg", 
                         "t.DRtg", "t.nrtg", "o.FG", "o.FGA", "o.FG_perc", "o.3P", "o.3PA", 
                         "o.3P_perc", "o.FT", "o.FTA", "o.FT_perc", "o.ORB", "o.DRB", "o.TRB", "o.AST", 
                         "o.STL", "o.BLK", "o.TOV", "o.PF", "o.PTS", "o.ORtg", "o.DRtg", "o.nrtg")   

#sum(model_df$home_win)/length(model_df$home_win) - 58% is baseline

model.df <- model_df %>% 
  mutate(net_FG = t.FG - o.FG,
         net_FGA = t.FGA - o.FGA,
         net_FG_per = t.FG_perc - o.FG_perc,
         net_3P = t.3P - o.3P,
         net_3PA = t.3PA - o.3PA,
         net_3P_per = t.3P_perc - o.3P_perc,
         net_FT  = t.FT - o.FT,
         net_FTA = t.FTA - o.FTA,
         net_FT_per = t.FT_perc - o.FT_perc,
         net_ORB = t.ORB - o.ORB,
         net_DRB = t.DRB - o.DRB,
         net_TRB = t.TRB - o.TRB,
         net_AST = t.AST - o.AST,
         net_STL = t.STL - o.STL,
         net_BLK = t.BLK - o.BLK,
         net_TOV = t.TOV - o.TOV, 
         net_PF = t.PF - o.PF,
         net_PTS = t.PTS - o.PTS,
         net_ORtg = t.ORtg - o.ORtg, 
         net_DRtg = t.DRtg - o.DRtg)

final.model.df <- model.df[,52:71]
df.join <- model.df[,1:9]

model.df.final <- cbind(df.join,final.model.df)

# get days since last game for Home 
tmp <- bind_rows(model.df.final %>% select(team = Away, date), 
                 model.df.final %>% select(team = Home, date))
tmp <- tmp %>% 
  group_by(team) %>% 
  arrange(team, date) %>% 
  mutate(days_since_last_game = as.numeric(date - lag(date)))

model.df.final <- model.df.final %>% 
  left_join(tmp %>% select(Home = team, date, home_days_since_last_game = days_since_last_game), 
            by = c('Home', 'date'))

model.df.final <- model.df.final %>% 
  left_join(tmp %>% select(Away = team, date, away_days_since_last_game = days_since_last_game), 
            by = c('Away', 'date'))

#write_rds(model.df.final, "/Volumes/TOSHIBA EXT/NBA_data/model.df.final.rds")

