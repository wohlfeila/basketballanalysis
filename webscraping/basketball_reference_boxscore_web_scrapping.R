library(tm)
library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(magrittr)
library(XML)
library(readxl)
library(caret)
library(RCurl)
library(ggplot2)

####### scrapping data from Basketball-Reference.com 
year <- 2018
month <- "october"

#pulling links to each individual game's boxscore
url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_games-", month, ".html")
webpage <- read_html(url)

############################################ BOXSCORE SCRAPE ############################################# 

x <- webpage %>% html_nodes("div div") %>% html_nodes("td a") %>% html_attr("href")

df <- seq(3, 312, 3)
tmp <- x[df]

#pulling data from each boxscore
url1 <- paste0("https://www.basketball-reference.com")

boxscore <- read_html(paste0(url1,tmp[1]))

#pulling data from basketball-reference.com - need to scale up for full years
y <- boxscore %>% 
    html_nodes("div table") %>% 
    html_table()

#pulling team names - to know who was home/away
teams <- boxscore %>% 
  html_nodes("strong a") %>% 
  html_text()

#pulling date of game
date <- boxscore %>% 
  html_nodes("div h1") %>% 
  html_text() 

#cleaning date from website - are single dates coming in with 0? does function still work on single date?
game_date <- sub(".+?,","", date) %>% trimws() %>% str_replace(",", "")
game_year <- str_sub(game_date, -4, -1)
game_day <- str_sub(game_date, -7, -6)

myFun <- function(x, dummyDay = game_day, dummyYear = game_year){
  
  x <- ifelse(substr(x, 1, 3) %in% month.abb,
              paste(match(substr(x, 1, 3), month.abb),
                    dummyDay,
                    dummyYear, sep = "/"), x)
  #return date
  mdy(x)
}

# use to translate dates 
game_date <- myFun(game_date)

#creating empty list
datalist <- list()

# cleaning data after it's been pulled from basketball-reference.com
for (i in 1:length(y)) {

  t <- y[[i]]
  actual_names <- as.character(t[1,])
  t <- t[-c(1,6),]
  names(t) <- actual_names
  final <- as.data.frame(t)
  colnames(final)[1] <- "Players"
  final[final == ""] <- 0
  datalist[[i]] <- final 

}

game_data = do.call(rbind, datalist) #need to make number of columns the same

########################################### END BOXSCORE ########################################### 




##################################### SCHEDULE RESULTS ##################################### 

#function to scrape game data
scores_scrape <- function(year, month){
  #pulling links to each individual game's boxscore
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_games-", month, ".html")
  webpage <- read_html(url)
  #scraping dates
  dates <- webpage %>% 
    html_nodes("th a") %>% 
    html_text()
  #scraping game data
  game_data <- webpage %>% 
    html_nodes("tr td") %>% 
    html_text()
  #formatting game data
  game_data_format <- matrix(game_data, nrow = 9) %>% 
    as.data.frame(row.names = FALSE) %>% 
    t() %>% 
    data.frame()
  colnames(game_data_format) <- c("Start", "Away",	"Away_PTS",	"Home",	"Home_PTS", "boxscore", "OTflag",	"Attend",	"Notes")
  #adding dates to each game
  dates_clean <- dates %>% 
    str_sub(6) %>% 
    mdy()
  #adding date column
  game_data_clean <- game_data_format %>% 
    mutate(date = dates_clean, OTflag = ifelse(OTflag == "", "NA", "OT")) %>% 
    select(-boxscore, -Attend, -Notes, -Start)

  return(game_data_clean)

}

#scraping game data for 2018-2019 season

month <- c("october","november","december","january","february","march","april")

scores_list <- list()

for (i in month) {
  tmp <- scores_scrape(2020, i)
  scores_list[[i]] <- tmp
  }

# scores_df_01 <- do.call(rbind, scores_list)
# scores_df_02 <- do.call(rbind, scores_list)
# scores_df_03 <- do.call(rbind, scores_list)
# scores_df_04 <- do.call(rbind, scores_list)
# scores_df_05 <- do.call(rbind, scores_list)
# scores_df_06 <- do.call(rbind, scores_list)
# scores_df_07 <- do.call(rbind, scores_list)
# scores_df_08 <- do.call(rbind, scores_list)
# scores_df_09 <- do.call(rbind, scores_list)
# scores_df_10 <- do.call(rbind, scores_list)
# scores_df_11 <- do.call(rbind, scores_list)
# scores_df_12 <- do.call(rbind, scores_list)
# scores_df_13 <- do.call(rbind, scores_list)
# scores_df_14 <- do.call(rbind, scores_list)
# scores_df_15 <- do.call(rbind, scores_list)
# scores_df_16 <- do.call(rbind, scores_list)
# scores_df_17 <- do.call(rbind, scores_list)
# scores_df_18 <- do.call(rbind, scores_list)
# scores_df_19 <- do.call(rbind, scores_list)
scores_df_20 <- do.call(rbind, scores_list)

scores_clean <- function(df, end_date){
  df %>% 
    filter(date < end_date) %>% 
    mutate(Away = as.character(Away), 
                 Away_PTS = as.numeric(levels(Away_PTS)[Away_PTS]),
                 Home = as.character(Home),
                 Home_PTS = as.numeric(levels(Home_PTS)[Home_PTS]),
                 Win = ifelse(Home_PTS>Away_PTS,Home,Away))
  }

# results_2000_2001 <- scores_clean(scores_df_01, "2001-04-19")
# results_2001_2002 <- scores_clean(scores_df_02, "2002-04-18")
# results_2002_2003 <- scores_clean(scores_df_03, "2003-04-17")
# results_2003_2004 <- scores_clean(scores_df_04, "2004-04-15")
# results_2004_2005 <- scores_clean(scores_df_05, "2005-04-21")
# results_2005_2006 <- scores_clean(scores_df_06, "2006-04-20")
# results_2006_2007 <- scores_clean(scores_df_07, "2007-04-19")
# results_2007_2008 <- scores_clean(scores_df_08, "2008-04-17")
# results_2008_2009 <- scores_clean(scores_df_09, "2009-04-16")
# results_2009_2010 <- scores_clean(scores_df_10, "2010-04-15")
# results_2010_2011 <- scores_clean(scores_df_11, "2011-04-14")
# results_2011_2012 <- scores_clean(scores_df_12, "2012-04-27")
# results_2012_2013 <- scores_clean(scores_df_13, "2013-04-18")
# results_2013_2014 <- scores_clean(scores_df_14, "2014-04-17")
# results_2014_2015 <- scores_clean(scores_df_15, "2015-04-16")
# results_2015_2016 <- scores_clean(scores_df_16, "2016-04-14")
# results_2016_2017 <- scores_clean(scores_df_17, "2017-04-13")
# results_2017_2018 <- scores_clean(scores_df_18, "2018-04-12")
# results_2018_2019 <- scores_clean(scores_df_19, "2019-04-11")
results_2019_2020 <- scores_clean(scores_df_20, "2020-03-12")

results_final_df <- rbind(results_2016_2017,
                          results_2017_2018,
                          results_2018_2019,
                          results_2019_2020)

write_rds(results_final_df, "data/schedule_final.rds")


#### get advance stats

url <- paste0("https://www.basketball-reference.com/leagues/NBA_2019.html#div_misc_stats")

webpage <- read_html(url)

b <- webpage %>% 
  html_nodes("div table") %>% 
  html_table()


#adding win loss for each team and creating standings chart

standings_19 <- scores_final_19 %>% 
  group_by(Win) %>% 
  summarise(W = n()) %>% 
  mutate(L = 82-W)










######################################## END ############################################




##################################### ADVANCE STATS ##################################### 

day_one <- c("16","22","29","05","12","19","26","03","10","17","24",'31',"07","14","21",'28',"04","11","18","25","04","11","18","25","01","08")
month_one <- c("10","10","10","11","11","11","11","12",'12',"12",'12',"12","01","01","01","01","02","02","02","02","03","03","03","03","04","04")
day_two <- c("21","28","04","11","18","25","02","09","16","23","30","06","13","20",'27',"03","10","17","24","03","10","17","24","31","07","10")
month_two <- c("10","10","11","11","11","11","12","12","12","12","12","01","01","01","01","02","02","02",'02',"03","03","03","03","03","04","04")

url <- paste0("https://stats.nba.com/teams/advanced/?sort=W&dir=-1&Season=2018-19&SeasonType=Regular%20Season&DateFrom=",month_one,
              "%2F",day_one,"%2F2018&DateTo=",month_two,"%2F",day_two,"%2F2018")
url <- paste0("https://stats.nba.com/teams/advanced/?sort=W&dir=-1&Season=2018-19&SeasonType=Regular%20Season")

#pulling in data from excel
advance_stats_1 <- read_excel("Documents/advance_stats.xlsx", 
                            sheet = "10.16-10.28")
row.names(advance_stats_1) <- advance_stats_1$TEAM
advance_stats_2 <- read_excel("Documents/advance_stats.xlsx", 
                              sheet = "10.29-11.11")
row.names(advance_stats_2) <- advance_stats_2$TEAM
advance_stats_3 <- read_excel("Documents/advance_stats.xlsx", 
                              sheet = "11.12-11.25")
row.names(advance_stats_3) <- advance_stats_3$TEAM
advance_stats_4 <- read_excel("Documents/advance_stats.xlsx", 
                              sheet = "11.26-12.9")
row.names(advance_stats_4) <- advance_stats_4$TEAM
advance_stats_5 <- read_excel("Documents/advance_stats.xlsx", 
                              sheet = "12.10-12.23")
row.names(advance_stats_5) <- advance_stats_5$TEAM
advance_stats_6 <- read_excel("Documents/advance_stats.xlsx", 
                              sheet = "12.24-1.6")
row.names(advance_stats_6) <- advance_stats_6$TEAM
advance_stats_7 <- read_excel("Documents/advance_stats.xlsx", 
                              sheet = "1.7-1.20")
row.names(advance_stats_7) <- advance_stats_7$TEAM
advance_stats_8 <- read_excel("Documents/advance_stats.xlsx", 
                              sheet = "1.21-2.3")
row.names(advance_stats_8) <- advance_stats_8$TEAM
advance_stats_9 <- read_excel("Documents/advance_stats.xlsx", 
                              sheet = "2.4-2.17")
row.names(advance_stats_9) <- advance_stats_9$TEAM
advance_stats_10 <- read_excel("Documents/advance_stats.xlsx", 
                              sheet = "2.18-3.3")
row.names(advance_stats_10) <- advance_stats_10$TEAM
advance_stats_11 <- read_excel("Documents/advance_stats.xlsx", 
                              sheet = "3.4-3.17")
row.names(advance_stats_11) <- advance_stats_11$TEAM
advance_stats_12 <- read_excel("Documents/advance_stats.xlsx", 
                              sheet = "3.18-4.1")
row.names(advance_stats_12) <- advance_stats_12$TEAM
advance_stats_13 <- read_excel("Documents/advance_stats.xlsx", 
                              sheet = "4.2-4.10")
row.names(advance_stats_13) <- advance_stats_13$TEAM

teams <- advance_stats_1$TEAM
col <- colnames(df)[6:19]

#function to aggregate data for individual team - NEED TO CORRECT WEIGHTED AVERAGE
team_data <- function(team_name){
  team <- team_name
  
  df <- bind_rows(advance_stats_1[team,],advance_stats_2[team, ],advance_stats_3[team, ],advance_stats_4[team, ],
                advance_stats_5[team, ],advance_stats_6[team, ],advance_stats_7[team, ],advance_stats_8[team, ],
                advance_stats_9[team, ],advance_stats_10[team, ],advance_stats_11[team, ],advance_stats_12[team, ],
                advance_stats_13[team, ])
  
  #### this is where rolling average calc happens
  test_df <- df %>% mutate(rollsum = cumsum(GP), agg_off = OFFRTG*GP, offrtg = cumsum(agg_off)/rollsum) %>% 
    mutate(agg_def = DEFRTG*GP, defrtg = cumsum(agg_def)/rollsum) %>% 
    mutate(netrtg = offrtg-defrtg) %>% 
    mutate(agg_astper = `AST%`*GP, ast_pct = cumsum(agg_astper)/rollsum) %>% 
    mutate(agg_ast_to = `AST/TO`*GP, ast_to = cumsum(agg_ast_to)/rollsum) %>% 
    mutate(agg_ast_ratio = `AST RATIO`*GP, ast_ratio = cumsum(agg_ast_ratio)/rollsum) %>% 
    mutate(agg_oreb_pct = `OREB%`*GP, oreb_pct = cumsum(agg_oreb_pct)/rollsum) %>% 
    mutate(agg_dreb_pct = `DREB%`*GP, dreb_pct = cumsum(agg_dreb_pct)/rollsum) %>% 
    mutate(agg_reb_pct = `REB%`*GP, reb_pct = cumsum(agg_reb_pct)/rollsum) %>% 
    mutate(agg_tov_pct = `TOV%`*GP, tov_pct = cumsum(agg_tov_pct)/rollsum) %>% 
    mutate(agg_efg_pct = `EFG%`*GP, efg_pct = cumsum(agg_efg_pct)/rollsum) %>% 
    mutate(agg_ts_pct = `TS%`*GP, ts_pct = cumsum(agg_ts_pct)/rollsum) %>% 
    mutate(agg_pace = PACE*GP, pace = cumsum(agg_pace)/rollsum) %>% 
    mutate(agg_pie = PIE*GP, pie = cumsum(agg_pie)/rollsum) %>% 
    select(TEAM, offrtg, defrtg, netrtg, ast_pct, ast_to, ast_ratio, oreb_pct, dreb_pct, reb_pct, tov_pct, efg_pct, ts_pct, pace, pie)
    

  df_expanded <- test_df[rep(row.names(df), df$GP), 1:15]

  twolves <- scores_final_19 %>% 
  filter(Away == team | Home == team)

  wolves_df <- df_expanded %>% 
  mutate(date = twolves$date)
  
  return(wolves_df)
}

team_list <- list()

for (i in teams) {
  tmp <- team_data(i)
  team_list[[i]] <- tmp
}

team_final_2019 <- do.call(rbind, team_list)

model_df <- scores_final_19 %>% 
  left_join(team_final_2019, by = c("Away" = "TEAM", "date" = "date")) %>% 
  left_join(team_final_2019, by = c("Home" = "TEAM", "date" = "date"))

colnames(model_df) <- c("Away","Away_PTS","Home","Home_PTS","OTflag","date","Win","offrtg.away","defrtg.away","netrtg.away",   
                        "ast_pct.away","ast_to.away","ast_ratio.away","oreb_pct.away","dreb_pct.away","reb_pct.away","tov_pct.away","efg_pct.away",
                        "ts_pct.away","pace.away","pie.away","offrtg.home","defrtg.home","netrtg.home","ast_pct.home","ast_to.home","ast_ratio.home",
                        "oreb_pct.home","dreb_pct.home","reb_pct.home","tov_pct.home","efg_pct.home","ts_pct.home","pace.home","pie.home")

#saveRDS(model_df, "/Users/andrewwohlfeil/Documents/model_df.rds")

######################################## END ############################################


temp = list.files(path = "/Users/andrewwohlfeil/Documents/data/Games/2018-19", pattern="*.csv")
myfiles = lapply(paste0("/Users/andrewwohlfeil/Documents/data/Games/2018-19/",temp), 
                 read.delim, stringsAsFactors = FALSE, sep=",")
boxscoes <- do.call(rbind, myfiles)

