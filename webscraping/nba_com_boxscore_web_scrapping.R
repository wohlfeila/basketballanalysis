library(RSelenium)
library(XML)

rD <- rsDriver(browser = 'firefox')
remDr <- rD[['client']]

remDr$navigate("https://stats.nba.com/teams/boxscores-advanced/?Season=2016-17&SeasonType=Regular%20Season")

select_all <- remDr$findElement(using = "xpath", 
                                value = "/html/body/main/div[2]/div/div[2]/div/div/nba-stat-table/div[1]/div/div/a[2]")

# scrapping first table
gettable <- remDr$findElement(using = "css selector", value = ".nba-stat-table")
webElem5txt <- gettable$getElementAttribute("outerHTML")[[1]]
table_aggregate <- readHTMLTable(webElem5txt, header=TRUE, as.data.frame=TRUE)[[1]]

# scrapping remaining tables and appending it to first table
for (i in 1:49){
  select_all$clickElement()
  
  Sys.sleep(1)
  
  gettable <- remDr$findElement(using = "css selector", value = ".nba-stat-table")

  Sys.sleep(1)
  
  webElem5txt <- gettable$getElementAttribute("outerHTML")[[1]]
  table <- readHTMLTable(webElem5txt, header=TRUE, as.data.frame=TRUE)[[1]]
  table_aggregate <- rbind(table_aggregate, table)
}

table_aggregate <- taRifx::unfactor.data.frame(table_aggregate)

# colnames(table_aggregate) <- c("player","team", "match_up",
#                                "game_date", "season", "w_l", "min",
#                                "pts", "fgm", "fga", "fg_pct", "three_pm", "three_pa",
#                                "three_pct", "ftm", "fta", "ft_pct", "oreb", "dreb",
#                                "reb", "ast", "stl", "blk", "tov", "pf", "plus_minus")

# colnames(table_aggregate) <- c("team", "match_up",
#                                "game_date", "season", "w_l", "min",
#                                "pts", "fgm", "fga", "fg_pct", "three_pm", "three_pa",
#                                "three_pct", "ftm", "fta", "ft_pct", "oreb", "dreb",
#                                "reb", "ast", "stl", "blk", "tov", "pf", "plus_minus")

colnames(table_aggregate) <- c("team,match_up,game_date,season,
                               w_l,min,off_rtg,def_rtg,net_rtg,ast_pct,
                               ast_to_ratio,ast_ratio,oreb_pct,dreb_pct,
                               red_pct,tov_pct,efg_pct,ts_pct,pace,pie")

table_aggregate$season <- "2016-2017"

readr::write_rds(table_aggregate, "data/team_boxscore/team_boxscore_advance/team_boxscore_advance_20162017.rds")
