library(tidyverse)  # for data wrangling
library(janitor)  # for data cleaning
library(rvest)      # for web scraping
library(corrplot)   # correlation plots
library(factoextra) # clustering algorithms & visualization


scrape_stats <- function(season){
  #total stats
  #scrape
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_totals.html")
  stats_tot <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  #clean
  player_stats_tot <- stats_tot %>% 
    remove_empty("cols") %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-rk)
  
  #per minute
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_per_minute.html")
  stats_pm <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  player_stats_pm <- stats_pm %>% 
    remove_empty("cols") %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    rename_at(vars(9:29),funs(paste0(.,"_pm"))) %>% 
    select(-rk)
  
  #advanced
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_",season,"_advanced.html")
  stats_adv <- url %>% 
    read_html() %>% 
    html_table() %>% 
    .[[1]]
  
  player_stats_adv <- stats_adv %>% 
    remove_empty("cols") %>%
    clean_names() %>% 
    dplyr::filter(!player=="Player") %>%
    mutate_at(vars(-c(player,tm,pos)),as.numeric) %>% 
    mutate_at(vars(-c(player,tm,pos)), funs(replace(., is.na(.), 0))) %>% 
    as_tibble() %>% 
    group_by(player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    select(-rk)
  
  player_stats <- full_join(player_stats_tot,player_stats_pm,
                            by = c("player", "pos", "age", "tm", "g", "gs", "mp")) %>% 
    full_join(player_stats_adv,
              by = c("player", "pos", "age", "tm", "g", "mp"))
  return(player_stats)
}

season_2016 <- scrape_stats(2016)
season_2017 <- scrape_stats(2017)
season_2018 <- scrape_stats(2018)
season_2019 <- scrape_stats(2019)


df_2019 <- season_2019 %>% 
  filter(mp > 500) 

player <- df_2019$player

df_2019 <- df_2019 %>% 
  select(fg:vorp) %>% 
  mutate_all(funs(scale(.)))

rownames(df_2019) <- player

k2 <- kmeans(df_2019, centers = 5, nstart = 2)
str(k2)
k2

fviz_cluster(k2, df_2019)



