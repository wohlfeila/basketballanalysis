library(tidyverse)
library(tidymodels)
library(plotly)
library(htmlwidgets)
library(htmltools)
library(shiny)
library(rlang)
library(shinyWidgets)
library(DT)

options(scipen = 999)

# reading in nba stats
nba_stats <- read_csv("player_stats.csv")

years <- seq(1980, 2020, 1)
years_to_select <- c(years, "All")

addResourcePath("nba_player_clustering", getwd())

colnames_reorder <-  c("Player", "year", "Pos", "Age", "Tm", "G", "GS", 
                       "MP", "FG", "FGA", "FG%", "3P", "3PA", "3P%", "2P", 
                       "2PA", "2P%", "eFG%", "FT", "FTA", "FT%", "ORB", "DRB", 
                       "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS")

kmeans_calc <- function(df, num_cluster){
  num_clust <- num_cluster
  players <- df$unique_identifier
  players_names <- rep(players, num_clust)
  
  pca_prep <- df %>% 
    select(-Player, -year, -Pos, -Age, -Tm, -G) %>% 
    replace(is.na(.), 0)
  
  pca_recipe <- recipe(~., data = pca_prep)
  
  pca_trans <- pca_recipe %>%
    # center the data
    step_center(all_numeric()) %>%
    # scale the data
    step_scale(all_numeric()) %>%
    # pca on all numeric variables
    step_pca(all_numeric())
  
  pca_estimates <- prep(pca_trans)
  
  juice <- juice(pca_estimates)
  
  kmeans_df <- juice %>%
    select(-unique_identifier)
  
  kclusts <- tibble(k = 1:num_clust) %>%
    mutate(kclust = map(k, ~kmeans(kmeans_df, .x)),
           tidied = map(kclust, tidy),
           glanced = map(kclust, glance),
           augmented = map(kclust, augment, kmeans_df))
  
  plotly_data <- kclusts %>% 
    unnest(cols = c(augmented)) %>% 
    mutate(players = players_names) %>% 
    filter(k == num_cluster)
  
  return(plotly_data)
}
