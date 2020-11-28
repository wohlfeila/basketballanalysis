library(tidyverse)
library(grid)
library(jpeg)
library(RCurl)

shots_2019 <- read_csv("data/shot_chart_data/shots-2019.csv")

shots_clean <- shots_2019 %>% 
  mutate(x = as.numeric(sub("px", "", x)),
         y = as.numeric(sub("px", "", y)))

sc_scatter_one_variable <- function(data, var1, y_chart_max, x_chart_max, ...){
  courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
  court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                      width = unit(1,"npc"), height = unit(1,"npc"))
  var1 <- enquo(var1)
  
  data %>% 
    ggplot(aes(x=y, y=x, color = !!var1)) + 
    annotation_custom(court, -10, x_chart_max, -10, y_chart_max) +
    geom_point() +
    theme_void()
}

timberwolves_shots <- shots_clean %>% 
  filter(team == "Minnesota")

spurs_wolves <- timberwolves_shots %>% 
  filter(game_id == "201912080LAL")

sc_scatter_one_variable(spurs_wolves, attempt, y_chart_max = 450, x_chart_max = 485)
