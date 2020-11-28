library(tidyverse)
library(tidymodels)

final_model_df <- read_rds("data/final_model_df.rds")

model_input <- final_model_df %>% 
  select(-Home, -Away) %>% 
  mutate(home_win = as.factor(home_win))

# Since teams change over time, this needs to be set up as a time-series problem.
train <- model_input %>% 
  filter(date < "2019-10-22") 

test <- model_input %>% 
  filter(date >= "2019-10-22")

# Home teams win more often than the away team.  What would my accuracy be if I predicted every home team to win?
home_winning_pct <- sum(as.numeric(as.character(train$home_win))) / nrow(train)

write_rds(train, "data/nba_train.rds")
write_rds(test, "data/nba_test.rds")
