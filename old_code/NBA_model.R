library(tidyverse)
library(readr)


teams <- c("Milwaukee Bucks", "Golden State Warriors", "Toronto Raptors", "Utah Jazz",
           "Denver Nuggets", "Portland Trail Blazers", "Houston Rockets", "Boston Celtics",
           "Indiana Pacers", "Oklahoma City Thunder", "Philadelphia 76ers", "San Antonio Spurs",
           "Los Angeles Clippers", "Detroit Pistons", "Orlando Magic", "Miami Heat",
           "Sacramento Kings", "New Orleans Pelicans", "Minnesota Timberwolves", "Brooklyn Nets",
           "Dallas Mavericks", "Charlotte Hornets", "Los Angeles Lakers", "Memphis Grizzlies",
           "Washington Wizards", "Atlanta Hawks", "Chicago Bulls", "Phoenix Suns",
           "New York Knicks", "Cleveland Cavaliers")  

#nba sched - test model
nba_sched <- read_csv("Documents/nba_sched.csv")
colnames(nba_sched) <- c("Date", "Away", "Home", "Notes")

#how can I create and update team off and def team ratings - use players?
team_ratings <- read_csv("Documents/team_ratings.csv")
colnames(team_ratings) <- c("Rk", "Team", "Conf", "Div", "W", "L", "WL_pct", "MOV", 
                            "ORtg", "DRtg", "NRtg", "MOVA", "ORtgA", "DRtgA", 
                            "NRtgA")
#need more accurate team ratings
#team ratings should be based on individual player ratings - this allows for injuries and hot streaks also, 
#needs to be able update standing predictions each week.

win_pct <- function(off,def){
  (off)^14/((off)^14+(def)^14)
}

#adding in winning percentage - taken from NBA reference.com
ratings_clean <- team_ratings %>% 
  mutate(winning_pct = win_pct(ORtgA, DRtgA))

#CARMELO rating from 538 
ratings_clean <- ratings_clean %>% 
  mutate(CARMELO_Rating = 1504.6-450*log10((1/winning_pct)-1))

#team one's probability of beating team two
win_prob_v1 <- function(team_one,team_two){
  tmp1 <- ratings_clean %>% filter(Team == team_one) %>% pull(CARMELO_Rating)
  tmp2 <- ratings_clean %>% filter(Team == team_two) %>% pull(CARMELO_Rating)
  difference <- -(tmp1-tmp2) #difference between the two CARMELO ratings - this is where 
  #we can add game specific adjustments in CARMELO
  foo <- exp(difference/400)
  1/(10*foo+1)
}

#workaround until I figure out how to make win_prob_v1 eqaul one
win_prob <- function(team_one, team_two){
  one <- win_prob_v1(team_one,team_two)
  two <- win_prob_v1(team_two, team_one)
  prob <- one/(one+two)
  print(prob)
}

#probibility of team one winning
model_pred <- nba_sched %>%
  select(Date, Away, Home) %>% 
  mutate(away_win_prob = apply(nba_sched, 1, function(x) win_prob(x[2], x[3])),
         home_win_prob = apply(nba_sched, 1, function(x) win_prob(x[3], x[2])),
         winner = if_else(away_win_prob > home_win_prob, Away, Home))

#creating standings dataframe
w_l <- function(team){
  test <- model_pred 
  test %>% 
  filter(Away == team | Home == team) %>% 
  mutate(win = if_else(winner == team, 1, 0))
}

df <- c()

for (i in teams){
  boo <- w_l(i)
  w <-sum(boo$win)
  df <- c(df, w)
}

standings <- data_frame(Teams = teams, 
                        W = df, L = 82-df)
standings
  
  
  
  
  
  
