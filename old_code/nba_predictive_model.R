library(dplyr) # data manipulation
library(stringr) # working with strings
library(lubridate) # working with dates
library(caret) # used for building test and train data sets and check linear relationship between dependent variables
library(e1071) # used for SVM model
library(ISLR) # used for logistic regression model
library(readr) # write_rds
library(ggplot2) # visualizations
library(factoextra)

# loading data
model.data <- readRDS("/Volumes/TOSHIBA EXT/NBA_data/model.df.final.rds")


###################### Preparing Data for Model #####################################################
# removing columns not to be included in fitting model
model.df <- model.data %>% mutate(game_count = seq(1,nrow(model.data), 1))

# splitting data between test and train
train.index <- createDataPartition(model.df$home_win, p = .9, 
                                   list = FALSE, 
                                   times = 1)

model.df.final <- model.df %>% 
  select(-Away, -Away_PTS, -Home, -Home_PTS, -Win, -date, -OTflag, -season)

model.df.final$home_days_since_last_game[is.na(model.df.final$home_days_since_last_game)] <- 0
model.df.final$away_days_since_last_game[is.na(model.df.final$away_days_since_last_game)] <- 0

#write_csv(x = model.df.final, path = "nba.model.df.csv")

# creating train/test data frames
nba.train <- model.df.final[ train.index,]
nba.test  <- model.df.final[-train.index,]

# building accuracy function to run off of table() results
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100} # will also need kappa metric

########################## VISUALIZATION/DISCOVERY ##################################################

# # looking for correlation between independent variables
# descr.cor <-  cor(final.model.df)
# summary(descr.cor[upper.tri(descr.cor)])
# corrplot::corrplot(descr.cor)

#####################################################################################################

################## Support Vector Machine ###########################################################
# fitting svm model
kernels <- c("linear", "radial", "sigmoid", "polynomial")

svm.metrics <- list()

for (i in kernels) {
  svm.model <- svm(as.factor(home_win) ~ ., data = nba.train, kernel = i, cost = 0.1)

  svm.pred <- predict(svm.model, nba.test)

  svm.pred.table <- table(svm.pred, nba.test$home_win)

  tmp <- accuracy(svm.pred.table)
  
  svm.metrics[i] <- tmp
}

final.svm.metrics <- do.call(rbind, svm.metrics)
final.svm.metrics

# fitting best SVM model
svm.model <- svm(as.factor(home_win) ~ ., data = nba.train, kernel = "linear", cost = 0.1)
svm.pred <- predict(svm.model, nba.test)
svm.pred.table <- table(svm.pred, nba.test$home_win)
accuracy(svm.pred.table)


sched.pred.svm <- model.df %>% 
  right_join(nba.test, by = "game_count") %>% 
  mutate(pred = svm.pred) %>% 
  select(Away, Away_PTS, Home, Home_PTS, date, Win, season, home_win.x, pred) 


#####################################################################################################

################## Logistic Regression ##############################################################
# fitting logistic regression model
log.model <- glm(home_win ~ ., data = nba.train, family = binomial)

summary(log.model)

log.probs <- predict(log.model, newdata = nba.test, type = "response")

log.pred <- ifelse(log.probs > 0.5, 1, 0)

log.pred.table <- table(log.pred, nba.test$home_win)

accuracy(log.pred.table)

sched.pred.log <- model.df %>% 
  right_join(nba.test, by = "game_count") %>% 
  mutate(pred = log.pred) %>% 
  select(Away, Away_PTS, Home, Home_PTS, date, Win, season, home_win.x, pred)


#####################################################################################################

# creating train/test data frames

nba.train <- model.df.final[1:6642,]
nba.test  <- model.df.final[6643:7380,]

train_labels <- to_categorical(nba.train$home_win)
test_labels <- to_categorical(nba.test$home_win)

nba.train <- nba.train %>% select(-home_win, -game_count)
nba.test <- nba.test %>% select(-home_win, -game_count)

nba.train <- as.matrix(nba.train)
nba.test <- as.matrix(nba.test)

network <- keras_model_sequential() %>% 
  layer_dense(units = 512, activation = "softmax", input_shape = c(1*22)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 512, activation = "sigmoid", input_shape = c(1*22)) %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 2, activation = "sigmoid")

network %>% compile(
  optimizer = "Nadam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

network %>% fit(nba.train, train_labels, epochs = 150, batch_size = 128)

metrics <- network %>% evaluate(nba.test, test_labels)
metrics

network %>% predict_classes(nba.test[1:10, ])




