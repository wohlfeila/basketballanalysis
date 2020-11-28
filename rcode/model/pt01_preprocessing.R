train <- read_rds("data/nba_train.rds") %>% select(-date)
test <- read_rds("data/nba_test.rds") %>% select(-date)

# percent of data used for testing
nrow(test) / (nrow(train) + nrow(test))

# creating recipe and centering data
model_rec <- recipe(home_win ~ ., train) %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric())

# preparing the recipe
model_recipe_train <- prep(model_rec, training = train, verbose = TRUE)

# creating final pre-processed test and train sets
train <- juice(model_recipe_train)
test <- bake(model_recipe_train, new_data = test)
