library(plyr)
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o) 
library(ggplot2)
library(grid)
library(ggmap)
library(dplyr)
library(knitr)
library(magick)
library(gifski)
library(gganimate)
#Stacking DFs on top of each other
path <- 'Bike_rds/bikes_' 
all_files <- paste0(path,500:1000, ".RDS")
bikes <- lapply(all_files, readRDS)
bikes <- do.call(rbind, bikes)

#Determining the average stationary time for bikes to decide when to begin counting the regression model. 
quan_ttlt <- as.numeric(quantile(bikes$time_to_last_trip, 0.75))
starting_idx <- round(quan_ttlt/5) + 33

#We load in the starting index and the subsequent 24hrs of data. 
train_files <- paste0(path,starting_idx:(starting_idx+120), ".RDS")
bikes.train <- lapply(train_files, readRDS)
bikes.train <- do.call(rbind, bikes.train)
#The columns that will be used in the initial analysis
cols <- c("latitude", "longitude", "dayOfWeek", "dayOrNight", "temperatureFeelsLike", "precip24Hour", "windSpeed", "Elevation", "Rent", "nn_dis", "prev_trip_length", "time_to_last_trip")
bikes.train <- bikes.train[, cols]


test_files <- paste0(path,starting_idx+121, ".RDS")
bikes.test <- lapply(test_files, readRDS)
bikes.test <- do.call(rbind, bikes.test)
#The columns that will be used in the initial analysis
cols <- c("latitude", "longitude", "dayOfWeek", "dayOrNight", "temperatureFeelsLike", "precip24Hour", "windSpeed", "Elevation", "Rent", "nn_dis", "prev_trip_length", "time_to_last_trip")
bikes.test <- bikes.test[, cols]


set.seed(123)
valid_split <- initial_split(bikes.train,  .8)
bikes.train_2 <- analysis(valid_split)

bikes.valid <- assessment(valid_split)
x.features <- setdiff(names(bikes.train), "time_to_last_trip")
x_test <- bikes.valid[, x.features]
y_test <- bikes.valid$time_to_last_trip


bikes.rf_oob <- randomForest(time_to_last_trip ~ ., data = bikes.train, mtry = 3,
                         importance = TRUE, na.action = na.omit, ntree = 300, xtest = x_test, ytest = y_test)


oob <- sqrt(bikes.rf_oob$mse)
validation <- sqrt(bikes.rf_oob$test$mse)

tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:bikes.rf_oob$ntree
) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  xlab("Number of trees")


set.seed(123)

system.time(
  bikes.ranger <- ranger(
    formula   = time_to_last_trip ~ ., 
    data      = bikes.train, 
    num.trees = 300,
    mtry      = floor(length(x.features) / 3)
  )
)

#Parameter Values for thr grid search
hyper_grid <- expand.grid(
  mtry       = seq(1, length(x.features), by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

# total number of combinations
nrow(hyper_grid)

#Run Grid Search in Ranger
for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = time_to_last_trip ~ ., 
    data            = bikes.train, 
    num.trees       = 300,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

#Checking RMSE Values 
hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

#Now optimal paramter values have been chosen, we repeat the running of the model to assess it's variability. 
OOB_RMSE <- vector(mode = "numeric", length = 30)

for(i in seq_along(OOB_RMSE)) {
  
  optimal_ranger <- ranger(
    formula         = time_to_last_trip ~ ., 
    data            = bikes.train, 
    num.trees       = 300,
    mtry            = 7,
    min.node.size   = 3,
    sample.fraction = .8,
    importance      = 'impurity'
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}

hist(OOB_RMSE, breaks = 20)

#Assessing Variable importance accoring to the Ranger Model
optimal_ranger$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(11) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 10 important variables")




dub_bb <- c(
  left = -6.5,
  bottom = 53.25,
  right = -6.0,
  top = 53.5
)

dub_stamen <- get_stamenmap(
  bbox = dub_bb,
  zoom = 11
)

for (i in 121:241){
  bikes.test <- readRDS(paste0('Bike_rds/bikes_' ,starting_idx+i, ".RDS"))
  time <- as.character(bikes.test$harvest_time[1])
  #The columns that will be used in the initial analysis
  cols <- c("latitude", "longitude", "dayOfWeek", "dayOrNight", "temperatureFeelsLike", "precip24Hour", "windSpeed", "Elevation", "Rent", "nn_dis", "prev_trip_length", "time_to_last_trip")
  bikes.test <- bikes.test[, cols]
  
  
  #Making Predictions from the Ranger Model
  pred_ranger <- predict(optimal_ranger, bikes.test)
  bikes.pred <- pred_ranger$predictions
  
  #Developing Score as the normalized residual of the fit
  resid <- bikes.test$time_to_last_trip - bikes.pred
  resid[resid < 0] <- 0
  bikes.test$score <- resid/max(resid)

  
  #Which bikes have highest Score
  #bikes.test[order(bikes.test$score, decreasing = T), ]
  bikes.test.full <- readRDS(paste0('Bike_rds/bikes_' ,starting_idx+i, ".RDS"))
  bikes.test.full$score <- bikes.test$score
  saveRDS(bikes.test.full, file  = paste0('Bike_rds/bikes_' ,starting_idx+i, ".RDS"))
  
  bikes.test <- bikes.test[order(bikes.test$score), ]
  p <- ggmap(dub_stamen) + 
    geom_point(data = bikes.test, mapping = aes(longitude, latitude, color = score, size = score))+ 
    scale_color_viridis_c()+
    ggtitle(paste0("Time: ",time))
  
  png(paste0("Bike_pngs/bike_", as.character(i), "_map.png"))
  print(p)
  dev.off()
  
  png(paste0("Bike_pngs/bike_", as.character(i), "_hist.png"))
  hist(bikes.test$score, breaks = 30, xlab = "Broken Score", main = paste0("Histogram of Scores for ", time))
  dev.off()
  
}







