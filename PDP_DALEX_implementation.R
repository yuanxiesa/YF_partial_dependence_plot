rm(list=ls())

# use the bike rental data 
# from https://christophm.github.io/interpretable-ml-book/bike-data.html#bike-data
load("data/bike.RData")

library(DALEX)
library(tidyverse)

# use DALEX
# install.packages("DALEX")

# select a subsection of the bike dataset
bike_sub <- bike %>% dplyr::select(cnt, season, temp, hum, windspeed)


# create a random forest model for the bike dataset
# package randomForest
bike_rf_1 <- randomForest::randomForest(cnt ~., data = bike_sub)

# create an explainer
# The "explain" creates a unified representation of a model, 
# which can be further processed by functions for explanations.
bike_rf_explainer_1 <- DALEX::explain(bike_rf_1, data = bike_sub, 
                                      label = 'randomForest')

# create partial dependence function
# default only compute PDP on 100 datapoints
# you can add n=NULL to compute PDP using all datapoints
# try it to see the difference
bike_rf_model_profile <- DALEX::model_profile(explainer = bike_rf_explainer_1,
                                              variables = 'windspeed',
                                              type = 'partial')

# use plot function to visualize the result
plot(bike_rf_model_profile)

# it will be better to write a customized visualization function
# to (1) understand the results better and 
# (2) to get an impression on the distribution of samples


ggplot() +
  geom_point(mapping = aes(x=bike_rf_model_profile$cp_profiles$windspeed, 
                           y=bike_rf_model_profile$cp_profiles$`_yhat_`),
             shape = "circle open", color = 'light blue') +
  geom_line(mapping = aes(x=bike_rf_model_profile$agr_profiles$`_x_`,
                          y=bike_rf_model_profile$agr_profiles$`_yhat_`)) +
  theme_bw() +
  xlab('Windspeed') +
  ylab("Bike Rental")


