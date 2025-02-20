# File: 15_RandomForest.R
# Project: Dissertation - Bagging
# Author: Mykel Brinkerhoff
# Date: 2025-01-09 (Th)
# Description: This script performs a Random FOrest CART analysis on the voice quality contrasts
#   in the SLZ dataset.
#
# Usage:
#   Rscript 15_RandomForest.R
#
# Notes:
#   - Ensure all required packages are installed.
#   - Modify the script as needed for your specific dataset and analysis requirements.

#installing the necessary packages to perform Random Forest CART analysis
install.packages("ranger") # for performing Random Forest CART analysis
install.packages("here") # for creating pathways relative to the top-level directory
install.packages("tidyverse") # for data manipulation, graphic, and data wrangling
install.packages("Cairo") # for saving the plots as .eps files
install.packages("remotes") # <- if not already installed

# Load necessary libraries
library(ranger) # for performing Random Forest CART analysis
library(here) # for creating pathways relative to the top-level directory
library(tidyverse) # for data manipulation, graphic, and data wrangling
library(Cairo)
library(remotes)

# Loading the data
slz_clean <- read.csv("data/processed/slz_normalized.csv", header = TRUE)

slz_rf <- slz_clean %>%
    select(Phonation, H1c_resid, h2cz, h4cz, a1cz, a2cz, a3cz, h1h2cz, h2h4cz, h1a1cz, h1a2cz, h1a3cz, h42Kcz, h2Kh5Kcz, cppz, energyz, hnr05z, hnr15z, hnr25z, hnr35z, shrz, norm.soe)

#factorizing the Phonation column
slz_rf$Phonation <- factor(slz_rf$Phonation, levels = c("modal", 
                                                        "breathy", 
                                                        "checked", 
                                                        "laryngealized"))

# Splitting the data into training and test sets
set.seed(123) # needed for reproducibility

split_strat  <- initial_split(slz_rf, prop = 0.7, 
                              strata = "Phonation")
rf_train  <- training(split_strat)
rf_test   <- testing(split_strat)

# Define the control
trControl <- trainControl(method = "cv",
    number = 10,
    search = "grid")

# Random Forest CART analysis
## train a default random forest model
rf_default <- train(Phonation~.,
    data = rf_train,
    method = "rf",
    metric = "Accuracy",
    trControl = trControl)
# Print the results
print(rf_default)

# Calculate OOB RMSE for the default random forest model
default_rmse <- sqrt(rf_default$finalModel$prediction.error)
print(paste("OOB RMSE for the default random forest model:", default_rmse))

# Tune hyperparameters for classification
tuneGrid <- expand.grid(.mtry = c(1: n_features))

rf_tune <- train(Phonation~.,
    data = rf_train,
    method = "rf",
    metric = "Accuracy",
    trControl = trControl,
    tuneGrid = tuneGrid)
print(rf_tune)

best_mtry <- rf_tune$bestTune$mtry 
print(paste("Best mtry:", best_mtry))

# Best ntrees
tuneGrid <- expand.grid(.mtry = best_mtry)
ntree <- seq(20, 1000, by = 20)
store_maxtrees <- list()

for (i in seq_along(ntree)) {
    rf_tune <- train(Phonation~.,
        data = rf_train,
        method = "rf",
        metric = "Accuracy",
        trControl = trControl,
        tuneGrid = tuneGrid,
        ntree = ntree[i])
    store_maxtrees[[i]] <- rf_tune
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

# Extract the best ntree
best_ntree <- results_tree$bestTune$ntree
print(paste("Best ntree:", best_ntree))

# Best nodesize
tuneGrid <- expand.grid(.mtry = best_mtry)
nodesize_values <- c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
store_nodesize <- list()

for (i in seq_along(nodesize_values)) {
    rf_tune <- train(Phonation~.,
        data = rf_train,
        method = "rf",
        metric = "Accuracy",
        trControl = trControl,
        tuneGrid = tuneGrid,
        nodesize = nodesize_values[i])
    store_nodesize[[i]] <- rf_tune
}
results_nodesize <- resamples(store_nodesize)
summary(results_nodesize)

# Extract the best nodesize
best_nodesize <- results_nodesize$bestTune$.nodesize
print(paste("Best nodesize:", best_nodesize))

# Train the final model with the best hyperparameters
final_rf_model <- randomForest(Phonation~.,
    data = rf_train,
    mtry = best_mtry,
    ntree = 700,  # Assuming 840 is the best ntree found
    nodesize = 5)
print(final_rf_model)

# Predict the test set
rf_pred <- predict(final_rf_model, rf_test)
confusionMatrix(rf_pred, rf_test$Phonation)

# variable importance
var_imp_rf <- importance(final_rf_model)
var_imp_rf <- data.frame(Variable = rownames(var_imp_rf), Importance = var_imp_rf[, 1])
var_imp_rf <- var_imp_rf %>% arrange(desc(Importance))

# Create a Lollipop chart of variable importance scores
variable_importance_rf <- ggplot(var_imp_rf, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_segment(aes(xend = Variable, yend = 0)) +
    geom_point() +
    coord_flip() +
    labs(title = "Variable Importance for Random Forest", 
        x = "Variable", 
        y = "Importance (Gini Index)") +
    theme_bw()
variable_importance_rf
