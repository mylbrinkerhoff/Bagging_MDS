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
    select(Phonation, 
           H1c_resid, 
           h2cz, 
           h4cz, 
           a1cz, 
           a2cz, 
           a3cz, 
           h1h2cz, 
           h2h4cz, 
           h1a1cz, 
           h1a2cz, 
           h1a3cz, 
           h42Kcz, 
           h2Kh5Kcz, 
           cppz, 
           energyz, 
           hnr05z, 
           hnr15z, 
           hnr25z, 
           hnr35z, 
           shrz, 
           norm.soe)

slz_rf_dur <- slz_clean %>%
  select(Phonation, 
         H1c_resid, 
         h2cz, 
         h4cz, 
         a1cz, 
         a2cz, 
         a3cz, 
         h1h2cz, 
         h2h4cz, 
         h1a1cz, 
         h1a2cz, 
         h1a3cz, 
         h42Kcz, 
         h2Kh5Kcz, 
         cppz, 
         energyz, 
         hnr05z, 
         hnr15z, 
         hnr25z, 
         hnr35z, 
         shrz, 
         norm.soe,
         durationz)

#factorizing the Phonation column
slz_rf$Phonation <- factor(slz_rf$Phonation, levels = c("modal", 
                                                        "breathy", 
                                                        "checked", 
                                                        "rearticulated"))

slz_rf_dur$Phonation <- factor(slz_rf_dur$Phonation, levels = c("modal",
                                                                "breathy",
                                                                "checked",
                                                                "rearticulated"))

# Splitting the data into training and test sets
set.seed(123) # needed for reproducibility

split_strat  <- initial_split(slz_rf, prop = 0.7, 
                              strata = "Phonation")
rf_train  <- training(split_strat)
rf_test   <- testing(split_strat)

set.seed(123) # needed for reproducibility
split_strat_dur  <- initial_split(slz_rf_dur, prop = 0.7, 
                                  strata = "Phonation")
rf_train_dur  <- training(split_strat_dur)
rf_test_dur   <- testing(split_strat_dur)

# number of features
n_features <- length(setdiff(names(rf_train), "Phonation"))
n_features_dur <- length(setdiff(names(rf_train_dur), "Phonation"))

# train a default rf model
rf_model_default <- ranger(Phonation ~ ., 
                   data = rf_train, 
                   num.trees = n_features * 100, 
                   mtry = floor(sqrt(n_features)), 
                   respect.unordered.factors = "order",
                   seed = 123)

default_rmse <- rf_model_default$prediction.error

# hyperparameter tuning
hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .2, .25, .333, .4, 1)), 
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE), 
  sample.fraction = c(.5, .63, .8), 
  rmse = NA )

# execute full cartesian grid search
for (i in seq_len(nrow(hyper_grid))) {
  # fit the model with i-th hyperparameter combination
  fit <- ranger(Phonation ~ ., 
                data = rf_train, 
                num.trees = n_features * 100, 
                mtry = hyper_grid$mtry[i], 
                min.node.size = hyper_grid$min.node.size[i], 
                replace = hyper_grid$replace[i], 
                sample.fraction = hyper_grid$sample.fraction[i], 
                respect.unordered.factors = "order",
                seed = 123)
  
  # export OOB RMSE
  hyper_grid$rmse[i] <- fit$prediction.error
} 

# assessing the model parameters
hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)

# plotting the OOOB RMSE
hyper_grid_plotting <- expand.grid(
  num.trees = seq(50, 2100, 50),
  mtry = floor(n_features * c(.05, .15, .2, .25, .333, .4, 1)), 
  # min.node.size = c(1, 3, 5, 10), 
  # replace = c(TRUE, FALSE), 
  # sample.fraction = c(.5, .63, .8), 
  accuracy = NA 
)

# execute full cartesian grid search for plotting
for (i in seq_len(nrow(hyper_grid_plotting))) {
  # fit the model with i-th hyperparameter combination
  fit_plotting <- ranger(
    formula = Phonation ~ ., 
    data = rf_train,
    num.trees = hyper_grid_plotting$num.trees[i], 
    mtry = hyper_grid_plotting$mtry[i], 
    min.node.size = 1,
    replace = FALSE,
    sample.fraction = 0.80, 
    respect.unordered.factors = "order",
    seed = 123)
  
  # export OOB RMSE
  hyper_grid_plotting$accuracy[i] <- fit_plotting$prediction.error
} 

hyper_grid_plotting %>%
  ggplot(aes(x = num.trees, y = accuracy, color = factor(mtry))) +
  geom_line() +
  labs(title = "Prediction error for Random Forest Hyperparameter Tuning", 
       x = "number of trees", 
       y = "% incorrect", 
       color = "mtry") +
  theme_bw()

hyper_grid_plotting %>%
  arrange(accuracy) %>%
  mutate(perc_gain = (default_rmse - accuracy) / default_rmse * 100) %>%
  head(10)

# Final model with best hyperparameters
final_rf_model <- ranger(Phonation ~ ., 
                          data = rf_train, 
                          num.trees = 2000, 
                          mtry = 3, 
                          min.node.size = 1, 
                          replace = FALSE, 
                          sample.fraction = .8, 
                          respect.unordered.factors = "order",
                          seed = 123,
                         classification = TRUE,  # Specify that it's a classification problem
                         importance = 'impurity',  # To measure feature importance
                         probability = TRUE  # To get probabilities for each class
                         )
final_rf_model

(rf_predictions <- final_rf_model$confusion.matrix)

# Make predictions on the test set
predictions <-  predict(final_rf_model, data = rf_test)

# Extract the predicted classes
predicted_classes <- apply(predictions$predictions, 1, which.max)
predicted_classes <- colnames(predictions$predictions)[predicted_classes]

# Evaluate model performance
accuracy <- mean(predicted_classes == rf_test$Phonation)
print(paste("Accuracy:", accuracy))

#Variable importance
rf_var_imp <- vip::vip(final_rf_model, num_features = 21, 
                       geom = "point",
                       bar = FALSE,
                       aesthetics = list(size = 3),
                       include_type = TRUE
)

# re-run model with impurity-based variable importance
rf_impurity <- ranger(
  formula = Phonation ~ ., 
  data = rf_train, 
  num.trees = 2000, 
  mtry = 3, 
  min.node.size = 1, 
  replace = FALSE, 
  sample.fraction = .8, 
  respect.unordered.factors = "order",
  seed = 123, 
  verbose = FALSE,
  importance = "impurity"
)

rf_impurity_plotting <- ranger(
  formula = Phonation ~ ., 
  data = rf_train, 
  num.trees = 2000, 
  mtry = 3, 
  min.node.size = 1, 
  replace = FALSE, 
  sample.fraction = .8, 
  respect.unordered.factors = "order",
  seed = 123, 
  verbose = FALSE,
  importance = "impurity"
)

# re-run model with permutation-based variable importance
rf_permutation <- ranger(
  formula = Phonation ~ ., 
  data = rf_train, 
  num.trees = 2000, 
  mtry = 3, 
  min.node.size = 1, 
  replace = FALSE, 
  sample.fraction = .8, 
  respect.unordered.factors = "order",
  seed = 123, 
  verbose = FALSE,
  importance = "permutation"
)

rf_permutation_plotting <- ranger(
  formula = Phonation ~ ., 
  data = rf_train, 
  num.trees = 2000, 
  mtry = 3, 
  min.node.size = 1, 
  replace = FALSE, 
  sample.fraction = .8, 
  respect.unordered.factors = "order",
  seed = 123, 
  verbose = FALSE,
  importance = "permutation"
)

p1 <- vip::vip(rf_impurity, num_features = 21, 
               geom = "point",
               bar = FALSE,
               aesthetics = list(size = 3),
               include_type = TRUE
)

p2 <- vip::vip(rf_permutation, num_features = 21,
               geom = "point",
               bar = FALSE,
               aesthetics = list(size = 3),
               include_type = TRUE
)

rf_output <- gridExtra::grid.arrange(p1, p2, nrow = 1)

# Extract variable importance scores for impurity-based importance
rf_impurity_scores <- vip::vi(rf_impurity_plotting)

# Extract variable importance scores for permutation-based importance
rf_permutation_scores <- vip::vi(rf_permutation_plotting)

# Create a Lollipop chart of variable importance scores
rf_impurity_plot <- ggplot(rf_impurity_scores, aes(x = reorder(Variable,
                                                         Importance), y = Importance)) +
  geom_segment(aes(xend = Variable, yend = 0)) +
  geom_point(size = 2) +
  coord_flip() +
  labs(title = "Random Forest Variable Importance Plot", 
       x = "Variable", 
       y = "Importance (Impurity)") +
  theme_bw()
rf_impurity_plot

rf_permutation_plot <- ggplot(rf_permutation_scores, aes(x = reorder(Variable,
                                                               Importance),
                                                         y = Importance)) +
  geom_segment(aes(xend = Variable, yend = 0)) +
  geom_point(size = 2) +
  coord_flip() +
  labs(title = "Random Forest Variable Importance Plot", 
       x = "Variable", 
       y = "Importance (Permutation)") +
  theme_bw()
rf_permutation_plot

rf_plot <- cowplot::plot_grid(rf_impurity_plot, rf_permutation_plot, nrow = 1)

# Save the plots as .eps files
ggsave(filename = "figs/rf_output.eps",
       plot = rf_plot,
       width = 6, height = 4, dpi = 300, units = "in")

ggsave(filename = "figs/rf_impurity_plot.eps",
       plot = rf_impurity_plot,
       width = 6, height = 4, dpi = 300, units = "in")

ggsave(filename = "figs/rf_permutation_plot.eps",
       plot = rf_permutation_plot,
       width = 6, height = 4, dpi = 300, units = "in")
