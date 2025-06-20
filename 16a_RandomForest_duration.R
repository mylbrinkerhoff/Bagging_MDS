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
install.packages("rsample") # for splitting the data into training and test sets
install.packages("vip") # for variable importance plots

# Load necessary libraries
library(ranger) # for performing Random Forest CART analysis
library(here) # for creating pathways relative to the top-level directory
library(tidyverse) # for data manipulation, graphic, and data wrangling
library(rsample) # for splitting the data into training and test sets
library(vip) # for variable importance plots

# Loading the data
slz_clean <- read.csv("data/processed/slz_normalized.csv", header = TRUE)

slz_rf_dur <- slz_clean %>%
  dplyr::select(Phonation, 
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
slz_rf_dur$Phonation <- factor(slz_rf_dur$Phonation, levels = c("modal",
                                                                "breathy",
                                                                "checked",
                                                                "rearticulated"))

# Splitting the data into training and test sets
set.seed(123) # needed for reproducibility
split_strat_dur  <- rsample::initial_split(slz_rf_dur, prop = 0.7, 
                                  strata = "Phonation")
rf_train_dur  <- rsample::training(split_strat_dur)
rf_test_dur   <- rsample::testing(split_strat_dur)

# number of features
n_features_dur <- length(setdiff(names(rf_train_dur), "Phonation"))

# train a default rf model
rf_duration_default <- ranger::ranger(Phonation ~ ., 
                           data = rf_train_dur, 
                           num.trees = n_features_dur * 100, 
                           mtry = floor(sqrt(n_features)), 
                           respect.unordered.factors = "order",
                           seed = 123)

default_error <- rf_duration_default$prediction.error

# hyperparameter tuning
hyper_grid_duration <- expand.grid(
  mtry = floor(n_features_dur * c(.05, .15, .2, .25, .333, .4, 1)), 
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE), 
  sample.fraction = c(.5, .63, .8), 
  error = NA )

# execute full cartesian grid search
for (i in seq_len(nrow(hyper_grid_duration))) {
  # fit the model with i-th hyperparameter combination
  fit <- ranger(Phonation ~ ., 
                data = rf_train_dur, 
                num.trees = n_features_dur * 100, 
                mtry = hyper_grid$mtry[i], 
                min.node.size = hyper_grid$min.node.size[i], 
                replace = hyper_grid$replace[i], 
                sample.fraction = hyper_grid$sample.fraction[i], 
                respect.unordered.factors = "order",
                seed = 123)
  
  # export OOB RMSE
  hyper_grid_duration$error[i] <- fit$prediction.error
} 

# assessing the model parameters
hyper_grid_duration %>%
  dplyr::arrange(error) %>%
  dplyr::mutate(perc_gain = (default_error - error) / default_error * 100) %>%
  head(10) %>%
  xtable::xtable(digits = 3)

# plotting the OOOB RMSE
hyper_grid_plotting_duration <- expand.grid(
  num.trees = seq(50, 2200, 50),
  mtry = floor(n_features_dur * c(.05, .15, .2, .25, .333, .4, 1)), 
  # min.node.size = c(1, 3, 5, 10), 
  # replace = c(TRUE, FALSE), 
  # sample.fraction = c(.5, .63, .8), 
  accuracy = NA 
)

# execute full cartesian grid search for plotting
for (i in seq_len(nrow(hyper_grid_plotting_duration))) {
  # fit the model with i-th hyperparameter combination
  fit_plotting <- ranger::ranger(
    formula = Phonation ~ ., 
    data = rf_train_dur,
    num.trees = hyper_grid_plotting_duration$num.trees[i], 
    mtry = hyper_grid_plotting_duration$mtry[i], 
    min.node.size = 1,
    replace = TRUE,
    sample.fraction = 0.80, 
    respect.unordered.factors = "order",
    seed = 123)
  
  # export OOB RMSE
  hyper_grid_plotting_duration$accuracy[i] <- fit_plotting$prediction.error
} 

tree_num_dur <- hyper_grid_plotting_duration %>%
  ggplot(aes(x = num.trees, y = accuracy, color = factor(mtry))) +
  geom_line(linewidth = 1) +
  # geom_line(aes(linetype = factor(mtry)), linewidth = 1) +
  labs(title = "Prediction error for Random Forest Hyperparameter Tuning", 
       x = "number of trees", 
       y = "% incorrect", 
       color = "mtry") +
  scale_color_manual(values = colorblind) +
  theme_bw()
tree_num_dur

# save this plot as an .eps file
ggplot2::ggsave(filename = here::here("figs", "tree_num_dur.eps"),
       plot = tree_num_dur,
       width = 6, height = 4, dpi = 300, units = "in")

hyper_grid_plotting_duration %>%
  dplyr::arrange(accuracy) %>%
  mutate(perc_gain = (default_rmse - accuracy) / default_rmse * 100) %>%
  head(10)

# Final model with best hyperparameters
rf_duration_final <- ranger::ranger(Phonation ~ ., 
                         data = rf_train_dur, 
                         num.trees = 300, 
                         mtry = 5, 
                         min.node.size = 1, 
                         replace = TRUE, 
                         sample.fraction = .8, 
                         respect.unordered.factors = "order",
                         seed = 123,
                         classification = TRUE,  # Specify that it's a classification problem
                         importance = 'impurity',  # To measure feature importance
                         probability = TRUE  # To get probabilities for each class
)
rf_duration_final


(rf_dur_predictions <- rf_duration_final$confusion.matrix)

# Make predictions on the test set
dur_predictions <-  predict(rf_duration_final, data = rf_test_dur)

# Extract the predicted classes
dur_predicted_classes <- apply(dur_predictions$predictions, 1, which.max)
dur_predicted_classes <- colnames(dur_predictions$predictions)[dur_predicted_classes]

# Evaluate model performance
dur_accuracy <- mean(dur_predicted_classes == rf_test_dur$Phonation)
print(paste("Accuracy:", dur_accuracy))

#Variable importance
rf_dur_vip <- vip::vip(rf_duration_final, num_features = 22, 
                       geom = "point",
                       bar = FALSE,
                       aesthetics = list(size = 3),
                       include_type = TRUE
)

# re-run model with impurity-based variable importance
rf_dur_impurity <- ranger::ranger(
  formula = Phonation ~ ., 
  data = rf_train_dur, 
  num.trees = 300, 
  mtry = 5, 
  min.node.size = 1, 
  replace = TRUE, 
  sample.fraction = .8, 
  respect.unordered.factors = "order",
  seed = 123, 
  verbose = FALSE,
  importance = "impurity"
)

rf_dur_impurity_plotting <- ranger::ranger(
  formula = Phonation ~ ., 
  data = rf_train_dur, 
  num.trees = 300, 
  mtry = 5, 
  min.node.size = 1, 
  replace = TRUE, 
  sample.fraction = .8, 
  respect.unordered.factors = "order",
  seed = 123, 
  verbose = FALSE,
  importance = "impurity"
)

# re-run model with permutation-based variable importance
rf_dur_permutation <- ranger::ranger(
  formula = Phonation ~ ., 
  data = rf_train_dur, 
  num.trees = 300, 
  mtry = 5, 
  min.node.size = 1, 
  replace = TRUE, 
  sample.fraction = .8, 
  respect.unordered.factors = "order",
  seed = 123, 
  verbose = FALSE,
  importance = "permutation"
)

rf_dur_permutation_plotting <- ranger::ranger(
  formula = Phonation ~ ., 
  data = rf_train_dur, 
  num.trees = 300, 
  mtry = 5, 
  min.node.size = 1, 
  replace = TRUE, 
  sample.fraction = .8, 
  respect.unordered.factors = "order",
  seed = 123, 
  verbose = FALSE,
  importance = "permutation"
)

p1 <- vip::vip(rf_dur_impurity, num_features = 21, 
               geom = "point",
               bar = FALSE,
               aesthetics = list(size = 3),
               include_type = TRUE
)

p2 <- vip::vip(rf_dur_permutation, num_features = 21,
               geom = "point",
               bar = FALSE,
               aesthetics = list(size = 3),
               include_type = TRUE
)

rf_dur_output <- gridExtra::grid.arrange(p1, p2, nrow = 1)

# Extract variable importance scores for impurity-based importance
rf_dur_impurity_scores <- vip::vi(rf_dur_impurity_plotting)

# Extract variable importance scores for permutation-based importance
rf_dur_permutation_scores <- vip::vi(rf_dur_permutation_plotting)

# Create a Lollipop chart of variable importance scores
rf_dur_impurity_plot <- ggplot2::ggplot(rf_dur_impurity_scores, aes(x = reorder(Variable,
                                                               Importance), y = Importance)) +
  geom_segment(aes(xend = Variable, yend = 0)) +
  geom_point(size = 2) +
  coord_flip() +
  labs(title = "Impurity Importance", 
       x = "Variable", 
       y = "Importance (Impurity)") +
  theme_bw()
rf_dur_impurity_plot

rf_dur_permutation_plot <- ggplot2::ggplot(rf_dur_permutation_scores, aes(x = reorder(Variable,
                                                                     Importance),
                                                         y = Importance)) +
  geom_segment(aes(xend = Variable, yend = 0)) +
  geom_point(size = 2) +
  coord_flip() +
  labs(title = "Permutation Importance", 
       x = "Variable", 
       y = "Importance (Permutation)") +
  theme_bw()
rf_dur_permutation_plot

rf_dur_plot <- cowplot::plot_grid(rf_dur_impurity_plot, rf_dur_permutation_plot, nrow = 1)

# rf_dur_impurity_plot + rf_dur_permutation_plot

# Save the plots as .eps files
ggplot2::ggsave(filename = here::here("figs", "rf_dur_plots.eps"),
       plot = rf_dur_plot,
       width = 6, height = 4, dpi = 300, units = "in")

ggplot2::ggsave(filename = here::here("figs", "rf_dur_impurity_plot.eps"),
       plot = rf_dur_impurity_plot,
       width = 6, height = 4, dpi = 300, units = "in")

ggplot2::ggsave(filename = here::here("figs", "rf_dur_permutation_plot.eps"),
       plot = rf_dur_permutation_plot,
       width = 6, height = 4, dpi = 300, units = "in")
