# Load necessary libraries
library(caret)
library(ipred)
library(randomForest)
library(tidyverse)

# Load dataset
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

splitStrat  <- initial_split(slz_rf, prop = 0.7, 
                              strata = "Phonation")
slzTrain  <- training(split_strat)
slzTest   <- testing(split_strat)

# Define a range of tree counts
tree_counts <- seq(20, 1000, by = 20)

# Initialize data frame to store results
results <- data.frame(Trees = integer(), Model = character(), TestError = numeric(), OOBError = numeric())

# Loop over tree counts
for (n_trees in tree_counts) {
  # Train Bagging model
  bagging_model <- bagging(Phonation ~ ., data = slzTrain, nbagg = n_trees, coob = TRUE)
  bagging_pred <- predict(bagging_model, slzTest)
  bagging_test_error <- mean(bagging_pred != slzTest$Phonation)
  bagging_oob_error <- bagging_model$err[n_trees]
  
  # Train Random Forest model
  rf_model <- randomForest(Phonation ~ ., data = slzTrain, ntree = n_trees, keep.inbag = TRUE)
  rf_pred <- predict(rf_model, slzTest)
  rf_test_error <- mean(rf_pred != slzTest$Phonation)
  rf_oob_error <- rf_model$err.rate[n_trees, "OOB"]
  
  # Store results
  results <- rbind(results, data.frame(Trees = n_trees, Model = "Bagging", TestError = bagging_test_error, OOBError = bagging_oob_error))
  results <- rbind(results, data.frame(Trees = n_trees, Model = "RandomForest", TestError = rf_test_error, OOBError = rf_oob_error))
}

# Reshape data for ggplot
results_long <- results %>%
  gather(key = "ErrorType", value = "Error", TestError, OOBError)

# Plot the errors using ggplot2
ggplot(results_long, aes(x = Trees, y = Error, color = Model, linetype = ErrorType)) +
  geom_line() +
  geom_point() +
  labs(title = "Test Error and OOB Error for Bagging vs Random Forest",
       x = "Number of Trees",
       y = "Error") +
  scale_colour_manual(values = colorblind)
  theme_minimal()