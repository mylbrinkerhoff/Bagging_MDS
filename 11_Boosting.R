#-------------------------------------------------------------------------------
#
# 10_Boosting.R
#
# Generating a bossted decission tree
#
# M. Brinkerhoff  * UCSC  * 2024-08-16 (F)
#
#-------------------------------------------------------------------------------

# Load necessary libraries
install.packages("caret")
install.packages("gbm")
library(caret)
library(gbm)

# Define the training control with a custom summary function to ensure RMSE is included
train_control <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = defaultSummary
)

# Define the tuning grid with a wider range of n.trees
tune_grid <- expand.grid(
  n.trees = seq(50, 1000, by = 50),
  interaction.depth = c(1, 3, 5),
  shrinkage = c(0.01, 0.1),
  n.minobsinnode = 10
)

# Train the model using cross-validation
set.seed(123)
slz_boost <- train(
  Phonation ~ H1c.resid + h1h2c + h2h4c + h1a1c + h1a2c + h1a3c +
    h42Kc + h2Kh5Kc + cpp + energy + hnr05 + hnr15 + hnr25 + hnr35 + shr +
    f0 + f1 + f2 + b1 + b2 + epoch + soe,
  data = slz_train,
  method = "gbm",
  trControl = train_control,
  tuneGrid = tune_grid,
  verbose = FALSE
)

slz_boost
# Extract the results
results <- slz_boost$results
print(results)

# Plot the RMSE against the number of trees
ggplot(results, aes(x = n.trees, y = RMSE, color = as.factor(interaction.depth))) +
  geom_line() +
  geom_point() +
  labs(title = "Model Performance vs. Number of Trees",
       x = "Number of Trees",
       y = "RMSE",
       color = "Interaction Depth") +
  theme_minimal()

# Get variable importance
var_importance_boost <- varImp(slz_boost, scale = FALSE)

# Print variable importance
print(var_importance_boost)

# Plot variable importance
plot(var_importance_boost, main = "Variable Importance Plot for Boosted Model")

# Assuming var_importance_boost is already created as shown in the provided code
# Convert variable importance to a data frame
var_importance_df <- as.data.frame(var_importance_boost$importance)
var_importance_df$Variable <- rownames(var_importance_df)
rownames(var_importance_df) <- NULL

# Create the lollipop chart using ggplot2
ggplot(var_importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_segment(aes(xend = Variable, yend = 0), color = "skyblue") +
  geom_point(color = "blue", size = 4) +
  coord_flip() +
  labs(title = "Variable Importance Plot for Boosted Model",
       x = "Variables",
       y = "Importance") +
  theme_bw()
