#-------------------------------------------------------------------------------
#
# 06_Bagging.R
#
# Generating a Bagging decission tree
#
# M. Brinkerhoff  * UCSC  * 2024-08-16 (F)
#
#-------------------------------------------------------------------------------

slz_plot <- read.csv("data/interim/slz_plot_standardized.csv", header = TRUE)

### convert certain columns into factors.
slz_plot$Phonation <- factor(slz_plot$Phonation, levels = c("modal", 
                                                              "breathy", 
                                                              "checked", 
                                                              "rearticulated"))

# stratified sampling so the training and test sets have similar distributions
table(slz_plot$Phonation) %>% prop.table() # initial distributions of VQ

# stratified sampling with the rsample package
set.seed(123) # needed for reproducibility

split_strat  <- initial_split(slz_plot, prop = 0.7, 
                              strata = "Phonation")
slz_train  <- training(split_strat)
slz_test   <- testing(split_strat)

# consistent response ratio between train & test
table(slz_train$Phonation) %>% prop.table()
table(slz_test$Phonation) %>% prop.table()

# save the training and test sets
write.csv(slz_train, "data/processed/slz_train.csv", row.names = FALSE)
write.csv(slz_test, "data/processed/slz_test.csv", row.names = FALSE)

# save the formula as a variable
formula_zscore <- Phonation ~ H1c_resid + h2cz + h4cz + a1cz + a2cz + a3cz + 
  h1h2cz + h2h4cz + h1a1cz + h1a2cz + 
  h1a3cz + h42Kcz + h2Kh5Kcz + cppz + energyz + hnr05z + hnr15z + hnr25z + 
  hnr35z + shrz + norm.soe

# make bootstrapping reproducible
set.seed(123)

# Determining the number of trees
# assess 10-1000 bagged trees
ntree <- seq(20, 1000, by = 20)

# create empty vector to store OOB RMSE values
rmse <- vector(mode = "numeric", length = length(ntree))

for (i in seq_along(ntree)) {
  # reproducibility
  set.seed(123)
  # perform bagged model
  model <- bagging(
    formula = formula_zscore,
    data    = slz_train,
    coob    = TRUE,
    control = rpart.control(minsplit = 2, cp = 0),
    nbagg   = ntree[i]
  )
  # get OOB error
  oob_error <- model$err
  rmse[i] <- sqrt(mean(oob_error^2))
  # rmse[i] <- model$err
}

# Store the results in a dataframe
bagging_errors <- data.frame(ntree, rmse)

# Visualize the OOB RMSE values using ggplot2
bagging_numbers <- ggplot(bagging_errors, aes(x = ntree, y = rmse)) +
                          geom_line() +
                          labs(title = "OOB RMSE vs Number of Trees in Bagging Model",
                          x = "Number of Trees",
                          y = "OOB RMSE") +
                          theme_bw()

# train bagged model
slz_bag1 <- bagging(
  formula = formula_zscore,
  data = slz_train,
  nbagg = 400, 
  coob = T, 
  control = rpart.control(minsplit = 2, cp = 0)
)

slz_bag1

# Applying with caret to use 10-fold CV to see how well it performs
slz_bag2 <- train(formula_zscore,
  data = slz_train,
  method = "treebag",
  trControl = trainControl(method = "cv", number = 10),
  nbagg = 400,  
  control = rpart.control(minsplit = 2, cp = 0)
)

slz_bag2

prediction <- predict(slz_bag2, slz_test)
confusionMatrix(prediction, slz_test$Phonation)
