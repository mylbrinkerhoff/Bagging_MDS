#-------------------------------------------------------------------------------
#
# 07a_bagging_number_low.R
#
# Generating a Bagging decission tree
#
# M. Brinkerhoff  * UCSC  * 2024-08-16 (F)
#
#-------------------------------------------------------------------------------

# save the formula as a variable
formula_zscore <- Phonation ~ H1c_resid + h2cz + h4cz + a1cz + a2cz + 
  a3cz + h1h2cz + h2h4cz + h1a1cz + h1a2cz + 
  h1a3cz + h42Kcz + h2Kh5Kcz + cppz + energyz + hnr05z + hnr15z + hnr25z + 
  hnr35z + shrz + norm.soe

# Determining the number of trees
# assess 10-1000 bagged trees
ntree <- seq(50, 3000, by = 50)

# create empty vector to store OOB RMSE values
rmse <- vector(mode = "numeric", length = length(ntree))

for (i in seq_along(ntree)) {
  # reproducibility
  set.seed(123)
  # perform bagged model
  model <- ipred::bagging(
    formula = formula_zscore,
    data    = slz_train_low,
    coob    = TRUE,
    control = rpart.control(minsplit = 2, cp = 0),
    nbagg   = ntree[i]
  )
  # get OOB error
  # oob_error <- model$err
  # rmse[i] <- sqrt(mean(oob_error^2))
  rmse[i] <- model$err
}

# Store the results in a dataframe
bagging_errors_low <- data.frame(ntree, rmse)

# Arrange the dataframe by rmse
bagging_errors_low %>%
  arrange(rmse)

# Visualize the OOB RMSE values using ggplot2
bagging_numbers_low <- ggplot(bagging_errors_low, aes(x = ntree, y = rmse)) +
                          geom_line() +
                          labs(title = "OOB RMSE vs Number of Trees in Bagging Model",
                          x = "Number of Trees",
                          y = "OOB RMSE") +
                          theme_bw()
bagging_numbers_low

# ggplot(bagging_errors, aes(ntree, rmse)) +
#   geom_line() +
#   # geom_hline(yintercept = 0.45, lty = "dashed", color = "grey50") +
#   # annotate("text", x = 200, y = 1, label = "Best individual pruned tree", vjust = 0, hjust = 0, color = "grey50") +
#   # annotate("text", x = 200, y = 1, label = "Bagged trees", vjust = 0, hjust = 0) +
#   ylab("RMSE") +
#   xlab("Number of trees")
  

ggsave(filename = "figs/bagging_numbers_low.eps", 
        plot = bagging_numbers, 
        width = 6, height = 4, units = "in", dpi = 300)
