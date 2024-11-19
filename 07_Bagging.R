#-------------------------------------------------------------------------------
#
# 06_Bagging.R
#
# Generating a Bagging decission tree
#
# M. Brinkerhoff  * UCSC  * 2024-08-16 (F)
#
#-------------------------------------------------------------------------------

# save the formula as a variable
formula_zscore <- Phonation ~ H1c_resid + h2cz + h4cz + a1cz + a2cz + a3cz + 
  h1h2cz + h2h4cz + h1a1cz + h1a2cz + 
  h1a3cz + h42Kcz + h2Kh5Kcz + cppz + energyz + hnr05z + hnr15z + hnr25z + 
  hnr35z + shrz + norm.soe

# make bootstrapping reproducible
set.seed(123)

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

