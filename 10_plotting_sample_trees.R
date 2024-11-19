#-------------------------------------------------------------------------------
#
# 09_plotting_sample_trees.R
#
# Plotting some of the trees that were produced by 
#
# M. Brinkerhoff  * UCSC  * 2024-08-16 (F)
#
#-------------------------------------------------------------------------------

library(caret)
library(randomForest)
iter = 20
par(mfrow = c(1,1))
for(i in 1:iter){
  set.seed(i+30)
  # create train/test sets
  train_index <- caret::createDataPartition(slz.clean$Phonation, p = .6333,
                                            list = FALSE,
                                            times = 1)
  
  train_DF <- slz.clean[train_index,]
  validate_DF <- slz.clean[-train_index,]
  
  train_y <- train_DF$Phonation
  train_x <- train_DF[, setdiff(names(train_DF), "Phonation")]
  
  validate_y <- validate_DF$Phonation
  validate_x <- validate_DF[, setdiff(names(validate_DF), "Phonation")]
  
  d_tree <- rpart::rpart(Phonation ~ H1c.resid + h1h2c + h2h4c + h1a1c + h1a2c + h1a3c +
                           h42Kc + h2Kh5Kc + cpp + energy + hnr05 + hnr15 + hnr25 + hnr35 + shr +
                           f0 + f1 + f2 + b1 + b2 + epoch + soe,method="class", data=slz.clean,control=rpart.control(minsplit=20, cp=0.01,maxdepth=3))
  
  # graphs
  
  rpart.plot::rpart.plot(d_tree, main = paste0("Decision Tree ", i), type = 0, extra = 0) 
  
}
