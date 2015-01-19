# follow steps in http://topepo.github.io/caret/training.html

library(mlbench)
data(Sonar)
str(Sonar[, 1:10])
help(Sonar)
head(Sonar)
summary(Sonar$Class)

library(caret)
set.seed(998)
#use simple bootstrap resampling to split data into a series of train and test set
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE) 
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

# custom function to specifiy the type of resampling
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# fit a boosted tree model via the gbm package (that also requires package e1071)
set.seed(825)
gbmFit1 <- train(Class ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1
