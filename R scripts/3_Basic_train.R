setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(caret);library(pROC)
load('data/train_validation.RData')
load('data/test.RData');ls()

### Training Model
# Config
train$flag_class <- as.factor(train$flag_class)

fitControl <- trainControl(method = "none",
                           number = 2,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
# Grid <-  expand.grid(n.trees = 180, interaction.depth = 6, shrinkage = 0.02)
Grid <-  expand.grid(mtry=8)

# Training
set.seed(825)
fit <- train(flag_class ~ ., data=train[,-c(1,2,49)], # classification
             method = "rf",
             trControl = fitControl,
             tuneGrid = Grid,
             # preProcess = c('center', 'scale'),
             metric ='ROC',
             verbose = T)

# Variable Imp
fitImp <- varImp(fit, scale = T)
fitImp[1]

# Predict
p <- predict(fit, newdata=validation, type = 'prob')
p <- predict(fit, newdata=test_dt, type = 'prob')


# Validation
validation$flag_class <- as.factor(validation$flag_class)

# With a roc object:
rocobj <- roc(validation$flag_class, p$Y)
# Full AUC:
auc(rocobj)
# Partial AUC:
auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE)

save(fit, file='model/randomForest_test.RData')


