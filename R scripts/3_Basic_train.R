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

### Predict
p <- predict(fit, newdata=validation, type = 'prob')

INVEST <- validation$TRANSACTION_COUNT_INPLAY * validation$AVG_BET_SIZE_INPLAY + validation$TRANSACTION_COUNT_OUTPLAY * validation$AVG_BET_SIZE_OUTPLAY
Y <- p$Y
pred <- cbind(validation[,c(1,2)], INVEST, Y, validation[,c(49,50)])
pred$PROFIT_LOSS <- (pred$Y - 0.5) * pred$INVEST

pred_fin <- aggregate(PROFIT_LOSS ~ ACCOUNT_ID + EVENT_ID, data=pred, sum, na.rm=T)
pred_fin$Y <- ifelse(pred_fin$PROFIT_LOSS>=0, 'Y', 'N')

### Validation
validation$flag_class <- as.factor(validation$flag_class)
val_fin <- aggregate(flag_regr ~ ACCOUNT_ID + EVENT_ID, data=validation, sum, na.rm=T)

# With a roc object:
rocobj <- roc(val_fin$flag_regr, pred_fin$PROFIT_LOSS)
# Full AUC:
auc(rocobj) # 0.8434
# Partial AUC:
auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE) # 0.7389
# Plot
plot(rocobj)

save(fit, file='model/randomForest_test.RData')

### test
p <- predict(fit, newdata=test_dt, type = 'prob')
test_dt$INVEST <- test_dt$TRANSACTION_COUNT_INPLAY * test_dt$AVG_BET_SIZE_INPLAY + test_dt$TRANSACTION_COUNT_OUTPLAY * test_dt$AVG_BET_SIZE_OUTPLAY
test_dt$Y <- p$Y
test_dt$PRED_PROFIT_LOSS <- (test_dt$Y - 0.5) * test_dt$INVEST
pred_fin <- aggregate(PRED_PROFIT_LOSS ~ ACCOUNT_ID, data=test_dt, sum, na.rm=T)

### Submission
submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
names(pred_fin) <- c('Account_ID', 'PRED_PROFIT_LOSS')
submit <- merge(submit,pred_fin,all.x = TRUE,all.y = FALSE)
submit$PRED_PROFIT_LOSS[is.na(submit$PRED_PROFIT_LOSS)] <- 0
submit$Prediction <- submit$PRED_PROFIT_LOSS
submit$PRED_PROFIT_LOSS <- NULL

write.csv(submit,'pred/submission_20151028_1.csv',quote = FALSE,row.names = FALSE)


# Imputation
load('data/mbr_event_data.RData')
test_dt[is.na(test_dt$AVG_TAKEN_HOUR_INPLAY),'AVG_TAKEN_HOUR_INPLAY'] <- median(mbr.event$AVG_TAKEN_HOUR_INPLAY, na.rm=T)
test_dt[is.na(test_dt$AVG_TAKEN_HOUR_OUTPLAY),'AVG_TAKEN_HOUR_OUTPLAY'] <- median(mbr.event$AVG_TAKEN_HOUR_OUTPLAY, na.rm=T)
