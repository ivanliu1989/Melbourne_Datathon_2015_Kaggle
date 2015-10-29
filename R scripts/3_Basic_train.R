setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(caret);library(pROC)
load('data/train_validation_test.RData');ls()


######################
### Training Model ###
######################
# Config
train$flag_class <- as.factor(train$flag_class)

fitControl <- trainControl(method = "none",
                           number = 2,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
Grid <-  expand.grid(mtry=8)

# Training
set.seed(825)
fit <- train(flag_class ~ ., data=train[,-c(1,2,53)], # classification
             method = "rf",
             trControl = fitControl,
             tuneGrid = Grid,
             # preProcess = c('center', 'scale'),
             metric ='ROC',
             verbose = T)


# Config 2
fitControl2 <- trainControl(method = "adaptive_cv",
                            number = 5,
                            repeats = 2,
                            ## Estimate class probabilities
                            classProbs = TRUE,
                            ## Evaluate performance using 
                            ## the following function
                            summaryFunction = twoClassSummary,
                            ## Adaptive resampling information:
                            adaptive = list(min = 10,
                                            alpha = 0.05,
                                            method = "BT", #gls for Linear
                                            complete = TRUE))

set.seed(825)
svmFit2 <- train(flag_class ~ ., data=total[,-c(1,2,49)],
                 method = "rf", #svmRadial
                 trControl = fitControl2,
                 # preProc = c("center", "scale"),
                 tuneLength = 8,
                 metric = "ROC")

# Variable Imp
fitImp <- varImp(fit, scale = T)
fitImp[1]


##################
### Validation ###
##################
### Predict
p <- predict(fit, newdata=validation, type = 'prob')
validation$INVEST <- validation$TRANSACTION_COUNT_INPLAY * validation$AVG_BET_SIZE_INPLAY + validation$TRANSACTION_COUNT_OUTPLAY * validation$AVG_BET_SIZE_OUTPLAY
validation$Y <- p$Y
validation$PRED_PROFIT_LOSS <- (validation$Y - 0.5) * validation$INVEST * 2
pred_fin <- aggregate(PRED_PROFIT_LOSS ~ ACCOUNT_ID, data=validation, sum, na.rm=F)
pred_fin$PRED_PROFIT_LOSS_2 <- ifelse(pred_fin$PRED_PROFIT_LOSS > 0, 1, ifelse(pred_fin$PRED_PROFIT_LOSS < 0, 0, 0.5))

### Validation
# validation$flag_class <- as.factor(validation$flag_class)
val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=validation, sum, na.rm=F)
val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, ifelse(val_fin$flag_regr < 0, 0, 0.5))


#########################
### Model Performance ###
#########################
v <- merge(val_fin,pred_fin,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')

# With a roc object:
rocobj <- roc(v$flag_regr, v$PRED_PROFIT_LOSS)
rocobj <- roc(validation$flag_class, p$Y)
# Full AUC:
auc(rocobj) # 0.8434 | 0.7256
# Partial AUC:
auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE) # 0.7389 | 0.5946
# Plot
plot(rocobj)


############
### test ###
############
p <- predict(fit, newdata=test, type = 'prob')
test$INVEST <- test$TRANSACTION_COUNT_INPLAY * test$AVG_BET_SIZE_INPLAY + test$TRANSACTION_COUNT_OUTPLAY * test$AVG_BET_SIZE_OUTPLAY
test$Y <- p$Y
test$PRED_PROFIT_LOSS <- (test$Y - 0.5) * test$INVEST * 2

pred_fin <- aggregate(PRED_PROFIT_LOSS ~ ACCOUNT_ID, data=test, sum, na.rm=T)

### Submission
submit <- read.csv('data/sample_submission_bet_size.csv', stringsAsFactors=FALSE,na.strings = "")
names(pred_fin) <- c('Account_ID', 'PRED_PROFIT_LOSS')
submit <- merge(submit,pred_fin,all.x = TRUE,all.y = FALSE)
table(is.na(submit$PRED_PROFIT_LOSS))
submit$PRED_PROFIT_LOSS[is.na(submit$PRED_PROFIT_LOSS)] <- 0
submit$Prediction <- submit$PRED_PROFIT_LOSS
submit$PRED_PROFIT_LOSS <- NULL

write.csv(submit,'pred/submission_20151028_1.csv',quote = FALSE,row.names = FALSE)
save(fit, file='../rf.RData')
