setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list = ls()); gc()
library(xgboost);library(pROC);library(caret)
load('data/9_train_validation_test_20151106.RData');ls()

events <- unique(total$EVENT_ID)
for (i in 1:(length(events) - 2)) {
    print(paste0('Round ', i, ' | Subsetting without event: ', events[i:(i + 2)]))
    train <- total
    training <- train[!train$EVENT_ID %in% events[i:(i + 2)],]
    training$flag_class <- ifelse(training$flag_class == 'Y', 1, 0)
    feat <- colnames(training)[c(3:57)]
    
    #-------------Basic Training using XGBoost-----------------
    bst <-
        xgboost(
            data = as.matrix(training[,feat]), label = training$flag_class, max.depth = 6, eta = 0.02, nround = 1200, maximize = F, #500,0.15
            nthread = 4, objective = "binary:logistic", metrics = 'auc', verbose = 0
        )
    
    #-------------Test prediction-----------------
    p <- predict(bst, as.matrix(test[,feat]))
    
    if (i == 1) {
        pred <- p
    }else{
        pred <- pred + p
    }
}

submit <- pred / length(events)

write.csv(
    as.data.frame(p),'pred/submission_20151105_xg_blend.csv',quote = FALSE,row.names = FALSE
)
