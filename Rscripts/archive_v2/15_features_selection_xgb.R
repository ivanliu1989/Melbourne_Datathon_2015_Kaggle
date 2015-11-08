setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);library(caret)
load('data/9_train_validation_test_kmean.RData');ls()

################
### Register ###
################
# train <- total
set.seed(18)
dim(train); dim(validation)
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
independent <- colnames(train)[c(3:56,58:59)]

##################
### Importance ###
##################
feat_imp <- independent

#################
### eliminate ###
#################
for(f in 50:1){ #length(feat_imp)
    print(f)
    print(paste0('Eliminating feature: ', feat_imp[f]))
    feat <- independent[which(!independent %in% feat_imp[f])]
    
    bst <-
        xgboost(  
            data = as.matrix(train[,feat]), label = train$flag_class, max.depth = 6, eta = 0.15, nround = 500, maximize = F,
            nthread = 4, objective = "binary:logistic", metrics = 'auc', verbose = 0)
    
    ##################
    ### Prediction ###
    ##################
    val <- validation
    p <- predict(bst, as.matrix(val[,feat])) 
    val$Y <- p
    tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=val, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
    val <- merge(val, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    val$INVEST_PERCENT <- val$INVEST/val$TOT_INVEST * (val$Y - 0.5) * 2
    pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=val, mean, na.rm=F)
    pred_fin2 <- aggregate(Y ~ ACCOUNT_ID, data=val, mean, na.rm=F)
    ### Validation
    val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
    val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, ifelse(val_fin$flag_regr < 0, 0, 0.5))
    
    #########################
    ### Model Performance ###
    #########################
    v <- merge(val_fin,pred_fin,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
    v <- merge(v,pred_fin2,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
    # With a roc object:
    rocobj1 <- roc(v$PRED_PROFIT_LOSS_3, v$INVEST_PERCENT);
    perf_new1 <- auc(rocobj1, partial.auc = c(1, .8), partial.auc.focus = "se", partial.auc.correct = TRUE)
    print(rocobj1);print(perf_new1)
    
    rocobj2 <- roc(v$PRED_PROFIT_LOSS_3, v$Y);
    perf_new2 <- auc(rocobj2, partial.auc = c(1, .8), partial.auc.focus = "se", partial.auc.correct = TRUE)
    print(rocobj2);print(perf_new2)
    
    if(f == 50) { #length(feat_imp)
        sig <- c(feat_imp[f], as.numeric(rocobj1$auc), as.numeric(perf_new1),as.numeric(rocobj2$auc), as.numeric(perf_new2))
    }else{
        sig2 <-  c(feat_imp[f], as.numeric(rocobj1$auc), as.numeric(perf_new1),as.numeric(rocobj2$auc), as.numeric(perf_new2))
        sig <- rbind(sig, sig2)
    }
}