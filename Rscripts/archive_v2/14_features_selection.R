setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(h2o);library(pROC);library(doMC)
# load('data/9_train_validation_test_normal.RData');ls()
# load('data/9_train_validation_test_log.RData');ls()
load('data/9_train_validation_test_kmean.RData');ls()

################
### Register ###
################
set.seed(8)
registerDoMC(cores = 4)
localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '12g')

train$flag_class <- as.factor(train$flag_class); levels(train$flag_class) <- c(0,1) 
val_class <- validation$flag_class
validation$flag_class <- as.factor(validation$flag_class); levels(validation$flag_class) <- c(0,1) 
total$flag_class <- as.factor(total$flag_class); levels(total$flag_class) <- c(0,1) 

######################
### Feature Select ###
######################
train_df <- as.h2o(localH2O, train)
validation_df <- as.h2o(localH2O, validation)

independent <- c(colnames(train_df[,3:(ncol(train_df)-2)]))
dependent <- "flag_class"

##################
### Importance ###
##################
# fit <- h2o.gbm(
#     y = dependent, x = independent, data = train_df, #train_df | total_df
#     n.trees = 200, interaction.depth = 8, n.minobsinnode = 1,
#     shrinkage = 0.25, distribution = "bernoulli", n.bins = 20,  #AUTO
#     importance = T
# )

# feat_imp <- rownames(fit@model$varimp)
feat_imp <- independent

#################
### eliminate ###
#################
for(f in length(feat_imp):1){
    print(f)
    print(paste0('Eliminating feature: ', feat_imp[f]))
    independent2 <- independent[which(!independent %in% feat_imp[f])]
    
    #     fit <- h2o.gbm(
    #         y = dependent, x = independent2, data = train_df, #train_df | total_df
    #         n.trees = 200, interaction.depth = 8, n.minobsinnode = 1,
    #         shrinkage = 0.25, distribution = "bernoulli", n.bins = 20,  #AUTO
    #         importance = T
    #     )
    
    fit <-
        h2o.glm(
            y = dependent, x = independent2, data = train_df, #train_df | total_df
            family = 'binomial', link = 'logit',alpha = 0.5, # 1 lasso 0 ridge
            lambda = 1e-08, lambda_search = T, nlambda = 12, lambda.min.ratio = 0.1,
            strong_rules = T, standardize = T, intercept = F, use_all_factor_levels = F,
            epsilon = 1e-4, iter.max = 100, higher_accuracy = T, disable_line_search = F
        )
    
    ##################
    ### Prediction ###
    ##################
    val <- validation
    pred <- h2o.predict(object = fit, newdata = validation_df)
    # pred <- h2o.predict(object = fit, newdata = train_df)
    val <- cbind(val, as.data.frame(pred[,3]))
    tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=val, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
    val <- merge(val, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
    val$INVEST_PERCENT <- val$INVEST/val$TOT_INVEST * (val$X1 - 0.5) * 2
    pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=val, mean, na.rm=F)
    pred_fin2 <- aggregate(X1 ~ ACCOUNT_ID, data=val, mean, na.rm=F)
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
    
    rocobj2 <- roc(v$PRED_PROFIT_LOSS_3, v$X1);
    perf_new2 <- auc(rocobj2, partial.auc = c(1, .8), partial.auc.focus = "se", partial.auc.correct = TRUE)
    print(rocobj2);print(perf_new2)
    
    if(f == length(feat_imp)) {
        sig <- c(feat_imp[f], as.numeric(perf_new1))
    }else{
        sig2 <-  c(feat_imp[f], as.numeric(perf_new1))
        sig <- rbind(sig, sig2)
    }
}