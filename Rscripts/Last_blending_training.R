setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
load('data/9_train_validation_test_20151122.RData');ls()

submit_xgb_gbm <- list.files('ReadyForBlending/submission/train/xgboost_gbm/', full.names = T)
submit_vw <- list.files('ReadyForBlending/submission/train/vw/', full.names = T)
submit_lasagne <- list.files('ReadyForBlending/submission/train/lasagne/', full.names = T)
submit_fm <- list.files('ReadyForBlending/submission/train/libffm/', full.names = T)
submit_svm <- list.files('ReadyForBlending/submission/train/libsvm/', full.names = T)
submit_h2o_lg <- list.files('ReadyForBlending/submission/train/h2o_glm/', full.names = T)
submit_h2o_rf <- list.files('ReadyForBlending/submission/train/h2o_rf/', full.names = T)
submit_h2o_nnet <- list.files('ReadyForBlending/submission/train/h2o_nnet/', full.names = T)
submit_xgb_glm <- list.files('ReadyForBlending/submission/train/xgboost_glm/', full.names = T)


combine_results <- function(files, header, sep=','){
    for(f in 1:length(files)){
        #print(f)
        if(f == 1){
            pred <- read.csv(files[f], header = header, sep = sep)
        }else{
            pred <- pred + read.csv(files[f], header = header, sep=sep)
        }
        if(f == length(files)){
            pred <- pred/f
        }
    }
    return(pred)
}

pred_xgb_gbm <- combine_results(submit_xgb_gbm, header = TRUE); head(pred_xgb_gbm); dim(pred_xgb_gbm)
pred_xgb_glm <- combine_results(submit_xgb_glm, header = TRUE); head(pred_xgb_glm); dim(pred_xgb_glm)
pred_vw <- combine_results(submit_vw, header = FALSE); head(pred_vw); dim(pred_vw)
pred_lasagne <- combine_results(submit_lasagne, header = FALSE); head(pred_lasagne); dim(pred_lasagne)
pred_fm <- combine_results(submit_fm, header = FALSE); head(pred_fm); dim(pred_fm)
pred_svm <- combine_results(submit_svm, header = TRUE, sep = ' '); head(pred_svm); dim(pred_svm)
pred_h2o_lg <- combine_results(submit_h2o_lg, header = TRUE); head(pred_h2o_lg); dim(pred_h2o_lg)
pred_h2o_rf <- combine_results(submit_h2o_rf, header = TRUE); head(pred_h2o_rf); dim(pred_h2o_rf)
pred_h2o_nnet <- combine_results(submit_h2o_nnet, header = TRUE); head(pred_h2o_nnet); dim(pred_h2o_nnet)


pred_all <- as.data.frame(cbind(pred_xgb_gbm[,2],pred_xgb_glm[,2],pred_h2o_lg[,4],pred_vw[,1],pred_lasagne[,2],pred_h2o_nnet[,4]
             ,pred_fm[,1],pred_svm[,3],pred_h2o_rf[,4],total$flag_class))
names(pred_all) <- c('xgb_gbm','xgb_glm', 'h2o_lg', 'vw', 'lasagne', 'h2o_nnet', 'fm', 'svm', 'h2o_rf', 'flag_class')
head(pred_all); dim(pred_all)
save(pred_all, file='pred_all.RData')

### Model
load('pred_all.RData')
library(xgboost);
train <- pred_all
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
feat <- colnames(train)[c(1:(ncol(train)-1))] # train
train[,feat] <- apply(train[,feat], 2, as.numeric)
dtrain <- xgb.DMatrix(as.matrix(train[,feat]), label = train$flag_class)
watchlist <- list(eval = dtrain, train = dtrain)

blend_gbm <-
    xgb.train( # max.depth = 6, eta = 0.02, nround = 1200,
        data = dtrain, max.depth = 4, eta = 0.01, nround = 500, maximize = F, min_child_weight = 1, colsample_bytree = 1, 
        nthread = 4, objective = "binary:logistic", verbose = 1, print.every.n = 10, metrics = 'auc', watchlist = watchlist
    )
pred = predict(blend_gbm,dtrain)

blend_glm <- xgb.train(
    data = dtrain, nround = 550, objective = "binary:logistic", booster = "gblinear", eta = 0.3,
    nthread = 4, alpha = 1e-3, lambda = 1e-6, print.every.n = 10,watchlist = watchlist
)
p_glm = predict(bst,dtest)

blend_rf <-
    xgb.train(
        data = dtrain, max.depth = 8, eta = 0.01, nround = 1, maximize = F, min_child_weight = 2, colsample_bytree = 1,watchlist = watchlist,
        nthread = 4, objective = "binary:logistic", verbose = 1, print.every.n = 10, metrics = 'auc', num_parallel_tree = 1000, gamma = 0.01
    )
pred = predict(bst,dtrain)

save(blend_gbm,blend_glm,blend_rf, file='blending_algorithm.RData')