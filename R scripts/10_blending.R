setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(pROC)
load('data/9_train_validation_test_TREE_2.RData');ls()
# load('data/9_train_validation_test_ONEHOT_1.RData');ls()

file.names <- list.files('ReadyForBlending/validation/')
########################
### Average Blending ###
########################
# for(file in 1:(length(file.names)-1)){
#     p <- read.csv(paste0('ReadyForBlending/validation/', file.names[file]))
#     if(file==1){
#         pred <- p
#     }else{
#         pred <- p + pred
#     }
#     if(file==(length(file.names)-1)) pred <- pred/file
# }
# head(pred)

#######################
### Linear Blending ###
#######################
X.train.combine <- ifelse(validation$flag_class == 'N', 0, 1)

for(file in 1:(length(file.names)-1)){
    p <- read.csv(paste0('ReadyForBlending/validation/', file.names[file]))
    X.train.combine <- cbind(X.train.combine,p[,3])
}
X.train.combine <- as.data.frame(X.train.combine)
combine.model <- lm((X.train.combine==1) ~ ., data=X.train.combine)

combine.y.train.predict <- predict(combine.model, X.train.combine) # ~ 0.05 increase


##################
### Prediction ###
##################
val <- validation
# X1 <- pred[,3]
X1 <- combine.y.train.predict
val <- cbind(val, X1)
val$PRED_PROFIT_LOSS <- (val[,ncol(val)] - 0.5) * val$INVEST * 2
pred_fin <- aggregate(PRED_PROFIT_LOSS ~ ACCOUNT_ID, data=val, sum, na.rm=T)
pred_fin$PRED_PROFIT_LOSS_2 <- ifelse(pred_fin$PRED_PROFIT_LOSS > 0, 1, ifelse(pred_fin$PRED_PROFIT_LOSS < 0, 0, 0.5))
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
rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$PRED_PROFIT_LOSS_2)
rocobj <- roc(v$PRED_PROFIT_LOSS_3, v[,6])
perf_new <- auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE)
rocobj;perf_new



