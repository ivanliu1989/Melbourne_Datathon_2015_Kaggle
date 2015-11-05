setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
# setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(pROC)
load('data/9_train_validation_test_20151105.RData');ls()
# path <- 'ReadyForBlending/validation/test/'
path <- 'ReadyForBlending/validation/deeplearning/'
file.names <- list.files(path)
########################
### Average Blending ###
########################
# p1 <- read.csv(paste0(path, file.names[1]))[,3]
# p2 <- read.csv(paste0(path, file.names[2]))[,3]
# p3 <- read.csv(paste0(path, file.names[3]))[,3]
# p4 <- read.csv(paste0(path, file.names[4]))[,3]
# p5 <- read.csv(paste0(path, file.names[5]))
# X1 <- (p1+p2+p3+p4+p5)/5; names(X1) <- 'X1'

for(file in 1:length(file.names)){
    p <- read.csv(paste0(path, file.names[file]))
    if(file==1){
        pred <- p
    }else{
        pred[,c(2,3)] <- p[,c(2,3)] + pred[,c(2,3)]
    }
    if(file==length(file.names)){ pred[,c(2,3)] <- pred[,c(2,3)]/file}
}
X1 <- pred[,3]

#####################
### Bias Blending ###
#####################
# p1 <- read.csv(paste0(path, file.names[1]))[,3]
# p2 <- read.csv(paste0(path, file.names[2]))[,3]
# p3 <- read.csv(paste0(path, file.names[3]))[,3]
# p4 <- read.csv(paste0(path, file.names[4]))[,3]
# p5 <- read.csv(paste0(path, file.names[5]))
# X1 <- 5/(1/p1 + 1/p2 + 1/p3 + 1/p4 + 1/p5); names(X1) <- 'X1'

#######################
### Linear Blending ###
#######################
# X.train.combine <- ifelse(train$flag_class == 'N', 0, 1)
# X.train.combine <- ifelse(validation$flag_class == 'N', 0, 1)
# X.train.combine <- cbind(X.train.combine,p1,p2,p3,p4,p5)
# for(file in 1:length(file.names)){
#     p <- read.csv(paste0(path, file.names[file]))
#     X.train.combine <- cbind(X.train.combine,p[,3])
# }
# X.train.combine <- as.data.frame(X.train.combine)
# combine.model <- lm((X.train.combine==1) ~ ., data=X.train.combine)
# combine.y.train.predict <- predict(combine.model, X.train.combine) # ~ 0.05 increase
# p[,3] <- combine.y.train.predict
# write.csv(as.data.frame(p),file=paste0('ReadyForBlending/validation/1_randomforest_0.8987_0.8747.csv'),quote = FALSE,row.names = FALSE)

##################
### Prediction ###
##################
val <- validation
# X1 <- combine.y.train.predict
val <- cbind(val, X1)
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
rocobj1 <- roc(v$PRED_PROFIT_LOSS_3, v$INVEST_PERCENT);rocobj1
perf_new1 <- auc(rocobj1, partial.auc = c(1, .8), partial.auc.focus = "se", partial.auc.correct = TRUE)
perf_new1
rocobj2 <- roc(v$PRED_PROFIT_LOSS_3, v$X1);rocobj2
perf_new2 <- auc(rocobj2, partial.auc = c(1, .8), partial.auc.focus = "se", partial.auc.correct = TRUE)
perf_new2

write.csv(as.data.frame(pred),paste0('ReadyForBlending/validation/deeplearning/2_dl_score', as.numeric(perf_new1),'.csv'),quote = FALSE,row.names = FALSE)
