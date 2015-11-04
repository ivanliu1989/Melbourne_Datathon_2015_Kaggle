setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
# setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(pROC)
load('data/9_train_validation_test_TREE_1.RData');ls()
# load('data/9_train_validation_test_ONEHOT_1.RData');ls()

# path <- 'ReadyForBlending/validation/model/'
path <- 'ReadyForBlending/submission/randomforest/'
file.names <- list.files(path)
########################
### Average Blending ###
########################
p1 <- read.csv(paste0(path, file.names[1]))[,3]
p2 <- read.csv(paste0(path, file.names[2]))[,3]
p3 <- read.csv(paste0(path, file.names[3]))[,3]
p4 <- read.csv(paste0(path, file.names[4]))[,3]
p5 <- read.csv(paste0(path, file.names[5]))
X1 <- (p1+p2+p3+p4+p5)/5; names(X1) <- 'X1'


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
plot(rocobj)
