setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc(); 
require(hydroGOF); require(parcor); require(prospectr)
library(caret);library(pROC);library(doMC)
registerDoMC(cores = 4)
load('data/6_train_validation_test_center_scale_no_dummy.RData');ls()

### Prepare data
train$flag_class <- as.factor(train$flag_class); levels(train$flag_class) <- c(0,1) 
y <- as.numeric(train$flag_class)-1
x <- train[,-c(1,2,47,48,49)]
x <- as.matrix(x)
y <- as.matrix(y)

### Start modeling
library('e1071')
fit_svmL <- svm(x,y,scale=T,kernel='linear',cost=100, cross=10, cachesize=1000, tolerance=.001)
fit_svmL$SV
fit_svmL$coefs

### Tune
fit_svmL_tune <- tune.svm(flag_class~., data=train[,-c(1,2,47,49)], gamma=2^(-1:1),cost=2^(2:6))

tc <- tune.control(nrepeat=5,repeat.aggregate=min, sampling='cross',sampling.aggregate=min,
                   sampling.dispersion=sd,cross=10,best.model=T,
                   performances=T,error.fun=NULL)
gamma = 1/length(names(train_pH_1[,-1]))
fit_svmL_tune <- tune(svm, flag_class~., data=train[,-c(1,2,47,49)], ranges=list(gamma=2^(-3:1),cost=2^(2:10)),
                         tunecontrol = tc)
best.tune()

summary(fit_svmL_tune)
png('svm_tune.png')
plot(fit_svmL_tune)
dev.off()


### Bash SVM ###
set.seed(888)
Grid <- expand.grid(C=c(1,4,8,16,32,64,128,256,512,1024,2048,4096,8192,10000),
                    sigma=c(0.00028,0.0118,0.3,1,3,9,27,81))
fitControl <- trainControl(method="repeatedcv",number=10,
                           repeats=10, summaryFunction = defaultSummary,
                           returnResamp = "all")
fit_pH <- train(pH~., data=train_pH, method='svmRadial',trControl = fitControl,
                preProc = c('center', 'scale'),tuneLength=10,tuneGrid = Grid,
                verbose=T,metric='RMSE')


##################
### Validation ###
##################
### Predict
val <- validation#[!validation$COUNTRY_OF_RESIDENCE_NAME %in% c('Qatar'),]
p <- predict(fit_svmL_tune$best.model, newdata=val)#, type = 'prob')
fit_svmL_tune$best.parameters
fit_svmL_tune$best.performance
fit_svmL_tune$performances
fit_svmL_tune$train.ind
fit_svmL_tune$best.model

# val$Y <- p$Y
# val$PRED_PROFIT_LOSS <- (val$Y - 0.5) * val$INVEST * 2
# pred_fin <- aggregate(PRED_PROFIT_LOSS ~ ACCOUNT_ID, data=val, sum, na.rm=F)
# pred_fin$PRED_PROFIT_LOSS_2 <- ifelse(pred_fin$PRED_PROFIT_LOSS > 0, 1, ifelse(pred_fin$PRED_PROFIT_LOSS < 0, 0, 0.5))
# pred_fin2 <- aggregate(Y ~ ACCOUNT_ID, data=val, mean, na.rm=F)
# 
# ### Validation
# val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
# val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, ifelse(val_fin$flag_regr < 0, 0, 0.5))
# # p_rf <- p
# # p_lg <- p
# # p$Y <- (p_rf$Y + p_lg$Y)/2
# 
# #########################
# ### Model Performance ###
# #########################
# v <- merge(val_fin,pred_fin,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
# v <- merge(v,pred_fin2,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
# # With a roc object:
# rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$PRED_PROFIT_LOSS_2);auc(rocobj) # Invest * Possibility
# rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$Y);auc(rocobj) # Average Possibility
# # rocobj <- roc(val$flag_class, p$Y);auc(rocobj) # Single Events 
# 
# # Partial AUC:
# auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE) 
# 
# # Plot
# plot(rocobj)