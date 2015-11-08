setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list = ls()); gc()
library(RSofia);library(pROC);library(caret)
load('data/9_train_validation_test_20151108_feat.RData');ls()

### Center Scale
feat <- c(3:76) #3:59
prep <- preProcess(train[, feat], method = c('center',"scale"), verbose =T)
train[, feat] <- predict(prep, train[, feat])
validation[, feat] <- predict(prep, validation[, feat])
train$flag_class <- ifelse(train$flag_class == 'Y', 1, -1)
validation$flag_class <- ifelse(validation$flag_class == 'Y', 1, -1)
feat <- c(3:77)

#--------------------parse--------------
# dt <- parse_formula(flag_class ~ ., train[,feat])

#--------------------sofia model--------------
fit <- sofia(flag_class ~ ., data=train[,feat], lambda = 1e-3, iiterations = 1e+25, random_seed = 13560,
             learner_type = 'logreg-pegasos', #c("pegasos", "sgd-svm","passive-aggressive", "margin-perceptron", "romma", "logreg-pegasos"),
             eta_type = 'pegasos', #c("pegasos", "basic", "constant"), 
             loop_type = 'balanced-stochastic', #c("stochastic","balanced-stochastic", "rank", "roc", "query-norm-rank","combined-ranking", "combined-roc"),
             rank_step_probability = 0.5,
             passive_aggressive_c = 1e+07, passive_aggressive_lambda = 1e+1, dimensionality = 60,
             perceptron_margin_size = 1, training_objective = F, hash_mask_bits = 0,
             verbose = T, reserve = 1
             )

#--------------------basic prediction using sofia--------------
val <- validation
p <- predict(fit, newdata=val[,feat], prediction_type = "logistic") #logistic linear
# p[is.na(p)] <- 1
val$Y <- p

tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=val, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
val <- merge(val, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
val$INVEST_PERCENT <- val$INVEST/val$TOT_INVEST * val$Y#(val$Y - 0.5) * 2
pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=val, mean, na.rm=F)
pred_fin2 <- aggregate(Y ~ ACCOUNT_ID, data=val, mean, na.rm=F)
### Validation
val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, ifelse(val_fin$flag_regr < 0, 0, 0.5))

### Model Performance
v <- merge(val_fin,pred_fin,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
v <- merge(v,pred_fin2,all.x = TRUE,all.y = FALSE, by = 'ACCOUNT_ID')
rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$INVEST_PERCENT);print(auc(rocobj)) # Invest * Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$Y);print(auc(rocobj)) # Average Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))

# data(irismod)
# model.logreg <- sofia(Is.Virginica ~ ., data=irismod, learner_type="logreg-pegasos")
# p <- predict(model.logreg, newdata=irismod, prediction_type = "logistic")
