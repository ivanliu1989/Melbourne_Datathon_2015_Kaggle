setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(h2o);library(pROC);library(doMC)
load('../S9_train_validation_test_20151110.RData');ls()

################
### Register ###
################
set.seed(8)
localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '12g')

train$flag_class <- as.factor(train$flag_class); levels(train$flag_class) <- c(0,1) 
val_class <- validation$flag_class
validation$flag_class <- as.factor(validation$flag_class); levels(validation$flag_class) <- c(0,1) 
total$flag_class <- as.factor(total$flag_class); levels(total$flag_class) <- c(0,1) 

######################
### Feature Select ###
######################
training <- train[!train$EVENT_ID %in% c(101183757,101183885,101184013),]
testing <- train[train$EVENT_ID %in% c(101183757,101183885,101184013),]
dim(training); dim(testing)
# total_df <- as.h2o(localH2O, total)
train_df <- as.h2o(localH2O, training) # train
validation_df <- as.h2o(localH2O, validation)
test_df <- as.h2o(localH2O, testing) # test

independent <- c(colnames(train_df[,3:(ncol(train_df)-2)]))#'INVEST','win_hist','EVENT_COUNT',
dependent <- "flag_class"

##############
### Models ###
##############

#     fit <- h2o.gbm(
#         y = dependent, x = independent, data = train_df, #train_df | total_df
#         n.trees = 200, interaction.depth = 8, n.minobsinnode = 1,
#         shrinkage = 0.25, distribution = "bernoulli", n.bins = 20,  #AUTO
#         importance = F
#     )
#     
# fit <-
#     h2o.deeplearning(
#         y = dependent, x = independent, data = train_df, classification = T,
#         activation = "RectifierWithDropout",#TanhWithDropout "RectifierWithDropout" nfolds = 5, 
#         hidden = c(64,32), adaptive_rate = T, rho = 0.99, 
#         epsilon = 1e-4, rate = 0.01, rate_decay = 0.9, # rate_annealing = , 
#         momentum_start = 0.5, momentum_stable = 0.99, # momentum_ramp
#         nesterov_accelerated_gradient = F, input_dropout_ratio = 0.5, hidden_dropout_ratios = c(0.5,0.5), 
#         l2 = 3e-6, max_w2 = 4, #Rect
#         loss = 'CrossEntropy', classification_stop = -1,
#         diagnostics = T, variable_importances = T, ignore_const_cols = T,
#         force_load_balance = T, replicate_training_data = T, shuffle_training_data = T,
#         sparse = F, epochs = 300 #, reproducible, score_validation_sampling seed = 8, 
#     )

#     fit <-
#         h2o.randomForest(
#             y = dependent, x = independent, data = train_df, #train_df | total_df #validation_frame
#             ntree = 1000, depth = 10, mtries = 8, sample.rate = 0.8, nbins = 10, importance = T
#         )

    fit <-
        h2o.naiveBayes(
            y = dependent, x = independent, data = train_df, laplace = 0
        )
#     
#     fit <-
#         h2o.glm(
#             y = dependent, x = independent, data = train_df, #train_df | total_df
#             family = 'binomial', link = 'logit',alpha = 0.5, # 1 lasso 0 ridge
#             lambda = 1e-01, lambda_search = T, nlambda = 55, lambda.min.ratio = 1e-08,
#             strong_rules = T, standardize = T, intercept = T, use_all_factor_levels = T,
#             epsilon = 1e-4, iter.max = 900, higher_accuracy = T, disable_line_search = F
#         )

##################
### Prediction ###
##################
val <- validation; pred <- h2o.predict(object = fit, newdata = validation_df)
# val <- testing; pred <- h2o.predict(object = fit, newdata = test_df)
val <- cbind(val, as.data.frame(pred[,3])) #X1=pred[,3]
tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=val, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
val <- merge(val, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
val$INVEST_PERCENT <- val$INVEST/val$TOT_INVEST * val$X1#(val$X1 - 0.5) * 2
pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=val, sum, na.rm=F)
pred_fin2 <- aggregate(X1 ~ ACCOUNT_ID, data=val, mean, na.rm=F)
### Validation
val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, 0)

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

prediction <- as.factor(ifelse(v$INVEST_PERCENT >=0.5, 1, 0))
confusionMatrix(as.factor(v$PRED_PROFIT_LOSS_3), prediction)




h2o.shutdown(localH2O)
