setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(Rtsne);require(data.table);library(caret);library(RSofia);library(h2o)
load('data/9_train_validation_test_20151122.RData');ls()
options(scipen=999);set.seed(19890624)
library(h2o)
localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '12g')


test <- test
train <- train #total
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
test$flag_class <- ifelse(test$flag_class == 'Y', 1, 0)
validation$flag_class <- ifelse(validation$flag_class == 'Y', 1, 0)
train$flag_class <- as.factor(train$flag_class)
test$flag_class <- as.factor(test$flag_class)
validation$flag_class <- as.factor(validation$flag_class)
train_df <- as.h2o(localH2O, train) # train
test_df <- as.h2o(localH2O, test) # test
valid_df <- as.h2o(localH2O, validation) # test
independent <- colnames(train)[c(3:(ncol(train)-2))]
dependent <- "flag_class"

# gbm
# fit <- h2o.gbm(
#     y = dependent, x = independent, training_frame = train_df, #train_df | total_df
#     ntrees = 1000, max_depth = 8, min_rows = 2,
#     learn_rate = 0.15, distribution = "bernoulli", nbins_cats = 20,  #AUTO
#     importance = F
# )
# # p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))
# p <- as.data.frame(h2o.predict(object = fit, newdata = valid_df))

# deeplearning
for(i in 1:50){
    set.seed(i*8)
    fit <-
        h2o.deeplearning(
            y = dependent, x = independent, training_frame = train_df, overwrite_with_best_model = T, #autoencoder
            use_all_factor_levels = T, activation = "RectifierWithDropout",#TanhWithDropout "RectifierWithDropout"
            hidden = c(256,128), epochs = 9, train_samples_per_iteration = -2, adaptive_rate = T, rho = 0.99,  #c(300,150,75)
            epsilon = 1e-6, rate = 0.01, rate_decay = 0.9, momentum_start = 0.9, momentum_stable = 0.99,
            nesterov_accelerated_gradient = T, input_dropout_ratio = 0.25, hidden_dropout_ratios = c(0.25,0.25), 
            l1 = NULL, l2 = 3e-5, loss = 'CrossEntropy', classification_stop = 0.01,
            diagnostics = T, variable_importances = F, fast_mode = F, ignore_const_cols = T,
            force_load_balance = T, replicate_training_data = T, shuffle_training_data = T
        )
    # p <- as.data.frame(h2o.predict(object = fit, newdata = train_df))
    p <- as.data.frame(h2o.predict(object = fit, newdata = valid_df))
    # write.csv(p, paste0('ReadyForBlending/submission/train/h2o_nnet/submission_h2o_nnet_20151202_',i,'.csv'))
    
}


# random forest
fit <-
    h2o.randomForest(
        y = dependent, x = independent, training_frame = train_df, mtries = -1, 
        ntrees = 800, max_depth = 16, sample.rate = 0.632, min_rows = 1, 
        nbins = 20, nbins_cats = 1024, binomial_double_trees = T
    )
p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))
# p <- as.data.frame(h2o.predict(object = fit, newdata = valid_df))

# glm   
fit <-
    h2o.glm(
        y = dependent, x = independent, training_frame = train_df, #train_df | total_df
        max_iterations = 100, beta_epsilon = 1e-4, solver = "L_BFGS", #IRLSM  L_BFGS
        standardize = T, family = 'binomial', link = 'logit', alpha = 0.5, # 1 lasso 0 ridge
        lambda = 0, lambda_search = T, nlambda = 55, #lambda_min_ratio = 1e-08,
        intercept = T
        #higher_accuracy = T, disable_line_search = F, use_all_factor_levels = T,strong_rules = T
    )
p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))
# p <- as.data.frame(h2o.predict(object = fit, newdata = valid_df))

#########################
### Validation ##########
#########################
# val <- test
val <- validation
val$Y <- p[,3]
tot_invest <- aggregate(INVEST ~ ACCOUNT_ID,data=val, sum, na.rm=T); names(tot_invest) <- c('ACCOUNT_ID', 'TOT_INVEST')
val <- merge(val, tot_invest, all.x = TRUE, all.y = FALSE, by = c('ACCOUNT_ID'))
val$INVEST_PERCENT <- val$INVEST/val$TOT_INVEST * val$Y
pred_fin <- aggregate(INVEST_PERCENT ~ ACCOUNT_ID, data=val, sum, na.rm=F)
pred_fin2 <- aggregate(Y ~ ACCOUNT_ID, data=val, mean, na.rm=F)
val_fin <- aggregate(flag_regr ~ ACCOUNT_ID, data=val, sum, na.rm=F)
val_fin$PRED_PROFIT_LOSS_3 <- ifelse(val_fin$flag_regr > 0, 1, 0)

### Model Performance ###
rocobj <- roc(val_fin$PRED_PROFIT_LOSS_3, pred_fin[,2]);print(auc(rocobj)) # Invest * Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
rocobj <- roc(val_fin$PRED_PROFIT_LOSS_3, pred_fin2[,2]);print(auc(rocobj)) # Average Possibility
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))

prediction <- as.factor(ifelse(pred_fin[,2] >=0.5, 1, 0))
confusionMatrix(as.factor(val_fin$PRED_PROFIT_LOSS_3), prediction)

### Submission
write.csv(p, paste0('ReadyForBlending/submission/test/h2o_glm/submission_h2o_glm_201511203.csv'))
