setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);require(randomForest);library(Rtsne);require(data.table);library(caret);library(RSofia);library(h2o)
load('data/Ivan_Train_Test_Scale_Center_20151116.RData');ls()
options(scipen=999);set.seed(19890624)
library(h2o)
localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '12g')

test <- train[train$EVENT_ID %in% c(101183757,101183885,101184013),]#validation
train <- train[!train$EVENT_ID %in% c(101183757,101183885,101184013),]
train$flag_class <- ifelse(train$flag_class == 'Y', 1, 0)
test$flag_class <- ifelse(test$flag_class == 'Y', 1, 0)
validation$flag_class <- ifelse(validation$flag_class == 'Y', 1, 0)
train$flag_class <- as.factor(train$flag_class)
test$flag_class <- as.factor(test$flag_class)
validation$flag_class <- as.factor(validation$flag_class)
train_df <- as.h2o(localH2O, train) # train
test_df <- as.h2o(localH2O, test) # test
valid_df <- as.h2o(localH2O, validation) # test
independent <- colnames(train)[c(3:(ncol(train)-3))]
independent <- independent[!independent %in% c('tsne_3d_1','tsne_3d_2','tsne_3d_3')]
dependent <- "flag_class"

# gbm
fit <- h2o.gbm(
    y = dependent, x = independent, data = train_df, #train_df | total_df
    n.trees = 1000, interaction.depth = 8, n.minobsinnode = 1,
    shrinkage = 0.05, distribution = "bernoulli", n.bins = 20,  #AUTO
    importance = F
)
p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))
p <- as.data.frame(h2o.predict(object = fit, newdata = valid_df))

# deeplearning
fit <-
    h2o.deeplearning(
        y = dependent, x = independent1, data = train_df, classification = T,
        activation = "RectifierWithDropout",#TanhWithDropout "RectifierWithDropout" nfolds = 5, 
        hidden = c(64,32), adaptive_rate = T, rho = 0.99, 
        epsilon = 1e-4, rate = 0.01, rate_decay = 0.9, # rate_annealing = , 
        momentum_start = 0.5, momentum_stable = 0.99, # momentum_ramp
        nesterov_accelerated_gradient = F, input_dropout_ratio = 0.5, hidden_dropout_ratios = c(0.5,0.5), 
        l2 = 3e-6, max_w2 = 4, #Rect
        loss = 'CrossEntropy', classification_stop = -1,
        diagnostics = T, variable_importances = T, ignore_const_cols = T,
        force_load_balance = T, replicate_training_data = T, shuffle_training_data = T,
        sparse = F, epochs = 100 #, reproducible, score_validation_sampling seed = 8, 
    )
p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))

# random forest
fit <-
    h2o.randomForest(
        y = dependent, x = independent1, data = train_df, #train_df | total_df #validation_frame
        ntree = 100, depth = 10, mtries = 8, sample.rate = 0.8, nbins = 10, importance = F
    )
p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))

# glm   
fit <-
    h2o.glm(
        y = dependent, x = independent1, data = train_df, #train_df | total_df
        family = 'binomial', link = 'logit',alpha = 0.5, # 1 lasso 0 ridge
        lambda = 1e-01, lambda_search = T, nlambda = 55, lambda.min.ratio = 1e-08,
        strong_rules = T, standardize = T, intercept = T, use_all_factor_levels = T,
        epsilon = 1e-4, iter.max = 900, higher_accuracy = T, disable_line_search = F
    )
p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))


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

### New Calc
rocobj <- roc(val$flag_class, val$Y);print(auc(rocobj)) 
print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
