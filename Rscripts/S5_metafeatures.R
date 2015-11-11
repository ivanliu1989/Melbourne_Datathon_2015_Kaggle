setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);library(caret)
load('../S9_train_validation_test_20151110.RData');ls()

all <- rbind(total, test); str(all)
# feat2 <- colnames(total)[c(3:22,47:59)]#[c(3:56, 59)]
feat <- colnames(total)[c(3:76)]

###################################
# 1. xgboost meta features ########
###################################
total$flag_class <- ifelse(total$flag_class == 'Y', 1, 0)
# gbm - raw
bst <-
    xgboost( 
        data = as.matrix(total[,feat]), label = total$flag_class, max.depth = 6, eta = 0.15, nround = 500,
        nthread = 4, objective = "binary:logistic", verbose = 0, metrics = 'auc'
    )

p <- predict(bst, as.matrix(all[,feat]))  
all$xgb_gbm_meta_raw <- p
# gbm - new
bst <-
    xgboost( 
        data = as.matrix(total[,feat2]), label = total$flag_class, max.depth = 6, eta = 0.15, nround = 500,
        nthread = 4, objective = "binary:logistic", verbose = 0, metrics = 'auc'
    )

p <- predict(bst, as.matrix(all[,feat2]))  
all$xgb_gbm_meta_new <- p

# rf - old
bst <-
    xgboost(
        data = as.matrix(total[,feat]), label = total$flag_class, max.depth = 9, num_parallel_tree = 150, subsample = 0.5, colsample_bytree =
            0.5, nround = 1, objective = "binary:logistic"
    )
p <- predict(bst, as.matrix(all[,feat]))  
all$xgb_rf_meta_raw <- p
# rf - new
bst <-
    xgboost(
        data = as.matrix(total[,feat2]), label = total$flag_class, max.depth = 9, num_parallel_tree = 150, subsample = 0.5, colsample_bytree =
            0.5, nround = 1, objective = "binary:logistic"
    )
p <- predict(bst, as.matrix(all[,feat2]))  
all$xgb_rf_meta_new <- p

save(all, file='data/9_train_validation_test_20151108_meta_xgb_v1.RData')

###############################
# 2. h2o meta features ########
###############################
library(h2o)
localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '12g')

total$flag_class <- as.factor(total$flag_class); levels(total$flag_class) <- c(0,1) 
train_df <- as.h2o(localH2O, total) # train
test_df <- as.h2o(localH2O, all) # test
independent1 <- c(colnames(train_df[,feat]))
independent2 <- c(colnames(train_df[,feat2]))
dependent <- "flag_class"

# gbm
fit <- h2o.gbm(
    y = dependent, x = independent1, data = train_df, #train_df | total_df
    n.trees = 200, interaction.depth = 8, n.minobsinnode = 1,
    shrinkage = 0.25, distribution = "bernoulli", n.bins = 20,  #AUTO
    importance = F
)
p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))
all$h2o_gbm_meta_raw <- as.vector(p$X1)

fit <- h2o.gbm(
    y = dependent, x = independent2, data = train_df, #train_df | total_df
    n.trees = 200, interaction.depth = 8, n.minobsinnode = 1,
    shrinkage = 0.25, distribution = "bernoulli", n.bins = 20,  #AUTO
    importance = F
)
p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))
all$h2o_gbm_meta_new <- as.vector(p$X1)

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
all$h2o_dl_meta_raw <- as.vector(p$X1)

fit <-
    h2o.deeplearning(
        y = dependent, x = independent2, data = train_df, classification = T,
        activation = "RectifierWithDropout",#TanhWithDropout "RectifierWithDropout" nfolds = 5, 
        hidden = c(64,32), adaptive_rate = T, rho = 0.99, 
        epsilon = 1e-4, rate = 0.01, rate_decay = 0.9, # rate_annealing = , 
        momentum_start = 0.5, momentum_stable = 0.99, # momentum_ramp
        nesterov_accelerated_gradient = F, input_dropout_ratio = 0.5, hidden_dropout_ratios = c(0.5,0.5), 
        l2 = 3e-6, max_w2 = 4, #Rect
        loss = 'CrossEntropy', classification_stop = -1,
        diagnostics = T, variable_importances = T, ignore_const_cols = T,
        force_load_balance = T, replicate_training_data = T, shuffle_training_data = T,
        sparse = F, epochs = 300 #, reproducible, score_validation_sampling seed = 8, 
    )
p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))
all$h2o_dl_meta_new <- as.vector(p$X1)

# random forest
fit <-
    h2o.randomForest(
        y = dependent, x = independent1, data = train_df, #train_df | total_df #validation_frame
        ntree = 100, depth = 10, mtries = 8, sample.rate = 0.8, nbins = 10, importance = F
    )
p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))
all$h2o_rf_meta_raw <- as.vector(p$X1)

fit <-
    h2o.randomForest(
        y = dependent, x = independent2, data = train_df, #train_df | total_df #validation_frame
        ntree = 100, depth = 10, mtries = 8, sample.rate = 0.8, nbins = 10, importance = F
    )
p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))
all$h2o_rf_meta_new <- as.vector(p$X1)

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
all$h2o_glm_meta_raw <- as.vector(p$X1)

fit <-
    h2o.glm(
        y = dependent, x = independent2, data = train_df, #train_df | total_df
        family = 'binomial', link = 'logit',alpha = 0.5, # 1 lasso 0 ridge
        lambda = 1e-01, lambda_search = T, nlambda = 55, lambda.min.ratio = 1e-08,
        strong_rules = T, standardize = T, intercept = T, use_all_factor_levels = T,
        epsilon = 1e-4, iter.max = 900, higher_accuracy = T, disable_line_search = F
    )
p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))
all$h2o_glm_meta_new <- as.vector(p$X1)

save(all, file='data/9_train_validation_test_20151108_meta_h2o_v2.RData')

############################
# 3. sofia features ########
############################
library(RSofia);library(caret)
### Center Scale
prep <- preProcess(all[, feat], method = c('center',"scale"), verbose =T)
tot <- all
tot[, feat] <- predict(prep, tot[, feat])
tot$flag_class <- ifelse(tot$flag_class == 'Y', 1, -1)
feat_sofia <- c(feat, 'flag_class')

fit <- sofia(flag_class ~ ., data=tot[,feat_sofia], lambda = 1e-3, iiterations = 1e+25, random_seed = 13560,
             learner_type = 'logreg-pegasos', #c("pegasos", "sgd-svm","passive-aggressive", "margin-perceptron", "romma", "logreg-pegasos"),
             eta_type = 'pegasos', #c("pegasos", "basic", "constant"), 
             loop_type = 'balanced-stochastic', #c("stochastic","balanced-stochastic", "rank", "roc", "query-norm-rank","combined-ranking", "combined-roc"),
             rank_step_probability = 0.5,
             passive_aggressive_c = 1e+07, passive_aggressive_lambda = 1e+1, dimensionality = 60,
             perceptron_margin_size = 1, training_objective = F, hash_mask_bits = 0,
             verbose = T, reserve = 1
)
p <- predict(fit, newdata=tot[,feat_sofia], prediction_type = "logistic")
all$sofia_bal_meta_raw <- as.vector(p)

fit <- sofia(flag_class ~ ., data=tot[,feat_sofia], lambda = 1e-3, iiterations = 1e+25, random_seed = 13560,
             learner_type = 'logreg-pegasos', #c("pegasos", "sgd-svm","passive-aggressive", "margin-perceptron", "romma", "logreg-pegasos"),
             eta_type = 'pegasos', #c("pegasos", "basic", "constant"), 
             loop_type = 'combined-roc', #c("stochastic","balanced-stochastic", "rank", "roc", "query-norm-rank","combined-ranking", "combined-roc"),
             rank_step_probability = 0.5,
             passive_aggressive_c = 1e+07, passive_aggressive_lambda = 1e+1, dimensionality = 60,
             perceptron_margin_size = 1, training_objective = F, hash_mask_bits = 0,
             verbose = T, reserve = 1
)
p <- predict(fit, newdata=tot[feat_sofia], prediction_type = "logistic")
all$sofia_roc_meta_raw <- as.vector(p)

# new
prep <- preProcess(all[, feat2], method = c('center',"scale"), verbose =T)
tot <- all
tot[, feat2] <- predict(prep, tot[, feat2])
tot$flag_class <- ifelse(tot$flag_class == 'Y', 1, -1)
feat_sofia <- c(feat2, 'flag_class')

fit <- sofia(flag_class ~ ., data=tot[,feat_sofia], lambda = 1e-3, iiterations = 1e+25, random_seed = 13560,
             learner_type = 'logreg-pegasos', #c("pegasos", "sgd-svm","passive-aggressive", "margin-perceptron", "romma", "logreg-pegasos"),
             eta_type = 'pegasos', #c("pegasos", "basic", "constant"), 
             loop_type = 'balanced-stochastic', #c("stochastic","balanced-stochastic", "rank", "roc", "query-norm-rank","combined-ranking", "combined-roc"),
             rank_step_probability = 0.5,
             passive_aggressive_c = 1e+07, passive_aggressive_lambda = 1e+1, dimensionality = 60,
             perceptron_margin_size = 1, training_objective = F, hash_mask_bits = 0,
             verbose = T, reserve = 1
)
p <- predict(fit, newdata=tot[,feat_sofia], prediction_type = "logistic")
all$sofia_bal_meta_raw <- as.vector(p)

fit <- sofia(flag_class ~ ., data=tot[,feat_sofia], lambda = 1e-3, iiterations = 1e+25, random_seed = 13560,
             learner_type = 'logreg-pegasos', #c("pegasos", "sgd-svm","passive-aggressive", "margin-perceptron", "romma", "logreg-pegasos"),
             eta_type = 'pegasos', #c("pegasos", "basic", "constant"), 
             loop_type = 'combined-roc', #c("stochastic","balanced-stochastic", "rank", "roc", "query-norm-rank","combined-ranking", "combined-roc"),
             rank_step_probability = 0.5,
             passive_aggressive_c = 1e+07, passive_aggressive_lambda = 1e+1, dimensionality = 60,
             perceptron_margin_size = 1, training_objective = F, hash_mask_bits = 0,
             verbose = T, reserve = 1
)
p <- predict(fit, newdata=tot[feat_sofia], prediction_type = "logistic")
all$sofia_roc_meta_raw <- as.vector(p)

save(all, file='data/9_train_validation_test_20151108_meta_sofia_v3.RData')

#######################
# 4. caret KNN ########
#######################
load('data/9_train_validation_test_20151108_meta_sofia_v3.RData')
library(class)
total <- all
feat <- c(3:59)
preProcValues <- preProcess(x = total[,feat],method = c("center", "scale"))
total[,feat] <- predict(preProcValues, total[,feat])

fit <- knn(train= total[,feat],  test=total[,feat], cl=total$flag_class, k=2, prob=T, use.all=F)
all$knn_2_meta_raw <- attr(fit, 'prob')
fit <- knn(train= total[,feat],  test=total[,feat], cl=total$flag_class, k=4, prob=T, use.all=F)
all$knn_4_meta_raw <- attr(fit, 'prob')
fit <- knn(train= total[,feat],  test=total[,feat], cl=total$flag_class, k=8, prob=T, use.all=F)
all$knn_8_meta_raw <- attr(fit, 'prob')
fit <- knn(train= total[,feat],  test=total[,feat], cl=total$flag_class, k=16, prob=T, use.all=F)
all$knn_16_meta_raw <- attr(fit, 'prob')
fit <- knn(train= total[,feat],  test=total[,feat], cl=total$flag_class, k=32, prob=T, use.all=F)
all$knn_32_meta_raw <- attr(fit, 'prob')
fit <- knn(train= total[,feat],  test=total[,feat], cl=total$flag_class, k=64, prob=T, use.all=F)
all$knn_64_meta_raw <- attr(fit, 'prob')
fit <- knn(train= total[,feat],  test=total[,feat], cl=total$flag_class, k=128, prob=T, use.all=F)
all$knn_128_meta_raw <- attr(fit, 'prob')
fit <- knn(train= total[,feat],  test=total[,feat], cl=total$flag_class, k=256, prob=T, use.all=F)
all$knn_256_meta_raw <- attr(fit, 'prob')


all <- all[,c(1:76, 79:92, 77:78)]
test <- all[all$flag_class == 'M', ]
total <- all[all$flag_class != 'M', ]
validation <- total[total$EVENT_ID %in% c(101150834,101153072,101149398),]
train <- total[!total$EVENT_ID %in% c(101150834,101153072,101149398),]
dim(train); dim(validation)