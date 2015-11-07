setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);library(caret)
load('data/9_train_validation_test_20151108.RData');ls()

all <- rbind(total, test); str(all)

###################################
# 1. xgboost meta features ########
###################################
total$flag_class <- ifelse(total$flag_class == 'Y', 1, 0)
feat <- colnames(total)[c(3:60)]
# gbm
bst <-
    xgboost( 
        data = as.matrix(total[,feat]), label = total$flag_class, max.depth = 6, eta = 0.15, nround = 30,
        nthread = 4, objective = "binary:logistic", verbose = 0, metrics = 'auc'
    )

p <- predict(bst, as.matrix(all[,feat]))  
all$xgb_gbm_meta <- p

# rf
bst <-
    xgboost(
        data = as.matrix(total[,feat]), label = total$flag_class, max.depth = 6, num_parallel_tree = 100, subsample = 0.5, colsample_bytree =
            0.5, nround = 1, objective = "binary:logistic"
    )
p <- predict(bst, as.matrix(all[,feat]))  
all$xgb_rf_meta <- p

###############################
# 2. h2o meta features ########
###############################
library(h2o)
localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '12g')

total$flag_class <- as.factor(total$flag_class); levels(total$flag_class) <- c(0,1) 
train_df <- as.h2o(localH2O, total) # train
test_df <- as.h2o(localH2O, all) # test
independent <- c(colnames(train_df[,3:(ncol(train_df)-2)]))
dependent <- "flag_class"

# gbm
fit <- h2o.gbm(
    y = dependent, x = independent, data = train_df, #train_df | total_df
    n.trees = 200, interaction.depth = 8, n.minobsinnode = 1,
    shrinkage = 0.25, distribution = "bernoulli", n.bins = 20,  #AUTO
    importance = F
)
p <- as.vector(h2o.predict(object = fit, newdata = test_df))
all$xgb_rf_meta <- p

# deeplearning
fit <-
    h2o.deeplearning(
        y = dependent, x = independent, data = train_df, classification = T,
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

# random forest
fit <-
    h2o.randomForest(
        y = dependent, x = independent, data = train_df, #train_df | total_df #validation_frame
        ntree = 100, depth = 10, mtries = 8, sample.rate = 0.8, nbins = 10, importance = F
    )

# naive bayes
fit <-
    h2o.naiveBayes(
        y = dependent, x = independent, data = train_df, laplace = 0
    )

# glm   
fit <-
    h2o.glm(
        y = dependent, x = independent, data = train_df, #train_df | total_df
        family = 'binomial', link = 'logit',alpha = 0.5, # 1 lasso 0 ridge
        lambda = 1e-01, lambda_search = T, nlambda = 55, lambda.min.ratio = 1e-08,
        strong_rules = T, standardize = T, intercept = T, use_all_factor_levels = T,
        epsilon = 1e-4, iter.max = 900, higher_accuracy = T, disable_line_search = F
    )