setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list = ls()); gc()
library(xgboost);library(pROC);library(caret)
load('data/9_train_validation_test_20151106.RData');ls()
localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '12g')

total$flag_class <- as.factor(total$flag_class); levels(total$flag_class) <- c(0,1) 
test_df <- as.h2o(localH2O, test)

events <- unique(total$EVENT_ID)
for (i in 1:(length(events) - 2)) {
    print(paste0('Round ', i, ' | Subsetting without event: ', events[i:(i + 2)]))
    training <- total[!total$EVENT_ID %in% events[i:(i + 2)],]
    training$flag_class <- as.factor(training$flag_class); levels(training$flag_class) <- c(0,1) 
    
    train_df <- as.h2o(localH2O, training) # train
    
    independent <- c(colnames(train_df[,3:(ncol(train_df)-2)]))
    dependent <- "flag_class"
    
#         fit <- h2o.gbm(
#             y = dependent, x = independent, data = train_df, #train_df | total_df
#             n.trees = 200, interaction.depth = 8, n.minobsinnode = 1,
#             shrinkage = 0.25, distribution = "bernoulli", n.bins = 20,  #AUTO
#             importance = F
#         )
    #     
#         fit <-
#             h2o.deeplearning(
#                 y = dependent, x = independent, data = train_df, classification = T,
#                 activation = "RectifierWithDropout",#TanhWithDropout "RectifierWithDropout" nfolds = 5, 
#                 hidden = c(256,256,256), adaptive_rate = T, rho = 0.99, 
#                 epsilon = 1e-4, rate = 0.01, rate_decay = 0.9, # rate_annealing = , 
#                 momentum_start = 0.5, momentum_stable = 0.99, # momentum_ramp
#                 nesterov_accelerated_gradient = T, input_dropout_ratio = 0.5, hidden_dropout_ratios = c(0.5,0.5,0.5), 
#                 l2 = 3e-6, max_w2 = 4, #Rect
#                 loss = 'CrossEntropy', classification_stop = -1,
#                 diagnostics = T, variable_importances = T, ignore_const_cols = T,
#                 force_load_balance = T, replicate_training_data = T, shuffle_training_data = T,
#                 sparse = F, epochs = 5 #, reproducible, score_validation_sampling seed = 8, 
#             )
    
    #     fit <-
    #         h2o.randomForest(
    #             y = dependent, x = independent, data = train_df, #train_df | total_df #validation_frame
    #             ntree = 100, depth = 10, mtries = 8, sample.rate = 0.8, nbins = 10, importance = F
    #         )
    
    #     fit <-
    #         h2o.naiveBayes(
    #             y = dependent, x = independent, data = train_df, laplace = 0
    #         )
    #     
        fit <-
            h2o.glm(
                y = dependent, x = independent, data = train_df, #train_df | total_df
                family = 'binomial', link = 'logit',alpha = 0.5, # 1 lasso 0 ridge
                lambda = 1e-08, lambda_search = T, nlambda = 12, lambda.min.ratio = 0.1,
                strong_rules = T, standardize = T, intercept = F, use_all_factor_levels = F,
                epsilon = 1e-4, iter.max = 900, higher_accuracy = T, disable_line_search = F
            )
    
    #-------------Test prediction-----------------
    p <- as.data.frame(h2o.predict(object = fit, newdata = test_df))
    
    if (i == 1) {
        pred <- p
    }else{
        pred[,3] <- pred[,3] + p[,3]
    }
}

submit <- pred[,3] / (length(events) - 2)

write.csv(
    as.data.frame(submit),'pred/submission_20151106_h2o_glm.csv',quote = FALSE,row.names = FALSE
)

