setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
# setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(h2o);library(pROC);library(doMC)
load('data/9_train_validation_test_TREE_2.RData');ls()
# load('data/9_train_validation_test_ONEHOT_1.RData');ls()

################
### Register ###
################
set.seed(18)
registerDoMC(cores = 4)
localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '12g')

train$flag_class <- as.factor(train$flag_class); levels(train$flag_class) <- c(0,1) 
val_class <- validation$flag_class
validation$flag_class <- as.factor(validation$flag_class); levels(validation$flag_class) <- c(0,1) 
# total$flag_class <- as.factor(total$flag_class); levels(total$flag_class) <- c(0,1) 

######################
### Feature Select ###
######################
# train <- train[,-c(27:30, 44:46)] # skew, kurt, bl_ratio

# total_df <- as.h2o(localH2O, total)
train_df <- as.h2o(localH2O, train)
validation_df <- as.h2o(localH2O, validation)
# test_df <- as.h2o(localH2O, test)

independent <- colnames(train_df[,3:(ncol(train_df)-3)])
dependent <- "flag_class"

##############
### Models ###
##############
perf <- 0
for(d0 in c(128, 256, 512)){
    for(d1 in c(0.2,0.3,0.5,0.75,0.9)){
        for(d2 in c(0.1,0.2,0.3,0.5,0.75)){
            for(d3 in c(1,3,9,15,20)){
                
                #             fit <- h2o.gbm(
                #                 y = dependent, x = independent, data = train_df,
                #                 n.trees = 200, interaction.depth = 8, n.minobsinnode = 1,
                #                 shrinkage = 0.25, distribution = "bernoulli", n.bins = 20,  #AUTO
                #                 importance = T
                #             )
                
                fit <-
                    h2o.deeplearning(
                        y = dependent, x = independent, data = train_df, classification = T,
                        nfolds = 5, activation = "RectifierWithDropout",#TanhWithDropout "RectifierWithDropout"
                        hidden = c(d0,d0/2,d0/4), seed = 8, adaptive_rate = T, rho = 0.99, 
                        epsilon = 1e-4, rate = 0.1, rate_decay = 0.9, # rate_annealing = , 
                        momentum_start = 0.5, momentum_stable = 0.99, # momentum_ramp
                        nesterov_accelerated_gradient = T, input_dropout_ratio = d1, hidden_dropout_ratios = c(d2,d2,d2), 
                        l2 = 3e-6, max_w2 = 4, #Rect
                        loss = 'CrossEntropy', classification_stop = -1,
                        diagnostics = T, variable_importances = F, ignore_const_cols = T,
                        force_load_balance = T, replicate_training_data = T, shuffle_training_data = T,
                        sparse = F, epochs = d3 #, reproducible, score_validation_sampling
                    )
                
                #             fit <-
                #                 h2o.randomForest(
                #                     y = dependent, x = independent, data = train_df, #validation_frame
                #                     ntree = 100, depth = 10, mtries =
                #                         8, sample.rate = 0.8, nbins = 10, seed = 8
                #                 )
                #             
                #             fit <-
                #                 h2o.naiveBayes(
                #                     y = dependent, x = independent, data = train_df, laplace = 0
                #                 )
                #             
                #             fit <-
                #                 h2o.glm(
                #                     y = dependent, x = independent, data = train_df,
                #                     family = 'binomial', link = 'logit',alpha = 0.5, # 1 lasso 0 ridge
                #                     lambda = 1e-08, lambda_search = T, nlambda = 8, lambda.min.ratio = 0.1,
                #                     strong_rules = T, standardize = T, intercept = T, use_all_factor_levels = F,
                #                     epsilon = 1e-4, iter.max = 100, higher_accuracy = T, disable_line_search = F
                #                 )
                #             
                ##################
                ### Prediction ###
                ##################
                val <- validation
                pred <- h2o.predict(object = fit, newdata = validation_df)
                val <- cbind(val, as.data.frame(pred[,3]))
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
                
                # Performance Selection
                perf_new <- auc(rocobj, partial.auc = c(1, .8), partial.auc.focus = "se", partial.auc.correct = TRUE)
                # rocobj;perf_new
                
                if (perf_new > perf) {
                    perf <- perf_new
                    print (
                        paste0(
                            'hidden: ', d0, ' | input_dropout_ratio: ', d1, ' | hidden_dropout_ratios: ', d2, ' | epochs: ', d3
                        )
                    )
                    print(auc(rocobj)); print(perf_new)
                }
                write.csv(as.data.frame(pred),
                          file=paste0('ReadyForBlending/validation/0_deeplearning_d0', d0,
                                      '_d1', d1, '_d2', d2, '_d3', d3, '_score', as.numeric(perf_new),'.csv'),quote = FALSE,row.names = FALSE)
            }
        }
    }
}



h2o.shutdown(localH2O)
    

