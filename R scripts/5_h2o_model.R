setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(h2o);library(pROC);library(doMC)
load('data/9_train_validation_test_TREE_1.RData');ls()
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
for(d1 in c(2:100)){
    
    for(d2 in c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)){
        
        for(d3 in c(1e-8, 3e-8, 1e-6, 3e-5, 1e-4, 3e-4, 1e-3, 3e-3, 0.01, 0.03, 0.1)){
            
                fit <- h2o.gbm(y = dependent, x = independent, data = train_df, 
                               n.trees = 200, interaction.depth = 8, n.minobsinnode = 1,
                               shrinkage = 0.25, distribution= "bernoulli", n.bins = 20,  #AUTO
                               importance = T)
            
            #     fit <- h2o.deeplearning(y = dependent, x = independent, data = train_df,
            #                             classification_stop = -1, activation="TanhWithDropout",#TanhWithDropout "RectifierWithDropout"
            #                             hidden=c(512,256), hidden_dropout_ratios = c(0.15,0.15), input_dropout_ratio = 0.5,
            #                             epochs=5, adaptive_rate = T, rho = 0.99, epsilon = 1e-10, # 1e-4
            #                             rate_decay=0.8,rate=0.1,momentum_start = 0.5, momentum_stable=0.99,
            #                             nesterov_accelerated_gradient = T, loss='CrossEntropy', l2=3e-6, max_w2=2,
            #                             seed=8,variable_importances=F,sparse= F,diagnostics=T,shuffle_training_data=T)# classification=T, autoencoder = F, 
            
#                 fit <- h2o.randomForest(y = dependent, x = independent, data = train_df, #validation_frame
#                                         ntree=100, depth=10, mtries=8, sample.rate=0.8, nbins = 10, seed=8)
            
            # fit <- h2o.naiveBayes(y = dependent, x = independent, data = train_df, laplace = 0)
            
#             fit <- h2o.glm(y = dependent, x = independent, data = train_df,
#                            family='binomial', link='logit',alpha = 0.5, # 1 lasso 0 ridge
#                            lambda = 1e-08, lambda_search = T, nlambda = 8, lambda.min.ratio = 0.1,
#                            strong_rules = T, standardize = T, intercept = T, use_all_factor_levels = F, 
#                            epsilon = 1e-4, iter.max = 100, higher_accuracy = T, disable_line_search = F)
            
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
            # print(fit)
            # With a roc object:
            rocobj <- roc(v$PRED_PROFIT_LOSS_3, v$PRED_PROFIT_LOSS_2)
            rocobj <- roc(v$PRED_PROFIT_LOSS_3, v[,6])
            # Partial AUC:
            # print(auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE))
            # Plot plot(rocobj)
            
            # Performance Selection 
            perf_new <- auc(rocobj, partial.auc=c(1, .8), partial.auc.focus="se", partial.auc.correct=TRUE)
            rocobj;perf_new
            
            
            pref_lg <- pred
            pref_rf <- pred
#             if (perf_new > perf){
#                 perf <- perf_new
#                 print (paste0('lambda: ', d3, ' | nlambda: ', d1, ' | lambda.min.ratio: ', d2))
#                 print(auc(rocobj)); print(perf_new)
#             }
        }
    }
}


h2o.shutdown(localH2O)
    

