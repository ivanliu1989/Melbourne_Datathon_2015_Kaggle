setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
library(xgboost);library(pROC);library(caret)
load('data/9_train_validation_test_kmean.RData');ls()

################
### Register ###
################
    # train <- total
    set.seed(18)
    dim(total); dim(validation)
    total$flag_class <- ifelse(total$flag_class == 'Y', 1, 0)
    feat <- colnames(total)[c(3:56,58:59)]
    dt <- total[,feat]

#######################################
# Zero- and Near Zero-Variance ########
#######################################
    nzv <- nearZeroVar(dt, saveMetrics= TRUE)
    nzv[nzv$nzv,][1:10,]
    # freqRatio percentUnique zeroVar  nzv
    # TRANSACTION_COUNT_OUTPLAY_L        20.01032    0.02525455   FALSE TRUE
    # AVG_BET_SIZE_OUTPLAY_L           1536.64151    3.68716495   FALSE TRUE
    # MAX_BET_SIZE_OUTPLAY_L           1508.18519    3.08622134   FALSE TRUE
    # MIN_BET_SIZE_OUTPLAY_L           1071.60526    3.01677132   FALSE TRUE
    # STDEV_BET_SIZE_OUTPLAY_L        13226.23077    1.23173350   FALSE TRUE
    # AVG_PLACED_TAKEN_TIME_OUTPLAY      61.33307    2.39918267   FALSE TRUE
    # STDEV_PLACED_TAKEN_TIME_INPLAY     61.96254    6.20114105   FALSE TRUE
    # STDEV_PLACED_TAKEN_TIME_OUTPLAY    77.88996    2.50594056   FALSE TRUE
    # SKEW_PLACED_TAKEN_TIME_INPLAY      28.57405    5.96868435   FALSE TRUE
    # SKEW_PLACED_TAKEN_TIME_OUTPLAY     46.58282    2.06398586   FALSE TRUE


####################################
# Remove Redundant Features ########
####################################
    set.seed(8)
    # calculate correlation matrix
    correlationMatrix <- cor(dt)
    # summarize the correlation matrix
    print(correlationMatrix)
    # find attributes that are highly corrected (ideally >0.75)
    highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.95)
    # print indexes of highly correlated attributes
    summary(correlationMatrix[upper.tri(correlationMatrix)])
    print(names(dt[,highlyCorrelated]))
    # filter
    dt_c <- dt[,-highlyCorrelated]
    descrCor2 <- cor(dt_c)
    summary(descrCor2[upper.tri(descrCor2)])


##############################
# Linear Dependencies ########
##############################
    # comboInfo <- findLinearCombos(dt)
    # comboInfo
    # dt[, -comboInfo$remove]


######################################
# Rank Features By Importance ########
######################################
    # ensure results are repeatable
    set.seed(8)
    # prepare training scheme
    control <- trainControl(method="repeatedcv", number=10, repeats=3)
    # train the model
    feat2 <- colnames(total)[c(3:56,58:60)]
    dt2 <- total[,feat2]
    dt2$flag_class <- as.factor(dt2$flag_class)
    model <- train(flag_class~., data=dt2, method="lvq", preProcess="scale", trControl=control)
    # estimate variable importance
    importance <- varImp(model, scale=FALSE)
    # summarize importance
    print(importance)
    # plot importance
    plot(importance)


############################
# Feature Selection ########
############################
    # ensure the results are repeatable
    set.seed(7)
    # load the library
    library(mlbench)
    library(caret)
    # load the data
    data(PimaIndiansDiabetes)
    # define the control using a random forest selection function
    control <- rfeControl(functions=rfFuncs, method="cv", number=10)
    # run the RFE algorithm
    results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
    # summarize the results
    print(results)
    # list the chosen features
    predictors(results)
    # plot the results
    plot(results, type=c("g", "o"))
    

