setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle/vowpal_wabbit')
library(r.vw)

## data
data("diamonds", package = "ggplot2")
dt = diamonds
dt$y = with(dt, ifelse(y < 5.71, 1, -1))

## separate train and validation data
ind_train = sample(1:nrow(dt), 40000)
dt_train = dt[ind_train,]
dt_val = dt[-ind_train,]


## first method: creating the vw data files before training
dt2vw(data = dt_train, fileName = "diamond_train.vw", target = "y")
dt2vw(data = dt_val, fileName = "diamond_val.vw", target = "y")

write.table(x = dt_val$y, file = "valid_labels.txt", row.names = F,
            col.names = F)

auc1 = vw(training_data = "diamond_train.vw", validation_data = "diamond_val.vw",
          validation_labels = "valid_labels.txt", use_perf = F)

## 2 method: use directly data.frames
auc2 = vw(training_data = dt_train, validation_data = dt_val,
          target = "y", use_perf = F)





#########################
### Validation ##########
#########################
library(caret);library(pROC)
val <- validation
p <- read.table('predictions/out.txt', header = F)
val$Y <- p[,1]
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