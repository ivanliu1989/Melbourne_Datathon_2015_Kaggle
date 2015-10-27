setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()

load('data/mbr_event_data.RData')
ls()


library(caret)
# Config
mbr.event$flag_class <- as.factor(mbr.event$flag_class)

fitControl <- trainControl(method = "cv",
                           number = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)
# Grid <-  expand.grid(n.trees = 150, interaction.depth = 6, shrinkage = 0.02)
Grid <-  expand.grid(mtry=6)

# Training
set.seed(825)
fit <- train(flag_class ~ ., data=mbr.event[,-c(1,2,49)], # classification
                   method = "rf",
                   trControl = fitControl,
                   tuneGrid = Grid,
                   # preProcess = c('center', 'scale'),
                   metric ='ROC',
                   verbose = T)


# Plot
trellis.par.set(caretTheme())
plot(fit)

# Variable Imp
fitImp <- varImp(fit, scale = T)
fitImp[1]