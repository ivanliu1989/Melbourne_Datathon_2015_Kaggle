setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle')
# setwd('C:\\Users\\iliu2\\Documents\\datathon\\Melbourne_Datathon_2015_Kaggle')
rm(list=ls()); gc()
path <- 'ReadyForBlending/submission/randomforest/'
file.names <- list.files(path)


for(file in 1:length(file.names)){
    p <- read.csv(paste0(path, file.names[file]))
    if(file == 1){pred <- p}
    else{
        pred[,2]<-pred[,2]+p[,2]
        if(file==length(file.names)) pred[,2] <- pred[,2]/file
        }
}
head(pred)
write.csv(pred,'ReadyForBlending/submission/submission_20151103_h2o_rf_blend.csv',quote = FALSE,row.names = FALSE)
