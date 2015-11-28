# install.packages("devtools")
# devtools::install_github("JohnLangford/vowpal_wabbit", subdir = "R/r.vw")
setwd('/Users/ivanliu/Google Drive/Melbourne Datathon/Melbourne_Datathon_2015_Kaggle/vowpal_wabbit')
rm(list=ls()); gc()
require(data.table);library(r.vw);library(ggplot2);library(pROC)
load('../data/9_train_validation_test_20151122.RData');ls()
source('../Rscripts/Ivan_vowpal_wabbit_func.R')

# setwd where the data would be
feat <- names(train)[c(3:(ncol(train)-1))]; target <- 'flag_class'
train_dt <- to_vw(total, feat, target, 'data/train_dt.vw') # total
test_dt <- to_vw(test, feat, target, 'data/test_dt.vw') # test
write.table(test_dt$flag_class, file='data/test_labels.txt', row.names = F, col.names = F, quote = F)

training_data='data/train_dt.vw'
test_data='data/test_dt.vw'
test_labels = "data/test_labels.txt"
out_probs = "predictions/sub.txt"
model = "models/mdl.vw"

# AUC using perf - Download at: osmot.cs.cornell.edu/kddcup/software.html
# Shows files in the working directory: /data
list.files('data/')
grid = expand.grid(eta=c(0.15, 0.25, 0.5),
                   extra=c(#'--holdout_period 2 --normalized --adaptive --invariant', 
                           #'--holdout_period 5 --normalized --adaptive --invariant', 
                           '--holdout_period 10 --normalized --adaptive --invariant', 
                           #'--nn 60 --holdout_period 2 --normalized --adaptive --invariant', 
                           #'--nn 60 --holdout_period 5 --normalized --adaptive --invariant',
                           '--nn 60 --holdout_period 10 --normalized --adaptive --invariant',
                           '-q:: --holdout_period 5 --normalized --adaptive --invariant',
                           '-q:: --holdout_period 10 --normalized --adaptive --invariant',
                           '-q:: --holdout_period 2 --normalized --adaptive --invariant'))
for(i in 1:nrow(grid)){
    g = grid[i, ]
    out_probs = paste0("predictions/submission_vw_20151126_", g[['eta']], "_", i,".txt")
    auc = vw(training_data, test_data, loss = "logistic",
             model, b = 30, learning_rate = 0.5,#g[['eta']], 
             passes = 50, l1=NULL, l2=NULL, early_terminate = 10,
             link_function = "--link=logistic", extra = '--ksvm --kernel linear',#g[['extra']],
             out_probs = out_probs, validation_labels = test_labels, verbose = TRUE, 
             do_evaluation = F, use_perf=FALSE, plot_roc=F)
    #extra='--decay_learning_rate 0.9 --ksvm --kernel linear -q ::'
    # print(auc)
    # [1] 0.7404759
    # 0.7749233 'nn 80'
}

# AUC using pROC - Saving plots to disk
### create a parameter grid
grid = expand.grid(l1=c(1e-06),
                   l2=c(1e-06),
                   eta=c(0.1, 0.2),
                   ps=c(12,18),
                   extra=c('--nn 120', '--nn 80'))


cat('Running grid search\n')
pdf('output/ROCs.pdf')
aucs <- lapply(1:nrow(grid), function(i){
    g = grid[i, ]
    auc = vw(training_data=training_data, # files relative paths
             validation_data=test_data,
             validation_labels=test_labels, model=model,
             # grid options
             loss='logistic', b=30, learning_rate=g[['eta']],
             passes=g[['ps']], l1=g[['l1']], l2=g[['l2']],
             early_terminate=2, extra=g[['extra']],
             # ROC-AUC related options
             use_perf=FALSE, plot_roc=TRUE,
             do_evaluation = TRUE # If false doesn't compute AUC, use only for prediction
    )
    auc
})
dev.off()

results = cbind(iter=1:nrow(grid), grid, auc=do.call(rbind, aucs))
print(results)
# iter    l1    l2  eta ps   extra       auc
# 1    1 1e-06 1e-06 0.05  6 --nn 30 0.7403335
# 2    2 1e-06 1e-06 0.15  6 --nn 30 0.7604067
# 3    3 1e-06 1e-06 0.05 12 --nn 30 0.7403335
# 4    4 1e-06 1e-06 0.15 12 --nn 30 0.7654396
# 5    5 1e-06 1e-06 0.05  6 --nn 80 0.7403404
# 6    6 1e-06 1e-06 0.15  6 --nn 80 0.7652404
# 7    7 1e-06 1e-06 0.05 12 --nn 80 0.7403404
# 8    8 1e-06 1e-06 0.15 12 --nn 80 0.7702607

# 1    1 1e-06 1e-06 0.1 12 --nn 120 0.7661254
# 2    2 1e-06 1e-06 0.2 12 --nn 120 0.7736231
# 3    3 1e-06 1e-06 0.1 18 --nn 120 0.7695463
# 4    4 1e-06 1e-06 0.2 18 --nn 120 0.7747579
# 5    5 1e-06 1e-06 0.1 12  --nn 80 0.7645808
# 6    6 1e-06 1e-06 0.2 12  --nn 80 0.7728860
# 7    7 1e-06 1e-06 0.1 18  --nn 80 0.7678433
# 8    8 1e-06 1e-06 0.2 18  --nn 80 0.7741317

p = ggplot(results, aes(iter, auc, color=extra)) +
    geom_point(size=3) +
    theme_bw() +
    labs(list(x='Iteration', y='AUC',
              title='Logistic regression results'))

print(p)
ggsave('output/results_plot.png', plot=p)
