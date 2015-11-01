# Is betting just guessing (do odds reflect the true probability)?

This competition is for those who entered the Data Science Melbourne 2015 Datathon.

You will already have the data for all games upto the semi-finals and finals. The task is to use this historical data to rank order the punters on their profit for the final 3 games of the tournament (which is why we didn't give you this data).

We provide the list of Account_IDs to make predictions for, along with some limited features for the final 3 games that you may make use of.

The objective is to determine if betting is just guessing, or if past performance can be indicative of future performance. We expect this to be very hard, and will be impressed if anyone can come up with an algorithm that is better than a random number generator! 

### Evaluation

We are treating this as a binary classification problem - did the account make a profit or not. The evaluation metric is the AUC.

An AUC of 0.5 is random guessing and 1 is a prefect solution.

### Steps

1. New member imputation (test median)
2. Feature selection
	1. PCA
    2. Centering and Scaling or Log transform
3. One-hot (pca, scale)
4. Evaluation formula (B&L classifier)
5. h2o (gbm, rf, nnet, nb, glm)
6. caret (svm)
7. xgboost

### Ideas
1. Win rate distributions

### Further improvements
1. Raw data (no transform for tree model)
2. Sparse (one-hot for svm, logit)
3. Weighted target formula
4. Logit + xgboost (blend)
5. Deep learning
6. Separate Models for Old & Existing Account IDs

7. All last events attributes (sparsity)
8. Difference between B & L