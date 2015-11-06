# Is betting just guessing (do odds reflect the true probability)?

This competition is for those who entered the Data Science Melbourne 2015 Datathon.

You will already have the data for all games upto the semi-finals and finals. The task is to use this historical data to rank order the punters on their profit for the final 3 games of the tournament (which is why we didn't give you this data).

We provide the list of Account_IDs to make predictions for, along with some limited features for the final 3 games that you may make use of.

The objective is to determine if betting is just guessing, or if past performance can be indicative of future performance. We expect this to be very hard, and will be impressed if anyone can come up with an algorithm that is better than a random number generator! 

### Evaluation

We are treating this as a binary classification problem - did the account make a profit or not. The evaluation metric is the AUC.

An AUC of 0.5 is random guessing and 1 is a prefect solution.

##### 1st submission - 0.60778 (+0.00, +0.00%)
1. basic features
2. random forest
3. weighted profit formula

##### 2nd submission - 0.62711 (+0.01933, +3.18%)
1. new features (BL ratio, cancel ratio etc.)
2. average profit formula

##### 3rd submission - 0.63243 (+0.00532, +0.85%)
1. new features (difference between L and B)
2. xgboost

##### 4th submission - 0.64118 (+0.00875, +1.384%)
1. new feature (invest amount)
2. blended models

##### 5th submission - 0.62621/0.64088
1. X New benchmark (past history by game) 
2. X Log transformation
3. X K-means (transactional features & customized imputation)
4. X Feature selection
5. X Multi-rounds
6. X New Calculation

##### 6th submission - 0.
1. Event Counts / Bag of Event
2. GBDT latent factors (meta data)
3. Subset modeling
4. Win hist weigeted calculation
5. Kmeans clustering based imputation

##### 7th submission - 0.
1. Factorization Machines (http://www.csie.ntu.edu.tw/~r01922136/libffm/)
2. Hash tricks

##### 8th submission - 0.
1. New feature inventation
2. Separate models (new/existing customers)
3. Blending strategies

##### Ref
1. https://github.com/Gzsiceberg/kaggle-avito
2. entropy based features