# -*- coding: utf-8 -*-
"""
Created on Fri Nov 20 21:40:32 2015

@author: ivanliu
"""
import graphlab as gl
import numpy as np
import pandas as pd
from util import float32
from sklearn.preprocessing import LabelEncoder
import ffm
    
train = gl.SFrame(data='../../python_train.csv')
test = gl.SFrame(data='../../python_validation.csv')

train.show()

# Save 
train.save('../../train.sframe')
test.save('../../validation.sframe')

# Features
features = [c for c in train.column_names() if c != 'y']

# Train a model
m = ffm.FFM(lam=.1, eta=.1)
m.fit(train, test, target='flag_class', features=features, nr_iters=50)
yhat = m.predict(test)