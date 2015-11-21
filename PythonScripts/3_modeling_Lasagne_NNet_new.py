# -*- coding: utf-8 -*-
"""
Created on Sat Nov 21 14:56:25 2015

@author: ivanliu
"""

import numpy as np
import theano
import theano.tensor as T
import lasagne
from sklearn.preprocessing import LabelEncoder
import pandas as pd

def load_train_data(path):
    df = pd.read_csv(path)
    X = df.values.copy()
    np.random.shuffle(X)
    X, labels = X[:, 2:43].astype(np.float32), X[:, 47]
    encoder = LabelEncoder()
    y = encoder.fit_transform(labels).astype(np.int32)
    return X, y, encoder
    
def load_test_data(path):
    df = pd.read_csv(path)
    X = df.values.copy()
    X, ids = X[:, 2:43].astype(np.float32), X[:, 0:1].astype(int)
    return X, ids
    
def make_submission(clf, X_test, ids, encoder, name='lasagne_nnet.csv'):
    y_prob = net0.predict_proba(X_test)
    np.savetxt(name, y_prob, delimiter=",")
    print("Wrote submission to file {}.".format(name))

# Load Data    
np.random.seed(888888)
X, y, encoder = load_train_data('../../python_train.csv')
X_test, ids = load_test_data('../../python_test.csv')
num_classes = len(encoder.classes_)
num_features = X.shape[1]
num_rows = X.shape[0]

# NNETS
def build_mlp(input_var=None):
    l_hid1_drop = lasagne.layers.DropoutLayer(l_hid1, p=0.5)

    l_hid2 = lasagne.layers.DenseLayer(
            l_hid1_drop, num_units=800,
            nonlinearity=lasagne.nonlinearities.rectify)
    
    l_hid2_drop = lasagne.layers.DropoutLayer(l_hid2, p=0.5)
    
    l_out = lasagne.layers.DenseLayer(
        l_hid2_drop, num_units=10,
        nonlinearity=lasagne.nonlinearities.softmax)
        
    return l_out