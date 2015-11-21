# -*- coding: utf-8 -*-
"""
Created on Fri Oct 20 23:18:38 2015

@author: Ivan
"""
import theano
import lasagne as lg
import numpy as np
import pandas as pd
from util import float32
from sklearn.preprocessing import LabelEncoder
from lasagne.layers import DenseLayer
from lasagne.layers import InputLayer
from lasagne.layers import DropoutLayer
from lasagne.nonlinearities import softmax
from lasagne.nonlinearities import rectify
from lasagne.updates import nesterov_momentum
from nolearn.lasagne import NeuralNet
from adjust_variable import AdjustVariable
from early_stopping import EarlyStopping

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
Comb = np.append(X, X_test, axis=0)
#pca = PCA()
#Comb = pca.fit_transform(Comb)
#X = Comb[:num_rows,:]
#X_test = Comb[num_rows:,:]

# Train
for i in range(1,31):
    
    layers0 = [('input', InputLayer),
               ('dropoutf', DropoutLayer),
               ('dense0', DenseLayer),
               ('dropout0', DropoutLayer),
               ('dense1', DenseLayer),
               ('dropout1', DropoutLayer),
               ('dense2', DenseLayer),
               ('dropout2', DropoutLayer),
               ('dense3', DenseLayer),
               ('dropout3', DropoutLayer),
               ('output', DenseLayer)]
               
    net0 = NeuralNet(layers=layers0,                 
                     input_shape=(None, num_features),
                     
                     dropoutf_p=0.15,
    
                     dense0_num_units=800,
                     dense0_nonlinearity=rectify,
                     #dense0_W=lg.init.Uniform(),
    
                     dropout0_p=0.25,
    
                     dense1_num_units=500,
                     dense1_nonlinearity=rectify,
                     #dense1_W=lg.init.Uniform(),
    
                     dropout1_p=0.25,
                     
                     dense2_num_units=300,
                     dense2_nonlinearity=rectify,
                     #dense2_W=lg.init.Uniform(),
                     
                     dropout2_p=0.25,
                     
                     dense3_num_units=100,
                     dense3_nonlinearity=rectify,
                     #dense3_W=lg.init.Uniform(),
                     
                     dropout3_p=0.25,
                     
                     output_num_units=num_classes,
                     output_nonlinearity=softmax,
                     #output_W=lg.init.Uniform(),
    
                     update=nesterov_momentum,
                     #update=adagrad,
                     update_learning_rate=theano.shared(float32(0.01)),
                     update_momentum=theano.shared(float32(0.9)),
                     
                     on_epoch_finished=[
                            AdjustVariable('update_learning_rate', start=0.015, stop=0.001),
                            AdjustVariable('update_momentum', start=0.9, stop=0.999),
                            EarlyStopping(patience=20)
                            ],
                     
                     eval_size=0.2,
                     verbose=1,
                     max_epochs=150)
                     
    net0.fit(X, y)
    # 0.467144 0.15 800 0.25 500 0.25 300 0.25 | 0.015
    # 0.474623 0.15 1200 0.25 800 0.25 500 0.25 | 0.015
    # 0.473870 0.15 800 0.25 500 0.25 300 0.25 | 0. leaky_recify
    # 0.473185 0.15 800 0.25 500 0.25 300 0.25 | 0. lg.init.uniform()
    
    # Submission 
    make_submission(net0, X_test, ids, encoder, name='lasagne/lasagne_4L_800_500_300_0015'+str(i)+'.csv')