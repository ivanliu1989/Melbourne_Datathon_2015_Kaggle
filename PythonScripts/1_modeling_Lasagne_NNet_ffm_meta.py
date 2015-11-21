# -*- coding: utf-8 -*-
"""
Created on Fri Oct 20 23:18:38 2015

@author: Ivan
"""
import theano
import numpy as np
import pandas as pd
from util import float32
from sklearn.preprocessing import LabelEncoder
from lasagne.layers import DenseLayer, InputLayer, DropoutLayer
from lasagne.nonlinearities import softmax, rectify, leaky_rectify
from lasagne.updates import nesterov_momentum, adagrad, rmsprop, adadelta, adam
from nolearn.lasagne import NeuralNet
from adjust_variable import AdjustVariable
from early_stopping import EarlyStopping

def load_train_data(path):
    df = pd.read_csv(path)
    X = df.values.copy()
    np.random.shuffle(X)
    X, labels = X[:, 2:46].astype(np.float32), X[:, 47]
    encoder = LabelEncoder()
    y = encoder.fit_transform(labels).astype(np.int32)
    return X, y, encoder
    
def load_test_data(path):
    df = pd.read_csv(path)
    X = df.values.copy()
    X, ids = X[:, 2:46].astype(np.float32), X[:, 0:1].astype(int)
    return X, ids
    
def make_submission(clf, X_test, ids, encoder, name='lasagne_nnet.csv'):
    y_prob = net0.predict_proba(X_test)
    np.savetxt(name, y_prob, delimiter=",")
    print("Wrote submission to file {}.".format(name))

# Load Data    
np.random.seed(888888)
X, y, encoder = load_train_data('../../python_train_ffm_meta.csv')
X_test, ids = load_test_data('../../python_validation_ffm_meta.csv')
num_classes = len(encoder.classes_)
num_features = X.shape[1]

num_rows = X.shape[0]

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
               #('dense3', DenseLayer),
               #('dropout3', DropoutLayer),
               ('output', DenseLayer)]
               
    net0 = NeuralNet(layers=layers0,                 
                     input_shape=(None, num_features),
                     
                     dropoutf_p=0.15,
    
                     dense0_num_units=800,
                     dense0_nonlinearity=leaky_rectify, # leaky_rectify, rectify
                     #dense0_W=lg.init.Uniform(),
    
                     dropout0_p=0.25,
    
                     dense1_num_units=500,
                     dense1_nonlinearity=leaky_rectify,
                     #dense1_W=lg.init.Uniform(),
    
                     dropout1_p=0.25,
                     
                     dense2_num_units=300,
                     dense2_nonlinearity=leaky_rectify,
                     #dense2_W=lg.init.Uniform(),
                     
                     dropout2_p=0.25,
                     
                     #dense3_num_units=100,
                     #dense3_nonlinearity=rectify,
                     #dense3_W=lg.init.Uniform(),
                     
                     #dropout3_p=0.25,
                     
                     output_num_units=num_classes,
                     output_nonlinearity=softmax,
                     #output_W=lg.init.Uniform(),
    
                     #update=nesterov_momentum,
                     #update_learning_rate=theano.shared(float32(0.01)),
                     #update_momentum=theano.shared(float32(0.9)),
                                          
                     update=adagrad,
                     update_learning_rate=theano.shared(float32(0.01)),
                     #update_epsilon=1e-06,
                     
                     #update=adadelta,
                     #update_learning_rate=theano.shared(float32(0.01)),
                     
                     #update=adam,
                     #update_learning_rate=theano.shared(float32(0.01)),
                     
                     on_epoch_finished=[
                            AdjustVariable('update_learning_rate', start=0.015, stop=0.0001),
                            #AdjustVariable('update_momentum', start=0.9, stop=0.999),
                            EarlyStopping(patience=20)
                            ],
                     
                     eval_size=0.2,
                     verbose=1,
                     max_epochs=10000)
                     
    net0.fit(X, y)
    # 0.467144 0.15 800 0.25 500 0.25 300 0.25 | 0.015
    # 0.471722 0.15 800 0.25 500 0.25 300 0.25 100 0.25
    # 0.468020 0.15 800 0.25 500 0.25 300 0.25 | 0.015, leaky_rectify, adagrad
    
    # Submission 
    make_submission(net0, X_test, ids, encoder, name='lasagne/lasagne_3L_ffm_meta_'+str(i)+'.csv')