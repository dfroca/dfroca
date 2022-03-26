# -*- coding: utf-8 -*-
"""
Created on Mon Feb 28 14:21:31 2022

@author: Abhinav Singh & Daniel Roca
"""
from sklearn.datasets import make_moons, make_regression
import numpy as np
from DecisionTreeImplementation import DecisionTree
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor


# Choose the task of the tree (either classification or regression)
# task = "classification"
task = "regression"

# Choose the data, in this we can use either numpy array or pandas dataframe
# The decision tree takes the last column of the pandas dataframe as the label
# and all the other columns as predicting variables.

# ------------------------------------
# Classification Test Data
# -------------------------------------
#N = 2000
#X, y = make_moons(N, noise=0.2)
#y = np.reshape(y, (N, 1))
#data = np.append(X, y, axis=1)
#train_data = data[:1500, :]
#val_data = data[1500:, :]

# -------------------------------------
# Regression Test Data
# -------------------------------------
N = 2000
X, y = make_regression(n_samples=N, n_features=3)
y = np.reshape(y, (N, 1))
data = np.append(X, y, axis=1)
train_data = data[:1500, :]
val_data = data[1500:, :]


dt = DecisionTree(train_data, task, max_depth=3)

# Train the decision tree with cross-entropy method
dt.train()

# After this, the dt has the trained tree. Now we can make predictions with data that
# has the same structure

#data_predict = test_data
predictions = dt.predict(val_data, val='validation')
#print(predictions)
acc = dt.evaluation(val_data)
print(acc)

# Check after pruning
#print("Results after pruning")
#dt.post_pruning(tree=None, train_data=train_data, val_data=val_data, ml_task=task)
#dt.print()
#data_predict = test_data
#predictions = dt.predict(data_predict, val='validation')
#print(predictions)
#acc = dt.evaluation(data_predict)
#print(acc)


# Sklearn
regressor = DecisionTreeRegressor(max_depth=3)
regressor.fit(train_data[:, :-1], train_data[:, -1])
y_pred = regressor.predict(val_data[:, :-1])
rmse = np.sqrt(np.mean((y_pred-val_data[:, -1])**2))
print("RMSE: ", rmse)

#classifier = DecisionTreeClassifier(max_depth=3)
#classifier.fit(train_data[:, :-1], train_data[:, -1])
#y_pred = classifier.predict(val_data[:, :-1])
#correct_results = val_data[:, -1] == y_pred
#accuracy = correct_results.mean()
#print(accuracy)

# Draw the resulting tree
# dt.draw_tree()


