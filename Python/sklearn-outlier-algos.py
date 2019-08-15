import pandas as pd
from sklearn import svm
from pyod.models.iforest import IForest



def calculate_OCSVM(DTtrain, DTtest):
  X_1 = pd.DataFrame(DTtrain)
  Xtrain = X_1.values
  clf = svm.OneClassSVM(nu=0.05, kernel="rbf", gamma=0.1)
  clf.fit(Xtrain)
  X_2 = pd.DataFrame(DTtest)
  Xtest = X_2.values
  Xtest_scores = clf.decision_function(Xtest)
  return Xtest_scores
  


def calculate_iForest_params(DTtrain, DTtest, given_nEstimators, given_maxSamples, given_maxFeatures):
  X_1 = pd.DataFrame(DTtrain)
  Xtrain = X_1.values
  clf = IForest(n_estimators = given_nEstimators, max_samples = given_maxSamples, max_features = given_maxFeatures, n_jobs = int(10), behaviour='new')
  clf.fit(Xtrain)
  X_2 = pd.DataFrame(DTtest)
  Xtest = X_2.values
  Xtest_scores = clf.decision_function(Xtest)
  return X_scores



def calculate_OCSVM_params(DTtrain, DTtest, given_nu, given_kernel, given_gamma):
  X_1 = pd.DataFrame(DTtrain)
  Xtrain = X_1.values
  clf = svm.OneClassSVM(nu=given_nu, kernel=given_kernel, gamma=given_gamma)
  clf.fit(Xtrain)
  X_2 = pd.DataFrame(DTtest)
  Xtest = X_2.values
  Xtest_scores = clf.decision_function(Xtest)
  return Xtest_scores



