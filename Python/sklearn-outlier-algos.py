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
  


def calculate_iForest(given_DT, given_nEstimators, given_maxSamples, given_maxFeatures):
  X_1 = pd.DataFrame(given_DT)
  X = X_1.values
  clf = IForest(n_estimators = given_nEstimators, max_samples = given_maxSamples, max_features = given_maxFeatures, n_jobs = 10)
  clf.fit(X)
  X_scores = clf.decision_scores_#clf.decision_function(XX_1)
  return X_scores




