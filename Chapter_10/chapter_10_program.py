# Regression Modeling with California Housing Values (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for analysis and modeling
import pandas as pd  # data frame operations
from pandas.tools.plotting import scatter_matrix  # scatter plot matrix
import numpy as np  # arrays and math functions
from scipy.stats import uniform  # for training-and-test split
import statsmodels.api as sm  # statistical models (including regression)
import statsmodels.formula.api as smf  # R-like model specification
from sklearn.tree import DecisionTreeRegressor  # machine learning tree
from sklearn.ensemble import RandomForestRegressor # ensemble method

# read in the housing data with white-space delimiters
prelim_houses = pd.read_table('houses_data.txt', header = None, \
     delim_whitespace = True, skipinitialspace = True, \
     names = ['value', 'income', 'age', 'rooms', 'bedrooms', \
     'pop', 'hh', 'latitude', 'longitude'])
prelim_houses['idx'] = range(len(prelim_houses))  # for use as index
houses = prelim_houses.set_index(['idx']) 
     
print(houses.shape)  # check the structure of the data frame
print(houses.head())

# compute descriptive statistics for original variables
print(houses.describe())

# computed variables for linear model used by Pace and Barry (1997)
houses['log_value'] = np.log(houses['value'])
houses['income_squared'] = np.power(houses['income'], 2) 
houses['income_cubed'] = np.power(houses['income'], 3) 
houses['log_age'] = np.log(houses['age'])   
houses['log_pc_rooms'] = np.log(np.divide(houses['rooms'], houses['pop']))       
houses['log_pc_bedrooms'] = \
    np.log(np.divide(houses['bedrooms'], houses['pop']))                   
houses['log_pop_hh'] = np.divide(houses['pop'], houses['hh'])           
houses['log_hh'] = np.log(houses['hh'])   

# structure of the Pace and Barry (1997) model for baseline for comparisons
pace_barry_model = 'log_value ~ income + income_squared + \
    income_cubed + log_age + log_pc_rooms + log_pc_bedrooms + \
    log_pop_hh + log_hh'

# for comparison lets look at a simple model with the original variables
simple_model = 'log_value ~ income + age + rooms + bedrooms + \
    pop + hh' 
  
# original variables plus variables that add value for trees 
# that is... variables that are not simple monotonic transformations
# of the original explanatory variables
full_model = 'log_value ~ income + age + rooms + bedrooms + \
  pop + hh + log_pc_rooms + log_pc_bedrooms + log_pop_hh'  
  
# define the bounding box for selecting the area
# here we are selecting the San Diego region
BB_TOP = 33
BB_BOTTOM = 32
BB_RIGHT = -116.75
BB_LEFT = -125

houses_selected = houses[houses['latitude'] < BB_TOP]
houses_selected = houses_selected[houses_selected['longitude'] < BB_RIGHT]
houses_selected = houses_selected[houses_selected['latitude'] > BB_BOTTOM]
houses_selected = houses_selected[houses_selected['longitude'] > BB_LEFT]

# examine structure of selected block groups
print(houses_selected.shape)
print(houses_selected.head())

# employ training-and-test regimen for model validation
np.random.seed(4444)
houses_selected['runiform'] = uniform.rvs(loc = 0, scale = 1, size = len(houses_selected))
houses_selected_train = houses_selected[houses_selected['runiform'] >= 0.33]
houses_selected_test = houses_selected[houses_selected['runiform'] < 0.33]
# check training data frame
print('\nhouses_selected_train data frame (rows, columns): ',\
    houses_selected_train.shape)
print(houses_selected_train.head())
# check test data frame
print('\nhouses_selected_test data frame (rows, columns): ',\
    houses_selected_test.shape)
print(houses_selected_test.head())
        
# examine the correlations across the variables before we begin modeling
houses_train_df_vars = houses_selected_train.loc[ : ,['log_value', 'income',\
    'log_pc_rooms', 'log_pc_bedrooms', 'rooms', 'bedrooms', 'hh', \
    'age', 'pop', 'log_pop_hh']]
print(houses_train_df_vars.corr())
    
# scatter plot matrix (splom) demonstration
houses_train_splom_vars = \
    houses_selected_train.loc[:, ['log_value', 'income', 'age', 'rooms']]
scatter_matrix(houses_train_splom_vars)   

# --------------------------------------------
# Linear regression a la Pace and Barry (1997)
# --------------------------------------------
# fit the model to the training set
pace_barry_train_fit = smf.ols(pace_barry_model, \
    data = houses_selected_train).fit()
# summary of model fit to the training set
print(pace_barry_train_fit.summary())
# training set predictions from the model fit to the training set
houses_selected_train['predict_log_value'] = pace_barry_train_fit.fittedvalues
# test set predictions from the model fit to the training set
houses_selected_test['predict_log_value'] = pace_barry_train_fit.predict(houses_selected_test)

# compute the proportion of response variance for training data
pace_and_barry_train_result = \
    round(np.power(houses_selected_train['log_value']\
        .corr(houses_selected_train['predict_log_value']),2),3)
print('\nPace and Barry Proportion of Training Set Variance Accounted for: ',\
    pace_and_barry_train_result)

# compute the proportion of response variance
# accounted for when predicting out-of-sample
pace_and_barry_test_result = \
    round(np.power(houses_selected_test['log_value']\
        .corr(houses_selected_test['predict_log_value']),2),3)
print('\nPace and Barry Proportion of Test Set Variance Accounted for: ',\
    pace_and_barry_test_result)

# --------------------------------------
# Tree-structured regression (simple)
# --------------------------------------
# try tree-structured regression on the original explantory variables
# note that one of the advantages of trees is no need for transformations
# of the explanatory variables... sklearn DecisionTreeRegressor
tree_model_maker = DecisionTreeRegressor(random_state = 9999, max_depth = 5)

y_train = houses_selected_train.loc[:, ['log_value']]

# simple model has six predictors
X_train_simple = houses_selected_train.loc[:, \
    ['income', 'age', 'rooms', 'bedrooms', 'pop', 'hh']]
X_test_simple = houses_selected_test.loc[:, \
    ['income', 'age', 'rooms', 'bedrooms', 'pop', 'hh']]

tree_model_fit = tree_model_maker.fit(X_train_simple, y_train)

# compute the proportion of response variance for training data
houses_selected_train['simple_tree_predict_log_value'] =\
    tree_model_fit.predict(X_train_simple)
simple_tree_train_result = \
    round(np.power(houses_selected_train['log_value']\
        .corr(houses_selected_train['simple_tree_predict_log_value']),2),3)
print('\nSimple Tree Proportion of Training Set Variance Accounted for: ',\
    simple_tree_train_result)

# compute the proportion of response variance for test data
houses_selected_test['simple_tree_predict_log_value'] =\
    tree_model_fit.predict(X_test_simple)
simple_tree_test_result = \
    round(np.power(houses_selected_test['log_value']\
        .corr(houses_selected_test['simple_tree_predict_log_value']),2),3)
print('\nSimple Tree Proportion of Test Set Variance Accounted for: ',\
    simple_tree_test_result)

# --------------------------------------
# Tree-structured regression (full)
# --------------------------------------
# same method as for simple tree
tree_model_maker = DecisionTreeRegressor(random_state = 9999, max_depth = 5)

y_train = houses_selected_train.loc[:, ['log_value']]

# full model has more predictors
X_train_full = houses_selected_train.loc[:, \
    ['income', 'age', 'rooms', 'bedrooms',\
        'pop', 'hh', 'log_pc_rooms', 'log_pc_bedrooms', 'log_pop_hh']]
X_test_full = houses_selected_test.loc[:, \
    ['income', 'age', 'rooms', 'bedrooms',\
        'pop', 'hh', 'log_pc_rooms', 'log_pc_bedrooms', 'log_pop_hh']]

tree_model_fit = tree_model_maker.fit(X_train_full, y_train)

# compute the proportion of response variance for training data
houses_selected_train['full_tree_predict_log_value'] =\
    tree_model_fit.predict(X_train_full)
full_tree_train_result = \
    round(np.power(houses_selected_train['log_value']\
        .corr(houses_selected_train['full_tree_predict_log_value']),2),3)
print('\nFull Tree Proportion of Training Set Variance Accounted for: ',\
    full_tree_train_result)

# compute the proportion of response variance for test data
houses_selected_test['full_tree_predict_log_value'] =\
    tree_model_fit.predict(X_test_full)
full_tree_test_result = \
    round(np.power(houses_selected_test['log_value']\
        .corr(houses_selected_test['full_tree_predict_log_value']),2),3)
print('\nFull Tree Proportion of Test Set Variance Accounted for: ',\
    full_tree_test_result)

# --------------------------------------
# Random forests (simple)
# --------------------------------------
rf_model_maker = RandomForestRegressor(random_state = 9999)

y_train = houses_selected_train.loc[:, ['log_value']]

# simple model has more predictors
X_train_simple = houses_selected_train.loc[:, \
    ['income', 'age', 'rooms', 'bedrooms', 'pop', 'hh']]
X_test_simple = houses_selected_test.loc[:, \
    ['income', 'age', 'rooms', 'bedrooms', 'pop', 'hh']]

rf_model_fit = rf_model_maker.fit(X_train_simple, y_train)

# compute the proportion of response variance for training data
houses_selected_train['simple_rf_predict_log_value'] =\
    rf_model_fit.predict(X_train_simple)
simple_rf_train_result = \
    round(np.power(houses_selected_train['log_value']\
        .corr(houses_selected_train['simple_rf_predict_log_value']),2),3)
print('\nSimple Random Forest Prop Training Set Variance Accounted for: ',\
    simple_rf_train_result)

# compute the proportion of response variance for test data
houses_selected_test['simple_rf_predict_log_value'] =\
    rf_model_fit.predict(X_test_simple)
simple_rf_test_result = \
    round(np.power(houses_selected_test['log_value']\
        .corr(houses_selected_test['simple_rf_predict_log_value']),2),3)
print('\nSimple Random Forest Prop of Test Set Variance Accounted for: ',\
    simple_rf_test_result)

# --------------------------------------
# Random forests (full)
# --------------------------------------
rf_model_maker = RandomForestRegressor(random_state = 9999)

y_train = houses_selected_train.loc[:, ['log_value']]

# full model has more predictors
X_train_full = houses_selected_train.loc[:, \
    ['income', 'age', 'rooms', 'bedrooms',\
        'pop', 'hh', 'log_pc_rooms', 'log_pc_bedrooms', 'log_pop_hh']]
X_test_full = houses_selected_test.loc[:, \
    ['income', 'age', 'rooms', 'bedrooms',\
        'pop', 'hh', 'log_pc_rooms', 'log_pc_bedrooms', 'log_pop_hh']]

rf_model_fit = rf_model_maker.fit(X_train_full, y_train)

# compute the proportion of response variance for training data
houses_selected_train['full_rf_predict_log_value'] =\
    rf_model_fit.predict(X_train_full)
full_rf_train_result = \
    round(np.power(houses_selected_train['log_value']\
        .corr(houses_selected_train['full_rf_predict_log_value']),2),3)
print('\nFull Random Forest Prop of Training Set Variance Accounted for: ',\
    full_rf_train_result)

# compute the proportion of response variance for test data
houses_selected_test['full_rf_predict_log_value'] =\
    rf_model_fit.predict(X_test_full)
full_rf_test_result = \
    round(np.power(houses_selected_test['log_value']\
        .corr(houses_selected_test['full_rf_predict_log_value']),2),3)
print('\nFull Random Forest Prop of Test Set Variance Accounted for: ',\
    full_rf_test_result)

# --------------------------------------
# Geographically weighted regression
# --------------------------------------    
# exercise for the student
# use rpy2 to obtain results from R
full_gwr_train_result = None
full_gwr_test_result = None

# --------------------------------------
# Gather results for a single report
# --------------------------------------     
# measurement model performance summary
table_data = {'method' : ['Linear regression Pace and Barry (1997)',\
    'Tree-structured regression (simple model)',\
    'Tree-structured regression (full model)',\
    'Random forests (simple model)',\
    'Random forests (full model)',\
    'Geographically weighted regression (GWR)'],\
    'Training Set Result' : [pace_and_barry_train_result,\
    simple_tree_train_result,\
    full_tree_train_result,\
    simple_rf_train_result,\
    full_rf_train_result,\
    full_gwr_train_result],\
    'Test Set Result' : [pace_and_barry_test_result,\
    simple_tree_test_result,\
    full_tree_test_result,\
    simple_rf_test_result,\
    full_rf_test_result,\
    full_gwr_test_result]}
  
table_data_frame = pd.DataFrame(table_data,\
    columns = ['method', 'Training Set Result', 'Test Set Result'])  
    
print(table_data_frame)    

# --------------------------------------------------
# we have been using a simple training-and-test split for validation
# an alternative is multi-fold cross-validation, as shown here
# for the simple tree-structured regression model

from sklearn import cross_validation

# specify number of folds for multi-fold cross-validation
# a simple training-and-test regimen would have two folds
specified_n_folds = 5

# specify the modeling technique or method of analysis
tree_model_maker = DecisionTreeRegressor(random_state = 9999, max_depth = 5)

# specify the response variable 
y = houses_selected.loc[:, ['log_value']]

# specify the explanatory variables 
X = houses_selected.loc[:, ['income', 'age', 'rooms', 'bedrooms', 'pop', 'hh']]

# specify cross-validation method, including number of folds
cvfold = cross_validation.KFold(len(y), n_folds = specified_n_folds,\
    indices = False)

# initialize list for storing cross-validation results
cv_results = []

# iterate across the folds fitting to train, testing on test
for train, test in cvfold:

    # define training and test sets for this fold
    X_train, X_test, y_train, y_test = X[train], X[test], y[train], y[test]
       
    # fit to training data for this fold
    tree_model_fit = tree_model_maker.fit(X_train, y_train)
        
    # compute proportion of response variance accounted 
    # for in the test set in this fold, and add to the
    # list of cross-validation results
    y_test_predict = tree_model_fit.predict(X_test)
    cv_fold_result = np.power(np.corrcoef(y_test.T, y_test_predict),2)
    cv_results.append(cv_fold_result[0, 1])  

print('\nProportion of Training Set Variance Accounted for ',\
    'using ', specified_n_folds, 'Folds in Cross-Validation:',
    round(np.mean(cv_results) ,3))


# Suggestions for the student:
# Try alternative formulations for the linear predictor.
# Try subset selection and all possible regression approaches. 
# Try additional transformations of predictors.
# Deal with the issues of censoring and multicollinearity.
# Evaluate possible interaction effects, such as between
# median household age and occupancy (log population per household).
# Try additional regression methods, including neural networks
# and robust regression methods. Try alternative spatial data models,  
# including models built upon spatial grid or lattice structures.
# See if you can improve out-of-sample prediction performance
# of the random forest by changing the settings for model building.
# Cross-validation is an alternative to simple training-and-test
# for model validation. Implement cross-validation with sklearn
# for all of the modeling techniques in this study.
# Complete the analysis by calling geographically weighted 
# regression from R, using the Python rpy2 package.
# Explore the data further using data visualization and maps.
# Work on another metropolitan area in California.
# Determine the degree to which models built on one region
# generalize to other regions.


