# Analysis of Economic Time Series (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for time series analysis and modeling
import pandas as pd  # data structures for time series analysis
import datetime  # date manipulation
import matplotlib.pyplot as plt
from statsmodels.tsa.arima_model import ARIMA  # time series modeling
from statsmodels.tsa.stattools import grangercausalitytests as granger

# additional time series functions available in R
# from rpy2.robjects import r  # interface from Python to R

# Economic time series were originally obtained from
# the Federal Reserve Bank of St. Louis (FRED system).
#
# National Civilian Unemployment Rate (monthly, percentage)
#     converted to the employment rate ER = 100 - UNRATENSA
#
# Manufacturers' New Orders: Durable Goods (millions of dollars) 
#     DGO = DGORDER/1000 expressed in  billions of dollars 
#
# University of Michigan Index of Consumer Sentiment (1Q 1966 = 100)
#     ICS = UMCSENT
#
# New Homes Sold in the US, not seasonally adjusted (monthly, millions)
#     NHS = HSN1FNSA   

# read data in from comma-delimited text files
ER_data_frame = pd.read_csv("FRED_ER_data.csv")
DGO_data_frame = pd.read_csv("FRED_DGO_data.csv")
ICS_data_frame = pd.read_csv("FRED_ICS_data.csv")
NHS_data_frame = pd.read_csv("FRED_NHS_data.csv")

# identify date fields as dates with apply and lambda function
ER_data_frame['date'] = \
    ER_data_frame['date']\
    .apply(lambda d: datetime.datetime.strptime(str(d), '%Y-%m-%d'))
DGO_data_frame['date'] = \
    DGO_data_frame['date']\
    .apply(lambda d: datetime.datetime.strptime(str(d), '%Y-%m-%d'))
ICS_data_frame['date'] = \
    ICS_data_frame['date']\
    .apply(lambda d: datetime.datetime.strptime(str(d), '%Y-%m-%d'))
NHS_data_frame['date'] = \
    NHS_data_frame['date']\
    .apply(lambda d: datetime.datetime.strptime(str(d), '%Y-%m-%d'))

# create data frames indexed by date
ER_data = ER_data_frame.set_index(['date']) 
DGO_data = DGO_data_frame.set_index(['date']) 
ICS_data = ICS_data_frame.set_index(['date']) 
NHS_data = NHS_data_frame.set_index(['date']) 

# plot the individual time series
# National Civilian Employment Rate
fig, axis = plt.subplots()
axis = fig.add_subplot(1, 1, 1)
axis.set_xlabel('Date')
axis.set_ylabel('Employment Rate (100 = Unemployment Rate)')
axis.set_title('National Civilian Employment Rate')
ER_data['ER'].plot(ax = axis, style = 'k-')
plt.savefig('fig_ER_Python.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  

# Manufacturers New Orders: Durable Goods (billions of dollars) 
fig, axis = plt.subplots()
axis = fig.add_subplot(1, 1, 1)
axis.set_xlabel('Date')
axis.set_ylabel('Durable Goods Orders (billions of dollars)')
axis.set_title\
    ('Manufacturers New Orders: Durable Goods (billions of dollars)')
DGO_data['DGO'].plot(ax = axis, style = 'k-')
plt.savefig('fig_DGO_Python.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  

# University of Michigan Index of Consumer Sentiment (1Q 1966 = 100)
fig, axis = plt.subplots()
axis = fig.add_subplot(1, 1, 1)
axis.set_xlabel('Date')
axis.set_ylabel('Index of Consumer Sentiment (1Q 1966 = 100)')
axis.set_title\
    ('University of Michigan Index of Consumer Sentiment (1Q 1966 = 100)')
ICS_data['ICS'].plot(ax = axis, style = 'k-')
plt.savefig('fig_ICS_Python.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  

# New Homes Sold in the US, not seasonally adjusted (monthly, millions)
fig, axis = plt.subplots()
axis = fig.add_subplot(1, 1, 1)
axis.set_xlabel('Date')
axis.set_ylabel('New Homes Sold (millions)')
axis.set_title('New Homes Sold (millions)')
NHS_data['NHS'].plot(ax = axis, style = 'k-')
plt.savefig('fig_NHS_Python.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  

# merge the time series data frames
economic_mts = pd.merge(ER_data, DGO_data,\
    how = 'outer', left_index = True, right_index = True)
economic_mts = pd.merge(economic_mts, ICS_data,\
    how = 'outer', left_index = True, right_index = True)
economic_mts = pd.merge(economic_mts, NHS_data,\
    how = 'outer', left_index = True, right_index = True)
print(economic_mts.shape)

# select dates with complete data on all four series
modeling_mts = economic_mts.dropna()
print(modeling_mts.head)

# select time series for multiple time series plot
initial_plotting_mts = \
    pd.DataFrame(modeling_mts, columns = ["ER","DGO","ICS","NHS"])
print(initial_plotting_mts.head)

# create multiple time series plot
initial_plotting_mts.plot(subplots = True, style = 'k-', sharex = True,)
plt.legend(loc = 'best')
plt.xlabel('')

# using March 1997 as reference data 
print(modeling_mts.ix['1997-03-01'])  # (ICS = 100 on this date)
# define indexing constants 
indexing_constant = modeling_mts.ix['1997-03-01']
ER0 = indexing_constant['ER']
DGO0 = indexing_constant['DGO']
NHS0 = indexing_constant['NHS']

# compute indexed time series
modeling_mts['IER'] = \
    modeling_mts['ER'].apply(lambda d: (d/ER0) * 100) 
modeling_mts['IDGO'] = \
    modeling_mts['DGO'].apply(lambda d: (d/DGO0) * 100) 
modeling_mts['INHS'] = \
    modeling_mts['NHS'].apply(lambda d: (d/NHS0) * 100) 

# create working multiple time series with just the indexed series
working_economic_mts = \
    pd.DataFrame(modeling_mts, columns = ["IER","IDGO","ICS","INHS"])
print(working_economic_mts.head)

# create multiple time series plot
working_economic_mts.plot(subplots = True,  \
     sharex = True, sharey = True, style = 'k-')
plt.legend(loc = 'best')
plt.xlabel('')
plt.savefig('fig_economic_time_series_indexed_Python.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='w', edgecolor='b', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  

# return to the individual economic time series prior to indexing  
# functions from statsmodels package for time series forecasting 

# ARIMA model search for the Employment Rate
# ignoring seasonal adjustments
# loop across alternative settings for p and q 
# p is order of autoregressive process (1 or 2)
# q is order of moving-average process (1 or 2)
# choose model with lowest AIC
print('\nER_arima_model Search')
for pindex in range(2):
    for qindex in range(2):
        p = pindex + 1
        q = qindex + 1
        ER_arima_model = ARIMA(ER_data['ER'], order = (p,1,q)).fit()
        print('AR:', p, 'MA:', q, 'AIC:', ER_arima_model.aic)
# for first differenced ER models searched, AR p=2 MA q=2 is best
ER_arima_model_selected = ARIMA(ER_data['ER'], order = (2,1,2)).fit()
# fitted parameters of the selected model
print(ER_arima_model_selected.params)
# look-ahead forecasts needed 
        
# ARIMA model search for the Durable Goods Orders
# ignoring seasonal adjustments
# loop across alternative settings for p and q 
# p is order of autoregressive process (1 or 2)
# q is order of moving-average process (1 or 2)
# choose model with lowest AIC
print('\nDGO_arima_model Search')
for pindex in range(2):
    for qindex in range(2):
        p = pindex + 1
        q = qindex + 1
        DGO_arima_model = ARIMA(DGO_data['DGO'], order = (p,1,q)).fit()
        print('AR:', p, 'MA:', q, 'AIC:', DGO_arima_model.aic)        
# for first differenced DGO models searched, AR p=1 MA q=2 is best
DGO_arima_model_selected = ARIMA(DGO_data['DGO'], order = (1,1,2)).fit()
# fitted parameters of the selected model
print(DGO_arima_model_selected.params)
# look-ahead forecasts needed 

# ARIMA model search for the Index of Consumer Sentiment
# ignoring seasonal adjustments
# loop across alternative settings for p and q 
# p is order of autoregressive process (1 or 2)
# q is order of moving-average process (1 or 2)
# choose model with lowest AIC
print('\nICS_arima_model Search')
for pindex in range(2):
    for qindex in range(2):
        p = pindex + 1
        q = qindex + 1
        ICS_arima_model = ARIMA(ICS_data['ICS'], order = (p,0,q)).fit()
        print('AR:', p, 'MA:', q, 'AIC:', ICS_arima_model.aic)
# for ICS models searched, AR p=2 MA q=2 is best
ICS_arima_model_selected = ARIMA(ICS_data['ICS'], order = (2,0,2)).fit()
# fitted parameters of the selected model
print(ICS_arima_model_selected.params)
# look-ahead forecasts needed 

# ARIMA model search for New Homes Sold
# ignoring seasonal adjustments
# loop across alternative settings for p and q 
# p is order of autoregressive process (1 or 2)
# q is order of moving-average process (1 or 2)
# choose model with lowest AIC
print('\nNHS_arima_model Search')
for pindex in range(2):
    for qindex in range(2):
        p = pindex + 1
        q = qindex + 1
        NHS_arima_model = ARIMA(NHS_data['NHS'], order = (p,1,q)).fit()
        print('AR:', p, 'MA:', q, 'AIC:', NHS_arima_model.aic)
# for first differenced NHS models searched, AR p=2 MA q=2 is best 
NHS_arima_model_selected = ARIMA(NHS_data['NHS'], order = (2,1,2)).fit()
# fitted parameters of the selected model
print(NHS_arima_model_selected.params)
# look-ahead forecasts needed 

# Which regressors have potential as leading indicators?
# look for relationships across three of the time series
# using the period of overlap for those series

# does time series in second column "cause" time series in first column
print('Granger Tests')
# R form of test: grangertest(ICS~ER, order = 3, data=modeling.mts)
ICS_from_ER =  pd.DataFrame(modeling_mts, columns = ['ICS','ER'])
test = granger(ICS_from_ER, maxlag = 3, addconst=True, verbose=False)
print('ICS_from_ER:',test[3][0]['params_ftest'])

# R form of test: grangertest(ICS~DGO, order = 3, data=modeling.mts)
ICS_from_DGO =  pd.DataFrame(modeling_mts, columns = ['ICS','DGO'])
test = granger(ICS_from_DGO, maxlag = 3, addconst=True, verbose=False)
print('ICS_from_DGO:',test[3][0]['params_ftest'])

# R form of test: grangertest(DGO~ER, order = 3, data=modeling.mts)
DGO_from_ER =  pd.DataFrame(modeling_mts, columns = ['DGO','ER'])
test = granger(DGO_from_ER, maxlag = 3, addconst=True, verbose=False)
print('DGO_from_ER:',test[3][0]['params_ftest'])

# R form of test: grangertest(DGO~ICS, order = 3, data=modeling.mts)
DGO_from_ICS =  pd.DataFrame(modeling_mts, columns = ['DGO','ICS'])
test = granger(DGO_from_ICS, maxlag = 3, addconst=True, verbose=False)
print('DGO_from_ICS:',test[3][0]['params_ftest'])

# R form of test: grangertest(ER~DGO, order = 3, data=modeling.mts)
ER_from_DGO =  pd.DataFrame(modeling_mts, columns = ['ER','DGO'])
test = granger(ER_from_DGO, maxlag = 3, addconst=True, verbose=False)
print('ER_from_DGO:',test[3][0]['params_ftest'])

# R form of test: grangertest(ER~ICS, order = 3, data=modeling.mts)
ER_from_ICS =  pd.DataFrame(modeling_mts, columns = ['ER','ICS'])
test = granger(ER_from_ICS, maxlag = 3, addconst=True, verbose=False)
print('ER_from_ICS:',test[3][0]['params_ftest'])

# Suggestions for the student:
# Explore additional forecasting methods such as exponential smoothing.
# Explore dynamic linear models and state space approaches.
# Gather data on additional economic measures that might be regarded
# as leading indicators. Select an industry to study, examine relevant 
# economic indicators and possible relationships to financial performance 
# of companies within that industry (stock prices or returns).

