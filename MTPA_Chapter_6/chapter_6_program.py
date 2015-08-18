# Workforce Scheduling for Anonymous Bank Call Center (Python)

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for analysis and modeling
import pandas as pd  # data frame operations
import numpy as np  # arrays and math functions
import datetime
from rpy2.robjects import r  # interface from Python to R

# Erlang C queueing theory  
# input c = number of servers (positive integer)
#       r = ratio of arrival rate over service rate 
# output = probability of waiting in queue (min 0, max 1)
# adapted from Pedro Canadilla (2014) function 
# C_erlang in the R queueing package 
def erlang_C (c = 1, r = 0):
    if (c <= 0):
        return(1)
    if (r <= 0):
        return(0)
    c = int(c)    
    tot = 1
    for i in range(c-1):
        i = i + 1
        tot = 1 + (tot * i * (1/r))
    return(max(0, min(1, (r * (1/tot)) / (c - (r * (1 - (1/tot)))))))
    
# focus upon February 1999
call_center_input_data = pd.read_table('data_anonymous_bank_february.txt')
# examine the structure of these data
print(call_center_input_data.head)

# delete PHANTOM calls
call_center_data = \
    call_center_input_data[call_center_input_data['outcome'] != 'PHANTOM']

# negative VRU times make no sense... drop these rows from data frame
call_center_data = call_center_data[call_center_data['vru_time'] >= 0]

# calculate wait time as sum of vru_time and q_time
call_center_data['wait_time'] = call_center_data['vru_time'] + \
    call_center_data['q_time']

# define date variable with apply and lambda function
call_center_data['date'] = \
    call_center_data['date']\
    .apply(lambda d: datetime.datetime.strptime(str(d), '%y%m%d'))

# define day of week as an integer 0 = Monday 6 = Sunday
call_center_data['day_of_week'] = \
    call_center_data['date'].apply(lambda d: d.weekday())
# use dictionary object for mapping day_of_week to string
day_of_week_to_string = {0 : 'Monday', 
     1 : 'Tuesday', 
     2 : 'Wednesday', 
     3 : 'Thursday', 
     4 : 'Friday',
     5 : 'Saturday',
     6 : 'Sunday'}
call_center_data['day_of_week'] = \
    call_center_data['day_of_week'].map(day_of_week_to_string)
# check structure and contents of the data frame
print(call_center_data.head)

# examine frequency of calls by day of week
print(call_center_data['day_of_week'].value_counts())

# identify the hour of entry into the system
call_center_data['vru_entry'] = \
    call_center_data['vru_entry']\
    .apply(lambda d: datetime.datetime.strptime(str(d), '%H:%M:%S'))
call_center_data['call_hour'] = \
    call_center_data['vru_entry'].apply(lambda d: d.hour)

# check frequency of calls in February by hour and day of week
# note that pandas alphabetizes on output 
print(pd.crosstab(call_center_data['day_of_week'],\
    call_center_data['call_hour'], margins = False))

# create an ordered table for Frequency of calls
table_data = call_center_data.ix[:,['day_of_week', 'call_hour']]
day_of_week_to_ordered_day_of_week = {'Monday' : '2_Monday', 
     'Tuesday' : '3_Tuesday', 
     'Wednesday' : '4_Wednesday', 
     'Thursday' : '5_Thursday', 
     'Friday' : '6_Friday',
     'Saturday' : '7_Saturday',
     'Sunday' : '1_Sunday'}
table_data['ordered_day_of_week'] = \
    table_data['day_of_week'].map(day_of_week_to_ordered_day_of_week)
print(pd.crosstab(table_data['ordered_day_of_week'],\
    table_data['call_hour'], margins = False))

# select first week of February 1999 for data visualization and analysis
# that week began on Monday, February 1 and ended on Sunday, February 7        
selected_week = call_center_data[call_center_data['date'] < 
    datetime.date(1999, 2, 8)]
print(selected_week.head)    
    
# check frequency of calls in February by hour and day of week
# create an ordered table for frequency of calls 
# for the first week in February 1999
table_data = selected_week.ix[:,['day_of_week', 'call_hour']]
day_of_week_to_ordered_day_of_week = {'Monday' : '2_Monday', 
     'Tuesday' : '3_Tuesday', 
     'Wednesday' : '4_Wednesday', 
     'Thursday' : '5_Thursday', 
     'Friday' : '6_Friday',
     'Saturday' : '7_Saturday',
     'Sunday' : '1_Sunday'}
table_data['ordered_day_of_week'] = \
    table_data['day_of_week'].map(day_of_week_to_ordered_day_of_week)
print(pd.crosstab(table_data['ordered_day_of_week'],\
    table_data['call_hour'], margins = False))

# wait-time ribbons were created with R ggplot2 software
# Python packages ggplot or rpy2 could be used for plotting

# select Wednesdays in February for the queueing model
wednesdays = call_center_data[call_center_data['day_of_week'] == \
    'Wednesday'] 
print(wednesdays.head) 

# arrival rate as average number of calls into VRU per hour 
arrived_for_hour = wednesdays['call_hour'].value_counts()
check_hourly_arrival_rate = arrived_for_hour/4  # four Wednesdays in February 1999
print(check_hourly_arrival_rate)

# organize hourly arrival rates according to 24-hour clock
hourly_arrival_rate = [6.75, 1.75, 1.25, 0.00, 0.50, 0.25,\
    4.75, 39.50, 97.25,107.50, 124.00,110.25, 95.50,\
    203.50, 115.75, 115.50, 67.75, 75.00, 88.75,\
    85.50, 68.00, 61.50, 57.50, 44.25]

# service times may vary hour-by-hour due to differences 
# in service requests and individuals calling hour-by-hour
# begin by selecting calls that receive service
wednesdays_served = wednesdays[wednesdays['server'] != \
    'NO_SERVER'] 
print(wednesdays_served.head) 
      
hourly_mean_service_time =\
    wednesdays_served.pivot_table('ser_time', cols = ['call_hour'],\
    aggfunc = 'mean', margins = False)

# hourly service rate given the current numbers of service operators
served_for_hour = wednesdays_served['call_hour'].value_counts()
print(served_for_hour)

# compute service rate noting that there are 3600 seconds in an hour
# adding 60 seconds to each mean service time for time between calls
# this 60 seconds is the wrap up time or time an service agent remains 
# unavailable to answer a new call after a call has been completed
mean_hourly_service_rate = 3600/(hourly_mean_service_time.mean() + 60)
print('\nHourly Service Rate for Wednesdays:',\
    round(mean_hourly_service_rate,3))

# use 15 calls per hour as the rate for one service operator
SERVICE_RATE = 15

# use a target for the probability of waiting in queue to be 0.50
PROBABILITY_GOAL = 0.50

# Erlang C queueing calculations with Python erlang_C function
# inputs c = number of servers
#        r = ratio of rate of arrivals and rate of service
# returns the propability of waiting in queue because all servers are busy
# use while-loop iteration to determine the number of servers needed 
# we do this for each hour of the day knowing the hourly arrival rate
servers_needed = [0] * 24
for index_for_hour in range(24):
    if (hourly_arrival_rate[index_for_hour] > 0):
        erlang_probability = 1 # initialize on entering while-loop
        while (erlang_probability > PROBABILITY_GOAL):
            servers_needed[index_for_hour] = servers_needed[index_for_hour] + 1
            erlang_probability = \
                erlang_C(c = servers_needed[index_for_hour],\
                    r = hourly_arrival_rate[index_for_hour]/SERVICE_RATE)
print(servers_needed)  # check queueing theory result 
# the result for servers.needed is obtained as
# 1  1  1  0  1  1  1  4  8  9 10  9  8 16 10 10  6  7  8  8  6  6  5  4
# we will assume the bank call center will be closed hours 00 through 05
# but use the other values as the bank's needed numbers of servers
for index_for_hour in range(6):
    servers_needed[index_for_hour] = 0
print('\nHourly Operator Requirements:\n',servers_needed)

# read in case data for the structure of call center worker shifts
bank_shifts_data_frame = pd.read_csv("data_anonymous_bank_shifts.csv")
# examine the structure of these data
print(bank_shifts_data_frame.head)

# constraint matrix as required for mathematical programming
constraint_matrix = np.array(bank_shifts_data_frame)[:,2:]
# we will create this type of object on the R side as well

# six-hour shift salaries in Israeli sheqels 
# 1 ILS = 3.61 USD in June 2013
# these go into the objective function for integer programing
# with the objective of minimizing total costs
cost_vector = [252, 288, 180, 180, 180, 288, 288, 288] 

# install lpsolove package and drivers for Python 
# noting the operating system being used
# or use rpy2 access to lpSolve in R as shown here

# assign lists from Python to R using rpy2
r.assign('servers_needed_R', servers_needed)
r.assign('cost_vector_R', cost_vector)

r('bank.shifts.data.frame <- read.csv("data_anonymous_bank_shifts.csv")')
r('constraint_matrix_R <- as.matrix(bank.shifts.data.frame[,3:10])')

# check mathematical programming inputs on the R side
r('print(as.numeric(unlist(servers_needed_R)))')
r('print(as.numeric(unlist(cost_vector_R)))')
r('print(constraint_matrix_R)')

# solve the mathematical programming problem
r('library(lpSolve)')  
r('call_center_schedule <- lp(const.mat=constraint_matrix_R,\
    const.rhs = as.numeric(unlist(servers_needed_R)),\
    const.dir = rep(">=", times = 8),\
    int.vec = 1:8,\
    objective = as.numeric(unlist(cost_vector_R)),\
    direction = "min")')
    
# prepare summary of the results for the call center problem
# working on the R side
r('ShiftID <- 1:8')
r('StartTime <- c(0,6,8,10,12,2,4,6)')
# c("Midnight","6 AM","8 AM","10 AM","Noon","2 PM","4 PM","6 PM")
r('ShiftDuration <- rep(6,times=8)')
r('HourlyShiftSalary <- c(42,48,30,30,30,48,48,48)')
r('HourlyShiftCost <- call_center_schedule$objective') # six x hourly shift salary
r('Solution <- call_center_schedule$solution')  
r('ShiftCost <- call_center_schedule$solution * call_center_schedule$objective')
r('call_center_summary <- \
  data.frame(ShiftID,StartTime,ShiftDuration,HourlyShiftSalary,\
  HourlyShiftCost,Solution,ShiftCost)')
r('cat("\n\n","Call Center Summary","\n\n")')
r('print(call_center_summary)')
r('print(call_center_schedule)')

# alternatively... bring the solution from R to Python
# and print the minimum-cost solution on the Python side
call_center_schedule = r('call_center_schedule')
print(call_center_schedule)

# Suggestion for the student:
# Attack the problem using discrete event simulation, 
# perhaps drawing on the SimPy package.
# Try running a sensitivity test, varying the workforce requirements
# and noting the effect upon the optimal assignment of workers to shifts.
# This can be done in a Python for-loop.
