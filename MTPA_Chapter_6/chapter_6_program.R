# Workforce Scheduling for Anonymous Bank Call Center (R)

library(lubridate)  # date functions
library(grid)  # graphics utilities needed for split-plotting
library(ggplot2)  # graphics package with ribbon plot
library(queueing)  # queueing functions, including Erlang C
library(lpSolve)  # linear programming package

# ensure that two binary files are in the working directory
# these come from running R code from R_Utilities_Appendix
# source("R_utility_program_3.R") provides split-plotting utilities
load("mtpa_split_plotting_utilities.Rdata")
# source("R_utility_program_4.R") provides wait-time ribbon plots
load("mtpa_wait_time_ribbon_utility.Rdata")

put.title.on.plots <- TRUE  # put title on wait-time ribbon plots
# The call center data from "Anonymous Bank" in Israel were provided 
# by Avi Mandelbaum, with the help of Ilan Guedj.
# data source: http://ie.technion.ac.il/serveng/callcenterdata/index.html
# variable names and definitions from documentation 
# VRU  Voice Response Unit automated service
# vru.line  6 digits Each entering phone-call is first routed through a VRU: 
#           There are 6 VRUs labeled AA01 to AA06. Each VRU has several lines
#           labeled 1-16. There are a total of 65 lines. Each call is assigned 
#           a VRU number and a line number.
# call.id  unique call identifier
# customer.id  unique identifier for existing customer, zero for non-customer  
# priority  0 or 1 for unidentified or regular customers
#           2 for priority customers who receive advanced position in queue
# type  type of service
#       PS  regular activity (coded 'PS' for 'Peilut Shotefet')
#       PE  regular activity in English (coded 'PE' for 'Peilut English')
#       IN  internet consulting (coded 'IN' for 'Internet')
#       NE  stock exchange activity (coded 'NE' for 'Niarot Erech') 
#       NW  potential customer getting information
#       TT  customers who left a message asking the bank to return their call 
#           but, while the system returned their call, the calling-agent became 
#           busy hence the customers were put on hold in the queue.
# date  year-month-day
# vru_entry  time that the phone-call enters the call-center or VRU
# vru_exit  time of exit from VRU directly to service or to queue
# vru_time  time in seconds spent in the VRU 
#           (calculated by exit_time – entry_time)
# q_start  time of joining the queue (00:00:00 for customers who abandon VRU
#          or do not enter the queue) 
# q_exit  time in seconds of exiting queue to receive service or abandonment
# q_time  time spent in queue (calculated by q_exit – q_start)
# outcome  AGENT = service
#          HANG = hang up
#          PHANTOM = a virtual call to be ignored
# ser_start  time of beginning of service by agent
# ser_exit  time of end of service by agent
# ser_time  service duration in seconds (calculated by ser_exit – ser_start)
# server  name of agent, NO_SERVER if no service provided

# focus upon February 1999
call.center.input.data <- read.table("data_anonymous_bank_february.txt", 
  header = TRUE, colClasses = c("character","integer","numeric",
  "integer","character","character","character","character","integer",
  "character","character","integer","factor","character","character",
  "integer","character"))
  
# check data frame object and variable values
print(summary(call.center.input.data))

# delete PHANTOM calls
call.center.data <- subset(call.center.input.data, subset = (outcome != "PHANTOM"))

# negative VRU times make no sense... drop these rows from data frame
call.center.data <- subset(call.center.data, subset = (vru_time >= 0))

# calculate wait time as sum of vru_time and q_time
call.center.data$wait_time <- 
  call.center.data$vru_time + call.center.data$q_time

# define four-digit year so year is not read as 2099
# convert date string to date variable 
call.center.data$date <- paste("19", call.center.data$date, sep ="")
call.center.data$date <- ymd(call.center.data$date)

# identify day of the week 1 = Sunday ... 7 = Saturday
call.center.data$day_of_week <- wday(call.center.data$date)
call.center.data$day_of_week <- factor(call.center.data$day_of_week,
  levels = c(1:7), labels = c("Sunday","Monday","Tuesday",
  "Wednesday","Thursday","Friday","Saturday"))

# examine frequency of calls by day of week
print(table(call.center.data$day_of_week))

# identify the hour of entry into the system
time.list <- strsplit(call.center.data$vru_entry,":")
call.hour <- numeric(nrow(call.center.data))
for (index.for.call in 1:nrow(call.center.data)) 
  call.hour[index.for.call] <- as.numeric(time.list[[index.for.call]][1])
call.center.data$call_hour <- call.hour

# check frequency of calls in February by hour and day of week
print(with(call.center.data, table(day_of_week, call_hour)))

# select first week of February 1999 for data visualization and analysis
# that week began on Monday, February 1 and ended on Sunday, February 7
selected.week <- subset(call.center.data, subset = (date < ymd("19990208")))

# check frequency of calls in week by hour and day of week
print(with(selected.week, table(day_of_week, call_hour)))



# loop for day of week ignoring Saturdays in Isreal
day.of.week.list <- c("Monday","Tuesday",
  "Wednesday","Thursday","Friday","Sunday")
  
# wait-time ribbon plots for the six selected days
# call upon utility function wait.time.ribbon
# the utility makes use of grid split-plotting 
# place ribbon plot and text table/plot on each file
# each plot goes to its own external pdf file
for(index.day in seq(along=day.of.week.list)) {
  this.day.of.week <- day.of.week.list[index.day]
  pdf(file = paste("fig_operations_management_ribbon_",
  tolower(this.day.of.week),".pdf",sep=""), width = 11, height = 8.5)  
  if(put.title.on.plots) {
    ribbon.plot.title <- paste(this.day.of.week,"Call Center Operations")
    }
    else {
    ribbon.plot.title <- "" 
    }
  selected.day <- subset(selected.week, 
    subset = (day_of_week == this.day.of.week),
    select = c("call_hour","wait_time","ser_time","server"))
  colnames(selected.day) <- c("hour","wait","service","server")
  wait.time.ribbon(wait.service.data = selected.day, 
    title = ribbon.plot.title,
    use.text.tagging = TRUE, wait.time.goal = 30, wait.time.max = 90,
    plotting.min = 0, plotting.max = 250)    
  dev.off()  
  }

# select Wednesdays in February for the queueing model
wednesdays <- subset(call.center.data, subset = (day_of_week == "Wednesday"))

# compute arrival rate of calls as calls for hour  
# we do not use table() here because some hours could have zero calls
calls.for.hour <- numeric(24)
for(index.for.hour in 1:24) { 
# 24-hour clock has first hour coded as zero in input data file
  coded.index.for.hour <- index.for.hour - 1  
  this.hour.calls <- 
    subset(wednesdays, subset = (call_hour == coded.index.for.hour))  
  if(nrow(this.hour.calls) > 0) 
    calls.for.hour[index.for.hour] <- nrow(this.hour.calls)  
  }

# compute arrival rate as average number of calls into VRU per hour
hourly.arrival.rate <- calls.for.hour/4  # four Wednesdays in February

# service times can vary hour-by-hour due to differences 
# in service requests and individuals calling hour-by-hour
# begin by selecting calls that receive service
wednesdays.served <- subset(wednesdays, subset = (server != "NO_SERVER"))

hourly.mean.service.time <- numeric(24)
served.for.hour <- numeric(24)
for(index.for.hour in 1:24) { 
# 24-hour clock has first hour coded as zero in input data file
  coded.index.for.hour <- index.for.hour - 1  
  this.hour.calls <- 
    subset(wednesdays.served, subset = (call_hour == coded.index.for.hour))
  if(nrow(this.hour.calls) > 0) {
    served.for.hour[index.for.hour] <- nrow(this.hour.calls)
    hourly.mean.service.time[index.for.hour] <- mean(this.hour.calls$ser_time)
    }
  } 
  
# hourly service rate given the current numbers of service operators
hourly.served.rate <- served.for.hour/4  # four Wednesdays in February

# build data frame for plotting arrival and service rates
hour <- 1:24  # hour for horizontal axix of line chart
type <- rep("Arrived", length = 24)
value <- hourly.arrival.rate
arrival.data.frame <- data.frame(hour, value, type) 
type <- rep("Served", length = 24)
value <- hourly.served.rate
service.data.frame <- data.frame(hour, value, type) 
arrival.service.data.frame <- rbind(arrival.data.frame, service.data.frame)

pdf(file = "fig_operations_management_wednesdays_arrived_served.pdf", 
  width = 11, height = 8.5)
plotting.object <- ggplot(data = arrival.service.data.frame, 
  aes(x = hour, y = value, fill = type)) + 
  geom_line() +
  geom_point(size = 4, shape = 21) +
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15,17,19,21,23,25),
    labels = 
      c("00","02","04","06","08","10","12","14","16","18","20","22","24")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  labs(x = "Hour of Day (24-Hour Clock)", y = "Average Calls per Hour") +
  scale_fill_manual(values = c("yellow","dark green"), 
    guide = guide_legend(title = NULL))  +
  theme(legend.position = c(1,1), legend.justification = c(1,1)) +
  theme(legend.text = element_text(size=15)) +
  coord_fixed(ratio = 1/10)    
print(plotting.object)
dev.off()

# examine service times per service operator
# for hours with no service time information use the mean as value
hourly.mean.service.time <- 
  ifelse((hourly.mean.service.time == 0),
    mean(wednesdays.served$ser_time),
    hourly.mean.service.time) 
# compute service rate noting that there are 3600 seconds in an hour
# adding 60 seconds to each mean service time for time between calls
# this 60 seconds is the wrap up time or time a service agent remains 
# unavailable to answer a new call after a call has been completed
hourly.service.rate <- 3600/(hourly.mean.service.time + 60)

# we observe that mean service times do not vary that much hour-by-hour
# so we use the mean hourly service rate in queueing calculations
# mean(hourly.service.rate) is 14.86443
# so we use 15 calls per hour as the rate for one service operator
SERVICE.RATE <- 15

# C_erlang function from the queueing package
# inputs c = number of servers
#        r = ratio of rate of arrivals and rate of service
# returns the propability of waiting in queue because all servers are busy
# let us set a target for the probability of waiting in queue to be 0.50
# using while-loop iteration we determine the number of servers needed 
# we do this for each hour of the day knowing the hourly arrival rate

PROBABILITY.GOAL <- 0.50
servers.needed <- integer(24)  # initialize to zero
for(index.for.hour in 1:24) {
  if (hourly.arrival.rate[index.for.hour] > 0) {
    erlang.probability <- 1.00  # intialization prior to entering while-loop
    while (erlang.probability > PROBABILITY.GOAL) {
      servers.needed[index.for.hour] <- servers.needed[index.for.hour] + 1
      erlang.probability <- C_erlang(c = servers.needed[index.for.hour], 
          r = hourly.arrival.rate[index.for.hour]/SERVICE.RATE)
      }  # end while-loop for defining servers needed given probability goal 
    }  # end if-block for hours with calls
  }  # end for-loop for the hour

# the result for servers.needed is obtained as
# 1  1  1  0  1  1  1  4  8  9 10  9  8 16 10 10  6  7  8  8  6  6  5  4
# we will assume the bank call center will be closed hours 00 through 05
# but use the other values as the bank's needed numbers of servers
servers.needed[1:6] <- 0

cat("\n","----- Hourly Operator Requirements -----","\n")
print(servers.needed)

# read in case data for the structure of call center worker shifts
bank.shifts.data.frame <- read.csv("data_anonymous_bank_shifts.csv")

# examine the structure of the case data frame
print(str(bank.shifts.data.frame))

constraint.matrix <- as.matrix(bank.shifts.data.frame[,3:10])
cat("\n","----- Call Center Shift Constraint Matrix -----","\n")
print(constraint.matrix)

# six-hour shift salaries in Israeli sheqels 
# 1 ILS = 3.61 USD in June 2013
# these go into the objective function for integer programing
# with the objective of minimizing total costs
cost.vector <- c(252,288,180,180,180,288,288,288) 

call.center.schedule <- lp(const.mat=constraint.matrix,
const.rhs = servers.needed,
const.dir = rep(">=",times=8),
int.vec = 1:8,
objective = cost.vector,
direction = "min")

# prepare summary of the results for the call center problem
ShiftID <- 1:8
StartTime <- c(0,6,8,10,12,2,4,6)
# c("Midnight","6 AM","8 AM","10 AM","Noon","2 PM","4 PM","6 PM")
ShiftDuration <- rep(6,times=8)
HourlyShiftSalary <- c(42,48,30,30,30,48,48,48)
HourlyShiftCost <- call.center.schedule$objective # six x hourly shift salary
Solution <- call.center.schedule$solution  
ShiftCost <- call.center.schedule$solution * call.center.schedule$objective

call.center.summary <- 
  data.frame(ShiftID,StartTime,ShiftDuration,HourlyShiftSalary,
  HourlyShiftCost,Solution,ShiftCost)
  
cat("\n\n","Call Center Summary","\n\n")
print(call.center.summary)

# the solution is obtained by print(call.center.schedule) 
# or by summing across the hourly solution times the cost objective
 print(call.center.schedule) 
cat("\n\n","Call Center Summary Minimum Cost Solution:",sum(ShiftCost),"\n\n")
# build data frame for plotting the solution compared with need
hour <- 1:24  # hour for horizontal axix of line chart
type <- rep("Hourly Need", length = 24)
value <- servers.needed
needs.data.frame <- data.frame(hour, value, type) 
type <- rep("Optimal Solution", length = 24)
value <- schedule.fit.to.need <- 
  constraint.matrix %*% call.center.schedule$solution
solution.data.frame <- data.frame(hour, value, type) 
plotting.data.frame <- rbind(needs.data.frame, solution.data.frame)

# plot the solution... solution match to the workforce need
pdf(file = "fig_operations_management_solution.pdf", width = 11, height = 8.5)
plotting.object <- ggplot(data = plotting.data.frame, 
  aes(x = hour, y = value, fill = type)) + 
  geom_line() +
  geom_point(size = 4, shape = 21) +
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15,17,19,21,23,25),
    labels = 
      c("00","02","04","06","08","10","12","14","16","18","20","22","24")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
  labs(x = "Hour of Day (24-Hour Clock)", y = "Number of Service Operators") +
  scale_fill_manual(values = c("white","blue"), 
    guide = guide_legend(title = NULL)) +
  theme(legend.position = c(1,1), legend.justification = c(1,1)) +
  theme(legend.text = element_text(size=15)) +
  coord_fixed(ratio = 2/2.25)    
print(plotting.object)
dev.off()

# Suggestion for the student:
# Try running a sensitivity test, varying the workforce requirements
# and noting the effect upon the optimal assignment of workers to shifts.


