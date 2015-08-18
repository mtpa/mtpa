# Wait-Time Ribbon Plot (R)

wait.time.ribbon <- function(wait.service.data, title = "", 
  wait.time.goal = 30, wait.time.max = 90, 
  plotting.min = 0, plotting.max = 250,
  use.text.tagging = TRUE) {
  
#  requires ggplot2 package
#  data visualization for operations management
#  wait.service.data is input data frame with the named columns as follows: 
#    hour: integer hour of the day on 24-hour clock
#    wait: integer call wait time in seconds
#    service:  integer call service time in seconds (NA for no service)
#    server:  character string for server name or code
#             assumes that there is a distict character string for no server
#             this string is coded as NO_SERVER
#  wait.time.goal:  desired maximum wait time (30 seconds default)
#                   represented as bottom of yellow region
#  wait.time.max: when wait time becomes intolerable (90 seconds default)
#                 represented as top of yellow region
# use.text.tagging default is TRUE for added text at bottom of plot

# set constants for ribbon plotting
MIN.SAMPLE <- 5  # min sample size for hourly calcuations
PERCENTILE.MIN <- 0.50  # used for bottom of acceptable wait time
PERCENTILE.MAX <- 0.90  # used for bottom of acceptable wait time

add_footnote_at_bottom_of_ribbon_plot <- TRUE
percentile.footnote <- paste("Bottom of ribbon = ",
  100*PERCENTILE.MIN, "th percentile of wait times",
  "    Top of ribbon = ", 100*PERCENTILE.MAX, "th percentile of wait times.", 
  sep = "")

x.hour <- seq(from=0,to=23) # for horixontal axis scale

# code for ribbon region counts
calls.per.hour <- numeric(24)  # total calls initialized as zero
served.calls <- numeric(24)  # served calls initialized as zero
dropped.calls <- numeric(24)  # dropped/abandoned calls initialize as zero
ymin.percentile <- rep(NA,times=24)  # store for minimum percentile values
ymax.percentile <- rep(NA,times=24)  # store maximum percentile values

# compute number of calls per hour
# code more versatile than table command
# to accommodate hours with no calls

for(index.for.hour in 1:24) { 
# begin for-loop for wait-time data call counts and percentile calculations

# 24-hour clock has first hour coded as zero in input data file
  coded.index.for.hour <- index.for.hour - 1  
  temporary.vector <- na.omit(wait.service.data$hour)
  calls.per.hour[index.for.hour] <- 
    sum(ifelse(temporary.vector==coded.index.for.hour,1,0))

    if(calls.per.hour[index.for.hour] >= MIN.SAMPLE) { 
# begin if-block for computing ymin and ymax values and number of servers
# when there are at least MIN.SAMPLE calls in the hour
    this.hour.wait.service.data <- 
      wait.service.data[(wait.service.data$hour == coded.index.for.hour),]

    ymin.percentile[index.for.hour] <- 
      quantile(this.hour.wait.service.data$wait,
      probs=c(PERCENTILE.MIN),na.rm = TRUE,names=FALSE,type=8)

    ymax.percentile[index.for.hour] <- 
      quantile(this.hour.wait.service.data$wait,
      probs=c(PERCENTILE.MAX),na.rm = TRUE,names=FALSE,type=8)      
    } # end if-block for computing ymin and ymax values 

# if insufficient data we set min and max to be wait.time.goal
  if(calls.per.hour[index.for.hour] < MIN.SAMPLE) {
    ymin.percentile[index.for.hour] <- wait.time.goal
    ymax.percentile[index.for.hour] <- wait.time.goal
    }
  } # end for-loop for wait-time data call counts and percentile calculations  

# compute number.of.servers data and served and dropped calls
number.of.servers <- numeric(24)  # initialize to zero
for(index.for.hour in 1:24) { 
# begin for-loop for obtaining server data for the ribbon plot
# 24-hour clock has first hour coded as zero in input data file
  coded.index.for.hour <- index.for.hour - 1  
  temporary.vector <- na.omit(wait.service.data$hour)
  calls.per.hour[index.for.hour] <- 
    sum(ifelse(temporary.vector==coded.index.for.hour,1,0))
  this.hour.wait.service.data <- 
      wait.service.data[(wait.service.data$hour == coded.index.for.hour),]   
      
  served.calls[index.for.hour] <- 
    nrow(subset(this.hour.wait.service.data, subset=(server != "NO_SERVER")))   
  dropped.calls[index.for.hour] <- 
    nrow(subset(this.hour.wait.service.data, subset=(server == "NO_SERVER")))    
      
  if (nrow(this.hour.wait.service.data) > 0) {
# count is based upon the number of unique server names less NO_SERVER
    servers <- 
      na.omit((unique(this.hour.wait.service.data$server)))
    valid.servers <- setdiff(servers, "NO_SERVER")
    number.of.servers[index.for.hour] <- length(valid.servers)
    }
  } # end for-loop for obtaining server data for the ribbon plot
  
       
greenmin <- rep(plotting.min, length=24)
greenmax <- rep(wait.time.goal, length=24)

yellowmin <- rep(wait.time.goal, length=24)
yellowmax <- rep(wait.time.max, length=24)

redmin <- rep(wait.time.max, length=24)
redmax <- rep(plotting.max, length=24)

ymax.topwhite <- rep(plotting.max,length=24)
ymin.topwhite <- ymax.percentile

ymax.bottomwhite <- ymin.percentile
ymin.bottomwhite <- rep(plotting.min,length=24)

# define data frame for plotting wait and service information for this day  
call.center.plotting.frame <- 
  data.frame(x.hour, ymin.percentile, ymax.percentile, 
    calls.per.hour, number.of.servers,
    greenmin,greenmax,
    yellowmin,yellowmax,
    redmin,redmax,
    ymin.bottomwhite,ymax.bottomwhite,
    ymin.topwhite,ymax.topwhite)  

#cat("\n\n","------------- ",title," -------------","\n")
#print(call.center.plotting.frame)

ggobject <- ggplot() + 
geom_ribbon(data=call.center.plotting.frame, 
mapping=aes(x=x.hour, ymin=greenmin, ymax=greenmax), 
stat="identity",colour="white",fill="darkgreen") + 
geom_ribbon(data=call.center.plotting.frame, 
mapping=aes(x=x.hour, ymin=yellowmin, ymax=yellowmax), 
stat="identity",colour="white",fill="yellow") + 
geom_ribbon(data=call.center.plotting.frame, 
mapping=aes(x=x.hour, ymin=redmin, ymax=redmax), 
stat="identity",colour="white",fill="red") + 
geom_ribbon(data=call.center.plotting.frame, 
mapping=aes(x=x.hour, ymin=ymin.topwhite, ymax=ymax.topwhite), 
stat="identity",colour="white",fill="white") + 
geom_ribbon(data=call.center.plotting.frame, 
mapping=aes(x=x.hour, ymin=ymin.bottomwhite, ymax=ymax.bottomwhite), 
stat="identity",colour="white",fill="white") + 
geom_hline(data=call.center.plotting.frame, 
mapping=aes(yintercept=yellowmin[1])) + 
geom_hline(data=call.center.plotting.frame, 
mapping=aes(yintercept=redmin[1])) + 
labs(title = title) + theme_bw(base_size = 12) + 
scale_y_continuous(limits = c(greenmin[1], redmax[1])) + 
xlab("Hour of Day (24-Hour Clock)") + 
ylab("Wait Time (Seconds)") 

# plotting with all default margins no text at bottom
if(!use.text.tagging) ggplot.print.with.margins(ggobject) 

# plotting with text tagging requires the creation of a ggplot text object
if(use.text.tagging) 
{

# define character data for the text taggging at bottom of plot
hour.title <- "Hour:"
hour.00 <- "00"
hour.01 <- "01"
hour.02 <- "02"
hour.03 <- "03"
hour.04 <- "04"
hour.05 <- "05"
hour.06 <- "06"
hour.07 <- "07"
hour.08 <- "08"
hour.09 <- "09"
hour.10 <- "10"
hour.11 <- "11"
hour.12 <- "12"
hour.13 <- "13"
hour.14 <- "14"
hour.15 <- "15"
hour.16 <- "16"
hour.17 <- "17"
hour.18 <- "18"
hour.19 <- "19"
hour.20 <- "20"
hour.21 <- "21"
hour.22 <- "22"
hour.23 <- "23"

calls.title <- "Calls:"  
calls.00 <- as.character(calls.per.hour[1])
calls.01 <- as.character(calls.per.hour[2])
calls.02 <- as.character(calls.per.hour[3])
calls.03 <- as.character(calls.per.hour[4])
calls.04 <- as.character(calls.per.hour[5])
calls.05 <- as.character(calls.per.hour[6])
calls.06 <- as.character(calls.per.hour[7])
calls.07 <- as.character(calls.per.hour[8])
calls.08 <- as.character(calls.per.hour[9])
calls.09 <- as.character(calls.per.hour[10])
calls.10 <- as.character(calls.per.hour[11])
calls.11 <- as.character(calls.per.hour[12])
calls.12 <- as.character(calls.per.hour[13])
calls.13 <- as.character(calls.per.hour[14])
calls.14 <- as.character(calls.per.hour[15])
calls.15 <- as.character(calls.per.hour[16])
calls.16 <- as.character(calls.per.hour[17])
calls.17 <- as.character(calls.per.hour[18])
calls.18 <- as.character(calls.per.hour[19])
calls.19 <- as.character(calls.per.hour[20])
calls.20 <- as.character(calls.per.hour[21])
calls.21 <- as.character(calls.per.hour[22])
calls.22 <- as.character(calls.per.hour[23])
calls.23 <- as.character(calls.per.hour[24])

servers.title <- "Servers:" 
servers.00 <- as.character(number.of.servers[1])
servers.01 <- as.character(number.of.servers[2])
servers.02 <- as.character(number.of.servers[3])
servers.03 <- as.character(number.of.servers[4])
servers.04 <- as.character(number.of.servers[5])
servers.05 <- as.character(number.of.servers[6])
servers.06 <- as.character(number.of.servers[7])
servers.07 <- as.character(number.of.servers[8])
servers.08 <- as.character(number.of.servers[9])
servers.09 <- as.character(number.of.servers[10])
servers.10 <- as.character(number.of.servers[11])
servers.11 <- as.character(number.of.servers[12])
servers.12 <- as.character(number.of.servers[13])
servers.13 <- as.character(number.of.servers[14])
servers.14 <- as.character(number.of.servers[15])
servers.15 <- as.character(number.of.servers[16])
servers.16 <- as.character(number.of.servers[17])
servers.17 <- as.character(number.of.servers[18])
servers.18 <- as.character(number.of.servers[19])
servers.19 <- as.character(number.of.servers[20])
servers.20 <- as.character(number.of.servers[21])
servers.21 <- as.character(number.of.servers[22])
servers.22 <- as.character(number.of.servers[23])
servers.23 <- as.character(number.of.servers[24])

served.title <- "Served:" 
served.00 <- as.character(served.calls[1])
served.01 <- as.character(served.calls[2])
served.02 <- as.character(served.calls[3])
served.03 <- as.character(served.calls[4])
served.04 <- as.character(served.calls[5])
served.05 <- as.character(served.calls[6])
served.06 <- as.character(served.calls[7])
served.07 <- as.character(served.calls[8])
served.08 <- as.character(served.calls[9])
served.09 <- as.character(served.calls[10])
served.10 <- as.character(served.calls[11])
served.11 <- as.character(served.calls[12])
served.12 <- as.character(served.calls[13])
served.13 <- as.character(served.calls[14])
served.14 <- as.character(served.calls[15])
served.15 <- as.character(served.calls[16])
served.16 <- as.character(served.calls[17])
served.17 <- as.character(served.calls[18])
served.18 <- as.character(served.calls[19])
served.19 <- as.character(served.calls[20])
served.20 <- as.character(served.calls[21])
served.21 <- as.character(served.calls[22])
served.22 <- as.character(served.calls[23])
served.23 <- as.character(served.calls[24])

dropped.title <- "Dropped:" 
dropped.00 <- as.character(dropped.calls[1])
dropped.01 <- as.character(dropped.calls[2])
dropped.02 <- as.character(dropped.calls[3])
dropped.03 <- as.character(dropped.calls[4])
dropped.04 <- as.character(dropped.calls[5])
dropped.05 <- as.character(dropped.calls[6])
dropped.06 <- as.character(dropped.calls[7])
dropped.07 <- as.character(dropped.calls[8])
dropped.08 <- as.character(dropped.calls[9])
dropped.09 <- as.character(dropped.calls[10])
dropped.10 <- as.character(dropped.calls[11])
dropped.11 <- as.character(dropped.calls[12])
dropped.12 <- as.character(dropped.calls[13])
dropped.13 <- as.character(dropped.calls[14])
dropped.14 <- as.character(dropped.calls[15])
dropped.15 <- as.character(dropped.calls[16])
dropped.16 <- as.character(dropped.calls[17])
dropped.17 <- as.character(dropped.calls[18])
dropped.18 <- as.character(dropped.calls[19])
dropped.19 <- as.character(dropped.calls[20])
dropped.20 <- as.character(dropped.calls[21])
dropped.21 <- as.character(dropped.calls[22])
dropped.22 <- as.character(dropped.calls[23])
dropped.23 <- as.character(dropped.calls[24])

# set up spacing and positioning for the table
y.current.level <- 1.0  # initialze position
y.large.space <- 0.175
y.medium.space <- 0.125
y.small.space <- 0.075

table.left.margin <- 0.1  # needed for row labels at left
horizontal.offset <- (1-table.left.margin)/24  # spacing in the text table

y.current.level <- y.current.level - y.medium.space

ggtextobject <- ggplot(data=data.frame(x = 0.5,y = y.current.level),
  aes(x=x,y=y,xmin=0,xmax=1,ymin=0,ymax=1), 
  stat="identity", position="identity") + labs(x=NULL,y=NULL) +  
geom_text(x = 0.025,y = y.current.level,label = hour.title,
size = 4.25,colour="black")  + 
geom_text(x = (00*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.00, size = 4.25,colour="black") + 
geom_text(x = (01*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.01, size = 4.25,colour="black") + 
geom_text(x = (02*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.02, size = 4.25,colour="black") + 
geom_text(x = (03*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.03, size = 4.25,colour="black") + 
geom_text(x = (04*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.04, size = 4.25,colour="black") + 
geom_text(x = (05*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.05, size = 4.25,colour="black") + 
geom_text(x = (06*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.06, size = 4.25,colour="black") + 
geom_text(x = (07*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.07, size = 4.25,colour="black") + 
geom_text(x = (08*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.08, size = 4.25,colour="black") + 
geom_text(x = (09*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.09, size = 4.25,colour="black") + 
geom_text(x = (10*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.10, size = 4.25,colour="black") + 
geom_text(x = (11*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.11, size = 4.25,colour="black") + 
geom_text(x = (12*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.12, size = 4.25,colour="black") + 
geom_text(x = (13*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.13, size = 4.25,colour="black") + 
geom_text(x = (14*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.14, size = 4.25,colour="black") + 
geom_text(x = (15*horizontal.offset + table.left.margin),
y= y.current.level,label = hour.15, size = 4.25,colour="black") + 
geom_text(x = (16*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.16, size = 4.25,colour="black") + 
geom_text(x = (17*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.17, size = 4.25,colour="black") + 
geom_text(x = (18*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.18, size = 4.25,colour="black") + 
geom_text(x = (19*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.19, size = 4.25,colour="black") + 
geom_text(x = (20*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.20, size = 4.25,colour="black") + 
geom_text(x = (21*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.21, size = 4.25,colour="black") + 
geom_text(x = (22*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.22, size = 4.25,colour="black") + 
geom_text(x = (23*horizontal.offset + table.left.margin),
y = y.current.level,label = hour.23, size = 4.25,colour="black")   


y.current.level <- y.current.level - y.medium.space

ggtextobject <- ggtextobject + geom_text(x = 0.025,
y = y.current.level, label = servers.title,size = 4.25,colour="black") + 
geom_text(x = (00*horizontal.offset + table.left.margin), 
y = y.current.level, label = servers.00,size = 4.25,colour="black") + 
geom_text(x = (01*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.01,size = 4.25,colour="black") + 
geom_text(x = (02*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.02,size = 4.25,colour="black") + 
geom_text(x = (03*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.03,size = 4.25,colour="black") + 
geom_text(x = (04*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.04,size = 4.25,colour="black") + 
geom_text(x = (05*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.05,size = 4.25,colour="black") + 
geom_text(x = (06*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.06,size = 4.25,colour="black") + 
geom_text(x = (07*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.07,size = 4.25,colour="black") + 
geom_text(x = (08*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.08,size = 4.25,colour="black") + 
geom_text(x = (09*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.09,size = 4.25,colour="black") + 
geom_text(x = (10*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.10,size = 4.25,colour="black") + 
geom_text(x = (11*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.11,size = 4.25,colour="black") + 
geom_text(x = (12*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.12,size = 4.25,colour="black") + 
geom_text(x = (13*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.13,size = 4.25,colour="black") + 
geom_text(x = (14*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.14,size = 4.25,colour="black") + 
geom_text(x = (15*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.15,size = 4.25,colour="black") + 
geom_text(x = (16*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.16,size = 4.25,colour="black") + 
geom_text(x = (17*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.17,size = 4.25,colour="black") + 
geom_text(x = (18*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.18,size = 4.25,colour="black") + 
geom_text(x = (19*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.19,size = 4.25,colour="black") + 
geom_text(x = (20*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.20,size = 4.25,colour="black") + 
geom_text(x = (21*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.21,size = 4.25,colour="black") + 
geom_text(x = (22*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.22,size = 4.25,colour="black") + 
geom_text(x = (23*horizontal.offset + table.left.margin),
y = y.current.level, label = servers.23,size = 4.25,colour="black")  

# store line position for bottom of text segment of the visualization

y.level.divider.line <- y.current.level - y.medium.space
# temporary data frame needed to input to geom_hline later
middle.line.data <- data.frame(y.level.divider.line) 

y.current.level <- y.level.divider.line - y.medium.space

ggtextobject <- ggtextobject +  
geom_text(x = 0.025,y = y.current.level,
label = calls.title, size = 4.25,colour="black") + 
geom_text(x = (00*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.00, size = 4.25,colour="black") + 
geom_text(x = (01*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.01, size = 4.25,colour="black") + 
geom_text(x = (02*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.02, size = 4.25,colour="black") + 
geom_text(x = (03*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.03, size = 4.25,colour="black") + 
geom_text(x = (04*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.04, size = 4.25,colour="black") + 
geom_text(x = (05*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.05, size = 4.25,colour="black") + 
geom_text(x = (06*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.06, size = 4.25,colour="black") + 
geom_text(x = (07*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.07, size = 4.25,colour="black") + 
geom_text(x = (08*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.08, size = 4.25,colour="black") + 
geom_text(x = (09*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.09, size = 4.25,colour="black") + 
geom_text(x = (10*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.10, size = 4.25,colour="black") + 
geom_text(x = (11*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.11, size = 4.25,colour="black") + 
geom_text(x = (12*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.12, size = 4.25,colour="black") + 
geom_text(x = (13*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.13, size = 4.25,colour="black") + 
geom_text(x = (14*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.14, size = 4.25,colour="black") + 
geom_text(x = (15*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.15, size = 4.25,colour="black") + 
geom_text(x = (16*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.16, size = 4.25,colour="black") + 
geom_text(x = (17*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.17, size = 4.25,colour="black") + 
geom_text(x = (18*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.18, size = 4.25,colour="black") + 
geom_text(x = (19*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.19, size = 4.25,colour="black") + 
geom_text(x = (20*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.20, size = 4.25,colour="black") + 
geom_text(x = (21*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.21, size = 4.25,colour="black") + 
geom_text(x = (22*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.22, size = 4.25,colour="black") + 
geom_text(x = (23*horizontal.offset + table.left.margin),
y = y.current.level, label = calls.23, size = 4.25,colour="black")   

y.current.level <- y.current.level - y.medium.space

ggtextobject <- ggtextobject +  
geom_text(x = 0.025,y = y.current.level,
label = served.title, size = 4.25,colour="black") + 
geom_text(x = (00*horizontal.offset + table.left.margin),
y = y.current.level, label = served.00, size = 4.25,colour="black") + 
geom_text(x = (01*horizontal.offset + table.left.margin),
y = y.current.level, label = served.01, size = 4.25,colour="black") + 
geom_text(x = (02*horizontal.offset + table.left.margin),
y = y.current.level, label = served.02, size = 4.25,colour="black") + 
geom_text(x = (03*horizontal.offset + table.left.margin),
y = y.current.level, label = served.03, size = 4.25,colour="black") + 
geom_text(x = (04*horizontal.offset + table.left.margin),
y = y.current.level, label = served.04, size = 4.25,colour="black") + 
geom_text(x = (05*horizontal.offset + table.left.margin),
y = y.current.level, label = served.05, size = 4.25,colour="black") + 
geom_text(x = (06*horizontal.offset + table.left.margin),
y = y.current.level, label = served.06, size = 4.25,colour="black") + 
geom_text(x = (07*horizontal.offset + table.left.margin),
y = y.current.level, label = served.07, size = 4.25,colour="black") + 
geom_text(x = (08*horizontal.offset + table.left.margin),
y = y.current.level, label = served.08, size = 4.25,colour="black") + 
geom_text(x = (09*horizontal.offset + table.left.margin),
y = y.current.level, label = served.09, size = 4.25,colour="black") + 
geom_text(x = (10*horizontal.offset + table.left.margin),
y = y.current.level, label = served.10, size = 4.25,colour="black") + 
geom_text(x = (11*horizontal.offset + table.left.margin),
y = y.current.level, label = served.11, size = 4.25,colour="black") + 
geom_text(x = (12*horizontal.offset + table.left.margin),
y = y.current.level, label = served.12, size = 4.25,colour="black") + 
geom_text(x = (13*horizontal.offset + table.left.margin),
y = y.current.level, label = served.13, size = 4.25,colour="black") + 
geom_text(x = (14*horizontal.offset + table.left.margin),
y = y.current.level, label = served.14, size = 4.25,colour="black") + 
geom_text(x = (15*horizontal.offset + table.left.margin),
y = y.current.level, label = served.15, size = 4.25,colour="black") + 
geom_text(x = (16*horizontal.offset + table.left.margin),
y = y.current.level, label = served.16, size = 4.25,colour="black") + 
geom_text(x = (17*horizontal.offset + table.left.margin),
y = y.current.level, label = served.17, size = 4.25,colour="black") + 
geom_text(x = (18*horizontal.offset + table.left.margin),
y = y.current.level, label = served.18, size = 4.25,colour="black") + 
geom_text(x = (19*horizontal.offset + table.left.margin),
y = y.current.level, label = served.19, size = 4.25,colour="black") + 
geom_text(x = (20*horizontal.offset + table.left.margin),
y = y.current.level, label = served.20, size = 4.25,colour="black") + 
geom_text(x = (21*horizontal.offset + table.left.margin),
y = y.current.level, label = served.21, size = 4.25,colour="black") + 
geom_text(x = (22*horizontal.offset + table.left.margin),
y = y.current.level, label = served.22, size = 4.25,colour="black") + 
geom_text(x = (23*horizontal.offset + table.left.margin),
y = y.current.level, label = served.23, size = 4.25,colour="black")   

y.current.level <- y.current.level - y.medium.space

ggtextobject <- ggtextobject +  
geom_text(x = 0.025,y = y.current.level,
label = dropped.title, size = 4.25,colour="black") + 
geom_text(x = (00*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.00, size = 4.25,colour="black") + 
geom_text(x = (01*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.01, size = 4.25,colour="black") + 
geom_text(x = (02*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.02, size = 4.25,colour="black") + 
geom_text(x = (03*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.03, size = 4.25,colour="black") + 
geom_text(x = (04*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.04, size = 4.25,colour="black") + 
geom_text(x = (05*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.05, size = 4.25,colour="black") + 
geom_text(x = (06*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.06, size = 4.25,colour="black") + 
geom_text(x = (07*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.07, size = 4.25,colour="black") + 
geom_text(x = (08*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.08, size = 4.25,colour="black") + 
geom_text(x = (09*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.09, size = 4.25,colour="black") + 
geom_text(x = (10*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.10, size = 4.25,colour="black") + 
geom_text(x = (11*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.11, size = 4.25,colour="black") + 
geom_text(x = (12*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.12, size = 4.25,colour="black") + 
geom_text(x = (13*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.13, size = 4.25,colour="black") + 
geom_text(x = (14*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.14, size = 4.25,colour="black") + 
geom_text(x = (15*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.15, size = 4.25,colour="black") + 
geom_text(x = (16*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.16, size = 4.25,colour="black") + 
geom_text(x = (17*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.17, size = 4.25,colour="black") + 
geom_text(x = (18*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.18, size = 4.25,colour="black") + 
geom_text(x = (19*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.19, size = 4.25,colour="black") + 
geom_text(x = (20*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.20, size = 4.25,colour="black") + 
geom_text(x = (21*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.21, size = 4.25,colour="black") + 
geom_text(x = (22*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.22, size = 4.25,colour="black") + 
geom_text(x = (23*horizontal.offset + table.left.margin),
y = y.current.level, label = dropped.23, size = 4.25,colour="black")  

y.level.divider.line <- y.current.level - y.medium.space
# temporary data frame needed to input to geom_hline later
bottom.line.data <- data.frame(y.level.divider.line) 

y.current.level <- y.level.divider.line - y.medium.space

# add footnote centered at bottom of plot if requested
if (add_footnote_at_bottom_of_ribbon_plot)
  ggtextobject <- ggtextobject +  
  geom_text(x = 0.5,y = y.current.level,
  label = percentile.footnote, size = 4.25, colour="black") 

# finish up the plot with background definition and divider lines
ggtextobject <- ggtextobject + geom_hline(aes(yintercept=1)) + 
geom_hline(data=bottom.line.data, 
  mapping = aes(yintercept = y.level.divider.line)) + 
geom_hline(data=middle.line.data, 
  mapping = aes(yintercept = y.level.divider.line)) + 
theme(legend.position = "none")  + 
theme(panel.grid.minor = element_blank()) + 
theme(panel.grid.major = element_blank())  + 
theme(panel.background = element_blank()) + 
theme(axis.ticks = element_blank()) + 
scale_y_continuous(breaks=c(0,1),label=c("","")) + 
scale_x_continuous(breaks=c(0,1),label=c("",""))

# user-defined function plots with text annotation/tagging at the bottom
special.top.bottom.ggplot.print.with.margins(ggobject,ggtextobject,
plot.pct=55,text.tagging.pct=35) 
}

} # end of wait-time ribbon function

# save wait-time ribbon utility for future work
save(wait.time.ribbon,
  file="mtpa_wait_time_ribbon_utility.Rdata")
