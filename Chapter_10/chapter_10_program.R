# Regression Modeling with California Housing Values (R)

library(maps)  # making making 
library(mapproj)  # projections for map making
library(spgwr)  # spatially-weighted regression
library(rpart)  # tree-structured modeling
library(randomForest)  # random forests
library(rpart.plot)  # plot tree-structured model information
library(lattice)  # statistical graphics
library(cvTools)  # cross-validation tools including rmspe 

# read in the housing data
houses <-  read.table("houses_data.txt", header = FALSE, sep = "", 
   dec = ".", row.names = NULL, 
   col.names = c("value", "income", "age", "rooms", "bedrooms", 
   "pop", "hh", "latitude", "longitude"))          

# computed variables for linear model used by Pace and Barry (1997)   
houses$log_value <- log(houses$value)  
houses$income_squared <- houses$income^2 
houses$income_cubed <- houses$income^3 
houses$log_age <- log(houses$age)
houses$log_pc_rooms <- log(houses$rooms / houses$pop)
houses$log_pc_bedrooms <- log(houses$bedrooms / houses$pop)
houses$log_pop_hh <- log(houses$pop / houses$hh)
houses$log_hh <- log(houses$hh)

# structure of the Pace and Barry (1997) model for baseline for comparisons
pace.barry.model <- {log_value ~ income + income_squared + 
  income_cubed + log_age + log_pc_rooms + log_pc_bedrooms + 
  log_pop_hh + log_hh}

# for comparison let's look at a simple model with the original variables
simple.model <-  {log_value ~ income + age + rooms + bedrooms +
  pop + hh} 
  
# original variables plus variables that add value for trees 
# that is... variables that are not simple monotonic transformations
# of the original explanatory variables
full.model <- {log_value ~ income + age + rooms + bedrooms +
  pop + hh + log_pc_rooms + log_pc_bedrooms + log_pop_hh}  
  
# define variable for selecting a geographically defined
# subset of the data... San Diego area  
# we use nested ifelse statements to do this

# define the bounding box for selecting the area
# here we are selecting the San Diego region
BB.TOP <- 33
BB.BOTTOM <- 32
BB.RIGHT <- -116.75
BB.LEFT <- -125

houses$select <- ifelse(((houses$latitude < BB.TOP)),
  ifelse((houses$longitude < BB.RIGHT),
  ifelse((houses$latitude > BB.BOTTOM),
  ifelse((houses$longitude > BB.LEFT),1,2),2),2),2)
houses$select <- factor(houses$select, levels = c(1,2), 
  labels = c("Selected","Not Selected"))
houses.selected <- subset(houses, subset = (select == "Selected"))
houses.notselected <- subset(houses, subset = (select == "Not Selected"))  
    
# plot the locations of block groups red in the selected area, blue otherwise
pdf(file = "fig_spatial_map_selected_region.pdf", width = 8.5, height = 8.5)
pointsize <- 0.5
map("state", region = c("california"), project="albers",par=c(39,45)) 
  points(mapproject(houses.selected$longitude, houses.selected$latitude,
  projection=""),pch=20,cex=pointsize,col="red")
  points(mapproject(houses.notselected$longitude, houses.notselected$latitude,
  projection=""),pch=20,cex=pointsize,col="darkblue")
legend("right", legend = c("Selected Region","Not Selected"), 
  col = c("red","darkblue"), pch = 20)
map.scale()  
dev.off()
        
# define training and test sets for the selected houses
set.seed(4444)
partition <- sample(nrow(houses.selected)) # permuted list of row index numbers
houses.selected$Group <- 
  ifelse((partition < nrow(houses.selected)/(3/2)),1,2)
houses.selected$Group <- 
  factor(houses.selected$Group,levels=c(1,2),labels=c("TRAIN","TEST"))
print(table(houses.selected$Group))  # review the split into training and test
print(head(houses.selected))  # review the selected data

houses.train <- 
  subset(houses.selected, subset = (Group == "TRAIN"))
houses.test <- 
  subset(houses.selected, subset = (Group == "TEST")) 
  
# examine the correlations across the variables before we begin modeling
houses.train.df.vars <- houses.train[,c("log_value","income","age",
  "rooms","bedrooms","pop","hh","log_pc_rooms",
  "log_pc_bedrooms","log_pop_hh")]
  
houses.train.cormat <- cor(as.matrix(houses.train.df.vars))
houses.train.cormat.line <- houses.train.cormat["log_value",]
# explanatory variables ordered by correlation with the response variable
ordered.houses.train.cormat <- 
  houses.train.cormat[names(sort(houses.train.cormat.line,decreasing=TRUE)),
  names(sort(houses.train.cormat.line,decreasing=FALSE))]

# code to obtain default colors from ggplot2...
number.of.default.colors <- 2  # two end-points for our scale
end.point.colors <- hcl(h=seq(15, 375-360/number.of.default.colors,
  length=number.of.default.colors)%%360, c=100, l=65)
# end.point.colors[1] and [2] used to define the three-point color scale

pdf(file = "fig_spatial_correlation_heat_map.pdf", width = 11, 
  height = 8.5)
x <- rep(1:nrow(ordered.houses.train.cormat),
  times=ncol(ordered.houses.train.cormat))
y <- NULL
for (i in 1:ncol(ordered.houses.train.cormat)) 
  y <- c(y,rep(i,times=nrow(ordered.houses.train.cormat)))
# use fixed format 0.XXX in cells of correlation matrix
cortext <- sprintf("%0.3f", as.numeric(ordered.houses.train.cormat))  
text.data.frame <- data.frame(x, y, cortext)
text.data.frame$cortext <- as.character(text.data.frame$cortext)
text.data.frame$cortext <- ifelse((text.data.frame$cortext == "1.000"),
    NA,text.data.frame$cortext)  # define diagonal cells as missing
text.data.frame <- na.omit(text.data.frame)  # diagonal cells have no text

print(levelplot(ordered.houses.train.cormat, cuts = 25, tick.number = 9,
  col.regions = 
    colorRampPalette(c(end.point.colors[1], "white", end.point.colors[2])),
  scales=list(tck=0, x=list(rot=90)),
  xlab = "", 
  ylab = "",
  panel = function(...) {
    panel.levelplot(...)  
    panel.text(text.data.frame$x, text.data.frame$y, 
    labels = text.data.frame$cortext)
    }))
dev.off()    

# scatter plot matrix (splom) demonstration
houses.train.splom.vars <- 
  houses.train[,c("log_value","income","age","rooms")]
pdf(file = "fig_spatial_sample_splom.pdf", width = 8.5, 
  height = 8.5)
pairs(houses.train.splom.vars, cex = 0.65, col = "darkblue")
dev.off()

# define spatial objects as needed for spatial modeling work 
# explanation of spatial objects may be found in chapter 2 of
# Bivand, R. S., Pebesma, E. J., and Gomez-Rubio, V. (2008) 
# Applied Spatial Data Analysis, New York: Springer.
# this involves adding coordinate objects to data frame objects
# training set coordinates to add
houses.coord <- cbind(houses.train$longitude,houses.train$latitude) 
# define spatial points data frame object
houses.train <- SpatialPointsDataFrame(houses.coord,houses.train,bbox = NULL) 

# test set coordinates to add
houses.coord <- cbind(houses.test$longitude,houses.test$latitude) 
# define spatial points data frame object
houses.test <- SpatialPointsDataFrame(houses.coord,houses.test,bbox = NULL) 

# examine the struction of the spatial points data frame
print(str(houses.train))  

# --------------------------------------------
# Linear regression a la Pace and Barry (1997)
# --------------------------------------------
pace.barry.train.fit <- lm(pace.barry.model, data = houses.train)

print(pace.barry.train.fit)
print(summary(pace.barry.train.fit))

# direct calculation of root-mean-squared prediction error 
# obtained directly on the training data
print(rmspe(houses.train$log_value, predict(pace.barry.train.fit))) 
# report R-squared on training data
print(cor(houses.train$log_value,predict(pace.barry.train.fit))^2)

cat("\n\nTraining set proportion of variance accounted",
  " for by linear regression = ",
  sprintf("%1.3f",cor(houses.train$log_value,
  predict(pace.barry.train.fit))^2),sep=" ")

# test model fit to training set on the test set
print(rmspe(houses.test$log_value, predict(pace.barry.train.fit, 
  newdata = houses.test))) 
print(cor(houses.test$log_value,
  predict(pace.barry.train.fit, newdata = houses.test))^2)
  
cat("\n\nTest set proportion of variance accounted",
  " for by linear regression = ",
  sprintf("%1.3f",cor(houses.test$log_value,
  predict(pace.barry.train.fit, newdata = houses.test))^2),sep=" ")  
  
# demonstrate cross-validation within the training set
# specify ten-fold cross-validation within the training set
# K = folds   R = replications of K-fold cross-validation
set.seed(1234)  # for reproducibility
folds <- cvFolds(nrow(houses.train), K = 10, R = 50)  
cv.pace.barry.train.fit <- cvLm(pace.barry.train.fit, cost = rtmspe, 
  folds = folds, trim = 0.1)
# root-mean-squared prediction error estimated by cross-validation
print(cv.pace.barry.train.fit)

# --------------------------------------
# Tree-structured regression (simple)
# --------------------------------------
# try tree-structured regression on the original explantory variables
# note that one of the advantages of trees is no need for transformations
# of the explanatory variables 
rpart.train.fit <- rpart(simple.model, data = houses.train)
print(summary(rpart.train.fit))  # tree summary statistics and split detail
houses.train$rpart.train.fit.pred <- predict(rpart.train.fit, 
  data = houses.train)

# root-mean-squared for trees on training set
print(rmspe(houses.train$log_value, houses.train$rpart.train.fit.pred)) 
# report R-squared on training data
print(cor(houses.train$log_value,houses.train$rpart.train.fit.pred)^2)

cat("\n\nTraining set proportion of variance accounted",
  " for by tree-structured regression = ",
  sprintf("%1.3f",cor(houses.train$log_value,
  houses.train$rpart.train.fit.pred)^2),sep=" ")

# root-mean-squared for trees on test set
houses.test$rpart.train.fit.pred <- predict(rpart.train.fit, newdata = houses.test)
print(rmspe(houses.test$log_value, houses.test$rpart.train.fit.pred)) 
# report R-squared on training data
print(cor(houses.test$log_value,houses.test$rpart.train.fit.pred)^2)

cat("\n\nTest set proportion of variance accounted",
  " for by tree-structured regression = ",
  sprintf("%1.3f",
  cor(houses.test$log_value,houses.test$rpart.train.fit.pred)^2),sep=" ")

# plot the regression tree result from rpart
pdf(file = "fig_spatial_rpart_model.pdf", width = 8.5, height = 8.5)
prp(rpart.train.fit, main="",
  digits = 3,  # digits to display in terminal nodes
  nn = TRUE,  # display the node numbers
  fallen.leaves = TRUE,  # put the leaves on the bottom of the page
  branch = 0.5,  # change angle of branch lines
  branch.lwd = 2,  # width of branch lines
  faclen = 0,  # do not abbreviate factor levels
  trace = 1,  # print the automatically calculated cex
  shadow.col = 0,  # no shadows under the leaves
  branch.lty = 1,  # draw branches using dotted lines
  split.cex = 1.2,  # make the split text larger than the node text
  split.prefix = "is ",  # put "is " before split text
  split.suffix = "?",  # put "?" after split text
  split.box.col = "blue",  # lightgray split boxes (default is white)
  split.col = "white",  # color of text in split box 
  split.border.col = "blue",  # darkgray border on split boxes
  split.round = .25)  # round the split box corners a tad
dev.off()

# --------------------------------------
# Tree-structured regression (full)
# --------------------------------------
# try tree-structured regression on the expanded set of variables 
rpart.train.fit.full <- rpart(full.model, data = houses.train)
print(summary(rpart.train.fit.full))  # tree summary statistics and split detail
houses.train$rpart.train.fit.full.pred <- 
  predict(rpart.train.fit.full, data = houses.train)

# root-mean-squared for trees on training set
print(rmspe(houses.train$log_value, houses.train$rpart.train.fit.full.pred)) 
# report R-squared on training data
print(cor(houses.train$log_value,houses.train$rpart.train.fit.full.pred)^2)

cat("\n\nTraining set proportion of variance accounted",
   " for by tree-structured regression (full model) = ",
  sprintf("%1.3f",cor(houses.train$log_value,
  houses.train$rpart.train.fit.full.pred)^2),sep=" ")

# root-mean-squared for trees on test set
houses.test$rpart.train.fit.full.pred <- predict(rpart.train.fit.full, 
  newdata = houses.test)
print(rmspe(houses.test$log_value, houses.test$rpart.train.fit.full.pred)) 
# report R-squared on training data
print(cor(houses.test$log_value,houses.test$rpart.train.fit.full.pred)^2)

cat("\n\nTest set proportion of variance accounted",
    " for by tree-structured regression (full model) = ",
  sprintf("%1.3f",cor(houses.test$log_value,
  houses.test$rpart.train.fit.full.pred)^2),sep=" ")

# plot the regression tree result from rpart
pdf(file = "fig_spatial_rpart_model_full.pdf", width = 8.5, height = 8.5)
prp(rpart.train.fit.full, main="",
  digits = 3,  # digits to display in terminal nodes
  nn = TRUE,  # display the node numbers
  fallen.leaves = TRUE,  # put the leaves on the bottom of the page
  branch = 0.5,  # change angle of branch lines
  branch.lwd = 2,  # width of branch lines
  faclen = 0,  # do not abbreviate factor levels
  trace = 1,  # print the automatically calculated cex
  shadow.col = 0,  # no shadows under the leaves
  branch.lty = 1,  # draw branches using dotted lines
  split.cex = 1.2,  # make the split text larger than the node text
  split.prefix = "is ",  # put "is" before split text
  split.suffix = "?",  # put "?" after split text
  split.box.col = "blue",  # lightgray split boxes (default is white)
  split.col = "white",  # color of text in split box 
  split.border.col = "blue",  # darkgray border on split boxes
  split.round = .25)  # round the split box corners a tad
dev.off()

# --------------------------------------
# Random forests (simple)
# --------------------------------------
set.seed (9999)  # for reproducibility
rf.train.fit <- randomForest(simple.model, 
  data=houses.train, mtry=3, importance=TRUE, na.action=na.omit) 

# review the random forest solution      
print(rf.train.fit)  

# check importance of the individual explanatory variables 
pdf(file = "fig_spatial_random_forest_simple_importance.pdf", 
width = 11, height = 8.5)
varImpPlot(rf.train.fit, main = "", pch = 20, col = "darkblue")
dev.off()

# random forest predictions for the training set
houses.train$rf.train.fit.pred <- predict(rf.train.fit, type="class", 
  newdata = houses.train)

# root-mean-squared for random forest on training set
print(rmspe(houses.train$log_value, houses.train$rf.train.fit.pred)) 
# report R-squared on training data
print(cor(houses.train$log_value,houses.train$rf.train.fit.pred)^2)

cat("\n\nTraining set proportion of variance accounted",
    "for by random forests (simple model) = ",
  sprintf("%1.3f",
  cor(houses.train$log_value,houses.train$rf.train.fit.pred)^2),sep=" ")
    
# random forest predictions for the test set using model from training set
houses.test$rf.train.fit.pred <- predict(rf.train.fit, 
  type="class", newdata = houses.test)

# root-mean-squared for random forest on test set
print(rmspe(houses.test$log_value, houses.test$rf.train.fit.pred)) 
# report R-squared on training data
print(cor(houses.test$log_value,houses.test$rf.train.fit.pred)^2)

cat("\n\nTest set proportion of variance accounted",
    " for by random forests (simple model) = ",
  sprintf("%1.3f",
  cor(houses.test$log_value,houses.test$rf.train.fit.pred)^2),sep=" ")

# --------------------------------------
# Random forests (full)
# --------------------------------------
set.seed (9999)  # for reproducibility
rf.train.fit.full <- randomForest(full.model, 
  data=houses.train, mtry=3, importance=TRUE, na.action=na.omit) 

# review the random forest solution      
print(rf.train.fit.full)  

# check importance of the individual explanatory variables 
pdf(file = "fig_spatial_random_forest_full_importance.pdf", 
width = 11, height = 8.5)
varImpPlot(rf.train.fit.full, main = "", pch = 20, 
  cex = 1.25, col = "darkblue", lcolor = "black")
dev.off()

# random forest predictions for the training set
houses.train$rf.train.fit.full.pred <- predict(rf.train.fit.full, type="class", 
  newdata = houses.train)

# root-mean-squared for random forest on training set
print(rmspe(houses.train$log_value, houses.train$rf.train.fit.full.pred)) 
# report R-squared on training data
print(cor(houses.train$log_value,houses.train$rf.train.fit.full.pred)^2)

cat("\n\nTraining set proportion of variance accounted",
    " for by random forests (full model) = ",
  sprintf("%1.3f",cor(houses.train$log_value,
    houses.train$rf.train.fit.full.pred)^2),sep=" ")
    
# random forest predictions for the test set using model from training set
houses.test$rf.train.fit.full.pred <- predict(rf.train.fit.full, type="class", 
  newdata = houses.test)

# root-mean-squared for random forest on test set
print(rmspe(houses.test$log_value, houses.test$rf.train.fit.full.pred)) 
# report R-squared on training data
print(cor(houses.test$log_value,houses.test$rf.train.fit.full.pred)^2)

cat("\n\nTest set proportion of variance accounted",
    " for by random forests (full model) = ",
  sprintf("%1.3f",cor(houses.test$log_value,
    houses.test$rf.train.fit.full.pred)^2),sep=" ")
           
# --------------------------------------
# Geographically weighted regression
# --------------------------------------    
# bandwidth calculation may take a while
set.bandwidth <-  gwr.sel(pace.barry.model, 
  data=houses.train, verbose = FALSE, show.error.messages = FALSE) 

# fit the geographically-weighted regression with bandwidth value set.bandwidth
gwr.train.fit <- gwr(pace.barry.model, bandwidth = set.bandwidth, 
  predictions = TRUE, data=houses.train, fit.points = houses.train)
# extract training set predictions
houses.train$grw.train.fit.pred <- gwr.train.fit$SDF$pred  

# root-mean-squared for grw on training set
print(rmspe(houses.train$log_value, houses.train$grw.train.fit.pred)) 
# report R-squared on training data
print(cor(houses.train$log_value,houses.train$grw.train.fit.pred)^2)

cat("\n\nTraining set proportion of variance accounted",
  " for by geographically-weighted regression = ",
  sprintf("%1.3f",cor(houses.train$log_value,
  houses.train$grw.train.fit.pred)^2),sep=" ")

# fit the geographically-weighted regression with bandwidth value set.bandwidth
# fit to training data and specify test data
gwr.train.fit <- gwr(pace.barry.model, bandwidth = set.bandwidth, 
  predictions = TRUE, data=houses.train, fit.points = houses.test)
# extract test set predictions
houses.test$grw.train.fit.pred <- gwr.train.fit$SDF$pred  

# root-mean-squared for grw on test set
print(rmspe(houses.test$log_value, houses.test$grw.train.fit.pred)) 
# report R-squared on training data
print(cor(houses.test$log_value,houses.test$grw.train.fit.pred)^2)

cat("\n\nTest set proportion of variance accounted",
  " for by geographically-weighted regression = ",
  sprintf("%1.3f",cor(houses.test$log_value,
  houses.test$grw.train.fit.pred)^2),sep=" ")

# --------------------------------------
# Gather results for a single report
# --------------------------------------     
# measurement model performance summary
methods <- c("Linear regression Pace and Barry (1997)",
  "Tree-structured regression (simple model)",
  "Tree-structured regression (full model)",
  "Random forests (simple model)",
  "Random forests (full model)",
  "Geographically weighted regression (GWR)")
methods.performance.data.frame <- data.frame(methods)

methods.performance.data.frame$training <- 
  c(round(cor(houses.train$log_value,predict(pace.barry.train.fit))^2
    ,digits=3),
    round(cor(houses.train$log_value,
    houses.train$rpart.train.fit.pred)^2,digits=3),
    round(cor(houses.train$log_value,
    houses.train$rpart.train.fit.full.pred)^2,digits=3),
    round(cor(houses.train$log_value,
    houses.train$rf.train.fit.pred)^2,digits=3),
     round(cor(houses.train$log_value,
     houses.train$rf.train.fit.full.pred)^2,digits=3),
    round(cor(houses.train$log_value,
    houses.train$grw.train.fit.pred)^2,digits=3))
  
methods.performance.data.frame$test <-
  c(round(cor(houses.test$log_value,
  predict(pace.barry.train.fit, newdata = houses.test))^2,digits=3),
    round(cor(houses.test$log_value,
    houses.test$rpart.train.fit.pred)^2,digits=3),
    round(cor(houses.test$log_value,
    houses.test$rpart.train.fit.full.pred)^2,digits=3),
    round(cor(houses.test$log_value,
    houses.test$rf.train.fit.pred)^2,digits=3),
    round(cor(houses.test$log_value,
    houses.test$rf.train.fit.full.pred)^2,digits=3),
    round(cor(houses.test$log_value,
    houses.test$grw.train.fit.pred)^2,digits=3))

print(methods.performance.data.frame)

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
# Explore the data further using data visualization and maps.
# Work on another metropolitan area in California.
# Determine the degree to which models built on one region
# generalize to other regions.

