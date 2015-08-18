# Sentiment Analysis Using the Movie Ratings Data (R)

# Note. Results from this program may differ from those published
#       in the book due to changes in the tm package.
#       The original analysis used the tm Dictionary() function,
#       which is no longer available in tm. This function has
#       been replaced by c(as.character()) to set the dictionary
#       as a character vector. Another necessary change concerns
#       the tolower() function, which must now be embedded within
#       the tm content_transformer() function.
# 
# Despite changes in the tm functions, we have retained the
# earlier positive and negative word lists for scoring, as
# implemented in the code and utilities appendix under the file
# name <R_utility_program_5.R>, which is brought in by source().

# install these packages before bringing them in by library()
library(tm)  # text mining and document management
library(stringr)  # character manipulation with regular expressions
library(grid)  # grid graphics utilities
library(ggplot2)  # graphics
library(latticeExtra) # package used for text horizon plot
library(caret)  # for confusion matrix function
library(rpart)  # tree-structured modeling
library(e1071)  # support vector machines
library(randomForest)  # random forests
library(rpart.plot)  # plot tree-structured model information

# R preliminaries to get the user-defined utilities for plotting 
# place the plotting code file <R_utility_program_3.R>
# in your working directory and execute it by
#     source("R_utility_program_3.R")
# Or if you have the R binary file in your working directory, use
#     load("mtpa_split_plotting_utilities.Rdata")
load("mtpa_split_plotting_utilities.Rdata")

# standardization needed for text measures
standardize <- function(x) {(x - mean(x)) / sd(x)}

# convert to bytecodes to avoid "invalid multibyte string" messages
bytecode.convert <- function(x) {iconv(enc2utf8(x), sub = "byte")}

# read in positive and negative word lists from Hu and Liu (2004)
positive.data.frame <- read.table(file = "Hu_Liu_positive_word_list.txt",
  header = FALSE, colClasses = c("character"), row.names = NULL, 
  col.names = "positive.words")
positive.data.frame$positive.words <- 
  bytecode.convert(positive.data.frame$positive.words)
  
negative.data.frame <- read.table(file = "Hu_Liu_negative_word_list.txt",
  header = FALSE, colClasses = c("character"), row.names = NULL, 
  col.names = "negative.words")  
negative.data.frame$negative.words <- 
  bytecode.convert(negative.data.frame$negative.words)

# we use movie ratings data from Mass et al. (2011) 
# available at http://ai.stanford.edu/~amaas/data/sentiment/
# we set up a directory under our working directory structure
# /reviews/train/unsup/ for the unsupervised reviews

directory.location <- 
  paste(getwd(),"/reviews/train/unsup/",sep = "")  
unsup.corpus <- Corpus(DirSource(directory.location, encoding = "UTF-8"),
  readerControl = list(language = "en_US"))
print(summary(unsup.corpus))

document.collection <- unsup.corpus

# strip white space from the documents in the collection
document.collection <- tm_map(document.collection, stripWhitespace)

# convert uppercase to lowercase in the document collection
document.collection <- tm_map(document.collection, content_transformer(tolower))

# remove numbers from the document collection
document.collection <- tm_map(document.collection, removeNumbers)

# remove punctuation from the document collection
document.collection <- tm_map(document.collection, removePunctuation)

# using a standard list, remove English stopwords from the document collection
document.collection <- tm_map(document.collection, 
  removeWords, stopwords("english"))

# there is more we could do in terms of data preparation 
# stemming... looking for contractions... possessives... 
# previous analysis of a list of top terms showed a number of word 
# contractions which we might like to drop from further analysis, 
# recognizing them as stop words to be dropped from the document collection
initial.tdm <- TermDocumentMatrix(document.collection)
examine.tdm <- removeSparseTerms(initial.tdm, sparse = 0.96)
top.words <- Terms(examine.tdm)
print(top.words)  

more.stop.words <- c("cant","didnt","doesnt","dont","goes","isnt","hes",
  "shes","thats","theres","theyre","wont","youll","youre","youve") 
document.collection <- tm_map(document.collection, 
  removeWords, more.stop.words)
  
some.proper.nouns.to.remove <- 
  c("dick","ginger","hollywood","jack","jill","john","karloff",
    "kudrow","orson","peter","tcm","tom","toni","welles","william","wolheim")
document.collection <- tm_map(document.collection, 
  removeWords, some.proper.nouns.to.remove)
  
# there is still more we could do in terms of data preparation  
# but we will work with the bag of words we have for now
  
# the workhorse technique will be TermDocumentMatrix()
# for creating a terms-by-documents matrix across the document collection
# in previous text analytics with the taglines data we let the data
# guide us to the text measures... with sentiment analysis we have
# positive and negative dictionaries (to a large extent) defined in 
# advance of looking at the data...
# positive.words and negative.words lists were read in earlier
# these come from the work of Hu and Liu (2004)   
# positive.words = list of  positive words
# negative.words = list of  negative words
# we will start with these lists to build dictionaries 
# that seem to make sense for movie reviews analysis
# Hu.Liu.positive.dictionary <- Dictionary(positive.data.frame$positive.words)
Hu.Liu.positive.dictionary <- 
    c(as.character(positive.data.frame$positive.words))
reviews.tdm.Hu.Liu.positive <- TermDocumentMatrix(document.collection, 
  list(dictionary = Hu.Liu.positive.dictionary))
examine.tdm <- removeSparseTerms(reviews.tdm.Hu.Liu.positive, 0.95)
top.words <- Terms(examine.tdm)
print(top.words)  
Hu.Liu.frequent.positive <- findFreqTerms(reviews.tdm.Hu.Liu.positive, 25)
# this provides a list positive words occurring at least 25 times
# a review of this list suggests that all make sense (have content validity)
# test.positive.dictionary <- Dictionary(Hu.Liu.frequent.positive)
test.positive.dictionary <- c(as.character(Hu.Liu.frequent.positive))

# .... now for the negative words
# Hu.Liu.negative.dictionary <- Dictionary(negative.data.frame$negative.words)
Hu.Liu.negative.dictionary <- 
    c(as.character(negative.data.frame$negative.words))
reviews.tdm.Hu.Liu.negative <- TermDocumentMatrix(document.collection, 
  list(dictionary = Hu.Liu.negative.dictionary))
examine.tdm <- removeSparseTerms(reviews.tdm.Hu.Liu.negative, 0.97)
top.words <- Terms(examine.tdm)
print(top.words)    
Hu.Liu.frequent.negative <- findFreqTerms(reviews.tdm.Hu.Liu.negative, 15)  
# this provides a short list negative words occurring at least 15 times
# across the document collection... one of these words seems out of place
# as they could be thought of as positive: "funny" 
test.negative <- setdiff(Hu.Liu.frequent.negative,c("funny"))
# test.negative.dictionary <- Dictionary(test.negative) 
test.negative.dictionary <- c(as.character(test.negative))  

# we need to evaluate the text measures we have defined
# for each of the documents count the total words 
# and the number of words that match the positive and negative dictionaries
total.words <- integer(length(names(document.collection)))
positive.words <- integer(length(names(document.collection)))
negative.words <- integer(length(names(document.collection)))
other.words <- integer(length(names(document.collection)))

reviews.tdm <- TermDocumentMatrix(document.collection)

for(index.for.document in seq(along=names(document.collection))) {
  positive.words[index.for.document] <- 
    sum(termFreq(document.collection[[index.for.document]], 
    control = list(dictionary = test.positive.dictionary)))
  negative.words[index.for.document] <- 
    sum(termFreq(document.collection[[index.for.document]], 
    control = list(dictionary = test.negative.dictionary)))  
  total.words[index.for.document] <- 
    length(reviews.tdm[,index.for.document][["i"]])
  other.words[index.for.document] <- total.words[index.for.document] -
    positive.words[index.for.document] - negative.words[index.for.document]
  }

document <- names(document.collection)
text.measures.data.frame <- data.frame(document,total.words,
  positive.words, negative.words, other.words, stringsAsFactors = FALSE) 
rownames(text.measures.data.frame) <- paste("D",as.character(0:499),sep="")

# compute text measures as percentages of words in each set
text.measures.data.frame$POSITIVE <- 
  100 * text.measures.data.frame$positive.words / 
  text.measures.data.frame$total.words
  
text.measures.data.frame$NEGATIVE <- 
  100 * text.measures.data.frame$negative.words / 
    text.measures.data.frame$total.words 
  
# let us look at the resulting text measures we call POSITIVE and NEGATIVE
# to see if negative and positive dimensions appear to be on a common scale
# that is... is this a single dimension in the document space
# we use principal component biplots to explore text measures 
# here we can use the technique to check on POSITIVE and NEGATIVE
principal.components.solution <- 
  princomp(text.measures.data.frame[,c("POSITIVE","NEGATIVE")], cor = TRUE)
print(summary(principal.components.solution))  
# biplot rendering of text measures and documents by year
pdf(file = "fig_sentiment_text_measures_biplot.pdf", width = 8.5, height = 11)
biplot(principal.components.solution, xlab = "First Pricipal Component",
  xlabs = rep("o", times = length(names(document.collection))),
  ylab = "Second Principal Component", expand = 0.7)
dev.off()

# results... the eigenvalues suggest that there are two underlying dimensions
# POSITIVE and NEGATIVE vectors rather than pointing in opposite directions
# they appear to be othogonal to one another... separate dimensions

# here we see the scatter plot for the two measures... 
# if they were on the same dimension, they would be negatively correlated
# in fact they are correlated negatively but the correlation is very small
with(text.measures.data.frame, print(cor(POSITIVE, NEGATIVE)))  
pdf(file = "fig_sentiment_text_measures_scatter_plot.pdf", 
  width = 8.5, height = 8.5)
ggplot.object <- ggplot(data = text.measures.data.frame,
  aes(x = NEGATIVE, y = POSITIVE)) + 
    geom_point(colour = "darkblue", shape = 1)
ggplot.print.with.margins(ggplot.object.name = ggplot.object,
  left.margin.pct=10, right.margin.pct=10,
  top.margin.pct=10,bottom.margin.pct=10)
dev.off()
  
# Perhaps POSITIVE and NEGATIVE can be combined in a way to yield effective 
# predictions of movie ratings. Let us move to a set of movie reviews for 
# supervised learning.  We select the 500 records from a set of positive 
# reviews (ratings between 7 and 10) and 500 records from a set of negative 
# reviews (ratings between 1 and 4).

# a set of 500 positive reviews... part of the training set
directory.location <- 
  paste(getwd(),"/reviews/train/pos/",sep = "")  

pos.train.corpus <- Corpus(DirSource(directory.location, encoding = "UTF-8"),
  readerControl = list(language = "en_US"))
print(summary(pos.train.corpus))

# a set of 500 negative reviews... part of the training set
directory.location <- 
  paste(getwd(),"/reviews/train/neg/",sep = "")  

neg.train.corpus <- Corpus(DirSource(directory.location, encoding = "UTF-8"),
  readerControl = list(language = "en_US"))
print(summary(neg.train.corpus))

# combine the positive and negative training sets
train.corpus <- c(pos.train.corpus, neg.train.corpus)

# strip white space from the documents in the collection
train.corpus <- tm_map(train.corpus, stripWhitespace)

# convert uppercase to lowercase in the document collection
train.corpus <- tm_map(train.corpus, content_transformer(tolower))

# remove numbers from the document collection
train.corpus <- tm_map(train.corpus, removeNumbers)

# remove punctuation from the document collection
train.corpus <- tm_map(train.corpus, removePunctuation)

# using a standard list, remove English stopwords from the document collection
train.corpus <- tm_map(train.corpus, 
  removeWords, stopwords("english"))

# there is more we could do in terms of data preparation 
# stemming... looking for contractions... possessives... 
# previous analysis of a list of top terms showed a number of word 
# contractions which we might like to drop from further analysis, 
# recognizing them as stop words to be dropped from the document collection
initial.tdm <- TermDocumentMatrix(train.corpus)
examine.tdm <- removeSparseTerms(initial.tdm, sparse = 0.96)
top.words <- Terms(examine.tdm)
print(top.words)  

more.stop.words <- c("cant","didnt","doesnt","dont","goes","isnt","hes",
  "shes","thats","theres","theyre","wont","youll","youre","youve") 
train.corpus <- tm_map(train.corpus, 
  removeWords, more.stop.words)
  
some.proper.nouns.to.remove <- 
  c("dick","ginger","hollywood","jack","jill","john","karloff",
    "kudrow","orson","peter","tcm","tom","toni","welles","william","wolheim")
train.corpus <- tm_map(train.corpus, 
  removeWords, some.proper.nouns.to.remove)

# compute list-based text measures for the training corpus
# for each of the documents count the total words 
# and the number of words that match the positive and negative dictionaries
total.words <- integer(length(names(train.corpus)))
positive.words <- integer(length(names(train.corpus)))
negative.words <- integer(length(names(train.corpus)))
other.words <- integer(length(names(train.corpus)))

reviews.tdm <- TermDocumentMatrix(train.corpus)

for(index.for.document in seq(along=names(train.corpus))) {
  positive.words[index.for.document] <- 
    sum(termFreq(train.corpus[[index.for.document]], 
    control = list(dictionary = test.positive.dictionary)))
  negative.words[index.for.document] <- 
    sum(termFreq(train.corpus[[index.for.document]], 
    control = list(dictionary = test.negative.dictionary)))  
  total.words[index.for.document] <- 
    length(reviews.tdm[,index.for.document][["i"]])
  other.words[index.for.document] <- total.words[index.for.document] -
    positive.words[index.for.document] - negative.words[index.for.document]
  }

document <- names(train.corpus)
train.data.frame <- data.frame(document,total.words,
  positive.words, negative.words, other.words, stringsAsFactors = FALSE) 
rownames(train.data.frame) <- paste("D",as.character(0:999),sep="")

# compute text measures as percentages of words in each set
train.data.frame$POSITIVE <- 
  100 * train.data.frame$positive.words / 
  train.data.frame$total.words
  
train.data.frame$NEGATIVE <- 
  100 * train.data.frame$negative.words / 
    train.data.frame$total.words 
    
# rating is embedded in the document name... extract with regular expressions
for(index.for.document in seq(along = train.data.frame$document)) {
  first_split <- strsplit(train.data.frame$document[index.for.document], 
    split = "[_]")
  second_split <- strsplit(first_split[[1]][2], split = "[.]")
  train.data.frame$rating[index.for.document] <- as.numeric(second_split[[1]][1])
  } # end of for-loop for defining ratings and thumbsupdown

train.data.frame$thumbsupdown <- ifelse((train.data.frame$rating > 5), 2, 1)
train.data.frame$thumbsupdown <- 
  factor(train.data.frame$thumbsupdown, levels = c(1,2), 
    labels = c("DOWN","UP"))

# a set of 500 positive reviews... part of the test set
directory.location <- 
  paste(getwd(),"/reviews/test/pos/",sep = "")  

pos.test.corpus <- Corpus(DirSource(directory.location, encoding = "UTF-8"),
  readerControl = list(language = "en_US"))
print(summary(pos.test.corpus))

# a set of 500 negative reviews... part of the test set
directory.location <- 
  paste(getwd(),"/reviews/test/neg/",sep = "")  
 
neg.test.corpus <- Corpus(DirSource(directory.location, encoding = "UTF-8"),
  readerControl = list(language = "en_US"))
print(summary(neg.test.corpus))

# combine the positive and negative testing sets
test.corpus <- c(pos.test.corpus, neg.test.corpus)

# strip white space from the documents in the collection
test.corpus <- tm_map(test.corpus, stripWhitespace)

# convert uppercase to lowercase in the document collection
test.corpus <- tm_map(test.corpus, content_transformer(tolower))

# remove numbers from the document collection
test.corpus <- tm_map(test.corpus, removeNumbers)

# remove punctuation from the document collection
test.corpus <- tm_map(test.corpus, removePunctuation)

# using a standard list, remove English stopwords from the document collection
test.corpus <- tm_map(test.corpus, 
  removeWords, stopwords("english"))

# there is more we could do in terms of data preparation 
# stemming... looking for contractions... possessives... 
# previous analysis of a list of top terms showed a number of word 
# contractions which we might like to drop from further analysis, 
# recognizing them as stop words to be dropped from the document collection
initial.tdm <- TermDocumentMatrix(test.corpus)
examine.tdm <- removeSparseTerms(initial.tdm, sparse = 0.96)
top.words <- Terms(examine.tdm)
print(top.words)  

more.stop.words <- c("cant","didnt","doesnt","dont","goes","isnt","hes",
  "shes","thats","theres","theyre","wont","youll","youre","youve") 
test.corpus <- tm_map(test.corpus, 
  removeWords, more.stop.words)
  
some.proper.nouns.to.remove <- 
  c("dick","ginger","hollywood","jack","jill","john","karloff",
    "kudrow","orson","peter","tcm","tom","toni","welles","william","wolheim")
test.corpus <- tm_map(test.corpus, 
  removeWords, some.proper.nouns.to.remove)

# compute list-based text measures for the testing corpus
# for each of the documents count the total words 
# and the number of words that match the positive and negative dictionaries
total.words <- integer(length(names(test.corpus)))
positive.words <- integer(length(names(test.corpus)))
negative.words <- integer(length(names(test.corpus)))
other.words <- integer(length(names(test.corpus)))

reviews.tdm <- TermDocumentMatrix(test.corpus)

for(index.for.document in seq(along=names(test.corpus))) {
  positive.words[index.for.document] <- 
    sum(termFreq(test.corpus[[index.for.document]], 
    control = list(dictionary = test.positive.dictionary)))
  negative.words[index.for.document] <- 
    sum(termFreq(test.corpus[[index.for.document]], 
    control = list(dictionary = test.negative.dictionary)))  
  total.words[index.for.document] <- 
    length(reviews.tdm[,index.for.document][["i"]])
  other.words[index.for.document] <- total.words[index.for.document] -
    positive.words[index.for.document] - negative.words[index.for.document]
  }

document <- names(test.corpus)
test.data.frame <- data.frame(document,total.words,
  positive.words, negative.words, other.words, stringsAsFactors = FALSE) 
rownames(test.data.frame) <- paste("D",as.character(0:999),sep="")

# compute text measures as percentages of words in each set
test.data.frame$POSITIVE <- 
  100 * test.data.frame$positive.words / 
  test.data.frame$total.words
  
test.data.frame$NEGATIVE <- 
  100 * test.data.frame$negative.words / 
    test.data.frame$total.words 
    
# rating is embedded in the document name... extract with regular expressions
for(index.for.document in seq(along = test.data.frame$document)) {
  first_split <- strsplit(test.data.frame$document[index.for.document], 
    split = "[_]")
  second_split <- strsplit(first_split[[1]][2], split = "[.]")
  test.data.frame$rating[index.for.document] <- as.numeric(second_split[[1]][1])
  } # end of for-loop for defining 

test.data.frame$thumbsupdown <- ifelse((test.data.frame$rating > 5), 2, 1)
test.data.frame$thumbsupdown <- 
  factor(test.data.frame$thumbsupdown, levels = c(1,2), 
    labels = c("DOWN","UP"))

# a set of 4 positive and 4 negative reviews... testing set of Tom's reviews
directory.location <- 
  paste(getwd(),"/reviews/test/tom/",sep = "")  

tom.corpus <- Corpus(DirSource(directory.location, encoding = "UTF-8"),
  readerControl = list(language = "en_US"))
print(summary(tom.corpus))

# strip white space from the documents in the collection
tom.corpus <- tm_map(tom.corpus, stripWhitespace)

# convert uppercase to lowercase in the document collection
tom.corpus <- tm_map(tom.corpus, content_transformer(tolower))

# remove numbers from the document collection
tom.corpus <- tm_map(tom.corpus, removeNumbers)

# remove punctuation from the document collection
tom.corpus <- tm_map(tom.corpus, removePunctuation)

# using a standard list, remove English stopwords from the document collection
tom.corpus <- tm_map(tom.corpus, 
  removeWords, stopwords("english"))

# there is more we could do in terms of data preparation 
# stemming... looking for contractions... possessives... 
# previous analysis of a list of top terms showed a number of word 
# contractions which we might like to drop from further analysis, 
# recognizing them as stop words to be dropped from the document collection
initial.tdm <- TermDocumentMatrix(tom.corpus)
examine.tdm <- removeSparseTerms(initial.tdm, sparse = 0.96)
top.words <- Terms(examine.tdm)
print(top.words)  

more.stop.words <- c("cant","didnt","doesnt","dont","goes","isnt","hes",
  "shes","thats","theres","theyre","wont","youll","youre","youve") 
tom.corpus <- tm_map(tom.corpus, 
  removeWords, more.stop.words)
  
some.proper.nouns.to.remove <- 
  c("dick","ginger","hollywood","jack","jill","john","karloff",
    "kudrow","orson","peter","tcm","tom","toni","welles","william","wolheim")
tom.corpus <- tm_map(tom.corpus, 
  removeWords, some.proper.nouns.to.remove)

# compute list-based text measures for tom corpus
# for each of the documents count the total words 
# and the number of words that match the positive and negative dictionaries
total.words <- integer(length(names(tom.corpus)))
positive.words <- integer(length(names(tom.corpus)))
negative.words <- integer(length(names(tom.corpus)))
other.words <- integer(length(names(tom.corpus)))

reviews.tdm <- TermDocumentMatrix(tom.corpus)

for(index.for.document in seq(along=names(tom.corpus))) {
  positive.words[index.for.document] <- 
    sum(termFreq(tom.corpus[[index.for.document]], 
    control = list(dictionary = test.positive.dictionary)))
  negative.words[index.for.document] <- 
    sum(termFreq(tom.corpus[[index.for.document]], 
    control = list(dictionary = test.negative.dictionary)))  
  total.words[index.for.document] <- 
    length(reviews.tdm[,index.for.document][["i"]])
  other.words[index.for.document] <- total.words[index.for.document] -
    positive.words[index.for.document] - negative.words[index.for.document]
  }

document <- names(tom.corpus)
tom.data.frame <- data.frame(document,total.words,
  positive.words, negative.words, other.words, stringsAsFactors = FALSE) 
rownames(tom.data.frame) <- paste("D",as.character(0:7),sep="")

# compute text measures as percentages of words in each set
tom.data.frame$POSITIVE <- 
  100 * tom.data.frame$positive.words / 
  tom.data.frame$total.words
  
tom.data.frame$NEGATIVE <- 
  100 * tom.data.frame$negative.words / 
    tom.data.frame$total.words 
    
# rating is embedded in the document name... extract with regular expressions
for(index.for.document in seq(along = tom.data.frame$document)) {
  first_split <- strsplit(tom.data.frame$document[index.for.document], 
    split = "[_]")
  second_split <- strsplit(first_split[[1]][2], split = "[.]")
  tom.data.frame$rating[index.for.document] <- as.numeric(second_split[[1]][1])
  } # end of for-loop for defining 

tom.data.frame$thumbsupdown <- ifelse((tom.data.frame$rating > 5), 2, 1)
tom.data.frame$thumbsupdown <- 
  factor(tom.data.frame$thumbsupdown, levels = c(1,2), 
    labels = c("DOWN","UP"))

tom.movies <- data.frame(movies = 
  c("The Effect of Gamma Rays on Man-in-the-Moon Marigolds",
    "Blade Runner","My Cousin Vinny","Mars Attacks",
    "Fight Club","Miss Congeniality 2","Find Me Guilty","Moneyball"))

# check out the measures on Tom's ratings
tom.data.frame.review <- 
  cbind(tom.movies,tom.data.frame[,names(tom.data.frame)[2:9]])
print(tom.data.frame.review)

# develop predictive models using the training data
# --------------------------------------
# Simple difference method
# --------------------------------------
train.data.frame$simple <- 
     train.data.frame$POSITIVE - train.data.frame$NEGATIVE

# check out simple difference method... is there a correlation with ratings?
with(train.data.frame, print(cor(simple, rating)))  

# we use the training data to define an optimal cutoff... 
# trees can help with finding the optimal split point for simple.difference
try.tree <- rpart(thumbsupdown ~ simple, data = train.data.frame)
print(try.tree)  # note that the first split value
# an earlier analysis had this value as -0.7969266
# create a user-defined function for the simple difference method
predict.simple <- function(x) {
  if (x >= -0.7969266) return("UP")
  if (x < -0.7969266) return("DOWN")
  }

# evaluate predictive accuracy in the training data
train.data.frame$pred.simple <- character(nrow(train.data.frame))
for (index.for.review in seq(along = train.data.frame$pred.simple)) {
  train.data.frame$pred.simple[index.for.review] <- 
    predict.simple(train.data.frame$simple[index.for.review])
  }   
train.data.frame$pred.simple <- 
  factor(train.data.frame$pred.simple)

train.pred.simple.performance <- 
  confusionMatrix(data = train.data.frame$pred.simple, 
  reference = train.data.frame$thumbsupdown, positive = "UP") 
  
# report full set of statistics relating to predictive accuracy
print(train.pred.simple.performance)

cat("\n\nTraining set percentage correctly predicted by",
  " simple difference method = ",
  sprintf("%1.1f",train.pred.simple.performance$overall[1]*100)," Percent",sep="")

# evaluate predictive accuracy in the test data
# SIMPLE DIFFERENCE METHOD
test.data.frame$simple <- 
     test.data.frame$POSITIVE - train.data.frame$NEGATIVE

test.data.frame$pred.simple <- character(nrow(test.data.frame))
for (index.for.review in seq(along = test.data.frame$pred.simple)) {
  test.data.frame$pred.simple[index.for.review] <- 
    predict.simple(test.data.frame$simple[index.for.review])
  }   
test.data.frame$pred.simple <- 
  factor(test.data.frame$pred.simple)

test.pred.simple.performance <- 
  confusionMatrix(data = test.data.frame$pred.simple, 
  reference = test.data.frame$thumbsupdown, positive = "UP") 
  
# report full set of statistics relating to predictive accuracy
print(test.pred.simple.performance)

cat("\n\nTest set percentage correctly predicted = ",
  sprintf("%1.1f",test.pred.simple.performance$overall[1]*100)," 
    Percent",sep="")

# --------------------------------------
# Regression difference method
# --------------------------------------
# regression method for determining weights on POSITIVE AND NEGATIVE
# fit a regression model to the training data
regression.model <- lm(rating ~ POSITIVE + NEGATIVE, data = train.data.frame)
print(regression.model)  # provides 5.5386 + 0.2962(POSITIVE) -0.3089(NEGATIVE)

train.data.frame$regression <- 
  predict(regression.model, newdata = train.data.frame)

# determine the cutoff for regression.difference
  try.tree <- rpart(thumbsupdown ~ regression, data = train.data.frame)
print(try.tree)  # note that the first split is at 5.264625
# create a user-defined function for the simple difference method
predict.regression <- function(x) {
  if (x >= 5.264625) return("UP")
  if (x < 5.264625) return("DOWN")
  }

train.data.frame$pred.regression <-  character(nrow(train.data.frame))
for (index.for.review in seq(along = train.data.frame$pred.simple)) {
  train.data.frame$pred.regression[index.for.review] <- 
    predict.regression(train.data.frame$regression[index.for.review])
  }   
train.data.frame$pred.regression <- 
  factor(train.data.frame$pred.regression)

train.pred.regression.performance <- 
  confusionMatrix(data = train.data.frame$pred.regression, 
  reference = train.data.frame$thumbsupdown, positive = "UP") 
  
# report full set of statistics relating to predictive accuracy
print(train.pred.regression.performance)  # result 67.3 percent

cat("\n\nTraining set percentage correctly predicted by regression = ",
  sprintf("%1.1f",train.pred.regression.performance$overall[1]*100),
    " Percent",sep="")

# regression method for determining weights on POSITIVE AND NEGATIVE
# for the test set we use the model developed on the training set
test.data.frame$regression <- 
  predict(regression.model, newdata = test.data.frame)

test.data.frame$pred.regression <-  character(nrow(test.data.frame))
for (index.for.review in seq(along = test.data.frame$pred.simple)) {
  test.data.frame$pred.regression[index.for.review] <- 
    predict.regression(test.data.frame$regression[index.for.review])
  }   
test.data.frame$pred.regression <- 
  factor(test.data.frame$pred.regression)

test.pred.regression.performance <- 
  confusionMatrix(data = test.data.frame$pred.regression, 
  reference = test.data.frame$thumbsupdown, positive = "UP") 
  
# report full set of statistics relating to predictive accuracy
print(test.pred.regression.performance)  # result 67.3 percent

cat("\n\nTest set percentage correctly predicted = ",
  sprintf("%1.1f",test.pred.regression.performance$overall[1]*100),
    " Percent",sep="")

# --------------------------------------------
# Word/item analysis method for train.corpus
# --------------------------------------------
# return to the training corpus to develop simple counts
# for each of the words in the sentiment list
# these new variables will be given the names of the words
# to keep things simple.... there are 50 such variables/words
# identified from an earlier analysis, as published in the book
working.corpus <- train.corpus
# run common code from utilities for scoring the working corpus
# this common code uses 25 positive and 25 negative words
# identified in an earlier analysis of these data
source("R_utility_program_5.R")

add.data.frame <- data.frame(amazing,beautiful,classic,enjoy,       
  enjoyed,entertaining,excellent,fans,favorite,fine,fun,humor,       
  lead,liked,love,loved,modern,nice,perfect,pretty,      
  recommend,strong,top,wonderful,worth,bad,boring,cheap,creepy,dark,dead,    
  death,evil,hard,kill,killed,lack,lost,miss,murder,mystery,plot,poor,    
  sad,scary,slow,terrible,waste,worst,wrong)  
  
train.data.frame <- cbind(train.data.frame,add.data.frame)  

# --------------------------------------------
# Word/item analysis method for test.corpus
# --------------------------------------------
# return to the testing corpus to develop simple counts
# for each of the words in the sentiment list
# these new variables will be given the names of the words
# to keep things simple.... there are 50 such variables/words
working.corpus <- test.corpus
# run common code from utilities for scoring the working corpus
source("R_utility_program_5.R")

add.data.frame <- data.frame(amazing,beautiful,classic,enjoy,       
  enjoyed,entertaining,excellent,fans,favorite,fine,fun,humor,       
  lead,liked,love,loved,modern,nice,perfect,pretty,      
  recommend,strong,top,wonderful,worth,bad,boring,cheap,creepy,dark,dead,    
  death,evil,hard,kill,killed,lack,lost,miss,murder,mystery,plot,poor,    
  sad,scary,slow,terrible,waste,worst,wrong)  
  
test.data.frame <- cbind(test.data.frame,add.data.frame)  

# --------------------------------------------
# Word/item analysis method for tom.corpus
# --------------------------------------------
# return to the toming corpus to develop simple counts
# for each of the words in the sentiment list
# these new variables will be given the names of the words
# to keep things simple.... there are 50 such variables/words
working.corpus <- tom.corpus
# run common code from utilities for scoring the working corpus
source("R_utility_program_5.R")

add.data.frame <- data.frame(amazing,beautiful,classic,enjoy,       
  enjoyed,entertaining,excellent,fans,favorite,fine,fun,humor,       
  lead,liked,love,loved,modern,nice,perfect,pretty,      
  recommend,strong,top,wonderful,worth,bad,boring,cheap,creepy,dark,dead,    
  death,evil,hard,kill,killed,lack,lost,miss,murder,mystery,plot,poor,    
  sad,scary,slow,terrible,waste,worst,wrong)  
  
tom.data.frame <- cbind(tom.data.frame,add.data.frame)  


# use phi coefficient... correlation with rating as index of item value
# again we draw upon the earlier positive and negative lists 
phi <- numeric(50)
item <- c("amazing","beautiful","classic","enjoy",       
  "enjoyed","entertaining","excellent","fans","favorite","fine","fun","humor",       
  "lead","liked","love","loved","modern","nice","perfect","pretty",      
  "recommend","strong","top","wonderful","worth",
  "bad","boring","cheap","creepy","dark","dead",    
  "death","evil","hard","kill","killed","lack",
  "lost","miss","murder","mystery","plot","poor",    
  "sad","scary","slow","terrible","waste","worst","wrong")
item.analysis.data.frame <- data.frame(item,phi)
item.place <- 14:63
for (index.for.column in 1:50) {
  item.analysis.data.frame$phi[index.for.column] <- 
    cor(train.data.frame[, item.place[index.for.column]],train.data.frame[,8])
  }

# sort by absolute value of the phi coefficient with the rating  
item.analysis.data.frame$absphi <- abs(item.analysis.data.frame$phi)
item.analysis.data.frame <- 
  item.analysis.data.frame[sort.list(item.analysis.data.frame$absphi,
    decreasing = TRUE),]
    
# subset of words with phi coefficients greater than 0.05 in absolute value    
selected.items.data.frame <- 
  subset(item.analysis.data.frame, subset = (absphi > 0.05))
  
# use the sign of the phi coefficient as the item weight
selected.positive.data.frame <-
  subset(selected.items.data.frame, subset = (phi > 0.0))
selected.positive.words <- as.character(selected.positive.data.frame$item)  
  
selected.negative.data.frame <-
  subset(selected.items.data.frame, subset = (phi < 0.0))  
selected.negative.words <- as.character(selected.negative.data.frame$item)    

# these lists define new dictionaries for scoring 

reviews.tdm <- TermDocumentMatrix(train.corpus)

temp.positive.score <- integer(length(names(train.corpus)))
temp.negative.score <- integer(length(names(train.corpus)))
for(index.for.document in seq(along=names(train.corpus))) {
  temp.positive.score[index.for.document] <- 
    sum(termFreq(train.corpus[[index.for.document]], 
    control = list(dictionary = selected.positive.words)))
  temp.negative.score[index.for.document] <- 
    sum(termFreq(train.corpus[[index.for.document]], 
    control = list(dictionary = selected.negative.words)))  
  }
  
train.data.frame$item.analysis.score <- 
  temp.positive.score - temp.negative.score
  
# use the training set and tree-structured modeling to determine the cutoff  
  try.tree<-rpart(thumbsupdown ~ item.analysis.score, data = train.data.frame)
print(try.tree)  # note that the first split is at -0.5
# create a user-defined function for the simple difference method
predict.item.analysis <- function(x) {
  if (x >= -0.5) return("UP")
  if (x < -0.5) return("DOWN")
  }
  
train.data.frame$pred.item.analysis <-  character(nrow(train.data.frame))
for (index.for.review in seq(along = train.data.frame$pred.simple)) {
  train.data.frame$pred.item.analysis[index.for.review] <- 
  predict.item.analysis(train.data.frame$item.analysis.score[index.for.review])
  }   
train.data.frame$pred.item.analysis <- 
  factor(train.data.frame$pred.item.analysis)

train.pred.item.analysis.performance <- 
  confusionMatrix(data = train.data.frame$pred.item.analysis, 
  reference = train.data.frame$thumbsupdown, positive = "UP") 
  
# report full set of statistics relating to predictive accuracy
print(train.pred.item.analysis.performance)  # result 73.9 Percent

cat("\n\nTraining set percentage correctly predicted by item analysis = ",
  sprintf("%1.1f",train.pred.item.analysis.performance$overall[1]*100),
    " Percent",sep="")  
    
# use item analysis method of scoring with the test set

reviews.tdm <- TermDocumentMatrix(test.corpus)

temp.positive.score <- integer(length(names(test.corpus)))
temp.negative.score <- integer(length(names(test.corpus)))
for(index.for.document in seq(along=names(test.corpus))) {
  temp.positive.score[index.for.document] <- 
    sum(termFreq(test.corpus[[index.for.document]], 
    control = list(dictionary = selected.positive.words)))
  temp.negative.score[index.for.document] <- 
    sum(termFreq(test.corpus[[index.for.document]], 
    control = list(dictionary = selected.negative.words)))  
  }
  
test.data.frame$item.analysis.score <- 
  temp.positive.score - temp.negative.score
    
test.data.frame$pred.item.analysis <-  character(nrow(test.data.frame))
for (index.for.review in seq(along = test.data.frame$pred.simple)) {
  test.data.frame$pred.item.analysis[index.for.review] <- 
  predict.item.analysis(test.data.frame$item.analysis.score[index.for.review])
  }   
test.data.frame$pred.item.analysis <- 
  factor(test.data.frame$pred.item.analysis)

test.pred.item.analysis.performance <- 
  confusionMatrix(data = test.data.frame$pred.item.analysis, 
  reference = test.data.frame$thumbsupdown, positive = "UP") 
  
# report full set of statistics relating to predictive accuracy
print(test.pred.item.analysis.performance)  # result 74 Percent

cat("\n\nTest set percentage correctly predicted by item analysis = ",
  sprintf("%1.1f",test.pred.item.analysis.performance$overall[1]*100),
    " Percent",sep="")  
    
# --------------------------------------
# Logistic regression method
# --------------------------------------
text.classification.model <- {thumbsupdown ~ amazing + beautiful + 
  classic + enjoy + enjoyed + 
  entertaining + excellent +   
  fans + favorite + fine + fun + humor + lead + liked +       
  love + loved + modern + nice + perfect + pretty + 
  recommend + strong + top + wonderful + worth +       
  bad + boring + cheap + creepy + dark + dead + 
  death + evil + hard + kill +    
  killed + lack + lost + miss + murder + mystery + 
  plot + poor + sad + scary +   
  slow + terrible + waste + worst + wrong}

# full logistic regression model
logistic.regression.fit <- glm(text.classification.model, 
  family=binomial(link=logit), data = train.data.frame)
print(summary(logistic.regression.fit))

# obtain predicted probability values for training set
logistic.regression.pred.prob <- 
  as.numeric(predict(logistic.regression.fit, newdata = train.data.frame,
  type="response")) 

train.data.frame$pred.logistic.regression <- 
  ifelse((logistic.regression.pred.prob > 0.5),2,1)

train.data.frame$pred.logistic.regression <- 
  factor(train.data.frame$pred.logistic.regression, levels = c(1,2), 
    labels = c("DOWN","UP"))

train.pred.logistic.regression.performance <- 
  confusionMatrix(data = train.data.frame$pred.logistic.regression, 
  reference = train.data.frame$thumbsupdown, positive = "UP") 

# report full set of statistics relating to predictive accuracy
print(train.pred.logistic.regression.performance)  # result 75.2 Percent

cat("\n\nTraining set percentage correct by logistic regression = ",
  sprintf("%1.1f",train.pred.logistic.regression.performance$overall[1]*100),
    " Percent",sep="")
    
# now we use the model developed on the training set with the test set
# obtain predicted probability values for test set
logistic.regression.pred.prob <- 
  as.numeric(predict(logistic.regression.fit, newdata = test.data.frame,
  type="response")) 

test.data.frame$pred.logistic.regression <- 
  ifelse((logistic.regression.pred.prob > 0.5),2,1)

test.data.frame$pred.logistic.regression <- 
  factor(test.data.frame$pred.logistic.regression, levels = c(1,2), 
    labels = c("DOWN","UP"))

test.pred.logistic.regression.performance <- 
  confusionMatrix(data = test.data.frame$pred.logistic.regression, 
  reference = test.data.frame$thumbsupdown, positive = "UP") 

# report full set of statistics relating to predictive accuracy
print(test.pred.logistic.regression.performance)  # result 72.6 Percent

cat("\n\nTest set percentage correctly predicted by logistic regression = ",
  sprintf("%1.1f",test.pred.logistic.regression.performance$overall[1]*100),
    " Percent",sep="")

# --------------------------------------
# Support vector machines
# --------------------------------------
# determine tuning parameters prior to fitting model
train.tune <- tune(svm, text.classification.model, data = train.data.frame,
                   ranges = list(gamma = 2^(-8:1), cost = 2^(0:4)),
                   tunecontrol = tune.control(sampling = "fix"))
# display the tuning results (in text format)
print(train.tune)

# fit the support vector machine to the training data using tuning parameters
train.data.frame.svm <- svm(text.classification.model, data = train.data.frame, 
  cost=4, gamma=0.00390625, probability = TRUE)
  
train.data.frame$pred.svm <- predict(train.data.frame.svm, type="class",
newdata=train.data.frame)

train.pred.svm.performance <- 
  confusionMatrix(data = train.data.frame$pred.svm, 
  reference = train.data.frame$thumbsupdown, positive = "UP") 

# report full set of statistics relating to predictive accuracy
print(train.pred.svm.performance)  # result 79.0 Percent

cat("\n\nTraining set percentage correctly predicted by SVM = ",
  sprintf("%1.1f",train.pred.svm.performance$overall[1]*100),
    " Percent",sep="")

# use the support vector machine model identified in the training set
# to do text classification on the test set
test.data.frame$pred.svm <- predict(train.data.frame.svm, type="class",
newdata=test.data.frame)

test.pred.svm.performance <- 
  confusionMatrix(data = test.data.frame$pred.svm, 
  reference = test.data.frame$thumbsupdown, positive = "UP") 

# report full set of statistics relating to predictive accuracy
print(test.pred.svm.performance)  # result 71.6 Percent

cat("\n\nTest set percentage correctly predicted by SVM = ",
  sprintf("%1.1f",test.pred.svm.performance$overall[1]*100),
    " Percent",sep="")

# --------------------------------------
# Random forests
# --------------------------------------
# fit random forest model to the training data
set.seed (9999)  # for reproducibility
train.data.frame.rf <- randomForest(text.classification.model, 
  data=train.data.frame, mtry=3, importance=TRUE, na.action=na.omit) 

# review the random forest solution      
print(train.data.frame.rf)  

# check importance of the individual explanatory variables 
pdf(file = "fig_sentiment_random_forest_importance.pdf", 
width = 11, height = 8.5)
varImpPlot(train.data.frame.rf, main = "")
dev.off()

train.data.frame$pred.rf <- predict(train.data.frame.rf, type="class", 
  newdata = train.data.frame)

train.pred.rf.performance <- 
  confusionMatrix(data = train.data.frame$pred.rf, 
  reference = train.data.frame$thumbsupdown, positive = "UP") 

# report full set of statistics relating to predictive accuracy
print(train.pred.rf.performance)  # result 82.2 Percent

cat("\n\nTraining set percentage correctly predicted by random forests = ",
  sprintf("%1.1f",train.pred.rf.performance$overall[1]*100),
    " Percent",sep="")
    
# use the model fit to the training data to predict the the test data     
test.data.frame$pred.rf <- predict(train.data.frame.rf, type="class", 
  newdata = test.data.frame)

test.pred.rf.performance <- 
  confusionMatrix(data = test.data.frame$pred.rf, 
  reference = test.data.frame$thumbsupdown, positive = "UP") 

# report full set of statistics relating to predictive accuracy
print(test.pred.rf.performance)  # result 74.0 Percent

cat("\n\nTest set percentage correctly predicted by random forests = ",
  sprintf("%1.1f",test.pred.rf.performance$overall[1]*100),
    " Percent",sep="")    
 
# measurement model performance summary
methods <- c("Simple difference","Regression difference",
  "Word/item analysis","Logistic regression",
  "Support vector machines","Random forests")
methods.performance.data.frame <- data.frame(methods)

methods.performance.data.frame$training <- 
  c(train.pred.simple.performance$overall[1]*100,
    train.pred.regression.performance$overall[1]*100,
    train.pred.item.analysis.performance$overall[1]*100,
    train.pred.logistic.regression.performance$overall[1]*100,
    train.pred.svm.performance$overall[1]*100,
    train.pred.rf.performance$overall[1]*100)
  
methods.performance.data.frame$test <-
  c(test.pred.simple.performance$overall[1]*100,
    test.pred.regression.performance$overall[1]*100,
    test.pred.item.analysis.performance$overall[1]*100,
    test.pred.logistic.regression.performance$overall[1]*100,
    test.pred.svm.performance$overall[1]*100,
    test.pred.rf.performance$overall[1]*100)

# random forest predictions for Tom's movie reviews
tom.data.frame$pred.rf <- predict(train.data.frame.rf, type="class", 
  newdata = tom.data.frame)
  
print(tom.data.frame[,c("thumbsupdown","pred.rf")])  

tom.pred.rf.performance <- 
  confusionMatrix(data = tom.data.frame$pred.rf, 
  reference = tom.data.frame$thumbsupdown, positive = "UP") 

# report full set of statistics relating to predictive accuracy
print(tom.pred.rf.performance)  # result 74.0 Percent

cat("\n\nTraining set percentage correctly predicted by random forests = ",    
  sprintf("%1.1f",tom.pred.rf.performance$overall[1]*100),
    " Percent",sep="")    

# building a simple tree to classify reviews
simple.tree <- rpart(text.classification.model, 
  data=train.data.frame,)

# plot the regression tree result from rpart
pdf(file = "fig_sentiment_simple_tree_classifier.pdf", width = 8.5, height = 8.5)
prp(simple.tree, main="",
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

# simple tree predictions for Tom's movie reviews
tom.data.frame$pred.simple.tree <- predict(simple.tree, type="class", 
  newdata = tom.data.frame)

print(tom.data.frame[,c("thumbsupdown","pred.rf","pred.simple.tree")])  

# Suggestions for the student:
# Employ stemming prior to the creation of terms-by-document matrices.
# Try alternative positive and negative word sets for sentiment scoring.
# Try word sets that relate to a wider variety of emotional or opinion states.
# Better still, move beyond a bag-of-words approach to sentiment. Use
# the tools of natural language processing and define text features
# based upon combinations of words such as bigrams (pairs of words)
# and taking note of parts of speech.  Yet another approach would be
# to define ignore negative and positive word lists and work directly 
# with identified text features that correlate with movie review ratings or
# do a good job of classifying reviews into positive and negative groups.
# Text features within text classification problems may be defined 
# on term document frequency alone or on measures of term document
# frequency adjusted by term corpus frequency. Using alternative 
# features and text measures as well as alternative classification methods,
# run a true benchmark within a loop, using hundreds or thousands of iterations.
# Use various methods of classifier performance to evaluate classifiers.

