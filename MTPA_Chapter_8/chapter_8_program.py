# Sentiment Analysis Using the Movie Ratings Data (Python)

# Note that results from this program may differ from the results
# documented in the book because algorithms for text parsing
# and text classification vary between Python and R. 
# The objectives of the analysis and steps in completing the analysis
# are consistent with those in the book. And results, although
# not identical between Python and R, should be very similar.

# prepare for Python version 3x features and functions
from __future__ import division, print_function

# import packages for text processing and machine learning
import os  # operating system commands
import re  # regular expressions
import nltk  # draw on the Python natural language toolkit
import pandas as pd  # DataFrame structure and operations
import numpy as np  # arrays and numerical processing
import matplotlib.pyplot as plt  # 2D plotting
import statsmodels.api as sm  # logistic regression
import statsmodels.formula.api as smf  # R-like model specification
import patsy  # translate model specification into design matrices
from sklearn import svm  # support vector machines
from sklearn.ensemble import RandomForestClassifier  # random forests

# import user-defined module
from python_utilities import evaluate_classifier, get_text_measures,\
    get_summative_scores

# list files in directory omitting hidden files
def listdir_no_hidden(path):
    start_list = os.listdir(path)
    end_list = []
    for file in start_list:
        if (not file.startswith('.')):
            end_list.append(file)
    return(end_list)            
            

# define list of codes to be dropped from document
# carriage-returns, line-feeds, tabs
codelist = ['\r', '\n', '\t']    

# there are certain words we will ignore in subsequent
# text processing... these are called stop-words 
# and they consist of prepositions, pronouns, and 
# conjunctions, interrogatives, ...
# we begin with the list from the natural language toolkit
# examine this initial list of stopwords
nltk.download('stopwords')

# let's look at that list 
print(nltk.corpus.stopwords.words('english'))

# previous analysis of a list of top terms showed a number of words, along 
# with contractions and other word strings to drop from further analysis, we add
# these to the usual English stopwords to be dropped from a document collection
more_stop_words = ['cant','didnt','doesnt','dont','goes','isnt','hes',\
    'shes','thats','theres','theyre','wont','youll','youre','youve', 'br'\
    've', 're', 'vs'] 

some_proper_nouns_to_remove = ['dick','ginger','hollywood','jack',\
    'jill','john','karloff','kudrow','orson','peter','tcm','tom',\
    'toni','welles','william','wolheim','nikita']

# start with the initial list and add to it for movie text work 
stoplist = nltk.corpus.stopwords.words('english') + more_stop_words +\
    some_proper_nouns_to_remove

# text parsing function for creating text documents 
# there is more we could do for data preparation 
# stemming... looking for contractions... possessives... 
# but we will work with what we have in this parsing function
# if we want to do stemming at a later time, we can use
#     porter = nltk.PorterStemmer()  
# in a construction like this
#     words_stemmed =  [porter.stem(word) for word in initial_words]  
def text_parse(string):
    # replace non-alphanumeric with space 
    temp_string = re.sub('[^a-zA-Z]', '  ', string)    
    # replace codes with space
    for i in range(len(codelist)):
        stopstring = ' ' + codelist[i] + '  '
        temp_string = re.sub(stopstring, '  ', temp_string)      
    # replace single-character words with space
    temp_string = re.sub('\s.\s', ' ', temp_string)   
    # convert uppercase to lowercase
    temp_string = temp_string.lower()    
    # replace selected character strings/stop-words with space
    for i in range(len(stoplist)):
        stopstring = ' ' + str(stoplist[i]) + ' '
        temp_string = re.sub(stopstring, ' ', temp_string)        
    # replace multiple blank characters with one blank character
    temp_string = re.sub('\s+', ' ', temp_string)    
    return(temp_string)    

# read in positive and negative word lists from Hu and Liu (2004)
with open('Hu_Liu_positive_word_list.txt','rt') as f:
    positive_word_list = f.read().split() 
with open('Hu_Liu_negative_word_list.txt','rt') as f:
    negative_word_list = f.read().split()   
    
# define counts of positive, negative, and total words in text document 
def count_positive(text):    
    positive = [w for w in text.split() if w in positive_word_list]
    return(len(positive))

# define text measure for negative score as percentage of negative words   
def count_negative(text):    
    negative = [w for w in text.split() if w in negative_word_list]
    return(len(negative))
    
# count number of words   
def count_total(text):    
    total = [w for w in text.split()]
    return(len(total))    

# define text measure for positive score as percentage of positive words    
def score_positive(text):    
    positive = [w for w in text.split() if w in positive_word_list]
    total = [w for w in text.split()]
    return 100 * len(positive)/len(total)

# define text measure for negative score as percentage of negative words   
def score_negative(text):    
    negative = [w for w in text.split() if w in negative_word_list]
    total = [w for w in text.split()]
    return 100 * len(negative)/len(total)

def compute_scores(corpus):
    # use the complete word lists for POSITIVE and NEGATIVE measures
    # to score all documents in a corpus or list of documents
    positive = []
    negative = []
    for document in corpus:
        positive.append(score_positive(document)) 
        negative.append(score_negative(document)) 
    return(positive, negative)
                           
# we use movie ratings data from Mass et al. (2011) 
# available at http://ai.stanford.edu/~amaas/data/sentiment/
# we set up a directory under our working directory structure
# /reviews/train/unsup/ for the unsupervised reviews
# /reviews/train/neg/ training set negative reviews
# /reviews/train/pos/ training set positive reviews
# /reviews/test/neg/ text set negative reviews
# /reviews/test/pos/ test set positive reviews
# /reviews/test/tom/ eight movie reviews from Tom

# function for creating corpus and aggregate document 
# input is directory path for documents
# document parsing accomplished by text_parse function
# directory of parsed files set up for manual inspection
def corpus_creator (input_directory_path, output_directory_path):
    # identify the file names in unsup directory
    file_names = listdir_no_hidden(path = input_directory_path)
    # create list structure for storing parsed documents 
    document_collection = [] 
    # initialize aggregate document for all documents in set
    aggregate_document = ''
    # create a directory for parsed files 
    parsed_file_directory = output_directory_path
    os.mkdir(parsed_file_directory)
    # parse each file and write to directory of parsed files
    for filename in file_names:
        with open(os.path.join(input_directory_path, filename), 'r') as infile:        
            this_document = text_parse(infile.read())
            aggregate_document = aggregate_document + this_document
            document_collection.append(this_document)
            outfile = parsed_file_directory + filename
            with open(outfile, 'wt') as f:
                f.write(str(this_document)) 
    aggregate_words = [w for w in aggregate_document.split()]   
    aggregate_corpus = nltk.Text(aggregate_words)    
    return(file_names, document_collection, aggregate_corpus)
    
# function for extracting rating from file name
# for file names of the form 'x_y.txt' where y is the rating
def get_rating(string):
    return(int(string.partition('.')[0].partition('_')[2])) 
    
# dictionary for mapping of ratings to thumbsupdown 
map_to_thumbsupdown = {1:'DOWN', 2:'DOWN', 3:'DOWN', 4:'DOWN',
    6:'UP', 7:'UP', 8:'UP', 9:'UP', 10:'UP'}     
    
# begin working with the unsup corpus
unsup_file_names, unsup_corpus, unsup_aggregate_corpus = \
    corpus_creator(input_directory_path = 'reviews/train/unsup/',\
        output_directory_path = 'reviews/train/unsup_parsed/')
                    
# examine frequency distribution of words in unsup corpus
unsup_freq = nltk.FreqDist(unsup_aggregate_corpus)
print('\nNumber of Unique Words in unsup corpus',len(unsup_freq.keys()))
print('\nTop Fifty Words in unsup Corpus:',unsup_freq.keys()[0:50])
         
# identify the most frequent unsup words from the positive word list
# here we use set intersection to find a list of the top 25 positive words 
length_test = 0  # initialize test length
nkeys = 0  # slicing index for frequency table extent
while (length_test < 25):
    length_test =\
        len(set(unsup_freq.keys()[:nkeys]) & set(positive_word_list))
    nkeys = nkeys + 1
selected_positive_set =\
    set(unsup_freq.keys()[:nkeys]) & set(positive_word_list)
selected_positive_words = list(selected_positive_set)
selected_positive_words.sort()
print('\nSelected Positive Words:', selected_positive_words)

# identify the most frequent unsup words from the negative word list
# here we use set intersection to find a list of the top 25 negative words 
length_test = 0  # initialize test length
nkeys = 0  # slicing index for frequency table extent
while (length_test < 25):
    length_test =\
        len(set(unsup_freq.keys()[:nkeys]) & set(negative_word_list))
    nkeys = nkeys + 1
selected_negative_set =\
    set(unsup_freq.keys()[:nkeys]) & set(negative_word_list)
# list is actually 26 items and contains both 'problem' and 'problems'
# so we will eliminate 'problems' from the selected negative words
selected_negative_set.remove('problems')
selected_negative_words = list(selected_negative_set)
selected_negative_words.sort()
print('\nSelected Negative Words:', selected_negative_words)

# use the complete word lists for POSITIVE and NEGATIVE measures/scores
positive, negative = compute_scores(unsup_corpus)

# create data frame to explore POSITIVE and NEGATIVE measures
unsup_data = {'file': unsup_file_names,\
    'POSITIVE': positive, 'NEGATIVE': negative}    
unsup_data_frame = pd.DataFrame(unsup_data)

# summary of distributions of POSITIVE and NEGATIVE scores for unsup corpus
print(unsup_data_frame.describe())

print('\nCorrelation between POSITIVE and NEGATIVE',\
    round(unsup_data_frame['POSITIVE'].corr(unsup_data_frame['NEGATIVE']),3))

# scatter plot of POSITIVE and NEGATIVE scores for unsup corpus
ax = plt.axes()
ax.scatter(unsup_data_frame['NEGATIVE'], unsup_data_frame['POSITIVE'],\
    facecolors = 'none', edgecolors = 'blue')
ax.set_xlabel('NEGATIVE')
ax.set_ylabel('POSITIVE')   
plt.savefig('fig_sentiment_text_measures_scatter_plot.pdf', 
    bbox_inches = 'tight', dpi=None, facecolor='none', edgecolor='blue', 
    orientation='portrait', papertype=None, format=None, 
    transparent=True, pad_inches=0.25, frameon=None)  

# work on the directory of training files-----------------------------------
# Perhaps POSITIVE and NEGATIVE can be combined in a way to yield effective 
# predictions of movie ratings. Let us move to a set of movie reviews for 
# supervised learning.  We select the 500 records from a set of positive 
# reviews (ratings between 7 and 10) and 500 records from a set of negative 
# reviews (ratings between 1 and 4). We begin with the training data.

# /reviews/train/pos/ training set positive reviews 
train_pos_file_names, train_pos_corpus, train_pos_aggregate_corpus = \
    corpus_creator(input_directory_path = 'reviews/train/pos/',\
        output_directory_path = 'reviews/train/pos_parsed/')
# use the complete word lists for POSITIVE and NEGATIVE measures/scores
positive, negative = compute_scores(train_pos_corpus)
rating = []
for file_name in train_pos_file_names:
    rating.append(get_rating(str(file_name)))

# create data frame to explore POSITIVE and NEGATIVE measures        
train_pos_data = {'train_test':['TRAIN'] * len(train_pos_file_names),\
    'pos_neg': ['POS'] * len(train_pos_file_names),\
    'file_name': train_pos_file_names,\
    'POSITIVE': positive, 'NEGATIVE': negative,\
    'rating': rating}        
train_pos_data_frame = pd.DataFrame(train_pos_data)

# /reviews/train/neg/ training set negative reviews
train_neg_file_names, train_neg_corpus, train_neg_aggregate_corpus = \
    corpus_creator(input_directory_path = 'reviews/train/neg/',\
        output_directory_path = 'reviews/train/neg_parsed/')
# use the complete word lists for POSITIVE and NEGATIVE measures/scores
positive, negative = compute_scores(train_neg_corpus)
rating = []
for file_name in train_neg_file_names:
    rating.append(get_rating(str(file_name)))

# create data frame to explore POSITIVE and NEGATIVE measures        
train_neg_data = {'train_test':['TRAIN'] * len(train_neg_file_names),\
    'pos_neg': ['NEG'] * len(train_neg_file_names),\
    'file_name': train_neg_file_names,\
    'POSITIVE': positive, 'NEGATIVE': negative,\
    'rating': rating}        
train_neg_data_frame = pd.DataFrame(train_neg_data)

# merge the positive and negative training data frames
train_data_frame = pd.concat([train_pos_data_frame, train_neg_data_frame],\
    axis = 0, ignore_index = True)

# determining thumbs up or down based on rating
train_data_frame['thumbsupdown'] = \
    train_data_frame['rating'].map(map_to_thumbsupdown)
# compute simple measure of sentiment as POSITIVE - NEGATIVE
train_data_frame['simple'] = \
    train_data_frame['POSITIVE'] - train_data_frame['NEGATIVE']    
# examine the data frame
print(pd.crosstab(train_data_frame['pos_neg'],\
    train_data_frame['thumbsupdown']))
print(train_data_frame.head())
print(train_data_frame.tail())
print(train_data_frame.describe())
ratings_grouped = train_data_frame['simple'].\
    groupby(train_data_frame['rating'])
print('\nTraining Data Simple Difference Means by Ratings:',\
    ratings_grouped.mean())
thumbs_grouped = \
    train_data_frame['simple'].groupby(train_data_frame['thumbsupdown'])
print('\nTraining Data Simple Difference Means by Thumbs UP/DOWN:',\
    thumbs_grouped.mean())
                
# repeat methods for the test data -----------------------------
# /reviews/test/pos/ testing set positive reviews
test_pos_file_names, test_pos_corpus, test_pos_aggregate_corpus = \
    corpus_creator(input_directory_path = 'reviews/test/pos/',\
        output_directory_path = 'reviews/test/pos_parsed/')
# use the complete word lists for POSITIVE and NEGATIVE measures/scores
positive, negative = compute_scores(test_pos_corpus)
rating = []
for file_name in test_pos_file_names:
    rating.append(get_rating(str(file_name)))

# create data frame to explore POSITIVE and NEGATIVE measures        
test_pos_data = {'train_test':['TEST'] * len(test_pos_file_names),\
    'pos_neg': ['POS'] * len(test_pos_file_names),\
    'file_name': test_pos_file_names,\
    'POSITIVE': positive, 'NEGATIVE': negative,\
    'rating': rating}        
test_pos_data_frame = pd.DataFrame(test_pos_data)

# /reviews/test/neg/ testing set negative reviews
test_neg_file_names, test_neg_corpus, test_neg_aggregate_corpus = \
    corpus_creator(input_directory_path = 'reviews/test/neg/',\
        output_directory_path = 'reviews/test/neg_parsed/')
# use the complete word lists for POSITIVE and NEGATIVE measures/scores
positive, negative = compute_scores(test_neg_corpus)
rating = []
for file_name in test_neg_file_names:
    rating.append(get_rating(str(file_name)))

# create data frame to explore POSITIVE and NEGATIVE measures        
test_neg_data = {'train_test':['TEST'] * len(test_neg_file_names),\
    'pos_neg': ['NEG'] * len(test_neg_file_names),\
    'file_name': test_neg_file_names,\
    'POSITIVE': positive, 'NEGATIVE': negative,\
    'rating': rating}        
test_neg_data_frame = pd.DataFrame(test_neg_data)

# merge the positive and negative testing data frames
test_data_frame = pd.concat([test_pos_data_frame, test_neg_data_frame],\
    axis = 0, ignore_index = True)

# determining thumbs up or down based on rating
test_data_frame['thumbsupdown'] = \
    test_data_frame['rating'].map(map_to_thumbsupdown)
# compute simple measure of sentiment as POSITIVE - NEGATIVE
test_data_frame['simple'] = \
    test_data_frame['POSITIVE'] - test_data_frame['NEGATIVE']    
# examine the data frame
print(pd.crosstab(test_data_frame['pos_neg'],\
    test_data_frame['thumbsupdown']))
print(test_data_frame.head())
print(test_data_frame.tail())
print(test_data_frame.describe())
ratings_grouped = test_data_frame['simple'].\
    groupby(test_data_frame['rating'])
print('\nTest Data Simple Difference Means by Ratings:',\
    ratings_grouped.mean())
thumbs_grouped = \
    test_data_frame['simple'].groupby(test_data_frame['thumbsupdown'])
print('\nTest Data Simple Difference Means by Thumbs UP/DOWN:',\
    thumbs_grouped.mean())
                               
# repeat methods for the Tom's movie reviews -----------------------------
# /reviews/test/tom/ testing set directory path
test_tom_file_names, test_tom_corpus, test_tom_aggregate_corpus = \
    corpus_creator(input_directory_path = 'reviews/test/tom/',\
        output_directory_path = 'reviews/test/tom_parsed/')

# word counts for Tom's reviews
positive_words = []
negative_words = []
total_words = []
for file in test_tom_corpus:
    positive_words.append(count_positive(file)) 
    negative_words.append(count_negative(file))       
    total_words.append(count_total(file))      
               
# POSITIVE and NEGATIVE measures/scores for Tom's reviews
positive, negative = compute_scores(test_tom_corpus)
rating = []
for file_name in test_tom_file_names:
    rating.append(get_rating(str(file_name)))

# create data frame to check calculations of counts and scores        
test_tom_data = {'train_test':['TOM'] * len(test_tom_file_names),\
    'pos_neg': ['POS', 'POS', 'NEG', 'POS', 'NEG', 'NEG', 'POS', 'NEG'],\
    'file_name': test_tom_file_names,\
    'movie': ['Marigolds',\
    'Blade Runner',\
    'Vinny',\
    'Mars Attacks',
    'Fight Club',\
    'Congeniality',\
    'Find Me Guilty',\
    'Moneyball'],\
    'positive_words' : positive_words,\
    'negative_words' : negative_words,\
    'total_words' : total_words,\
    'POSITIVE': positive, 'NEGATIVE': negative,\
    'rating': rating}        
test_tom_data_frame = pd.DataFrame(test_tom_data)

# determing thumbs up or down based upon rating
test_tom_data_frame['thumbsupdown'] = \
    test_tom_data_frame['rating'].map(map_to_thumbsupdown)
# compute simple measure of sentiment as POSITIVE - NEGATIVE
test_tom_data_frame['simple'] = \
    test_tom_data_frame['POSITIVE'] - test_tom_data_frame['NEGATIVE']    

# examine the data frame
print(test_tom_data_frame)
print(test_tom_data_frame.describe())
ratings_grouped = test_tom_data_frame['simple'].\
    groupby(test_tom_data_frame['rating'])
print('\nTom Simple Difference Means by Ratings:',ratings_grouped.mean())
thumbs_grouped = \
    test_tom_data_frame['simple'].groupby(test_tom_data_frame['thumbsupdown'])
print('\nTom Simple Difference Means by Thumbs UP/DOWN:',\
    thumbs_grouped.mean())
                
# develop predictive models using the training data
# --------------------------------------
# Simple difference method
# --------------------------------------
# use the median of the simple difference between POSITIVE and NEGATIVE
simple_cut_point = train_data_frame['simple'].median()

# algorithm for simple difference method based on training set median
def predict_simple(value):
    if (value > simple_cut_point):
        return('UP')
    else:
        return('DOWN')

train_data_frame['pred_simple'] = \
    train_data_frame['simple'].apply(lambda d: predict_simple(d))
print(train_data_frame.head())    

print('\n Simple Difference Training Set Performance\n',\
    'Percentage of Reviews Correctly Classified:',\
    100 * round(evaluate_classifier(train_data_frame['pred_simple'],\
    train_data_frame['thumbsupdown'])[4], 3),'\n')

# evaluate simple difference method in the test set
# using algorithm developed with the training set
test_data_frame['pred_simple'] = \
    test_data_frame['simple'].apply(lambda d: predict_simple(d))
print(test_data_frame.head())    

print('\n Simple Difference Test Set Performance\n',\
    'Percentage of Reviews Correctly Classified:',\
    100 * round(evaluate_classifier(test_data_frame['pred_simple'],\
    test_data_frame['thumbsupdown'])[4], 3), '\n')

# --------------------------------------
# Regression difference method
# --------------------------------------
# regression method for determining weights on POSITIVE AND NEGATIVE
# fit a regression model to the training data

regression_model = str('rating ~ POSITIVE + NEGATIVE')

# fit the model to the training set
train_regression_model_fit = smf.ols(regression_model,\
    data = train_data_frame).fit()
# summary of model fit to the training set
print(train_regression_model_fit.summary())

# because we are using predicted rating we use the midpoint 
# rating of 5 as the cut-point for making thumbs up or down predictions
regression_cut_point = 5

# algorithm for simple difference method based on training set median
def predict_regression(value):
    if (value > regression_cut_point):
        return('UP')
    else:
        return('DOWN')

# training set predictions from the model fit to the training set
train_data_frame['pred_regression_rating'] =\
    train_regression_model_fit.fittedvalues

# predict thumbs up or down based upon the predicted rating
train_data_frame['pred_regression'] = \
    train_data_frame['pred_regression_rating'].\
        apply(lambda d: predict_regression(d))
print(train_data_frame.head())    

print('\n Regression Difference Training Set Performance\n',\
    'Percentage of Reviews Correctly Classified:',\
    100 * round(evaluate_classifier(train_data_frame['pred_regression'],\
    train_data_frame['thumbsupdown'])[4], 3),'\n')

# evaluate regression difference method in the test set
# using algorithm developed with the training set
# predict thumbs up or down based upon the predicted rating

# test set predictions from the model fit to the training set
test_data_frame['pred_regression_rating'] =\
    train_regression_model_fit.predict(test_data_frame)

test_data_frame['pred_regression'] = \
    test_data_frame['pred_regression_rating'].\
        apply(lambda d: predict_regression(d))
print(test_data_frame.head())    

print('\n Regression Difference Test Set Performance\n',\
    'Percentage of Reviews Correctly Classified:',\
    100 * round(evaluate_classifier(test_data_frame['pred_regression'],\
    test_data_frame['thumbsupdown'])[4], 3), '\n')

# --------------------------------------------
# Compute text measures for each corpus
# --------------------------------------------
# return to score the document collections with get_text_measures
# for each of the selected words from the sentiment lists
# these new variables will be given the names of the words
# to keep things simple.... there are 50 such variables/words
# identified from our analysis of the unsup corpus above
# 
# start with the training document collection
working_corpus = train_pos_corpus + train_neg_corpus
add_corpus_data = get_text_measures(working_corpus)
add_corpus_data_frame = pd.DataFrame(add_corpus_data)
# merge the new text measures with the existing data frame
train_data_frame =\
    pd.concat([train_data_frame,add_corpus_data_frame],axis=1) 
# examine the expanded training data frame
print('\n xtrain_data_frame (rows, cols):',train_data_frame.shape,'\n')
print(train_data_frame.describe())
print(train_data_frame.head())

# start with the test document collection
working_corpus = test_pos_corpus + test_neg_corpus
add_corpus_data = get_text_measures(working_corpus)
add_corpus_data_frame = pd.DataFrame(add_corpus_data)
# merge the new text measures with the existing data frame
test_data_frame = pd.concat([test_data_frame,add_corpus_data_frame],axis=1) 
# examine the expanded testing data frame
print('\n xtest_data_frame (rows, cols):',test_data_frame.shape,'\n')
print(test_data_frame.describe())
print(test_data_frame.head())

# end with Tom's reviews as a document collection
working_corpus = test_tom_corpus
add_corpus_data = get_text_measures(working_corpus)
add_corpus_data_frame = pd.DataFrame(add_corpus_data)
# merge the new text measures with the existing data frame
tom_data_frame =\
    pd.concat([test_tom_data_frame,add_corpus_data_frame],axis=1) 
# examine the expanded testing data frame
print('\n xtom_data_frame (rows, cols):',tom_data_frame.shape,'\n')
print(tom_data_frame.describe())
print(tom_data_frame.head())

# --------------------------------------------
# Word/item analysis method for training set
# --------------------------------------------
# item-rating correlations for all 50 words

item_list = selected_positive_words + selected_negative_words
item_rating_corr = []
for item in item_list:
    item_rating_corr.\
        append(train_data_frame['rating'].corr(train_data_frame[item]))
item_analysis_data_frame =\
    pd.DataFrame({'item': item_list, 'item_rating_corr': item_rating_corr})    
# absolute value of item correlation with rating
item_analysis_data_frame['abs_item_rating_corr'] =\
    item_analysis_data_frame['item_rating_corr'].apply(lambda d: abs(d))
    
# look at sort by absolute value 
print(item_analysis_data_frame.sort_index(by = ['abs_item_rating_corr'],\
    ascending = False))    
    
# select subset of items with absolute correlations > 0.05
selected_item_analysis_data_frame =\
    item_analysis_data_frame\
        [item_analysis_data_frame['abs_item_rating_corr'] > 0.05]                    

# identify the positive items for word/item analysis measure
selected_positive_item_df =\
    selected_item_analysis_data_frame\
        [selected_item_analysis_data_frame['item_rating_corr'] > 0]
possible_positive_items = selected_positive_item_df['item']
print('Possible positive items:',possible_positive_items,'\n') 
# note some surprises in the list of positive items 
# select list consitent with initial list of positive words
selected_positive_items =\
    list(set(possible_positive_items) & set(positive_word_list))
print('Selected positive items:',selected_positive_items,'\n') 

# identify the negative items for word/item analysis measure
selected_negative_item_df =\
    selected_item_analysis_data_frame\
        [selected_item_analysis_data_frame['item_rating_corr'] < 0]
possible_negative_items = selected_negative_item_df['item']
print('Possible negative items:',possible_negative_items,'\n') 
# select list consitent with initial list of negative words
selected_negative_items =\
    list(set(possible_negative_items) & set(negative_word_list))
print('Selected negative items:',selected_negative_items,'\n') 
# the word "funny" remains a mystery... kept in negative list for now

# selected positive and negative items entered into function
# for obtaining word/item analysis summative score in which
# postive items get +1 point and negative items get -1 point
# ... implemented in imported Python utility get_summative_scores

# start with the training set... identify a cut-off
working_corpus = train_pos_corpus + train_neg_corpus
add_corpus_data = get_summative_scores(working_corpus)
add_corpus_data_frame = pd.DataFrame(add_corpus_data)
# merge the new text measures with the existing data frame
train_data_frame = pd.concat([train_data_frame,add_corpus_data_frame],axis=1) 
# examine the expanded training data frame and summative_scores
print('\n train_data_frame (rows, cols):',train_data_frame.shape,'\n')
print(train_data_frame['summative_score'].describe())
print('\nCorrelation of ratings and summative scores:'\
    ,round(train_data_frame['rating'].\
        corr(train_data_frame['summative_score']),3))
ratings_grouped = train_data_frame['summative_score'].\
    groupby(train_data_frame['rating'])
print('\nTraining Data Summative Score Means by Ratings:',\
    ratings_grouped.mean())
thumbs_grouped = \
    train_data_frame['summative_score'].\
        groupby(train_data_frame['thumbsupdown'])
print('\nTraining Data Summative Score Means by Thumbs UP/DOWN:',\
    thumbs_grouped.mean())
# analyses suggest a simple positive/negative cut on summative scores
# algorithm for word/item method based on training set summative_scores
def predict_by_summative_score(value):
    if (value > 0):
        return('UP')
    else:
        return('DOWN')

# evaluate word/item analysis method on training set
train_data_frame['pred_summative_score'] = \
    train_data_frame['summative_score'].\
    apply(lambda d: predict_by_summative_score(d))

print('\n Word/item Analysis Training Set Performance\n',\
    'Percentage of Reviews Correctly Classified by Summative Scores:',\
    100 * round(evaluate_classifier(train_data_frame['pred_summative_score'],\
    train_data_frame['thumbsupdown'])[4], 3),'\n')

# compute summative scores on test data frame
working_corpus = test_pos_corpus + test_neg_corpus
add_corpus_data = get_summative_scores(working_corpus)
add_corpus_data_frame = pd.DataFrame(add_corpus_data)
# merge the new text measures with the existing data frame
test_data_frame = pd.concat([test_data_frame,add_corpus_data_frame],axis=1) 

# evaluate word/item analysis method (summative score method) on test set
# using algorithm developed with the training set
test_data_frame['pred_summative_score'] = \
    test_data_frame['summative_score'].\
        apply(lambda d: predict_by_summative_score(d))
        
print('\n Word/item Analysis Test Set Performance\n',\
    'Percentage of Reviews Correctly Classified by Summative Scores:',\
    100 * round(evaluate_classifier(test_data_frame['pred_summative_score'],\
    test_data_frame['thumbsupdown'])[4], 3), '\n')

# --------------------------------------
# Logistic regression method
# --------------------------------------
# translate thumbsupdown into a binary indicator variable y
# here we let thumbs up have the higher value of 1
thumbsupdown_to_binary = {'UP':1,'DOWN':0}
train_data_frame['y'] =\
    train_data_frame['thumbsupdown'].map(thumbsupdown_to_binary)

# model specification in R-like formula syntax
text_classification_model = 'y ~  beautiful +\
    best + better + classic + enjoy + enough +\
    entertaining + excellent +\
    fans +  fun + good + great + interesting + like +\
    love +  nice + perfect + pretty + right +\
    top + well + won + wonderful + work + worth +\
    bad + boring + creepy + dark + dead+\
    death + evil + fear + funny + hard + kill +\
    killed + lack + lost + mystery +\
    plot + poor + problem + sad + scary +\
    slow + terrible + waste + worst + wrong'

# convert R-like formula into design matrix needed for statsmodels        
y,x = patsy.dmatrices(text_classification_model,\
    train_data_frame, return_type = 'dataframe')    

# define the logistic regression algorithm 
my_logit_model = sm.Logit(y,x)
# fit the model to training set
my_logit_model_fit = my_logit_model.fit()
print(my_logit_model_fit.summary())

# predicted probability of thumbs up for training set
train_data_frame['pred_logit_prob'] =\
    my_logit_model_fit.predict(linear = False)

# map from probability to thumbsupdown with simple 0.5 cut-off
def prob_to_updown(x):
    if(x > 0.5):
        return('UP')
    else:
        return('DOWN')
                    
train_data_frame['pred_logit'] =\
    train_data_frame['pred_logit_prob'].apply(lambda d: prob_to_updown(d))

print('\n Logistic Regression Training Set Performance\n',\
    'Percentage of Reviews Correctly Classified:',\
    100 * round(evaluate_classifier(train_data_frame['pred_logit'],\
    train_data_frame['thumbsupdown'])[4], 3),'\n')

# use the model developed on the training set to predict
# thumbs up or down reviews in the test set 
# assume that y is not known... only x used from patsy
y,x = patsy.dmatrices(text_classification_model,\
        test_data_frame, return_type = 'dataframe') 
y = []  # ignore known thumbs up/down from test set... 
# we want to predict thumbs up/down from the model fit to 
# the training set... my_logit_model_fit       
test_data_frame['pred_logit_prob'] =\
    my_logit_model_fit.predict(exog = x, linear = False)
test_data_frame['pred_logit'] =\
    test_data_frame['pred_logit_prob'].apply(lambda d: prob_to_updown(d))        

print('\n Logistic Regression Test Set Performance\n',\
    'Percentage of Reviews Correctly Classified:',\
    100 * round(evaluate_classifier(test_data_frame['pred_logit'],\
    test_data_frame['thumbsupdown'])[4], 3),'\n')

# --------------------------------------
# Support vector machines
# --------------------------------------
# fit the model to the training set
y,x = patsy.dmatrices(text_classification_model,\
    train_data_frame, return_type = 'dataframe')    
  
my_svm = svm.SVC()  
my_svm_fit = my_svm.fit(x, np.ravel(y))
train_data_frame['pred_svm_binary'] = my_svm_fit.predict(x)
binary_to_thumbsupdown = {0: 'DOWN', 1: 'UP'}
train_data_frame['pred_svm'] =\
    train_data_frame['pred_svm_binary'].map(binary_to_thumbsupdown)

print('\n Support Vector Machine Training Set Performance\n',\
    'Percentage of Reviews Correctly Classified:',\
    100 * round(evaluate_classifier(train_data_frame['pred_svm'],\
    train_data_frame['thumbsupdown'])[4], 3),'\n')

# use the model developed on the training set to predict
# thumbs up or down reviews in the test set 
# assume that y is not known... only x used from patsy
y,x = patsy.dmatrices(text_classification_model,\
        test_data_frame, return_type = 'dataframe') 
y = []  # ignore known thumbs up/down from test set... 
test_data_frame['pred_svm_binary'] = my_svm_fit.predict(x)
test_data_frame['pred_svm'] =\
    test_data_frame['pred_svm_binary'].map(binary_to_thumbsupdown)

print('\n Support Vector Machine Test Set Performance\n',\
    'Percentage of Reviews Correctly Classified:',\
    100 * round(evaluate_classifier(test_data_frame['pred_svm'],\
    test_data_frame['thumbsupdown'])[4], 3),'\n')

# --------------------------------------
# Random forests
# --------------------------------------
# fit random forest model to the training data
y,x = patsy.dmatrices(text_classification_model,\
    train_data_frame, return_type = 'dataframe')    

# for reproducibility set random number seed with random_state
my_rf_model = RandomForestClassifier(n_estimators = 10, random_state = 9999)
my_rf_model_fit = my_rf_model.fit(x, np.ravel(y))
train_data_frame['pred_rf_binary'] = my_rf_model_fit.predict(x)
train_data_frame['pred_rf'] =\
    train_data_frame['pred_rf_binary'].map(binary_to_thumbsupdown)

print('\n Random Forest Training Set Performance\n',\
    'Percentage of Reviews Correctly Classified:',\
    100 * round(evaluate_classifier(train_data_frame['pred_rf'],\
    train_data_frame['thumbsupdown'])[4], 3),'\n')

# use the model developed on the training set to predict
# thumbs up or down reviews in the test set 
# assume that y is not known... only x used from patsy
y,x = patsy.dmatrices(text_classification_model,\
        test_data_frame, return_type = 'dataframe') 
y = []  # ignore known thumbs up/down from test set... 
test_data_frame['pred_rf_binary'] = my_rf_model_fit.predict(x)
test_data_frame['pred_rf'] =\
    test_data_frame['pred_rf_binary'].map(binary_to_thumbsupdown)

print('\n Random Forest Test Set Performance\n',\
    'Percentage of Reviews Correctly Classified:',\
    100 * round(evaluate_classifier(test_data_frame['pred_rf'],\
    test_data_frame['thumbsupdown'])[4], 3),'\n')

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
# See if you can improve upon the performance of modeling methods by
# modifying the values of arguments to algorithms used here.
# Use various methods of classifier performance to evaluate classifiers.
# Try text classification for the movie reviews without using initial
# lists of positive an negative words. That is, identify text features
# for thumbs up/down text classification directly from the training set.


