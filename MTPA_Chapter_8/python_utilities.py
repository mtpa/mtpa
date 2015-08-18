# Evaluating Predictive Accuracy of a Binary Classifier (Python)

def evaluate_classifier(predicted, observed):
    import pandas as pd 
    if(len(predicted) != len(observed)):
        print('\nevaluate_classifier error:',\
             ' predicted and observed must be the same length\n')
        return(None) 
    if(len(set(predicted)) != 2):
        print('\nevaluate_classifier error:',\
              ' predicted must be binary\n')
        return(None)          
    if(len(set(observed)) != 2):
        print('\nevaluate_classifier error:',\
              ' observed must be binary\n')
        return(None)          

    predicted_data = predicted
    observed_data = observed
    input_data = {'predicted': predicted_data,'observed':observed_data}
    input_data_frame = pd.DataFrame(input_data)
    
    cmat = pd.crosstab(input_data_frame['predicted'],\
        input_data_frame['observed']) 
    a = float(cmat.ix[0,0])
    b = float(cmat.ix[0,1])
    c = float(cmat.ix[1,0]) 
    d = float(cmat.ix[1,1])
    n = a + b + c + d
    predictive_accuracy = (a + d)/n
    true_positive_rate = a / (a + c)
    false_positive_rate = b / (b + d)
    precision = a / (a + b)
    specificity = 1 - false_positive_rate   
    expected_accuracy = (((a + b)*(a + c)) + ((b + d)*(c + d)))/(n * n)
    kappa = (predictive_accuracy - expected_accuracy)\
       /(1 - expected_accuracy)   
    return(a, b, c, d, predictive_accuracy, true_positive_rate, specificity,\
        false_positive_rate, precision, expected_accuracy, kappa)

            
# Text Measures for Sentiment Analysis (Python)

def get_text_measures(corpus):
    # individually score each of the twenty-five selected positive words 
    # for each document in the working corpus... providing new text measures

    # initialize the list structures for each positive word
    beautiful = []; best =  []; better =  []; classic = [];
    enjoy = []; enough = []; entertaining = []; excellent = [];
    fans =  []; fun =  []; good =  []; great = []; interesting =  [];  
    like =  []; love =  []; nice = []; perfect =  []; pretty =  [];  
    right =  []; top = []; well = [];  
    won = []; wonderful = [];  work = []; worth = []
        
    # initialize the list structures for each negative word
    bad = []; boring = [];    creepy = [];    dark = []; 
    dead = []; death =  []; evil = []; fear = []; 
    funny = []; hard = []; kill = []; killed = []; 
    lack =  []; lost =  []; mystery = []; plot = []; 
    poor = []; problem = []; sad = []; scary = []; 
    slow = []; terrible = []; waste = []; worst = []; wrong  = []

    for text in corpus:
        beautiful.append(len([w for w in text.split() if w == 'beautiful']))
        best.append(len([w for w in text.split() if w == 'best']))
        better.append(len([w for w in text.split() if w == 'better']))
        classic.append(len([w for w in text.split() if w == 'classic']))

        enjoy.append(len([w for w in text.split() if w == 'enjoy']))
        enough.append(len([w for w in text.split() if w == 'enough']))
        entertaining.append(len([w for w in text.split() if w == 'entertaining']))
        excellent.append(len([w for w in text.split() if w == 'excellent']))

        fans.append(len([w for w in text.split() if w == 'fans']))
        fun.append(len([w for w in text.split() if w == 'fun']))
        good.append(len([w for w in text.split() if w == 'good']))
        great.append(len([w for w in text.split() if w == 'great']))

        interesting.append(len([w for w in text.split() if w == 'interesting']))
        like.append(len([w for w in text.split() if w == 'like']))
        love.append(len([w for w in text.split() if w == 'love']))
        nice.append(len([w for w in text.split() if w == 'nice']))

        perfect.append(len([w for w in text.split() if w == 'perfect']))
        pretty.append(len([w for w in text.split() if w == 'pretty']))
        right.append(len([w for w in text.split() if w == 'right']))
        top.append(len([w for w in text.split() if w == 'top']))

        well.append(len([w for w in text.split() if w == 'well']))
        won.append(len([w for w in text.split() if w == 'won']))
        wonderful.append(len([w for w in text.split() if w == 'wonderful']))
        work.append(len([w for w in text.split() if w == 'work']))
        worth.append(len([w for w in text.split() if w == 'worth']))

    # individually score each of the twenty-five selected negative words 
    # for each document in the working corpus... poviding new text measures
   
        bad.append(len([w for w in text.split() if w == 'bad']))
        boring.append(len([w for w in text.split() if w == 'boring']))
        creepy.append(len([w for w in text.split() if w == 'creepy']))
        dark.append(len([w for w in text.split() if w == 'dark']))

        dead.append(len([w for w in text.split() if w == 'dead']))
        death.append(len([w for w in text.split() if w == 'death']))
        evil.append(len([w for w in text.split() if w == 'evil']))
        fear.append(len([w for w in text.split() if w == 'fear']))

        funny.append(len([w for w in text.split() if w == 'funny']))
        hard.append(len([w for w in text.split() if w == 'hard']))
        kill.append(len([w for w in text.split() if w == 'kill']))
        killed.append(len([w for w in text.split() if w == 'killed']))

        lack.append(len([w for w in text.split() if w == 'lack']))
        lost.append(len([w for w in text.split() if w == 'lost']))
        mystery.append(len([w for w in text.split() if w == 'mystery']))
        plot.append(len([w for w in text.split() if w == 'plot']))

        poor.append(len([w for w in text.split() if w == 'poor']))
        problem.append(len([w for w in text.split() if w == 'problem']))
        sad.append(len([w for w in text.split() if w == 'sad']))
        scary.append(len([w for w in text.split() if w == 'scary']))

        slow.append(len([w for w in text.split() if w == 'slow']))
        terrible.append(len([w for w in text.split() if w == 'terrible']))
        waste.append(len([w for w in text.split() if w == 'waste']))
        worst.append(len([w for w in text.split() if w == 'worst']))
        wrong.append(len([w for w in text.split() if w == 'wrong']))

    # creat dictionary data structure as a preliminary 
    # to creating the data frame for the fifty text measures
    add_corpus_data = {'beautiful':beautiful,'best':best,'better':better,\
        'classic':classic, 'enjoy':enjoy, 'enough':enough,\
        'entertaining':entertaining, 'excellent':excellent,\
        'fans':fans, 'fun':fun, 'good':good, 'great':great,\
        'interesting':interesting, 'like':like, 'love':love, 'nice':nice,\
        'perfect':perfect, 'pretty':pretty, 'right':right, 'top':top,\
        'well':well, 'won':won, 'wonderful':wonderful, 'work':work,\
        'worth':worth,'bad':bad, 'boring':boring, 'creepy':creepy,\
        'dark':dark, 'dead':dead, 'death':death, 'evil':evil, 'fear':fear,\
        'funny':funny,'hard':hard, 'kill':kill, 'killed':killed, 'lack':lack,\
        'lost':lost, 'mystery':mystery, 'plot':plot,'poor':poor,\
        'problem':problem, 'sad':sad, 'scary':scary, 'slow':slow,\
        'terrible':terrible, 'waste':waste, 'worst':worst, 'wrong':wrong}    
     
    return(add_corpus_data)     
    
# Summative Scoring of Sentiment (Python)

def get_summative_scores(corpus):
    # individually score each of the positive and negative words/items 
    # for each document in the working corpus... 
    # providing a summative score 
     
    summative_score = []  # intialize list for summative scores
    
    for text in corpus:
        score = 0  # initialize for individual document
        # for each document in the working corpus... 
        # individually score each of the eight selected positive words 
        if (len([w for w in text.split() if w == 'beautiful']) > 0):
            score = score +1
        if (len([w for w in text.split() if w == 'best']) > 0):
            score = score +1
        if (len([w for w in text.split() if w == 'classic']) > 0):
            score = score +1
        if (len([w for w in text.split() if w == 'excellent']) > 0):
            score = score +1
        if (len([w for w in text.split() if w == 'great']) > 0):
            score = score +1
        if (len([w for w in text.split() if w == 'perfect']) > 0):
            score = score +1
        if (len([w for w in text.split() if w == 'well']) > 0):
            score = score +1
        if (len([w for w in text.split() if w == 'wonderful']) > 0):
            score = score +1
            
    # individually score each of the ten selected negative words 
   
        if (len([w for w in text.split() if w == 'bad']) > 0):
            score = score -1
        if (len([w for w in text.split() if w == 'boring']) > 0):
            score = score -1
        if (len([w for w in text.split() if w == 'funny']) > 0):
            score = score -1
        if (len([w for w in text.split() if w == 'lack']) > 0):
            score = score -1
        if (len([w for w in text.split() if w == 'plot']) > 0):
            score = score -1
        if (len([w for w in text.split() if w == 'poor']) > 0):
            score = score -1
        if (len([w for w in text.split() if w == 'problem']) > 0):
            score = score -1
        if (len([w for w in text.split() if w == 'terrible']) > 0):
            score = score -1
        if (len([w for w in text.split() if w == 'waste']) > 0):
            score = score -1
        if (len([w for w in text.split() if w == 'worst']) > 0):
            score = score -1
        
        summative_score.append(score)
        
    summative_score_data = {'summative_score': summative_score}
    return(summative_score_data)           
    