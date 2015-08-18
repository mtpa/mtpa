# Game-day Simulator for Baseball (Python)

from __future__ import division, print_function

import numpy as np
from scipy.stats import nbinom

def simulator(home_mean, away_mean, niterations):
    # estimates probability of home team win
    seed(1234)  # set to obtain reproducible results
    home_game_score = [0] * niterations
    away_game_score = [0] * niterations
    home_win = [0] * niterations
    i = 0
    while (i < niterations):
        home_game_score[i] = \
            nbinom.rvs(n = 4.0, p = 4.0/(4.0 + home_mean), size = 1)[0] 
        away_game_score[i] = \
            nbinom.rvs(n = 4.0, p = 4.0/(4.0 + away_mean), size = 1)[0]         
        if (home_game_score[i] > away_game_score[i]):
            home_win[i] = 1
        if ((away_game_score[i] > home_game_score[i]) or \
            (away_game_score[i] < home_game_score[i])):
            i = i + 1 
    n_home_win = sum(home_win)
    return n_home_win / niterations        
 
niterations = 100000  # use smaller number for testing
# probability matrix for results... home team rows, away team columns
probmat = array([[0.0] * 9] * 9)

# matrix representation of home and away team runs for table
homemat = array([[9] * 9, [8] * 9, [7] * 9, [6] * 9, [5] * 9,\
    [4] * 9, [3] * 9, [2] *9, [1] * 9])
awayrow = array([1, 2, 3, 4, 5, 6, 7, 8, 9])    
awaymat = array([awayrow] * 9)

# generate table of probabilities
for index_home in range(9):
    for index_away in range(9):
        if (homemat[index_home,index_away] != awaymat[index_home,index_away]):
            print(index_home, index_away)
            probmat[index_home, index_away] = \
                simulator(float(homemat[index_home, index_away]), \
                          float(awaymat[index_home, index_away]), niterations)
            
print(probmat)                

# Suggestion for the student: Develop simulators for football or basketball.
# Use matplotlib to create a probability heat-map for the probmat results.


