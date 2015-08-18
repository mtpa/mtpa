# Market Simulation Utilities (R)

# user-defined function for first-choice simulation rule
first.choice.simulation.rule <- function(response, alpha = 1) {
  # begin function for first-choice rule
  # returns binary vector or response vector with equal division 
  # of 1 across all locations at the maximum
  # use alpha for desired sum across respondents
  # alpha useful when the set of tested profiles is not expected to be one
  if(alpha < 0 || alpha > 1) stop("alpha must be between zero and one")
  response.vector <- numeric(length(response))
  for(k in seq(along=response))
    if(response[k] == max(response)) response.vector[k] <- 1
  alpha*(response.vector/sum(response.vector))  
  }  # end first-choice rule function

# user-defined function for predicted choices from four-profile choice sets 
choice.set.predictor <- function(predicted.probability) {
  predicted.choice <- length(predicted.probability)  # initialize 
  index.fourth <- 0  # initialize block-of-four choice set indices
  while (index.fourth < length(predicted.probability)) {
    index.first  <- index.fourth + 1
    index.second <- index.fourth + 2
    index.third  <- index.fourth + 3
    index.fourth <- index.fourth + 4
    this.choice.set.probability.vector <- 
      c(predicted.probability[index.first],
      predicted.probability[index.second],
      predicted.probability[index.third],
      predicted.probability[index.fourth])
    predicted.choice[index.first:index.fourth] <- 
      first.choice.simulation.rule(this.choice.set.probability.vector)  
    }
  predicted.choice <- factor(predicted.choice, levels = c(0,1), 
    labels = c("NO","YES"))
  predicted.choice  
  } # end choice.set.predictor function 
  
  
# save market simulation utilities for future work
save(first.choice.simulation.rule,
  choice.set.predictor,
  file="mtpa_market_simulation_utilities.Rdata")
  