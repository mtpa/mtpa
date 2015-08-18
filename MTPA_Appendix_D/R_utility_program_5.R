# Text Scoring Script for Sentiment Analysis (R)
# --------------------------------------
# Word/item analysis method 
# --------------------------------------
# return to the training corpus to develop simple counts
# for each of the words in the sentiment list
# these new variables will be given the names of the words
# to keep things simple.... there are 50 such variables/words
# and we work with a corpus called working.corpus

# and the number of words that match each word
amazing <- integer(length(names(working.corpus)))
beautiful <- integer(length(names(working.corpus)))
classic <- integer(length(names(working.corpus)))
enjoy <- integer(length(names(working.corpus)))       
enjoyed <- integer(length(names(working.corpus)))
entertaining <- integer(length(names(working.corpus)))
excellent <- integer(length(names(working.corpus)))
fans <- integer(length(names(working.corpus)))        
favorite <- integer(length(names(working.corpus)))
fine <- integer(length(names(working.corpus)))
fun <- integer(length(names(working.corpus)))
humor <- integer(length(names(working.corpus)))       
lead <- integer(length(names(working.corpus)))
liked <- integer(length(names(working.corpus)))
love <- integer(length(names(working.corpus)))
loved <- integer(length(names(working.corpus)))       
modern <- integer(length(names(working.corpus)))
nice <- integer(length(names(working.corpus)))
perfect <- integer(length(names(working.corpus)))
pretty <- integer(length(names(working.corpus)))      
recommend <- integer(length(names(working.corpus)))
strong <- integer(length(names(working.corpus)))
top <- integer(length(names(working.corpus)))
wonderful <- integer(length(names(working.corpus)))   
worth <- integer(length(names(working.corpus)))       

bad <- integer(length(names(working.corpus)))       
boring <- integer(length(names(working.corpus)))    
cheap <- integer(length(names(working.corpus)))     
creepy <- integer(length(names(working.corpus)))    
dark <- integer(length(names(working.corpus)))      
dead <- integer(length(names(working.corpus)))     
death <- integer(length(names(working.corpus)))     
evil <- integer(length(names(working.corpus)))      
hard <- integer(length(names(working.corpus)))      
kill <- integer(length(names(working.corpus)))      
killed <- integer(length(names(working.corpus)))    
lack <- integer(length(names(working.corpus)))     
lost <- integer(length(names(working.corpus)))      
miss <- integer(length(names(working.corpus)))      
murder <- integer(length(names(working.corpus)))    
mystery <- integer(length(names(working.corpus)))   
plot <- integer(length(names(working.corpus)))      
poor <- integer(length(names(working.corpus)))     
sad <- integer(length(names(working.corpus)))       
scary <- integer(length(names(working.corpus)))     
slow <- integer(length(names(working.corpus)))      
terrible <- integer(length(names(working.corpus))) 
waste <- integer(length(names(working.corpus)))     
worst <- integer(length(names(working.corpus)))    
wrong <- integer(length(names(working.corpus)))    

reviews.tdm <- TermDocumentMatrix(working.corpus)

for(index.for.document in seq(along=names(working.corpus))) {
  amazing[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "amazing")))
  beautiful[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "beautiful")))  
  classic[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "classic")))  
  enjoy[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "enjoy")))      
  enjoyed[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "enjoyed")))  
  entertaining[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "entertaining")))  
  excellent[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "excellent")))  
  fans[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "fans")))  
  favorite[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "favorite")))  
  fine[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "fine")))  
  fun[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "fun")))  
  humor[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "humor")))  
  lead[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "lead")))  
  liked[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "liked")))  
  love[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "love")))  
  loved[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "loved")))  
  modern[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "modern")))  
  nice[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "nice")))  
  perfect[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "perfect")))  
  pretty[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "pretty")))  
  recommend[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "recommend")))  
  strong[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "strong")))  
  top[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "top")))  
  wonderful[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "wonderful")))                                         
  worth[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "worth")))  
  bad[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "bad")))  
  boring[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "boring")))  
  cheap[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "cheap")))  
  creepy[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "creepy")))  
  dark[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "dark")))  
  dead[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "dead")))  
  death[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "death")))  
  evil[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "evil")))  
  hard[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "hard")))  
  kill[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "kill")))  
  killed[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "killed")))  
  lack[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "lack")))  
  lost[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "lost")))  
  miss[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "miss")))  
  murder[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "murder")))  
  mystery[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "mystery")))  
  plot[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "plot")))  
  poor[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "poor")))  
  sad[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "sad")))                        
  scary[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "scary")))  
  slow[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "slow")))  
  terrible[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "terrible")))  
  waste[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "waste")))  
  worst[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "worst")))  
  wrong[index.for.document] <- 
    sum(termFreq(working.corpus[[index.for.document]], 
    control = list(dictionary = "wrong")))  
  }
  