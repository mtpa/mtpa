# Making Word Clouds (R)

# wordcloud for Python program code and comments through book chapter 6
Python.code.text <- scan("mtpa_Python_code.txt", what = "char", sep = "\n")
# replace uppercase with lowercase letters
Python.code.text <- tolower(Python.code.text)
# strip out all non-letters and return vector
Python.code.text.preword.vector <- unlist(strsplit(Python.code.text, "\\W"))
# drop all empty words 
Python.code.text.vector <- 
  Python.code.text.preword.vector[which(nchar(Python.code.text.preword.vector) > 0)]
pdf(file = "fig_text_wordcloud_of_Python_code.pdf", width = 11, height = 8.5)
set.seed(1234) 
wordcloud(Python.code.text.vector,   min.freq = 10,
  max.words = 300,
  random.order=FALSE,
  random.color=FALSE,
  rot.per=0.0,
  colors="black",
  ordered.colors=FALSE, 
  use.r.layout=FALSE,
  fixed.asp=TRUE)
dev.off()


# wordcloud for R program code and comments through book chapter 6
R.code.text <- scan("mtpa_R_code.txt", what = "char", sep = "\n")
# replace uppercase with lowercase letters
R.code.text <- tolower(R.code.text)  
# strip out all non-letters and return vector
R.code.text.preword.vector <- unlist(strsplit(R.code.text, "\\W"))
# drop all empty words 
R.code.text.vector <- 
  R.code.text.preword.vector[which(nchar(R.code.text.preword.vector) > 0)]
pdf(file = "fig_text_wordcloud_of_R_code.pdf", width = 11, height = 8.5)
set.seed(1234) 
wordcloud(R.code.text.vector,   min.freq = 10,
  max.words = 300,
  random.order=FALSE,
  random.color=FALSE,
  rot.per=0.0,
  colors="black",
  ordered.colors=FALSE, 
  use.r.layout=FALSE,
  fixed.asp=TRUE)
dev.off()



