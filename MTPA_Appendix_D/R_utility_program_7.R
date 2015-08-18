# Movie Tagline Data Preparation Script for Text Analysis (R)

library(stringr)  # character manipulation with regular expressions

# convert to bytecodes to avoid "invalid multibyte string" messages
bytecode.convert <- function(x) {iconv(enc2utf8(x), sub = "byte")}

# NLINES <- 21  # for development and test runs
# input.data.file.name <- "taglines_list_sample.txt"
#  scan("taglines_list_sample.txt", what = "character")  # development runs
# nlines_to_read <- 21  # for development and test runs

# there are 345317 records in the full taglines data file
# the number of lines in the input data file
# or maximum number of lines to read
NLINES <- 345317   
input.data.file.name <- "taglines_copy_data.txt"  # production runs
# read the data in blocks of nlines_to_read at a time
nlines_to_read <- 10000  # size of block of lines to read

# debug print was used during the code development process
debub.print.mode <- FALSE
debug.print <- function(title,date,tagline,status) {
  cat("\n title =",title,"  date = ", date," tagline",
    tagline, " status = ",status,"\n")
  }

# this user-defined function shows how R can be used to parse text input
tagline.parser <- function(input.list) {
# where we start depends upon the status on entry
# tagline parser can only be in one status at a time
# begin
# indicator
# title (actually a title and date status)
# moretitle (another title and data status, but following a previous title)
# tagline
# comment

# data are not clean... if you get a new movie indicator "#" start a new movie
# we may lose a few movies this way... but that is better than editing a file
# with about 40 thousand movies..

# at this time all valid dates should look be six characters long
# four numbers surrounded by parentheses 
# lets use The Birth of a Nation (1915) as the earliest possible valid date
# and the current year as the latest possible valid date 
# obtained by as.numeric(format(Sys.time(), "%Y"))
valid.years <- 1915:as.numeric(format(Sys.time(), "%Y"))
valid.years.strings.four <- paste("(",as.character(valid.years),sep="")

   text <- input.list[[1]]
   status <- input.list[[2]]
   title <- input.list[[3]]
   date <- input.list[[4]]
   tagline <- input.list[[5]]
   
   nitems <- length(text)
   ncount <- 1  # initialize on entry
   tagline_data.store <- NULL
   
   while(ncount < nitems) {   
# debug printing was used in the development and testing of parsing logic   
     if (debub.print.mode) debug.print(title,date,tagline,status) 
     if (status == "indicator" | status == "begin") {
       if (ncount <= nitems) {
         ncount <- ncount + 1
         status <- "initialtitle"
         title <- " "  # blank title to start
         date <- " "   # blank date to start
         tagline <- " "  # blank tagline to start
         }
       }
       
     if (status == "initialtitle") {
       if (ncount <= nitems) {
         title <- text[ncount]
         ncount <- ncount + 1
         if (ncount <= nitems) {
           test_date <- text[ncount]
           if (substring(test_date,1,5) %in% valid.years.strings.four) {
             date <- test_date
             ncount <- ncount + 1
             status <- "tagline"
             }
           if (!(substring(test_date,1,5) %in% valid.years.strings.four)) {     
             if (test_date == "#") {
                 status <- "indicator"
                 }           
             if (test_date != "#") {
                 title <- paste(title, test_date)  
                 ncount <- ncount + 1 
                 status <- "moretitle"
                 }               
             } 
           }  
         }
       }
                                        
     if (status == "moretitle") {
       if (ncount <= nitems) {
         ncount <- ncount + 1
         if (ncount <= nitems) {
           test_date <- text[ncount]
           if (substring(test_date,1,5) %in% valid.years.strings.four) {
             date <- test_date
             ncount <- ncount + 1
             status <- "tagline"
             }
           if (!(substring(test_date,1,5) %in% valid.years.strings.four)) {     
             if (test_date == "#") {
                 status <- "indicator"
                 }           
             if (test_date != "#") {
                 title <- paste(title, test_date)  
                 ncount <- ncount + 1  
                 }               
             } 
           }  
         }
       }                    
                                       
       if (status == "tagline") {
         if (ncount <= nitems) {
           new_text <- text[ncount]
           if (new_text == "#") {
             tagline_data.store <- rbind(tagline_data.store,
                data.frame(title, date, tagline, stringsAsFactors = FALSE))
                status <- "indicator"
             }           
           if (new_text != "#") {
             if (substring(new_text,1,1) == "{") {
               ncount <- ncount + 1
               status <- "comment"
               }
             if (substring(new_text,1,1) != "{") {
               tagline <- paste(tagline, new_text)
               ncount <- ncount + 1
               }
             }             
           }          
         }
  
       if (status == "comment") {
         if (ncount <= nitems) {
           new_text <- text[ncount]         
           if (substring(new_text,nchar(new_text),nchar(new_text)) == "}") {
             ncount <- ncount + 1
             status <- "tagline"
             }
           if (substring(new_text,nchar(new_text),nchar(new_text)) != "}") {
             ncount <- ncount + 1
             }
           }
         }  
  } # end of primary while-loop
list(tagline_data.store, status, title, date, tagline)  # return list
}  # end of function

cat("\n\n","NUMBER OF LINES READ: ")

skip <- 0  # initialize the number of lines to skip
nlines_read_so_far <- 0  # initialize number of lines read so far


status <- "begin"  # initial status
title <- " "  # blank title to start
date <- " "   # blank date to start
tagline <- " "  # blank tagline to start

data.store <- NULL  # initialize the data frame for storing text data

while(nlines_read_so_far < NLINES)  {

if ((NLINES - nlines_read_so_far) < nlines_to_read) 
  nlines_to_read <- (NLINES - nlines_read_so_far)
  
text <- scan(file = input.data.file.name, what = "character",
    skip = nlines_read_so_far, nlines = nlines_to_read)
 
# convert individual text items to bytecodes 
# to avoid to avoid "invalid multibyte string" error messages going forward
text <- bytecode.convert(text)

input.list <- list(text, status, title, date, tagline)  
 
# parse this block of text with the tagline parser function 
output.list <- tagline.parser(input.list) 
  
  new_data_for_store <- output.list[[1]]
  status <- output.list[[2]]
  title <- output.list[[3]]
  date <- output.list[[4]]
  tagline <- output.list[[5]]
      
  data.store <- rbind(data.store, new_data_for_store)
  
  nlines_read_so_far <- nlines_read_so_far + nlines_to_read
  
  cat(" ","nlines_read_so_far:",nlines_read_so_far)
  }
  
# if there is full movie info in output list 
# add this last movie to the end of the data.store

if ((!is.null(output.list[[3]])) & 
   (!is.null(output.list[[4]])) &
   (!is.null(output.list[[5]]))) {
       title <- output.list[[3]]
       date <- output.list[[4]]
       tagline <- output.list[[5]] 
    data.store <- rbind(data.store, 
      data.frame(title, date, tagline, stringsAsFactors = FALSE))
  }
    
# data cleaning... check the date field... 
# if it does not start with "(" or end with ")"
# strip any character other than numeric in the date field
# using regular expressions coding and the string replace function from stringr
data.store$replace.date <- str_replace_all(data.store$date, "[^.(0-9)]", "")

# at this time all valid dates should be six characters long
# four numbers surrounded by parentheses 
# lets use The Birth of a Nation (1915) as the earliest possible valid date
# and the current year as the latest possible valid date 
# obtained by as.numeric(format(Sys.time(), "%Y"))
valid.years <- 1915:as.numeric(format(Sys.time(), "%Y"))
valid.years.strings <- paste("(",as.character(valid.years),")",sep="")

# valid observations must have dates with valid.years.strings
data.store$valid <- 
  ifelse((data.store$replace.date %in% valid.years.strings),"YES","NO")

# use the subset of movies with valid data
valid.data.store <- subset(data.store, subset = (valid == "YES"))

# add date field to title field to create unique identifier for each movie
valid.data.store$movie <- paste(valid.data.store$title, valid.data.store$date)

# strip parentheses from replace.date and create an integer variable for year
valid.data.store$replace.date <- 
  str_replace(valid.data.store$replace.date,"[(]","")
valid.data.store$replace.date <- 
  str_replace(valid.data.store$replace.date,"[)]","")
valid.data.store$year <- as.integer(valid.data.store$replace.date)

# merge title and tagline text into new movie text variable for text analysis
valid.data.store$text <- 
  paste(valid.data.store$title, valid.data.store$tagline)

# drop replace.date and reorder variables for text analysis
# at this point we have one large data frame with text columns
movies <- valid.data.store[,c("movie","year","title","tagline","text")]

cat("\n writing movies data frame to comma-delimited text file\n",
    "         <movie_tagline_data_parsed.csv>","\n")
write.csv(movies, file = "movie_tagline_data_parsed.csv", row.names = FALSE)

