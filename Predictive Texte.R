#**************************************************************************
#                  Chapter 5 : Predictive Text
#**************************************************************************

#install and load the tm library for text processing

#install.packages("tm")
library(tm)

#--------------------------------------------------------------------------
#                   5.2: Prepare Corpus for n-grams
#--------------------------------------------------------------------------


#Load text files into the VCorpus
course_corpus <- VCorpus(DirSource("C:/Users/ALOYS AYMRICK/Desktop/Projet R 2022/cours"))

#Convert to lower case
course_corpus2 <- tm_map(course_corpus, content_transformer(tolower))

#Remove punctuations
course_corpus3 <- tm_map(course_corpus2, removePunctuation)

#install.packages("RWeka")
library("RWeka")

#Convert to a Document Term Matrix with Bigrams
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

course_bigrams <- DocumentTermMatrix(course_corpus3, 
                                     control = list(tokenize = BigramTokenizer))
inspect(course_bigrams)

#Compute frequency of bigrams
bigram_frequency <- sort(colSums(as.matrix(course_bigrams)), 
                         decreasing=TRUE)

#Convert frequency table to a data frame
bigram_df <- data.frame(bigrams=names(bigram_frequency), 
                        freq=bigram_frequency)
#print the data frame
bigram_df[1:10,]

#--------------------------------------------------------------------------
#                   5.3: Build a Bigrams Database
#--------------------------------------------------------------------------

#Split each bigram into the first and second words and store them back
#into the same data frame

for ( irow in 1:nrow(bigram_df)) {
  
  grams = unlist(strsplit(as.character(bigram_df$bigrams[irow])," "))
  
  bigram_df$first[irow]= grams[1]
  bigram_df$second[irow]= grams[2]
}

#Review the bigrams data frame
bigram_df[1:10,]

#Query for for second words and frequency where first word = "data"
bigram_df[bigram_df$first == "data", c("second", "freq")]

#--------------------------------------------------------------------------
#                   5.4: Predicting Text
#--------------------------------------------------------------------------

###### Auto-complete for word "ap"

#filter data frame for rows where column first starts with "ap"
autocomplete_filtered = bigram_df[
  startsWith(
    as.character(bigram_df$first), "ap"), 
  c("first", "freq")]

#Aggregate across duplicate rows
autocomplete_summary =aggregate(freq ~ first, autocomplete_filtered, sum)

#Order in descending order of frequency
autocomplete_ordered = autocomplete_summary[
  with(autocomplete_summary, order(-freq)), ]

#The predictive auto complete list.
autocomplete_ordered$first

###### find next word for "apache"

#Filter data frame where first word is "apache"
nextword_filtered = bigram_df[
  bigram_df$first == "apache", 
  c("freq", "second")]

#Order in descending order of frequency
nextword_ordered = nextword_filtered[
  with(nextword_filtered, order(-freq)), ]

#The predicted next words
nextword_ordered$second
#--------------------------------------------------------------------------