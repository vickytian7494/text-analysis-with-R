#extract sentences contain keyword
extractword <- function(df,word){
  subset(df,str_detect(df$Contents, word))
}
word <- c('main building')
tayp_location <-  extractword(tayp, word)
word_counts(tayp_location, "words contains special  | 195 out of 1217 records")
bigrams(tayp_special, "special")

###what makes it special/amazing/wonderful
#use the documents that contains certain word, extract that perticular sentence,
#sentiment analysis find positive sentensive, 
#find top related words

#extract contains multiple keywords
lst_A <- c("main building", "guest house")
selectmultiwords<- function(df,wordlist){
  Pattern <- paste(wordlist, collapse="|")
  subset(df,grepl(Pattern, df$Contents))
}
special_location <- selectmultiwords(tayp,lst_A) 

#select positive reviews (ratings are equal or above 4)
clean_special_wonderful <- subset(special_wonderful, Ratings>=4) #410 records

#output: specific sentence that contains keyword
sentencesplit <- function(df, wordlist){
  allsentence <- data.frame(unlist(strsplit(df$Contents, "(?<=[^.][.][^.])", perl=TRUE)))
  colnames(allsentence) <- 'Contents'
  allsentence$Contents <- as.character(allsentence$Contents)
  Pattern <- paste(wordlist, collapse="|")
  selectsentence <- subset(allsentence,grepl(Pattern, allsentence$Contents, ignore.case = T))
  selectsentence$Reviewers <- seq(from=1,to=nrow(selectsentence))
  selectsentence <- selectsentence[, c(2,1)]
  selectsentence
}

sentencespecial <- sentencesplit(clean_special_wonderful, lst_A)
write.csv(sentencespecial, file='special.csv',row.names = FALSE)
str(sentencespecial)
##word network cloud
network(sentencespecial, "TAYP file - word contains special, amazing, wonderful | 591 records", 8)
word_counts(sentencespecial, "TAYP with word contains special, amazing, wonderful")

#general search
word_counts(tayp, "TAYP file")
bigrams(tayp, "TAYP file")



