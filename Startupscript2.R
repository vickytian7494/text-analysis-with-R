library(zoo)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(stringr)
library(tidyr)
library(scales)
library(broom)
library(purrr)
library(widyr)
library(igraph)
library(ggraph)
library(SnowballC)
library(wordcloud)
library(reshape2)
library(tm)
library(viridis)
library(hrbrthemes)
library(sentimentr)

theme_set(theme_minimal())
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}
left = function(text, num_char) {
  substr(text, 1, num_char)
}

#data prep
yp <- read.csv('yp.csv', stringsAsFactors=FALSE)
names(yp) <- c('Reviewers', 'Location', 'Ratings', 'Contents','Months')
write.csv(yp, file = "yp01.csv")

#prepare ta2
ta2 <- subset(ta[,2:3])
ta2$Qtr=as.yearqtr(ta2$Months)
ta2 <- ta2 %>% group_by(Qtr) %>% count(Ratings)
ta2$Qtr <- as.character(ta2$Qtr)
ta2$Ratings<-as.numeric(ta2$Ratings)
ta2$year <- left(ta2$Qtr,4)
str(ta2)
write.csv(ta2, file = "ta2.csv", row.names=FALSE)

yp2<- yp%>%select(4,6)
yp2$Qtr=as.yearqtr(yp2$Months)
yp2 <- yp2 %>% group_by(Qtr) %>% count(Ratings)
yp2$Qtr <- as.character(yp2$Qtr)
yp2$year <- left(yp2$Qtr,4)
write.csv(yp2, file = "yp2.csv", row.names=FALSE)

#preparelm for ta
class(ta)
ta_lm<- subset(ta,left(ta$Trip_types,6) =='Travel')
ta_lm$Trip_types <- tail(strsplit(ta_lm$Trip_types,split=" "))
ta_lm$Trip_types <- word(ta_lm$Trip_types,-1)
ta_lm$Trip_types <- as.factor(ta_lm$Trip_types)
ta_lm$Ratings <- as.numeric(ta_lm$Ratings)
write.csv(ta_lm, 'ta_triptype.csv', row.names = FALSE)

#preparelm for yp
colnames(yp)
yp_lm <- select(yp,Location, Ratings)
yp_lm$Location <- right(yp_lm$Location,2)
write.csv(yp_lm, 'yp_location.csv', row.names = FALSE)

#manually add words into stop words 
customer_stop_words <- tribble(
  ~word,~lexicon,
  'ritz','SMART',
  'carlton','SMART',
  'half','SMART',
  'moon','SMART',
  'bay','SMART',
  'stay','SMART',
  'hotel','SMART',
  'stay','SMART',
  'day','SMART',
  'night','SMART',
  'enjoy','SMART',
  'experience','SMART',
)
stop_words2<-stop_words %>% bind_rows(customer_stop_words)
                  

#preprocess - token words
#find the most common one word
word_counts <- function(df, df_name){ 
  df %>%
    distinct(Contents, .keep_all = TRUE) %>%
    unnest_tokens(word, Contents, drop = FALSE) %>%
    distinct(Reviewers, word, .keep_all = TRUE) %>%
    anti_join(stop_words2, by = "word") %>%
    filter(str_detect(word, "[^\\d]")) %>%
    mutate(word = wordStem(word)) %>%
    group_by(word) %>%
    mutate(word_total = n()) %>%
    ungroup() %>%
    count(word, sort = TRUE) %>%
    head(25) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col(fill = "lightblue") +
    scale_y_continuous(labels = comma_format()) +
    coord_flip() +
    labs(title = paste("Most common words in",df_name,"reviews", sep=" "),
         subtitle = "From Oct 2016 to Oct 2019",
         y = "Count of words")
}
                 

#prepare for bigrams
#find the most common two words
bigrams <- function(df,df_name) {
  df %>%
    unnest_tokens(bigram, Contents, token = "ngrams", n = 2)%>%
    separate(bigram, c("word1", "word2"), sep = " ")%>%
    filter(!word1 %in% stop_words2$word) %>%
    filter(!word2 %in% stop_words2$word) %>%
    mutate(word1 = wordStem(word1)) %>%
    mutate(word2 = wordStem(word2)) %>%
    unite(bigram, word1, word2, sep = " ") %>%
    count(bigram, sort = TRUE) %>%
    head(20) %>%
    mutate(bigram = reorder(bigram, n)) %>%
    ggplot(aes(bigram, n)) +
    geom_col(fill = "darkgreen") +
    scale_y_continuous(labels = comma_format()) +
    coord_flip() +
    labs(title = paste("Most common two words in",df_name,"reviews", sep=" "),
         subtitle = "From Oct 2016 to Oct 2019",
         y = "Count of words")
}

                   
#prepare for trigrams
trigrams <- function(df,df_name) {
  df %>%
    unnest_tokens(trigram, Contents, token = "ngrams", n = 3)%>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ")%>%
    filter(!word1 %in% stop_words2$word) %>%
    filter(!word2 %in% stop_words2$word) %>%
    filter(!word3 %in% stop_words$word) %>%
    mutate(word1 = wordStem(word1)) %>%
    mutate(word2 = wordStem(word2)) %>%
    mutate(word3 = wordStem(word3)) %>%
    unite(trigram, word1, word2, word3, sep = " ") %>%
    count(trigram, sort = TRUE) %>%
    head(15) %>%
    mutate(trigram = reorder(trigram, n)) %>%
    ggplot(aes(trigram, n)) +
    geom_col(fill = "darkblue") +
    scale_y_continuous(labels = comma_format()) +
    coord_flip() +
    labs(title = paste("Most common three words in",df_name,"reviews", sep=" "),
         subtitle = "From Oct 2016 to Oct 2019",
         y = "Count of words")
}

#prepare for keyword sentiment analysis by travel type
give.n <- function(x){
  return(c(y = median(x), label = length(x))) 
}
plotcomb<-function(combination, comb_name){
  ggplot(combination, aes(y=ave_sentiment, x=Type)) +
    geom_boxplot(fill = "grey80", colour = "#3366FF") +
    stat_summary(fun.data = give.n, geom = "text") + geom_hline(yintercept=0, color = "red")+
    labs(x = "Keywords",
         y = "Sentiment Average Score per Sentence",
         title = "Keyword Sentiment Analysis",
         subtitle = comb_name)+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.subtitle = element_text(hjust = 0.5)) 
}

#prepare for explore word network
network <- function(ta_type,type_name,num) {
  ta_type %>%
    distinct(Contents, .keep_all = TRUE) %>%
    unnest_tokens(word, Contents, drop = FALSE) %>%
    distinct(Reviewers, word, .keep_all = TRUE) %>%
    anti_join(stop_words2, by = "word")%>%
    distinct(Contents, .keep_all = TRUE) %>%
    unnest_tokens(word, Contents, drop = FALSE) %>%
    distinct(Reviewers, word, .keep_all = TRUE) %>%
    anti_join(stop_words2, by = "word") %>% 
    pairwise_count(word, Reviewers, sort = TRUE, upper = FALSE)%>%
    filter(n >= num) %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
    geom_node_point(size = 5) +
    geom_node_text(aes(label = name), repel = TRUE, 
                   point.padding = unit(0.2, "lines")) +
    theme_void()+labs(title = "Word co-ocurrences and correlations",
                      subtitle = type_name)
}

#customize stop word for couples
word_counts(ta_couple, "TripAdvisor as Couples")
couples_stop_words <- tribble(
  ~word,~lexicon,
  'ritz','SMART',
  'carlton','SMART',
  'half','SMART',
  'moon','SMART',
  'bay','SMART',
  'stay','SMART',
  'hotel','SMART',
  'day','SMART',
  'night','SMART',
  'experience','SMART',
  'property','SMART',
  'time','SMART',
  'stay','SMART',
)
stop_words_couple<-stop_words %>% bind_rows(couples_stop_words)
bigrams(ta_couple, "TripAdvisor as Couples")

?bind_rows
