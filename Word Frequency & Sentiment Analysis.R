#data exploration -  visualization
ta <- read.csv('ta.csv', stringsAsFactors=FALSE)
yp <- read.csv('yp.csv', stringsAsFactors=FALSE)
ta$Months <- as.yearmon(ta$Months, '%b %Y')
yp$Months <- as.yearmon(yp$Months, '%b %Y')
ta$Ratings <- as.factor(ta$Ratings)
yp$Ratings <- as.factor(yp$Ratings)
ta1 <- ta %>% group_by(Months) %>% count(Ratings)
yp1 <- yp %>% group_by(Months) %>% count(Ratings)
pic1 <- ggplot(ta1, aes(fill=Ratings, y=n, x=Months)) + 
  geom_bar(position="stack", stat="identity")+ggtitle("Tripadvisor Ratings by months from Oct 2016 to Oct 2019")+ 
  scale_fill_manual(values=c('darkred','deeppink3','darkorange1','deepskyblue','cyan4'))+scale_x_yearmon(format="%b %Y",n=5)+ylab('Counts')
pic2 <- ggplot(yp1, aes(fill=Ratings, y=n, x=Months)) + 
  geom_bar(position="stack", stat="identity")+ggtitle("Yelp Ratings by months from oct 2016 to Oct 2019")+ scale_fill_manual(values=c('darkred','deeppink3','darkorange1','deepskyblue','cyan4'))+
  scale_x_yearmon(format="%b %Y",n=5)+xlab('Months')+ylab('Counts')
grid.arrange(pic1, pic2, ncol=2)

yp2 <- subset(yp, Months == 'Feb 2017')
summary(yp2$Ratings)
?t.test
ta1<- ta %>% select(Ratings, Months)
ta1$Type <- 'Ta'
yp1<- yp %>% select(Ratings, Months)
yp1$Type <- 'yp'
tayp<- rbind(ta1,yp1)
tayp$Type <- as.factor(tayp$Type)
str(tayp)
tayp$Ratings<- as.numeric(tayp$Ratings)
t.test(Ratings~Type,data=tayp, alternative ="two.sided", var.equal = FALSE)
#top viewed words
pic_ta <- word_counts(ta,'Tripadvisor')
pic_yp <- word_counts(yp, 'Yelp')
grid.arrange(pic_ta, pic_yp, ncol=2)


##bigrams
bigram_ta <- bigrams(ta,'Tripadvisor')
bigram_yp <-bigrams(yp, 'Yelp')
grid.arrange(bigram_ta, bigram_yp, ncol=2)

##trigrams
trigram_ta <- trigrams(ta,'Tripadvisor')
trigram_yp <-trigrams(yp, 'Yelp')
grid.arrange(trigram_ta, trigram_yp, ncol=2)


#histogram
ta2 <- read.csv('ta2.csv', stringsAsFactors=FALSE)
ta2$Ratings <- as.factor(ta2$Ratings)
yp2 <- read.csv('yp2.csv', stringsAsFactors=FALSE)
yp2$Ratings <- as.factor(yp2$Ratings)

year_ratings <- function(df) {
  ggplot(df, aes(fill=Ratings, y=n, x=Qtr)) + 
    geom_bar(position="stack", stat="identity")+ coord_flip()+
    scale_fill_manual(values=c('darkred','deeppink3','darkorange1','deepskyblue','cyan4'))
}

pic3 <- year_ratings(ta2)
pic4 <-  year_ratings(yp2)
grid.arrange(pic3, pic4, ncol=2)


ta2$Ratings <- as.numeric(ta2$Ratings)
myAov1 <- aov(Ratings~year, data=ta2) # One-way analysis of variance
summary(myAov1)
#P-value is 0.939
#Tripadvisor rating didnt changes over year
yp2$Ratings<-as.numeric(yp2$Ratings)
myAov2 <- aov(Ratings~year, data=yp2) # One-way analysis of variance
summary(myAov2)
#P-value is 0.477
#Yelp rating didnt changes over year

#it is a five star hotel, so naturally has less 1 star ratings

#recreate plots
year_rat_percent <- function(df) {
  df %>% mutate('relative'=unlist(by(data = n, INDICES = Qtr, 
                                     FUN = function(x) round(x/sum(x)*100, digits = 1)))) %>% 
    ggplot(aes(fill=Ratings, y=n, x=Qtr)) + 
    geom_bar(position="stack", stat="identity")+
    scale_fill_manual(values=c('darkred','deeppink3','darkorange1','deepskyblue','cyan4'))+
    geom_text(aes(x = Qtr, label = paste0(relative,'%')),
              colour = 'black', position=position_stack(vjust=0.5),size=2.5)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
pic5 <- year_rat_percent(ta2)
pic6 <-  year_rat_percent(yp2)
grid.arrange(pic5, pic6, ncol=2)
str(ta2)
plot(Qtr,Ratings,data=ta2)
abline(lm(mpg~wt), col="red") # regression line (y~x) 

#lm for trip_type
class(ta)
str(ta)
?subset
ta_lm<- subset(ta,left(ta$Trip_types, 6) =='Travel')
ta_lm$Trip_types <- tail(strsplit(ta_lm$Trip_types,split=" "))
ta_lm$Trip_types <- word(ta_lm$Trip_types,-1)
ta_lm$Trip_types <- as.factor(ta_lm$Trip_types)
summary(ta_lm$Trip_types)

myAov3 <- aov(Ratings~Trip_types, data=ta_lm) # One-way analysis of variance
summary(myAov3)
#p_value is 0.281

#lm for trip_type

ta_lm <- read.csv('ta_triptype.csv', stringsAsFactors = FALSE)
ta_lm$Trip_types <- as.factor(ta_lm$Trip_types)

summary(ta_lm$Trip_types)
summary(lm(Ratings~Trip_types, data=ta_lm))
#business   couple   family  friends     solo 
#167      392      204       54       24 
#result: Business as base group
#solo has highest ratings
#friends has lowest ratings
plot(ta$Trip_types, ta$Ratings, main="Scatterplot", 
     xlab="Trip type ", ylab="Ratings")
str(ta)
#what friends do during the stay, why it has lowest ratings
ta_friends <- subset(ta_lm, Trip_types=='friends')
class(ta_friends)
#explore rating
friends_hist <- ta_friends %>% group_by(Ratings) %>% mutate(total=n())
friend_plot <- ggplot(friends_hist, aes(x=Ratings)) + 
  geom_bar(fill="steelblue")+ggtitle('Travel With Friends Ratings Distribution')
#explore top word
word_counts(ta_friends,'Travel with Friends Reviews')
bigrams(ta_friends,'Travel with Friends Reviews')
#explore word sentiment
sentence<- function(df){
  df %>% distinct(Contents, .keep_all = TRUE) %>%
    unnest_tokens(sentence, Contents, token='sentences') %>%
    distinct(Reviewers,sentence,.keep_all=TRUE) 
}
friend_sentence <- sentence(ta_friends) %>% select(Reviewers,sentence)
ta_word<-function(word){
  friend_sentence[grep(word, friend_sentence$sentence, ignore.case = TRUE),]
}
friend_view <- ta_word('view')
friend_service <- ta_word('service')
friend_staff <- ta_word('staff')
friend_food <- ta_word('food')
friend_wait <- ta_word('wait')
friend_location <- ta_word('location')
friend_latecheckout <- ta_word('late checkout')
friend_frontdesk <- ta_word('front desk')
friend_firepit <- ta_word('fire pit')

senti_view <- sentiment_by(friend_view$sentence)
senti_view$Type <- 'View'
senti_service <- sentiment_by(friend_service$sentence)
senti_service$Type <- 'Service'
senti_staff <- sentiment_by(friend_staff$sentence)
senti_staff$Type <- 'Staff'
senti_food <- sentiment_by(friend_food$sentence)
senti_food$Type <- 'Food'
senti_wait <- sentiment_by(friend_wait$sentence)
senti_wait$Type <- 'Wait'
senti_location <- sentiment_by(friend_location$sentence)
senti_location$Type <- 'Location'
senti_latecheckout <- sentiment_by(friend_latecheckout$sentence)
senti_latecheckout$Type <- 'Late Checkout'
senti_frontdesk<- sentiment_by(friend_frontdesk$sentence)
senti_frontdesk$Type <- 'Front Desk'
senti_firepit <- sentiment_by(friend_firepit$sentence)
senti_firepit$Type <- 'Fire Pit'
senti_f_comb <- rbind(senti_view,senti_service,senti_staff,senti_food,senti_wait,
                    senti_location,senti_latecheckout,senti_frontdesk,senti_firepit)
# function for number of observations 
plotcomb(senti_f_comb, 'Type: Travel with Friends')


#front desk check in experience need improvement
#word network
set.seed(1234)
network(ta_friends, "Type: Travel with Friends", 5)



#what couples do during the stay, since it has most data
ta_couple <- subset(ta_lm, Trip_types=='couple')
#explore rating
couple_hist <- ta_couple %>% group_by(Ratings) %>% mutate(total=n())
ggplot(couple_hist, aes(x=Ratings)) + 
  geom_bar(fill="steelblue")+ggtitle('Travel as Couple Ratings Distribution')
#explore top word
word_counts(ta_couple,'Travel as Couple Reviews')
bigrams(ta_couple,'Travel as Couple Reviews')
#explore word sentiment
couple_sentence <- sentence(ta_couple) %>% select(Reviewers,sentence)
cp_word<-function(word){
  couple_sentence[grep(word, couple_sentence$sentence, ignore.case = TRUE),]
}
cp_view <- cp_word('view')
cp_service <- cp_word('service')
cp_staff <- cp_word('staff')
cp_golf <- cp_word('golf')
cp_firepit <- cp_word('fire pit')
cp_clublounge <- cp_word('club lounge')
cp_clublevel <- cp_word('club level')
cp_frontdesk <- cp_word('front desk')
cp_mainbuild <- cp_word('main building')

senti_view <- sentiment_by(cp_view$sentence)
senti_view$Type <- 'View'
senti_service <- sentiment_by(cp_service$sentence)
senti_service$Type <- 'Service'
senti_staff <- sentiment_by(cp_staff$sentence)
senti_staff$Type <- 'Staff'
senti_golf <- sentiment_by(cp_golf$sentence)
senti_golf$Type <- 'Golf'
senti_firepit <- sentiment_by(cp_firepit$sentence)
senti_firepit$Type <- 'Fire Pit'
senti_clubloung <- sentiment_by(cp_clublounge$sentence)
senti_clubloung$Type <- 'Club Lounge'
senti_clublevel <- sentiment_by(cp_clublevel$sentence)
senti_clublevel$Type <- 'Club Level'
senti_frontdesk<- sentiment_by(cp_frontdesk$sentence)
senti_frontdesk$Type <- 'Front Desk'
senti_mainbuild <- sentiment_by(cp_mainbuild$sentence)
senti_mainbuild$Type <- 'Main Building'
senti_cp_comb <- rbind(senti_view,senti_service,senti_staff,senti_golf,senti_firepit,
                    senti_clubloung,senti_clublevel,senti_frontdesk,senti_mainbuild)
plotcomb(senti_cp_comb, 'Type: Travel as Couple')

network(ta_couple, "Type: Travel as Couples", 8)

#lm for location
yp_lm <- read.csv('yp_location.csv', stringsAsFactors = FALSE)
str(yp_lm)
yp_lm$Location <- as.factor(yp_lm$Location)
summary(yp_lm) #check the level to get top 6 location
relevel(yp_lm$Location, 'CA') #set base group to the most frequent location
summary(lm(Ratings~Location, data=yp_lm[yp_lm$Location %in% c("CA", 'CO', 'NY', 'NC', 'NV', 'HI'),]))
#CA has an average of 3.9 ratings, CO and NC have the highest rating
