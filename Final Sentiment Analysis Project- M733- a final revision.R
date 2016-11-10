rm(list = ls(all = TRUE))

# Required Libraries

require(devtools)
library(twitteR)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(NLP)
library(sentiment)
library(stringr)
library(Rfacebook)
library(e1071)
library(caret)
library(rvest)


# Working Directory Setting

setwd("C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output")


# Getting Data
RBCI_auto <- html("https://insureye.com/Reviews/Auto-Insurance-Reviews/21-RBC-Insurance")
RBCI_home <- html("https://insureye.com/Reviews/Home-Insurance-Reviews/21-RBC-Insurance")
RBCI_life <- html("https://insureye.com/Reviews/Life-Insurance-Reviews/21-RBC-Insurance")

RBCI_auto_reviews <- RBCI_auto %>%
  html_nodes(".review-text") %>%
  html_text()

RBCI_home_reviews <- RBCI_home %>%
  html_nodes(".review-text") %>%
  html_text()

RBCI_life_reviews <- RBCI_life %>%
  html_nodes(".review-text") %>%
  html_text()

RBC_text <- c(RBCI_auto_reviews,RBCI_home_reviews,RBCI_life)

# Data Cleansing
RBC_text= gsub("(RT|via) ((?:\\b\\w*a\\w+)+)","",RBC_text) # remove retweet
RBC_text= gsub("@\\w+","",RBC_text) # remove people
RBC_text = gsub("[[:punct:]]","",RBC_text) # remove punctuations
RBC_text = gsub("[[:digit:]]","",RBC_text) # remove numbers
RBC_text = gsub("http\\w+","",RBC_text) # remove html links
RBC_text = gsub("[ \t]{2,}","",RBC_text) # removing uncessary spaces
RBC_text = gsub("^\\s+|\\s+$","",RBC_text) # removing uncessary spaces
try.error = function(x)
{
  y= NA
  try_error = tryCatch(tolower(x), error= function(e) e)
  if (! inherits(try_error,"error"))
    y= tolower(x)
  return(y)
}

RBC_text= sapply(RBC_text, try.error)
RBC_text= RBC_text[!is.na(RBC_text)]

# Writing Tweets to a CSV file
write.csv(RBC_text,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\RBCInsurance_comments.csv")

#--------------------------------------------------------------------------------------------------------------
# Dividing words into differnt emotions

class_emo = classify_emotion(RBC_text,algorithm = "bayes", prior = 1)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol= classify_polarity(RBC_text,algorithm = "bayes")
polarity= class_pol[,4]

sent_df= data.frame(text= RBC_text, emotion = emotion, polarity = polarity, stringsAsFactors = FALSE)
sent_df= within(sent_df, emotion <- factor(emotion, levels = names(sort(table(emotion),decreasing = TRUE))))


ge<- ggplot(sent_df, aes(x=emotion))+
  geom_bar(aes(y=..count.., fill= emotion))+
  scale_fill_brewer(palette = "Dark2")+
  labs(x= "emotion categories", y = "number of comments")+
  ggtitle(" 'RBC Insurance")

mytheme <- theme(plot.title = element_text(size = 46, face = "bold", colour = "black", vjust = -8.5))+
  theme(axis.title.x = element_text(face = "bold", colour = "black", size = 24))+
  theme(axis.title.y= element_text(face = "bold",colour = "black", size = 24))+
  theme(axis.text= element_text(face = "bold", colour = "darkred", size = 20))
ge+mytheme


gp<- ggplot(sent_df, aes(x=polarity))+
  geom_bar(aes(y=..count..,fill= polarity))+
  scale_fill_brewer(palette = "RdGy")+
  labs(x=" ploarity categories", y= "number of comments", size = 14)+
  ggtitle(" 'RBC Insurance Polarities")
gp+mytheme

emos= levels(factor(sent_df$emotion))
nemo= length(emos)
emo.docs = rep("", nemo)
for(i in 1:nemo)
{
  tmp= RBC_text[emotion== emos[i]]
  emo.docs[i]= paste(tmp, collapse = " ")
}

emo.docs= removeWords(emo.docs,stopwords("english"))

#-----------------------------------------------------------------------------------------------------------------------
# Task 1 : Creating a wordcloud

corpus = Corpus(VectorSource(emo.docs))
meta(corpus)
tdm= TermDocumentMatrix(corpus)
tdm= as.matrix(tdm)
colnames(tdm) = emos
comparison.cloud(tdm, colors = brewer.pal(nemo,"Dark2"), scale = c(3,.5),random.order = FALSE, title.size = 1.5)
-------------------------------------------------------------------------------------------------------------------
# Task 2 Sentiment analysis on each comment

# Task2 --> Sentiment Score function
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  scores = laply(sentences, function(sentence, pos.words, neg.words)
  {

    # split sentence into words with str_split (stringr package)
    word.list = str_split(sentence, "\\s+")
    words = unlist(word.list)

    # compare words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)

    # get the position of the matched term or NA
    # we just want a TRUE/FALSE
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)

    # final score
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )

  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}


# Task 2--> Importing Lexicon
pos = readLines("C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\opinion-lexicon-English\\positive-words.txt")
neg = readLines("C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\opinion-lexicon-English\\negative-words.txt")

# Task 2 --> Sentiment analysis by applying positive and negative socres to each tweet
RBC.score = score.sentiment(RBC_text,pos,neg, .progress = 'text')


# Task 2 --> Writing down the final output
write.csv(RBC.score,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\RBCInsurance_comments_score.csv")

# Taks 2 --> Total Number of positive and negative scores

RBC.score$positive = as.numeric(RBC.score$score >= 1)
RBC.score$negative = as.numeric(RBC.score$score <= -1)

numpos.RBC = sum(RBC.score$positive)
numneg.RBC = sum(RBC.score$negative)

numpos.RBC
numneg.RBC

#Task 2 --> global score
global_score.RBC = round( 100 * numpos.RBC / (numpos.RBC + numneg.RBC) )
global_score.RBC

#Task 2 --> Creating a bar plot

RBC.score.table<- matrix(c(numpos.RBC,numneg.RBC, global_score.RBC),ncol=3,byrow=TRUE)
colnames(RBC.score.table) <- c(" Positve Comments"," Negative Comments","Ratio Positive/Total ")
rownames(RBC.score.table) <- "Total Number"
RBC.score.table <- as.table(RBC.score.table)
RBC.score.table

barplot(RBC.score.table, main="Positive Vs. Negative In RBC",xlab="Number of comments",ylab = "Total Number", col=c("darkblue","red","yellow"),legend = rownames(RBC.score.table), beside=TRUE)

#---------------------------------------------------------------------------------------------------------------------
# Task 3 -- Sentiment Analysis on Competitors
#TD Insurance

# Getting Data
TD_auto <- html("https://insureye.com/Reviews/Auto-Insurance-Reviews/18-Meloche-Monnex-(TD-Insurance)")
TD_home18 <- html("https://insureye.com/Reviews/Home-Insurance-Reviews/18-Meloche-Monnex-(TD-Insurance)")
TD_home25 <- html("https://insureye.com/Reviews/Home-Insurance-Reviews/25-TD-Insurance")
TD_life <- html("https://insureye.com/Reviews/Life-Insurance-Reviews/25-TD-Insurance")

TDI_auto_reviews <- TD_auto %>%
  html_nodes(".review-text") %>%
  html_text()

TDI_home18_reviews <- TD_home18 %>%
  html_nodes(".review-text") %>%
  html_text()

TDI_home25_reviews <- TD_home25 %>%
  html_nodes(".review-text") %>%
  html_text()

TDI_life_reviews <- TD_life  %>%
  html_nodes(".review-text") %>%
  html_text()


TD_text <- c(TDI_auto_reviews,TDI_home18_reviews,TDI_home25_reviews,TDI_life_reviews)

TD_text= gsub("(RT|via) ((?:\\b\\w*a\\w+)+)","",TD_text) # remove retweet
TD_text= gsub("@\\w+","",TD_text) # remove people
TD_text = gsub("[[:punct:]]","",TD_text) # remove punctuations
TD_text = gsub("[[:digit:]]","",TD_text) # remove numbers
TD_text = gsub("http\\w+","",TD_text) # remove html links
TD_text = gsub("[ \t]{2,}","",TD_text) # removing uncessary spaces
TD_text = gsub("^\\s+|\\s+$","",TD_text) # removing uncessary spaces
TD_text= sapply(TD_text, try.error)
TD_text= TD_text[!is.na(TD_text)]


write.csv(TD_text,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\TDInsurance_Comments.csv")
TD.score = score.sentiment(TD_text,pos,neg, .progress = 'text')

write.csv(TD.score,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\TDInsurance_Comments_score.csv")

TD.score$positive = as.numeric(TD.score$score >= 1)
TD.score$negative = as.numeric(TD.score$score <= -1)

numpos.TD = sum(TD.score$positive)
numneg.TD = sum(TD.score$negative)

numneg.TD
numpos.TD

global_score.TD = round( 100 * numpos.TD / (numpos.TD + numneg.TD) )

global_score.TD

TD.score.table<- matrix(c(numpos.TD,numneg.TD,global_score.TD),ncol=3,byrow=TRUE)
colnames(TD.score.table) <- c(" Positve Comments"," Negative Comments","Ratio Positive/Total")
rownames(TD.score.table) <- "Total Number"
TD.score.table <- as.table(TD.score.table)
TD.score.table

barplot(TD.score.table, main="Positive Vs. Negative In TD",xlab="Number of comments",ylab = "Total Number", col=c("darkblue","red","yellow"),legend = rownames(TD.score.table), beside=TRUE)

#--------
# Task 3
# Aviva Insurance

Aviva_auto <- html("https://insureye.com/Reviews/Auto-Insurance-Reviews/2-Aviva")
Aviva_home <- html("https://insureye.com/Reviews/Home-Insurance-Reviews/2-Aviva")

AvivaI_auto_reviews <- Aviva_auto %>%
  html_nodes(".review-text") %>%
  html_text()

AvivaI_home_reviews <- Aviva_home %>%
  html_nodes(".review-text") %>%
  html_text()

Aviva_text <- c(AvivaI_home_reviews,AvivaI_auto_reviews)

Aviva_text= gsub("(RT|via) ((?:\\b\\w*a\\w+)+)","",Aviva_text) # remove retweet
Aviva_text= gsub("@\\w+","",Aviva_text) # remove people
Aviva_text = gsub("[[:punct:]]","",Aviva_text) # remove punctuations
Aviva_text = gsub("[[:digit:]]","",Aviva_text) # remove numbers
Aviva_text = gsub("http\\w+","",Aviva_text) # remove html links
Aviva_text = gsub("[ \t]{2,}","",Aviva_text) # removing uncessary spaces
Aviva_text = gsub("^\\s+|\\s+$","",Aviva_text) # removing uncessary spaces
Aviva_text= sapply(Aviva_text, try.error)
Aviva_text= Aviva_text[!is.na(Aviva_text)]

write.csv(Aviva_text,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\AVIVAInsurance_Comments.csv")
Aviva.score = score.sentiment(Aviva_text,pos,neg, .progress = 'text')

write.csv(Aviva.score,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\AVIVAInsurance_comments_score.csv")


Aviva.score$positive = as.numeric(Aviva.score$score >= 1)
Aviva.score$negative = as.numeric(Aviva.score$score <= -1)

numpos.Aviva = sum(Aviva.score$positive)
numneg.Aviva = sum(Aviva.score$negative)

numneg.Aviva
numpos.Aviva

global_score.Aviva = round( 100 * numpos.Aviva / (numpos.Aviva + numneg.Aviva) )
global_score.Aviva

Aviva.score.table<- matrix(c(numpos.Aviva,numneg.Aviva,global_score.Aviva),ncol=3,byrow=TRUE)
colnames(Aviva.score.table) <- c(" Positve comments"," Negative comments","Ratio Positive/Total")
rownames(Aviva.score.table) <- "Total Number"
Aviva.score.table <- as.table(Aviva.score.table)
Aviva.score.table

barplot(Aviva.score.table, main="Positive Vs. Negative In Aviva",xlab="Number of comments",ylab = "Total Number", col=c("darkblue","red","yellow"),legend = rownames(Aviva.score.table), beside=TRUE)


#----------------------------------------------
# Task 3
# BMO Insurance

BMO_life <- html("https://insureye.com/Reviews/Life-Insurance-Reviews/5-BMO-Insurance")


BMOI_life_reviews <- BMO_life %>%
  html_nodes(".review-text") %>%
  html_text()


BMO_text <- BMOI_life_reviews

BMO_text= gsub("(RT|via) ((?:\\b\\w*a\\w+)+)","",BMO_text) # remove retweet
BMO_text= gsub("@\\w+","",BMO_text) # remove people
BMO_text = gsub("[[:punct:]]","",BMO_text) # remove punctuations
BMO_text = gsub("[[:digit:]]","",BMO_text) # remove numbers
BMO_text = gsub("http\\w+","",BMO_text) # remove html links
BMO_text = gsub("[ \t]{2,}","",BMO_text) # removing uncessary spaces
BMO_text = gsub("^\\s+|\\s+$","",BMO_text) # removing uncessary spaces
BMO_text= sapply(BMO_text, try.error)
BMO_text= BMO_text[!is.na(BMO_text)]

write.csv(BMO_text,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\BMOInsurance_Comments.csv")
BMO.score = score.sentiment(BMO_text,pos,neg, .progress = 'text')

write.csv(BMO.score,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\BMOInsurance_comments_score.csv")


BMO.score$positive = as.numeric(BMO.score$score >= 1)
BMO.score$negative = as.numeric(BMO.score$score <= -1)

numpos.BMO = sum(BMO.score$positive)
numneg.BMO = sum(BMO.score$negative)

numneg.BMO
numpos.BMO

global_score.BMO = round( 100 * numpos.BMO / (numpos.BMO + numneg.BMO) )
global_score.BMO

BMO.score.table<- matrix(c(numpos.BMO,numneg.BMO,global_score.BMO),ncol=3,byrow=TRUE)
colnames(BMO.score.table) <- c(" Positve Comments"," Negative Comments","Ratio Positive/Total")
rownames(BMO.score.table) <- "Total Number"
BMO.score.table <- as.table(BMO.score.table)
BMO.score.table

barplot(BMO.score.table, main="Positive Vs. Negative In BMO",xlab="Number of comments",ylab = "Total Number", col=c("darkblue","red","yellow"),legend = rownames(BMO.score.table), beside=TRUE)

#----------------------------------------------
# Task 3
# Intact Insurance

Intact_home <- html("https://insureye.com/Reviews/Home-Insurance-Reviews/15-Intact-Insurance")
Intact_auto <- html("https://insureye.com/Reviews/Auto-Insurance-Reviews/15-Intact-Insurance")

IntactI_Home_reviews <- Intact_home  %>%
  html_nodes(".review-text") %>%
  html_text()

IntactI_Auto_reviews <- Intact_auto %>%
  html_nodes(".review-text") %>%
  html_text()


Intact_text <- c(IntactI_Home_reviews,IntactI_Auto_reviews)

Intact_text= gsub("(RT|via) ((?:\\b\\w*a\\w+)+)","",Intact_text) # remove retweet
Intact_text= gsub("@\\w+","",Intact_text) # remove people
Intact_text = gsub("[[:punct:]]","",Intact_text) # remove punctuations
Intact_text = gsub("[[:digit:]]","",Intact_text) # remove numbers
Intact_text = gsub("http\\w+","",Intact_text) # remove html links
Intact_text = gsub("[ \t]{2,}","",Intact_text) # removing uncessary spaces
Intact_text = gsub("^\\s+|\\s+$","",Intact_text) # removing uncessary spaces
Intact_text= sapply(Intact_text, try.error)
Intact_text= Intact_text[!is.na(Intact_text)]

write.csv(Intact_text,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\IntactInsurance_Comments.csv")
intact.score = score.sentiment(Intact_text,pos,neg, .progress = 'text')

write.csv(intact.score,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\IntactInsurance_comments_score.csv")


intact.score$positive = as.numeric(intact.score$score >= 1)
intact.score$negative = as.numeric(intact.score$score <= -1)

numpos.intact = sum(intact.score$positive)
numneg.intact = sum(intact.score$negative)

numneg.intact
numpos.intact

global_score.intact = round( 100 * numpos.intact / (numpos.intact + numneg.intact) )
global_score.intact

Intact.score.table<- matrix(c(numpos.intact,numneg.intact,global_score.intact),ncol=3,byrow=TRUE)
colnames(Intact.score.table) <- c(" Positve Comments"," Negative Comments","Ratio Positive/Total")
rownames(Intact.score.table) <- "Total Number"
Intact.score.table <- as.table(Intact.score.table)
Intact.score.table

barplot(Intact.score.table, main="Positive Vs. Negative In Intact",xlab="Number of comments",ylab = "Total Number", col=c("darkblue","red","yellow"),legend = rownames(Intact.score.table), beside=TRUE)

#----------
# Task 3
# Intact Insurance

Desjardins_home <- html("https://insureye.com/Reviews/Home-Insurance-Reviews/9-Desjardins-Insurance")
Desjardins_auto <- html("https://insureye.com/Reviews/Auto-Insurance-Reviews/9-Desjardins-Insurance")
Desjardins_life <- html("https://insureye.com/Reviews/Life-Insurance-Reviews/9-Desjardins-Insurance")



DesjardinsI_Home_reviews <- Desjardins_home  %>%
  html_nodes(".review-text") %>%
  html_text()

DesjardinsI_Home_reviews <- Desjardins_auto %>%
  html_nodes(".review-text") %>%
  html_text()

DesjardinsI_Life_reviews <- Desjardins_life %>%
  html_nodes(".review-text") %>%
  html_text()

Desjardins_text <- c(DesjardinsI_Home_reviews,DesjardinsI_Home_reviews,DesjardinsI_Life_reviews)

Desjardins_text= gsub("(RT|via) ((?:\\b\\w*a\\w+)+)","",Desjardins_text) # remove retweet
Desjardins_text= gsub("@\\w+","",Desjardins_text) # remove people
Desjardins_text = gsub("[[:punct:]]","",Desjardins_text) # remove punctuations
Desjardins_text = gsub("[[:digit:]]","",Desjardins_text) # remove numbers
Desjardins_text = gsub("http\\w+","",Desjardins_text) # remove html links
Desjardins_text = gsub("[ \t]{2,}","",Desjardins_text) # removing uncessary spaces
Desjardins_text = gsub("^\\s+|\\s+$","",Desjardins_text) # removing uncessary spaces
Desjardins_text= sapply(Desjardins_text, try.error)
Desjardins_text= Desjardins_text[!is.na(Desjardins_text)]

write.csv(Desjardins_text,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\DesjardinsInsurance_Comments.csv")
Desjardins.score = score.sentiment(Desjardins_text,pos,neg, .progress = 'text')

write.csv(Desjardins.score,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\DesjardinsInsurance_comments_score.csv")


Desjardins.score$positive = as.numeric(Desjardins.score$score >= 1)
Desjardins.score$negative = as.numeric(Desjardins.score$score <= -1)

numpos.Desjardins = sum(Desjardins.score$positive)
numneg.Desjardins = sum(Desjardins.score$negative)

numpos.Desjardins
numneg.Desjardins

global_score.Desjardins = round( 100 * numpos.Desjardins / (numpos.Desjardins + numneg.Desjardins) )
global_score.Desjardins

Desjardins.score.table<- matrix(c(numpos.Desjardins,numneg.Desjardins,global_score.Desjardins),ncol=3,byrow=TRUE)
colnames(Desjardins.score.table) <- c(" Positve Comments"," Negative Comments","Ratio Positive/Total")
rownames(Desjardins.score.table) <- "Total Number"
Desjardins.score.table <- as.table(Desjardins.score.table)
Desjardins.score.table

barplot(Desjardins.score.table, main="Positive Vs. Negative In Desjardins ",xlab="Number of comments",ylab = "Total Number", col=c("darkblue","red","yellow"),legend = rownames(Desjardins.score.table), beside=TRUE)


#-----------------------------------------------------------
# Task 3 --> Positive Bar Plot ( RBC, TD, Aviva, BMO, Intact, Desjardins)

Total.score.table.pos <- matrix(c(numpos.RBC,numpos.Aviva,numpos.BMO,numpos.Desjardins,numpos.intact,numpos.TD), ncol= 6, byrow = TRUE)
colnames(Total.score.table.pos) <-c(" RBC"," Aviva"," BMO"," Desjardins"," Intact","TD")
rownames(Total.score.table.pos) <- "Total Number"
Total.score.table.pos<- as.table(Total.score.table.pos)
Total.score.table.pos


barplot(Total.score.table.pos, main="Positive Comments Comparison",xlab="Number of Comments",ylab = "Total Number", col=c("darkblue","pink","red","yellow","orange","green"),legend = rownames(Total.score.table.pos), beside=TRUE)

#-----------------------------------------------------------
# Task 3 --> Negative Bar Plot ( RBC, TD, Aviva, BMO, Intact, Desjardins)

Total.score.table.neg <- matrix(c(numneg.RBC,numneg.TD,numneg.Aviva,numneg.BMO,numneg.intact,numneg.Desjardins), ncol= 6, byrow = TRUE)
colnames(Total.score.table.neg) <-c(" RBC"," Aviva"," BMO"," Desjardins"," Intact","TD")
rownames(Total.score.table.neg) <- "Total Number"
Total.score.table.neg <- as.table(Total.score.table.neg)
Total.score.table.neg


barplot(Total.score.table.neg, main="Negative Comments Comparison",xlab="Number of Comments",ylab = "Total Number", col=c("darkblue","pink","red","yellow","orange","green"),legend = rownames(Total.score.table.neg), beside=TRUE)

#-----------------------------------------------------------
# Task 3 --> Global Bar Plot ( RBC, TD, Aviva, BMO, Intact, Desjardins)

Total.score.table.global <- matrix(c(global_score.RBC,global_score.TD,global_score.Aviva,global_score.BMO,global_score.intact,global_score.Desjardins), ncol= 6, byrow = TRUE)
colnames(Total.score.table.global) <-c(" RBC"," Aviva"," BMO"," Desjardins"," Intact","TD")
rownames(Total.score.table.global) <- "Total Number"
Total.score.table.global<- as.table(Total.score.table.global)
Total.score.table.global


barplot(Total.score.table.global, main="Global Mean Score Comparison",xlab="Number of Comments",ylab = "Total Number", col=c("darkblue","pink","red","yellow","orange","green"),legend = rownames(Total.score.table.pos), beside=TRUE)


#------------------------
# Task 4

Insurance_text= c(RBC_text,TD_text,Aviva_text,BMO_text,Intact_text,Desjardins_text)
write.csv(Insurance_text,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\Insurance_All.csv")

Insurance.score = score.sentiment(Insurance_text,pos,neg, .progress = 'text')
write.csv(Insurance.score,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\Insurance_All_score.csv")


Insurance.score$pos = as.numeric(Insurance.score$score >= 1)
Insurance.score$neg = as.numeric(Insurance.score$score <= -1)


numpos_insurance = sum(Insurance.score$pos)
numneg_insurance = sum(Insurance.score$neg)

global_score_insurance = round(100* numpos_insurance/(numpos_insurance+numneg_insurance))

Insurance.score["Score_Tag"] <- NA
write.csv(Insurance.score,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\Insurance_All_score_test.csv")



nrow(Insurance.score)

for (i in 1:nrow(Insurance.score))
{
  if(Insurance.score$score[i]> 0)
  {
    Insurance.score$Score_Tag[i] <- "Positive"
  }
  else if (Insurance.score$score[i] < 0)
  {
    Insurance.score$Score_Tag[i] <- "Negative"
  }
  else
  {
    Insurance.score$Score_Tag[i] <- "No Tag"
  }
}

write.csv(Insurance.score,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\Insurance_All_score_test1.csv")

# 4.b
termDM <- DocumentTermMatrix(Corpus(VectorSource(Insurance_text)))
termDM

termDM <- removeSparseTerms(termDM,0.99)
inspect(termDM)
termD

wordMatrix = as.data.frame(t(as.matrix(termDM)))
write.csv(wordMatrix,"C:\\Users\\eslamisp\\Desktop\\PhD Files\\Fall 2015\\M733\\Term Project\\Final code\\Output\\wordmatrix.csv")


#4.c

splitdf <- function(dataframe, seed=NULL, size) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)*size))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

splits <- splitdf(wordMatrix, seed=808, size=0.80)


str(splits)
lapply(splits,nrow)
lapply(splits,head)

training <- splits$trainset
testing <- splits$testset

nrow(training)
nrow(testing)

#4.d, 4.e




model <- naiveBayes(as.matrix(Insurance_text),as.factor(Insurance.score$Score_Tag) ,data = training)
summary(model)

predicted <- predict(model, newdata=testing[ ,-1])
summary(predicted)

actual <- Insurance.score$Score_Tag


#4.f


length(actual)
length(predicted)


confusionMatrix(predicted,sample(actual, size = length(predicted)))
