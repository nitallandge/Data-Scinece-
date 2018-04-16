library(RCurl);
library(RJSONIO);
api_key<-"AIzaSyB1ZZyH9IzlmeFmrdtJ7swjaIIkTdJ5RqU"
user_id <- "105616015219357887822"
data <- getURL(paste("https://www.googleapis.com/plus/v1/people/",user_id,"/activities/public?maxResults=100&key=", api_key, sep=""),ssl.verifypeer = FALSE)
data
js <- fromJSON(data);
js
df = data.frame(no = 1:length(js$items))
df
for (i in 1:nrow(df)){
  
  df$kind[i] = js$items[[i]]$verb
  
  
  df$title[i] = js$items[[i]]$title
  
  df$replies[i] = js$items[[i]]$object$replies$totalItems
  
  df$plusones[i] = js$items[[i]]$object$plusoners$totalItems
  
  df$reshares[i] = js$items[[i]]$object$resharers$totalItems
  
  df$url[i] = js$items[[i]]$object$url
  
}
filename <- paste("gplus_data_", user_id, sep="")
filename
write.table(df, file = paste0("gplus_data_105616015219357887822",".csv"), sep = ",", col.names = NA,
            qmethod = "double")
df_graph = df[,c(1,4,5,6)]
df_graph
library(ggplot2)
library(reshape2)
melted=melt(df_graph,id.vars='no')
ggplot(melted,aes(x=factor(no),y=value,color=factor(variable),group=factor(variable)))+
  geom_line()+xlab('no')+guides(color=guide_legend("metrics"))+
  labs(title="Google+")

#word cloude
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

aaDS=read.csv("gplus_data_105616015219357887822.csv")
aaDs=data.frame(aaDS)

some_txt <- toString(df$title)
some_txt
clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

clean_text = clean.text(some_txt)
tweet_corpus = Corpus(VectorSource(clean_text))

tdm = TermDocumentMatrix(tweet_corpus, control = list(removePunctuation = TRUE,stopwords = stopwords("english"), removeNumbers = TRUE, tolower = TRUE))


library(wordcloud)
m = as.matrix(tdm) 
word_freqs = sort(rowSums(m), decreasing=TRUE) #now we get the word orders in decreasing order
dm = data.frame(word=names(word_freqs), freq=word_freqs) #we create our data set
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2")) #and we visualize our data

#aaDS=read.csv("gplus_data_105616015219357887822.csv")
#aaDs=data.frame(aaDS)

# sentiment analysis 


library(sentiment)
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]

emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(some_txt, algorithm="bayes")
polarity = class_pol[,4]
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) + scale_fill_brewer(palette="Dark2")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) + scale_fill_brewer(palette="RdGy")

emos = levels(factor(sent_df$emotion))

nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = some_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 random.order = FALSE, title.size = 1.5)

