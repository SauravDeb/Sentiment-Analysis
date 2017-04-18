library(readr)
library(dplyr)
library(tidyr)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
demonetization_tweets <- read_csv("~/R works/demonetization-tweets.csv") 
#imported as a tibble. Hence the 'factor' type texts are converted to char.
demonet_text_tibble <- demonetization_tweets[c("X1" , "text")]
tidy_demonet <- demonet_text_tibble %>% unnest_tokens(word, text)
tidy_demonet <- tidy_demonet %>% anti_join(stop_words)
#Using the 'bing' lexicons
demonet_sentiment_bing <- tidy_demonet %>% inner_join(get_sentiments("bing")) %>% count(X1, sentiment)  %>% spread(sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative)
demonet_sentiment_bing$net_sentiment <- demonet_sentiment$sentiment
demonet_sentiment_bing$net_sentiment[demonet_sentiment$sentiment > 1] <- "Positive"
demonet_sentiment_bing$net_sentiment[demonet_sentiment$sentiment > -1 & demonet_sentiment$sentiment <= 1] <- "Neutral"
demonet_sentiment_bing$net_sentiment[demonet_sentiment$sentiment <= -1] <- "Negative"
#Using the 'afinn' lexicon
demonet_sentiment_afinn <- tidy_demonet %>% inner_join(get_sentiments("afinn")) %>%  group_by(Tweet_no. = X1) %>% summarise(sentiment = sum(score))
#Usiing the 'nrc' lexicon
demonet_sentiment_nrc <- tidy_demonet %>% inner_join(get_sentiments("nrc")) %>% filter(sentiment==c("positive", "negative"))
#Plotting our sentiments
qplot(demonet_sentiment$line, demonet_sentiment$sentiment, demonet_sentiment, geom = "line", xlab = "TWEETS", ylab = "SENTIMENT", color = 'red')
#Plotting a bar chart
ggplot(demonet_sentiment_afinn, aes(score,n, fill = score)) + geom_bar(stat="identity", colour="black") + labs(title = "Demonetisation sentiment analysis", x = "Sentiment", y = "Tweet Count")
#Forming wordcloud
corpus <- Corpus(VectorSource(demonetization_tweets$text))
 #--------clean and stem-----------
wordcloud(corpus, max.words = 500, random.order = F, scale(4,0.5), rot.per = 0.35, colors = brewer.pal(8,"Dark2"))