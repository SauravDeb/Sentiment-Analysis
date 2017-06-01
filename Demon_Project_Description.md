# Sentiment Analysis of the general public towards Demonetization

## Data
The data used are tweets scraped from Twitter which has been provided on Kaggle for analysis purpose. 

* * *
## Motivation

Demonetization received support from several bankers as well as from some international commentators.However it was heavily criticised by members of the opposition parties, leading to debates in both houses of parliament and triggering organised protests against the government in several places across India.

It would be interesting to interpret tweets from the general public as a whole, so as to attain insightful knowledge regarding the sentiments of citizens on Twitter.

* * *

### Load packages

```{r}
library(readr)
library(dplyr)
library(tidyr)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(tm)
library(SnowballC)
```

### Load the Data Set


```{r load-data}
demonetization_tweets <- read_csv("~/R works/demonetization-tweets.csv")
```

* * *





## Exploratory data analysis

```{r}
demonet_text_tibble <- demonetization_tweets %>% select(X1, text)

tidy_demonet <- demonet_text_tibble %>% unnest_tokens(word, text)

tidy_demonet <- tidy_demonet %>% anti_join(stop_words)

demonet_sentiment_afinn <- tidy_demonet %>% inner_join(get_sentiments("afinn")) %>%  group_by(Tweet_no. = X1) %>%                               summarise(sentiment = sum(score))

#Plotting our sentiments score as compared to the no. of tweets

ggplot(demonet_sentiment_afinn, aes(Tweet_no., sentiment, fill = 0)) +
         geom_col(show.legend = FALSE) + labs(title = "Sentiment score Distribution", x = "Tweet No.", y = "Sentiment           score")
```
## Plot
![](https://github.com/SauravDeb/Sentiment-Analysis/blob/master/Sentiment_distribution.png)

```{r}
#Plotting a bar chart of positive, negative and neutral sentiments
demonet_sentiment_afinn$score[demonet_sentiment_afinn$sentiment < 0] <- "Negative"

demonet_sentiment_afinn$score[demonet_sentiment_afinn$sentiment == 0] <- "Neutral"

demonet_sentiment_afinn$score[demonet_sentiment_afinn$sentiment > 0] <- "Positive"

demonet_sentiment_afinn <- demonet_sentiment_afinn %>% count(Tweet_no., score)

ggplot(demonet_sentiment_afinn, aes(score,n, fill = score)) + geom_bar(stat="identity", colour="black") + labs(title = "Demonetisation sentiment analysis", x = "Sentiment", y = "Tweet Count")
```
## Plot
![](https://github.com/SauravDeb/Sentiment-Analysis/blob/master/demonetisation_barplot.png)
```{r}
#Forming wordcloud
corpus <- Corpus(VectorSource(demonetization_tweets$text))

 #--------clean and stem-----------

corpus <- tm_map(corpus, tolower)

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeWords,c("demonetisation", "demonetization", "demonet",stopwords("english")))

corpus <- tm_map(corpus, stemDocument)

wordcloud(corpus, max.words = 500, random.order = F, scale(4,0.5), rot.per = 0.35, colors = brewer.pal(8,"Dark2"))
```
## Most popular words
![](https://github.com/SauravDeb/Sentiment-Analysis/blob/master/demonetisation_wordCloud.png)



* * *

