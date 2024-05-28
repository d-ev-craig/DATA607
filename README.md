# DATA607 Repository - Data Management Techniques | NLP | Dashboards
This course covered many basics in data management for different purposes. There were two main projects that were quite helpful covered here.

## Dashboard
The focus was puling information from an API, cleaning the data, and allowing user interactable items to view exploratory plots and create models based on their chosen variables. The data was pulled from Valve's API for Dota 2, a highly popular competitive video game with $40 million dollar tournaments.

The dashboard can be found [here](https://hhl7f9-daniel-craig.shinyapps.io/Predicting_Wins_Classification/).

![Dashboard Example](Projects/Final_Project_Predicting_Wins/dashboard_ex.png)

## NLP Data Management Techniques

The in-depth R Pubs Document can be found [here](https://rpubs.com/devcraig/1023099)

This document covers a Sentiment Analysis using Natural Language Processing Techniques (NLP) on Mark Twain's Adventures of Huckleberry Finn.

This document focuses on analyzing the progression of story sentiment (negative or positive) by chapter.

```{r}
ggplot(huckSentiment, aes(x = index, y = sentiment, fill = sentiment)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient2(low='red', mid='snow3', high='darkgreen', space='Lab') +
  geom_col(show.legend = FALSE)
```

![Chapter Sentiment](Week_10_Sentiment_Analysis/plots/chap_sent.png)

A comparison of sentiment between different lexicons used.
```{r}
value <- get_sentiment(tidyHuck$word,method = "syuzhet") #calling syuzhet's get_sentiment command

syuzhetHuck <- cbind(tidyHuck,value) #attaching to tidyHuck to match AFINN's tibble structure


#Below code finishes matching AFINN structure
syuzhet <- syuzhetHuck %>% 
  group_by(index = linenumber %/% 80) %>% #Re-iterating AFINN's 80 word division for comparison
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "syuzhet")


bind_rows(afinn, 
          bing_and_nrc,syuzhet) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
```
![Lexicon Sent Comparison](Week_10_Sentiment_Analysis/plots/lexicon_sent_comp.png)

Viewing which words contribute to the sentiment the most.


```
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
```
![Word Count](Week_10_Sentiment_Analysis/plots/sent_count.png)


As well as a sentiment word comparison between lexicons used.

```{r}
bing_word_counts <- bing_word_counts %>%
  mutate(method='bing')

bind_rows(bing_word_counts,nonNeutralSyuzhetCounts) %>%
  group_by(method,sentiment) %>%
  slice_max(n, n = 5) %>%
  arrange(desc(n)) %>%
  ggplot(aes(n,word, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_grid(sentiment~method)
```
![Word Count Comparison](Week_10_Sentiment_Analysis/plots/sent_count_comp.png)


With an overall wordcloud capturing both count and sentiment.
```
pal2 = brewer.pal(8, "RdYlGn")

tidyHuck %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "darkgreen"),
                   max.words = 100)
```
![Word Cloud](Week_10_Sentiment_Analysis/plots/pogneg_wordcloud.png)
