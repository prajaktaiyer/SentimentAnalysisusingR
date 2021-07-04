#Importing Libraries
install.packages("scales")
library(psych)
library(car)
library(psych)
library(dplyr)
library(corrplot)

library(Hmisc)
library(gplots)
library(car) # advanced scatter plots 
library(corrplot) # plot correlations 
library(dplyr) # data aggregates 
library(gmodels) # cross tabulation
library(gplots) # plot means with CI 
library(psych) # descriptives
library(ggplot2)
options(scipen=99)
#####################################################
library(tidytext)
sentiments
get_sentiments("bing")#bing lexicon model classifies the sentiment into a binary category of negative or positive
 

#Converting the text of the books into a tidy format using unnest_tokens() function.

#Importing the textual data in the form of books authored by the novelist Jane Austen
library(janeaustenr)
library(stringr)
#Tidytext: to perform efficient text analysis on the data.
library(tidytext)
tidy_data <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
###########################################
#We have performed the tidy operation on our text such that each row contains a single word. 
#Using the "bing" lexicon and filter the words that correspond to joy.
#Using the book "Emma" to derive its words to implement the sentiment analysis model.

positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")
tidy_data %>%
  filter(book == "Emma") %>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)
##################################################
#Segregating columns of positive and negative sentiments using spread() function.
#Using the mutate() function to calculate the total sentiment(Formula= the difference between positive and negative sentiment).

library(tidyr)
bing <- get_sentiments("bing")
Emma_sentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book = "Emma" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
################################################
#Visualizing words present in the book "Emma" based on corrosponding positive and negative scores.
library(ggplot2)
ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
###############################################
#Counting the common positive and negative words from the novel.

counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)
#######################################################
#Visualizing the sentiment score using ggplot() function.
#Plotting the scores along the axis labeled with both positive and negative words.
counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")
#################################################
#Creating a wordcloud to delineate the recurring positive and negative words using comparision.cloud() function.
library(reshape2)
library(wordcloud)
tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)
