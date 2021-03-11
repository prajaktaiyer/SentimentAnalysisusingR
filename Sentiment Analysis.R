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
get_sentiments("bing")#bing lexicon model on the other hand, classifies the sentiment into a binary category of negative or positive
#In this step, we will import our libraries 'janeaustenr', 'stringr' as well as 'tidytext'. 
#The janeaustenr package will provide us with the textual data in the form of books authored by the novelist Jane Austen. 
#Tidytext will allow us to perform efficient text analysis on our data.
#We will convert the text of our books into a tidy format using unnest_tokens() function.

library(janeaustenr)
library(stringr)
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
#We will now make use of the "bing" lexicon to and implement filter() over the words that correspond to joy.
#We will use the book Sense and Sensibility and derive its words to implement out sentiment analysis model.

positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")
tidy_data %>%
  filter(book == "Emma") %>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)
##################################################
#In the next step, we will use spread() function to segregate our data into separate columns of positive and negative sentiments.
#We will then use the mutate() function to calculate the total sentiment, that is, the difference between positive and negative sentiment.

library(tidyr)
bing <- get_sentiments("bing")
Emma_sentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book = "Emma" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
################################################
#In the next step, we will visualize the words present in the book "Emma" based on their corrosponding positive and negative scores.
library(ggplot2)
ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")
###############################################
#Now we will count the most common positive and negative words that are present in the novel.

counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)
#######################################################
#In the next step, we will perform visualization of our sentiment score.
#We will plot the scores along the axis that is labeled with both positive as well as negative words.
#We will use ggplot() function to visualize our data based on their scores.
counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")
#################################################
#In the final visualization, let us create a wordcloud that will delineate the most recurring positive and negative words.
#In particular, we will use the comparision.cloud() function to plot both negative and positive words in a single wordcloud as follows:
library(reshape2)
library(wordcloud)
tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)
