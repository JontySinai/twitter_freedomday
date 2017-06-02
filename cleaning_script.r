
## This is a cleaning script for analysing tweets streamed on Freedom Day (April 27th 2017) in South Africa. 
#  This script makes use of the 'tweets_freedom_day.feather' file which is not included in the repo to protect user information. 
#  The outputs of this script, however, are included so that the results can be rendered in 'freedomday_dashboard.rmd'. 

## Libraries:
library(feather)
library(rvest)

library(dplyr)
library(tidyr)

library(tm)
library(tidytext)
library(lubridate)

library(ggmap)

## Globals
options(stringsAsFactors = FALSE) # this will help with parsing text

search_query <- c("#FreedomDay", "#FreedomMovement", "#SAUnite", "#SaveSA", "#27April")

# Global functions ########################################################################################################

# text_vec is a character vector or string
# match_to is a regex pattern
# col_headings = c("heading for the matches", "frequency")

regex_match <- function(text_vec, match_to, col_headings = c("tag", "freq")) {
  
  start_idx <- grep(text_vec, pattern = match_to) # get start index of every pattern match
  
  match_idx <- gregexpr(pattern = match_to, text = text_vec[start_idx]) # get all string indices of each match
  
  extracted_matches <- regmatches(text_vec[start_idx], m = match_idx) # now get those matches with their counts
  
  match_df <- data.frame(table(unlist(extracted_matches)))
  
  colnames(match_df) <- col_headings
  
  return(match_df)
  
}

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

# Load tweets #############################################################################################################

raw_tweets <- read_feather("tweets_freedom_day.feather")

# use iconv to convert UTF-8 to ASCII (which R can read)
tweets <- raw_tweets %>% mutate(iconv(text, from = "UTF-8", to = "ASCII"))

# first use "byte" to ensures that the hex code is retained for reading emojis (see later)
text_emojis <- iconv(raw_tweets$text, from = "UTF-8", to = "ascii", sub = "byte")

## Load emoji dictionary

# read eomji dictionary (from https://github.com/today-is-a-good-day/emojis/blob/master/emDict.csv)
# Unfortunately we don't have access to the original emoji dictionary as the link appears to be out of date.
# I have raised an issue with @today-is-a-good-day. For now we will use her processed dictionary.

emDict <- read.csv2("emoji_dictionaries/emDict.csv")
colnames(emDict) <- c("description", "native", "bytes", "r.encoding")

write_feather(emDict, "emoji_dictionaries/emDict.feather")

## We'll now make a pattern for parsing emojis

r_encoding <- emDict$r.encoding
r_encoding_pattern <- paste(r_encoding, collapse = "|") # collapse the R encodings for regex parsing

# Summary data ############################################################################################################

# the first thing we will do is gather statistics on the number of tweets, unique users, replies and retweets. 
# afterwards user information won't be processed throughout the cleaning script and none of the outputs will retain user
# information to preserve anonymity

# finding the total number of tweets is easy
count_tweets <- nrow(tweets)

# next let's get the total number of retweets
# piping ensures that we ony end up with an aggregate number at the end of process
# we need to order the retweets in descending order before removing duplicates so we retain the latest count information
count_retweets <- tweets %>% select(text, retweet_count) %>% data.frame() %>% 
                  arrange(desc(retweet_count)) %>% subset(!duplicated(text)) %>%
                  select(retweet_count) %>% sum()

# now lets count the total number of replies
count_replies <- tweets %>% select(in_reply_to_screen_name) %>% 
                 subset(!is.na(in_reply_to_screen_name)) %>% 
                 nrow()

# and finally lets count the total number of unique users
count_users <- tweets %>% select(screen_name) %>%
               subset(!duplicated(screen_name)) %>%
               nrow()

# now we can create a data frame for the summary data
totals <-  c(count_tweets, count_users, count_retweets, count_replies)
class <-  c("Tweets", "Users" ,"Retweets", "Replies")

summary <- data.frame(totals = totals, class = class)

# save summary data (note that this is an aggregate and is fully anonymised since 'screen_name' is lost in the counts)
write_feather(summary, "feathers/summary.feather")

# Hashtags ################################################################################################################

# we need to use 'text_emojis' which has cleaner text for regex parsing

hashtag_pattern <- "#[[:alnum:]]+" # regex pattern: hashtags

all_hashtags <- regex_match(text_emojis, 
                            match_to = hashtag_pattern, 
                            col_headings = c("tag", "freq")) %>%
                filter(tag %not in% search_query)

write_feather(all_hashtags, "feathers/all_hashtags.feather")

# Mentions ################################################################################################################

mention_pattern <- "@[[:alnum:][:punct:]]+" # regex pattern: hashtags

all_mentions <-regex_match(text_emojis, 
                           match_to = mention_pattern, 
                           col_headings = c("mention", "freq"))

write_feather(all_mentions, "feathers/all_mentions.feather")

# Locations ###############################################################################################################

# note that we anonymise the location data by aggregating over a random sample of 2000 locations with no user information
# retained

locations <- tweets %>% select(location) %>% data.frame() %>% setNames("location") %>% 
             subset(!is.na(location))

# '%' char confuses Google's Geocoding API so replace it
locations$location <- gsub("%", "", locations$location)

# remove rows with empty values
locations <- subset(locations, location!="")

# now take a random sample of 2000 locations and get geocodes
geocodes <- geocode(location = sample(locations$location, 2000), output = "latlon", source = "dsk") %>% 
            na.omit() %>%
            group_by(lon, lat) %>%
            summarise(freq = n())

write_feather(geocodes, "feathers/geocodes.feather")

# Text cleaning ###########################################################################################################

# we'll keep the text and datetime in the same data frame for later use

clean_tweets <- data.frame(text = tweets$text, datetime = tweets$created_at)

clean_tweets$text <- gsub(r_encoding_pattern, "", clean_tweets$text) # remove emoji symbols
clean_tweets$text <- gsub("@[[:alnum:][:punct:]]+", "", clean_tweets$text) # remove @handles; note that we have to escape \w -> \\w
clean_tweets$text <- gsub("#\\w+", "", clean_tweets$text) # remove hashtags
clean_tweets$text <- gsub("http[[:alnum:][:punct:]]+","", clean_tweets$text) # remove urls
clean_tweets$text <- gsub("[[:punct:]]", "", clean_tweets$text) # remove punctuation
clean_tweets$text <- gsub("RT ", "", clean_tweets$text) # finally remove the RT symbol from all retweets

# the above regex produces strange non-printable characters <?> which we can remove. 
# these are artifacts of the encoding however information is lost when chaning the encoding (about 2/3 of all the words)
# thus we use a regex hack thanks to Joshua Ulrich:
clean_tweets$text <- gsub("[^[:alnum:]///' ]", "", clean_tweets$text)

## Corpus

# now we will convert the words into a 'corpus' of text

corpus <- Corpus(VectorSource(clean_tweets$text))

# now we use `tm` (text mining) techniques to clean the corpus further. One of these techniques is to remove stop words (words like
# 'and', 'the', 'is' etc). `tm` contains a library of stopwords for several languages. We can check the `user_lang` column in
# `tweets` to check which languages we need to remove stopwords for:

languages <- data.frame(lang = raw_tweets$user_lang) %>%
             group_by(lang) %>%
             summarise(freq = n()) %>%
             arrange(desc(freq))

# use View(languages) to inspect which language most users were tweeting in and its clear that the English is the only language
# we need to remove stop words for (the other languages are too infrequent to affect the distribution of the words in words)

corpus <- corpus %>%
          tm_map(removePunctuation) %>%
          tm_map(content_transformer(tolower)) %>%
          tm_map(removeWords, stopwords("en")) %>%
          tm_map(removeWords, "amp") %>%
          tm_map(stripWhitespace)

## Document Term Matrix

# a DTM is a sparse matrix of word occurences in each line of the corpus
# we'll use a DTM to get the frequency of the words contained in the corpus

dtm <- DocumentTermMatrix(corpus)

# now let's get the total frequencies of each term:

words <- data.frame(colSums(as.matrix(dtm)))

# this data frame contains each word as a row label, let's use `tibble` to get a more useful data frame

words <- tibble::rownames_to_column(words, var = "word")
colnames(words) <- c("word", "freq")

# and save the all important words
write_feather(words, "feathers/words.feather")

# Statuses #######################################################################################################

# We'll use the same techniques to get the words contained in the user's statuses

clean_statuses <- iconv(tweets$description, 'UTF-8', 'ASCII')

clean_statuses <- gsub(r_encoding_pattern, "", clean_statuses)
clean_statuses <- gsub("@\\w+", "", clean_statuses)
clean_statuses <- gsub("#\\w+", "", clean_statuses)
clean_statuses <- gsub("http[[:alnum:][:punct:]]+", "", clean_statuses)
clean_statuses <- gsub("[[:punct:]]", "", clean_statuses)

# remove NA
clean_statuses <- clean_statuses[!is.na(clean_statuses)]

# corpus
status_corpus <- Corpus(VectorSource(clean_statuses)) %>%
  
              tm_map(removePunctuation) %>%
              tm_map(content_transformer(tolower)) %>%
              tm_map(removeWords, stopwords("en")) %>%
              tm_map(removeWords, "amp") %>%
              tm_map(stripWhitespace) %>%
              tm_map(removeNumbers)

# dtm
status_dtm <- DocumentTermMatrix(status_corpus)

status_words <- data.frame(colSums(as.matrix(status_dtm)))
status_words <- tibble::rownames_to_column(status_words, var = "word")
colnames(status_words) <- c("word", "freq")

# save!
write_feather(status_words, "feathers/status_words.feather")

# Tidy Words ###############################################################################################################

# to use the sentiment analysis libraries effectively, we'll process 'clean_text' from earlier into a "tidy format"
# rather than processing it into a corpus or DTM
# with unnest_tokens, we preserve the datetime information for every word

tidy_words <- clean_tweets %>% unnest_tokens(word, text)

## format datetime
#  'unnest_tokens' is finnicky so we have to format the timezone after using it

# format corresponds to "Wed Apr 26 13:05:23 +0000 2017"
tidy_words$datetime <- strptime(tidy_words$datetime, format = "%a %b %d %H:%M:%S %z %Y")
tidy_words$datetime  <- with_tz(tidy_words$datetime , "Africa/Johannesburg")

# now lets add an time interval column which we will use to group the words by
tidy_words$time_interval <- cut(tidy_words$datetime, breaks = "1 hour")

tidy_words$time_interval <- as.POSIXct(tidy_words$time_interval)
tidy_words$datetime <- as.POSIXct(tidy_words$datetime)

# Test Sentiment Lexicons #################################################################################################

# The next step is to test out different sentiment lexicons (libraries/dictionaries) from the 'tidytext'package. These are:
#   
# - `AFINN` by Finn Årup Nielsen. This lexicon scores each word from -5 (most negative) to 5 (most positive).
# - `bing` by Bing Liu et al. This lexicon categorises each word as either positive or negative.
# - `nrc` by Saif Mohammad and Peter Turney. This lexicon classifies each word into the categories: 
#   positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.

# the following method is taken straight from Silge and Robinson which we can use to quickly compare the sentiment
# lexicons. There isn't too much variation so the choice of lexicon won't matter too much

afinn_sentiments <- tidy_words %>%
  inner_join(get_sentiments("afinn")) %>% 
  group_by(time_interval) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")  

bing_nrc_sentiments <- bind_rows(tidy_words %>% 
                                   inner_join(get_sentiments("bing")) %>%
                                   mutate(method = "Bing et al."),
                                 tidy_words %>% 
                                   inner_join(get_sentiments("nrc") %>% 
                                                filter(sentiment %in% c("positive", 
                                                                        "negative"))) %>%
                                   mutate(method = "NRC")) %>% 
  count(method, time_interval, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn_sentiments, 
          bing_nrc_sentiments) %>%
  ggplot(aes(time_interval, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# let's see if it helps to view sentiment per tweet in each time interval:
bing_nrc_sentiments$tweets_per_hour <- aggregate(tidy_words, by = list(tidy_words$time_interval), length)$time_interval

# Looking at the comparision graph, our choice of sentiment will not make a massive difference to the results. 
# The 'Bing' and 'NRC' lexicons have the advantage of being binary lexicons over 'AFINN' (which is good for measuring 
# sentiment intensity but will not be significant here). Moreover the 'Bing' lexicon appears to be slightly more 
# sensitive than the 'NRC' lexicon so I'll proceed with the 'Bing' lexicon to save sentiment data.

# Sentiment ##########################################################################################################

sentiment <- tidy_words %>% inner_join(get_sentiments("bing"))

# proportion of words with sentiment matches?

nrow(sentiment)/nrow(tidy_words) # answer is 0.1000051 i.e. 10%   :\

# save
write_feather(sentiment, "feathers/sentiment.feather")

# further processing for useful plots
sentiment_over_time <- count(sentiment_by_word, time_interval, sentiment) %>%
                       spread(sentiment, n, fill = 0)

write_feather(sentiment_over_time, "sentiment_over_time.feather")

# just the positive and negative words

pos_sentiment <- sentiment_by_word %>% select(word, sentiment) %>% subset(sentiment == "positive")
neg_sentiment <- sentiment_by_word %>% select(word, sentiment) %>% subset(sentiment == "negative")

write_feather(pos_sentiment, "feathers/pos_sentiment.feather")
write_feather(neg_sentiment, "feathers/neg_sentiment.feather")

# Emoji Sentiment Dictionary ##############################################################################################

# Sentiment analysis of emojis is possible thanks to a tudy(http://kt.ijs.si/data/Emoji_sentiment_ranking/about.html) by 
# P. Kralj Novak, J. Smailovic, B. Sluban and I. Mozetic at the Jožef Stefan Institute in Ljubljana, Slovenia. We will 
# use their results together with the R emoji decoder by Jessica Peterka-Bonetta
# (http://opiateforthemass.es/articles/emoji-analysis/).

# study results
url <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"

# scrape url and convert table to data frame
emSentiment <- url %>% read_html() %>% html_table() %>% data.frame %>% select(-Image.twemoji., -Sentiment.bar.c.i..95..)

colnames(emSentiment) <- c("native", "unicode", "occurrences", "position", "negative", "neutral", 
                           "positive", "sentiment_score", "description", "block")

# now lets merge the sentiment scores with 'emDict' to create an emoji sentiment dictionary
emSentimentDict <- emSentiment %>% merge(emDict[,c("description", "bytes", "r.encoding")], by = "description")

# reorder for readibility of the dictionary
emSentimentDict <- emSentimentDict[ , 
                   c("description", "native", "r.encoding", "bytes", "unicode", 
                     "positive", "negative", "neutral", "sentiment_score", 
                     "occurrences", "position")]

write_feather(emSentimentDict, "emoji_dictionaries/emSentimentDict.feather")

# Emoji Sentiment #########################################################################################################

# first lets get the emojis

all_emojis <- regex_match(text_emojis, match_to = r_encoding_pattern, col_headings = c("emoji_code", "emoji_freq") )

write_feather(all_emojis, "feathers/all_emojis.feather")

# now use emSentimentDict to match the parsed emojis with their sentiment

# to ensure that we align each score to each r_encoding correctly, we need order by encoding
emSentimentDict <- emSentimentDict[order(emSentimentDict$r.encoding), ]
all_emojis <- all_emojis[order(all_emojis$emoji_code), ]

# now we need to remove the emojis from 'all_emojis' which were not included in the study and therefore
# have no no sentiment score
all_emojis <- subset(all_emojis, all_emojis$emoji_code %in% emSentimentDict$r.encoding)

# now we map sentiment to occurences:
emoji_sentiment_occurences <- subset(emSentimentDict$sentiment_score, emSentimentDict$r.encoding %in% all_emojis$emoji_code)

# likewise get the ordered descriptions
emoji_description_occurences <- subset(emSentimentDict$description, emSentimentDict$r.encoding %in% all_emojis$emoji_code)

# and the ordered icons (native) <- note: the display of the icon will depend on the OS's character encoding settings
emoji_icon_occurences <- subset(emSentimentDict$native, emSentimentDict$r.encoding %in% all_emojis$emoji_code)

# create a catalogue of emojis from the stream, which we'll enrich with additional variables
emoji_sentiment <- data.frame(description = emoji_description_occurences, 
                              icon = emoji_icon_occurences, 
                              r_encoding = all_emojis$emoji_code, 
                              sentiment_score = emoji_sentiment_occurences, 
                              freq = all_emojis$emoji_freq)

# now let's classify the emojis as positive or negative
emoji_sentiment$sentiment <- NA # create empty column

emoji_sentiment$sentiment <-ifelse(emoji_sentiment$sentiment_score > 0, "positive", "negative")

# there are some cases where the score is 0, so correct the label to neutral
emoji_sentiment[emoji_sentiment$sentiment_score == 0, "sentiment"] <- "neutral"

# finally save the feather!
write_feather(emoji_sentiment, "feathers/emoji_sentiment.feather")
