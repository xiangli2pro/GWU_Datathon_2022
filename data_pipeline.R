## data imported

## load customized functions
source("functions.R")

## load packages
library(data.table)
library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud)
library(widyr)
library(reshape2)
library(lubridate)
library(igraph)
library(ggraph)

## select potential variables
var_names <- c("favorite_count", "retweet_count", "followers_count", "statuses_count", # relate to outcome
               "reply_to_status_id", "is_quote", "is_retweet", # relate to independence of the tweet
               "user_id", "screen_name", "friends_count", "listed_count", "favourites_count", # relate user profiles
               "status_id", "created_at", "text", "hashtags", 
               "urls_url", "media_url", "mentions_user_id", # relate to independent tweet content
               "quoted_status_id", "quoted_screen_name", "quoted_text", "quoted_created_at",
               "quoted_favorite_count", "quoted_retweet_count", "quoted_followers_count", 
               "quoted_friends_count", "quoted_statuses_count",  # relate to quoted tweet
               "retweet_status_id", "retweet_screen_name", "retweet_text", "retweet_created_at",
               "retweet_favorite_count", "retweet_retweet_count", "retweet_followers_count",
               "retweet_friends_count", "retweet_statuses_count" # relate to retweet tweet
               )


## hashtag frequency
hashtag_freq <- hashtag_extract(data$hashtags)

## mutate numeric variables
data_num <- data %>% 
  select(var_names) %>% 
  distinct() %>% 
  mutate(engage_rate = ((favorite_count+retweet_count)/followers_count +
           (favorite_count+retweet_count)/statuses_count)/2,
         engageQuoted_rate = ((quoted_favorite_count+quoted_retweet_count)/quoted_followers_count +
                                (quoted_favorite_count+quoted_retweet_count)/quoted_statuses_count)/2,
         engageRetweet_rate = ((retweet_favorite_count+retweet_retweet_count)/retweet_followers_count +
                                 (retweet_favorite_count+retweet_retweet_count)/retweet_statuses_count)/2) %>% 
  mutate(engage_active = ifelse(engage_rate > engage_2021, 1, 0),
         engageQuoted_active = ifelse(engageQuoted_rate > engage_2021, 1, 0),
         engageRetweet_active = ifelse(engageRetweet_rate > engage_2021, 1, 0)) %>% 
  mutate(listed_level = case_when(
    listed_count <= quantile(data$listed_count, probs = 0.25) ~ 1,
    listed_count > quantile(data$listed_count, probs = 0.25) & listed_count <= quantile(data$listed_count, probs = 0.50) ~ 2,
    listed_count > quantile(data$listed_count, probs = 0.50) & listed_count <= quantile(data$listed_count, probs = 0.75) ~ 3,
    listed_count > quantile(data$listed_count, probs = 0.75) & listed_count <= quantile(data$listed_count, probs = 1) ~ 4)) %>% 
  mutate(is_reply = ifelse(is.na(reply_to_status_id), 0, 1),
         is_quote = is_quote*1,
         is_retweet = is_retweet*1,
         has_url = ifelse(is.na(urls_url), 0, 1),
         has_media = ifelse(is.na(media_url), 0, 1),
         favor_perFriend = favourites_count/friends_count) %>%
  rowwise() %>% 
  mutate(is_independent = ifelse(is_reply+is_quote+is_retweet >= 1, 0, 1),
         hashtag_num = hashtag_counts(hashtags),
         mention_num = mention_counts(mentions_user_id)) %>% 
  select(-reply_to_status_id, -urls_url, -media_url, -listed_count,
         -favorite_count, -retweet_count, -followers_count, -statuses_count,
         -quoted_favorite_count, -quoted_retweet_count, -quoted_followers_count,
         -retweet_favorite_count, -retweet_retweet_count, -retweet_followers_count) 

## mutate_text variables
## tokenize
data_tweet <- data %>% 
  select(status_id, text) %>% 
  text_emotion()

data_quoted <- data %>% 
  select(status_id, quoted_text) %>% 
  rename(text = quoted_text) %>% 
  text_emotion()

data_textLen <- data %>% 
  select(status_id, text) %>%
  rowwise() %>% 
  mutate(text = text_clean(text)) %>% 
  mutate(tweetLength = length(unlist(str_split(text, " ")))) %>% 
  ungroup() %>% 
  select(-text) %>% 
  left_join(data %>% 
              select(status_id, quoted_text) %>%
              rowwise() %>% 
              mutate(quoted_text = text_clean(quoted_text)) %>% 
              mutate(quotedLength = length(unlist(str_split(quoted_text, " ")))) %>% 
              ungroup() %>% 
              select(-quoted_text), by = c("status_id")) %>% 
  mutate(across(everything(), ~replace_na(.x, 0)))
# save(data_textLen, file = "data/data_textLen.rda")
  
data_emotion <- (data_tweet %>% 
                   select(-word) %>% 
                   group_by(status_id) %>%
                   summarise(across(everything(), ~sum(.x))) %>% 
                   ungroup() %>% 
                   rename_with(~ paste0("tweet_", .x), -c(status_id))) %>% 
  left_join((data_quoted %>% 
               select(-word) %>% 
               group_by(status_id) %>%
               summarise(across(everything(), ~sum(.x))) %>% 
               ungroup() %>% 
               rename_with(~ paste0("quoted_", .x), -c(status_id))), by = "status_id") %>% 
  pivot_longer(-c("status_id"),
               names_to = c(".value", "emotion"),
               names_pattern = "(.+)_(.+)") %>% 
  right_join(data_textLen, by = c("status_id")) %>% 
  mutate(across(c(tweet:quoted), ~replace_na(.x, 0))) %>% 
  rowwise() %>% 
  mutate(emotion_num = sum(tweet/tweetLength, quoted/quotedLength)) %>%
  ungroup() %>% 
  select(status_id, emotion, emotion_num) %>% ###!!! has to select the variables specifically
  pivot_wider(names_from = emotion, values_from = emotion_num) %>% 
  select(status_id:disgust) %>% 
  mutate(across(everything(), ~replace_na(.x, 0)))

## add party 
load("data/twitter_users_upd.rda")

data_party <- data %>% 
  select(screen_name, name) %>% 
  party_matches(twitter_users_upd)

## combine the final variables for modelling
data_model <- data_num %>% 
  select(status_id, screen_name, 
         engage_rate, engage_active, engageQuoted_rate, engageQuoted_active,
         engageRetweet_rate, engageRetweet_active, listed_level,
         has_url, has_media, favor_perFriend, is_independent, hashtag_num,
         mention_num) %>% 
  left_join(data_textLen, by = c("status_id")) %>% 
  left_join(data_emotion, by = c("status_id")) %>% 
  mutate(screen_name = tolower(screen_name)) %>% 
  left_join(data_party, by = c("screen_name")) %>% 
  select(-status_id, -screen_name, -engage_rate, -quotedLength, 
         -engageQuoted_rate, -engageRetweet_rate) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  mutate(party = factor(party, levels = c("I", "R", "D")))



