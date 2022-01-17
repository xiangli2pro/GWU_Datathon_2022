# 75th percentile of the engagement in year 2021
engage_2021 <- 0.0152

# count hashtags
hashtag_counts <- function(hashtag){
  
  v <- str_extract_all(hashtag, "[a-zA-Z0-9]{2,}")[[1]]
  return(sum(!is.na(v)))
}

# popular hashtags
hashtag_popul <- function(hashtag, hashtag_freq){
  
  v <- str_extract_all(hashtag, "[a-zA-Z0-9]{2,}")[[1]]
  hashtag_set <- hashtag_freq %>% 
    arrange(desc(n)) %>% 
    filter(n >= 100)
  
  return(sum(hashtag_set$hashtag %in% v))
}

# extract hashtags
hashtag_extract <- function(hashtag_vec){
  
  hashtag_v <- vector()
  
  for (hashtag in hashtag_vec){
    
    v <- str_extract_all(hashtag, "[a-zA-Z0-9]{2,}")[[1]]
    
    if (length(v) > 1){
      hashtag_v <- c(hashtag_v, v)
    } else if (!is.na(v)){
      hashtag_v <- c(hashtag_v, v)
    }
  }
  
  data.frame(hashtag = hashtag_v) %>% 
    group_by(hashtag) %>% 
    summarise(n = n())
}

# count mentions
mention_counts <- function(mention){
  
  v <- str_extract_all(mention, "\\d+")[[1]]
  return(sum(!is.na(v)))
}

# clean text
text_clean <- function(text){
    text %>% 
    iconv(from = "latin1", to = "ascii", sub = "byte") %>% 
    str_replace_all("<.{2}>", "") %>% 
    str_replace_all("[$~+=-]", "") %>% 
    str_replace_all("[@|#|&]\\w+", "") %>% 
    str_replace_all("http[s]?://.+", "") %>% 
    str_replace_all("\\d+\\w*\\d*", "") %>% 
    str_replace_all("[[:punct:]]", "") %>% 
    str_replace_all("\n", "") %>% 
    str_replace_all("^\\s+", "") %>%
    str_replace_all("\\s+$", "") %>% 
    str_replace_all("[ |\t]{2,}", " ") %>% 
    tolower()
}

# find emotions the words belongs 
text_emotion <- function(data){
    data %>% 
    mutate(text = text_clean(text)) %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words, by = c("word")) %>% 
    inner_join(get_sentiments("nrc"), by=c("word")) %>% 
    count(status_id, word, sentiment) %>% 
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
    group_by()
}

# find Party informaiton of the users
# library(rvest)
# df_web <- read_html("https://triagecancer.org/congressional-social-media")
# df_web_tb <- html_nodes(df_web, "table") %>%
#   html_table()
# twitter_users <- df_web_tb[[1]] %>%
#   select(Name, Party, Twitter) %>%
#   rowwise() %>% 
#   mutate(ScreenName = tolower(str_replace(Twitter, "^@", ""))) %>%
#   mutate(Name = str_replace(Name, "(\\w+),\\s(\\w+)", "\\2 \\1")) %>%
#   mutate(secName = str_extract_all(Name, "\\w+")[[1]][2]) %>%
#   select(-Twitter)
# save(twitter_users, file = "data/twitter_users.rda")
# save(twitter_users_upd, file = "data/twitter_users_upd.rda")

# match twitter names
name_matches <- function(name, twitter_users){
  
  v <- str_detect(name, twitter_users$Name)
  idx <- which(v==TRUE)
  
  if (length(idx) !=0){
    screen_name = twitter_users$ScreenName[idx[1]]
  } else {
    screen_name = NA
  }
  
  return(screen_name)
}

secName_matches <- function(name, twitter_users){
  
  v <- str_detect(name, twitter_users$secName)
  idx <- which(v==TRUE)
  
  if (length(idx) !=0){
    screen_name = twitter_users$ScreenName[idx[1]]
  } else {
    screen_name = NA
  }
  
  return(screen_name)
}

# match party with user name
party_matches <- function(data, twitter_users){
  
  # match screen_name
  x1 <- data %>% 
    select(screen_name, name) %>% 
    distinct(screen_name, .keep_all = TRUE) %>%
    mutate(screen_name = tolower(screen_name)) %>% 
    left_join(twitter_users, by = c("screen_name" = "ScreenName")) 
  
  # match name
  x2 <- x1 %>% 
    filter(is.na(Party)) %>% 
    select(-Name, -Party, -secName) %>% 
    rowwise() %>% 
    mutate(screen_name_upd = name_matches(name, twitter_users)) %>% 
    left_join(twitter_users, by = c("screen_name_upd" = "ScreenName")) %>% 
    select(-screen_name_upd)
  
  # match second name 
  x3 <- x2 %>% 
    filter(is.na(Party)) %>% 
    select(-Name, -Party, -secName) %>% 
    rowwise() %>% 
    mutate(screen_name_upd = secName_matches(name, twitter_users)) %>% 
    left_join(twitter_users, by = c("screen_name_upd" = "ScreenName")) %>% 
    select(-screen_name_upd)
  
  data_party <- bind_rows(x1 %>% filter(!is.na(Name)),
                          x2 %>% filter(!is.na(Name)),
                          x3) %>% 
    select(screen_name, Party) %>% 
    rename(party = Party) %>% 
    mutate(party = ifelse(is.na(party), "I", party))
  
  data_party
}


# match emoji to description
descr_matches <- function(string, matchto, description) {
  
  v <- str_count(string, matchto)
  matches <- which(v != 0)
  
  descr <- NA
  if (length(matches) != 0) {
    descr <- paste0(description[matches], collapse = " ")
  } 
  
  return(descr)
}
