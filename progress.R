#### Topic: what emotion makes people engage with politician's twitter? happy, or angry?

# https://rpubs.com/yevonnael/intro-text-analysis
# https://towardsdatascience.com/text-mining-with-r-gathering-and-cleaning-data-8f8b0d65e67c
# https://resulumit.com/teaching/twtr_workshop.html#158
# https://www.tidytextmining.com/sentiment.html
# https://uc-r.github.io/sentiment_analysis
# https://cran.rstudio.com/web/packages/SentimentAnalysis/vignettes/SentimentAnalysis.html
# https://stringr.tidyverse.org/articles/regular-expressions.html#matching-multiple-characters



#### This data comes from the 512 senators and congressman.

#### 1. Selection
# select lang=en, year = 2021, month = 1
# reason: majority is English, the time between post and retweet/quote is less than 2 days.

#### 2. Clean data
# a. remove repeat/singluar/missing variables
# b. 

#### 3. Construct features
# engagement outcome Y =  (#favorite + #retweet + #replies)/3 /# followers (of full data)