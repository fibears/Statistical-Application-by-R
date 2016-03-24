library(tm)
library(wordcloud)
library(reshape2)
library(plyr)
library(dplyr)
library(stringr)
library(pipeR)

# Load Data
pearl <- read.csv('Exercise/Pearl_market_survey.csv',stringsAsFactors = FALSE)

# Extract Objective Data
Question22 <- pearl[-1,78:80] %>% mutate(id = 1:nrow(.)) %>% 
    melt(id.vars="id") %>% select(value) %>% filter(value != "")
Question23 <- pearl[-1,81:83] %>% mutate(id = 1:nrow(.)) %>% 
    melt(id.vars="id") %>% select(value) %>% filter(value != "")

# Create StopWords
StopWords <- stopwords() %>% .[!str_detect(., pattern = "n't|no")] %>% c(., c("want","online"))

# Create corpus
TextData1 <- VCorpus(DataframeSource(Question22))
TextData2 <- VCorpus(DataframeSource(Question23))

# Data Cleaning
Corpus1 <- tm_map(TextData1,tolower) %>% tm_map(removeWords, StopWords) %>% 
    tm_map(removePunctuation) %>%
    tm_map(str_trim) %>% 
    do.call(str_c, .) %>% as.data.frame(stringAsFactors=FALSE) %>% 
    apply(., 1, function(x) str_replace(x, "[nt|not|no][\\s]{1,2}", "not")) %>% 
    as.data.frame(stringAsFactors=FALSE) %>% 
    DataframeSource() %>% VCorpus()
Corpus2 <- tm_map(TextData2, tolower) %>% tm_map(removeWords, StopWords) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(str_trim) %>% do.call(str_c, .) %>% as.data.frame(stringsAsFactors=FALSE) %>% 
    apply(1, function(x) str_replace(x, "see[\\s]{1,3}", "see")) %>% 
    as.data.frame(stringsAsFactors=FALSE) %>% 
    apply(1, function(x) str_replace(x, "[nt|not|no][\\s]{1,2}", "not")) %>% 
    as.data.frame(stringsAsFactors=FALSE) %>% 
    DataframeSource() %>% VCorpus()

# Create DocumentTermMatrix
dtm1 <- DocumentTermMatrix(Corpus1)
dtm2 <- DocumentTermMatrix(Corpus2)

# Create wordcloud plot
## Compute wordFreq
Question1WordFreq <- data.frame(term = colnames(dtm1), freq = apply(dtm1, 2, sum))  
Question2WordFreq <- data.frame(term = colnames(dtm2), freq = apply(dtm2, 2, sum))

## wordcloud plot
colors <- colorRampPalette(brewer.pal(8,"Set1"))(length(Question1WordFreq$freq))
### Question22 plot
wordcloud(Question1WordFreq$term,Question1WordFreq$freq, scale=c(4,0.2), 
          random.order=FALSE, colors=colors)
### Question23 plot
wordcloud(Question2WordFreq$term,Question2WordFreq$freq, scale=c(4,0.2), 
          random.order=FALSE, colors=colors)




