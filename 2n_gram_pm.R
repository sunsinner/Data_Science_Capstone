#' ---
#' title: 'Task 04A: Fast Ngram Files'
#' author: "Mark Blackmore"
#' date: "`r format(Sys.Date())`"
#' output: 
#'   github_document:
#'     toc: true
#' ---

#+ setup, echo=FALSE
suppressPackageStartupMessages({
  library(tidytext)
  library(tidyverse)
  library(stringr)
  library(knitr)
})

#' ## Load the Data
#+ DataLoading

setwd("E:/cursos/online courses/Data Science Program/Data science capstone/prediction_model_2")

#' English Repository Files
blogs_file   <- "E:/cursos/online courses/Data Science Program/Data science capstone/final/en_US/en_US.blogs.txt"
news_file    <- "E:/cursos/online courses/Data Science Program/Data science capstone/final/en_US/en_US.news.txt"
twitter_file <- "E:/cursos/online courses/Data Science Program/Data science capstone/final/en_US/en_US.twitter.txt"


#' Read the data files 
blogs   <- readLines(blogs_file, skipNul = TRUE)
news    <- readLines(news_file,  skipNul = TRUE)
twitter <- readLines(twitter_file, skipNul = TRUE)

#' Create dataframes
blogs   <- tibble(text = blogs)
news    <- tibble(text = news)
twitter <- tibble(text = twitter)

#' ## Sample the data
#+ DataSampling
set.seed(1001)
sample_pct <- 0.10

blogs_sample <- blogs %>%
  sample_n(., nrow(blogs)*sample_pct)
news_sample <- news %>%
  sample_n(., nrow(news)*sample_pct)
twitter_sample <- twitter %>%
  sample_n(., nrow(twitter)*sample_pct)

#' Create tidy repository
repo_sample <- bind_rows(mutate(blogs_sample, source = "blogs"),
                         mutate(news_sample,  source = "news"),
                         mutate(twitter_sample, source = "twitter")) 
repo_sample$source <- as.factor(repo_sample$source)

#' Clean up
rm(list = c("blogs", "blogs_file", "blogs_sample","news", "news_file",     
            "news_sample", "sample_pct", "twitter","twitter_file", 
            "twitter_sample"))

#' ## Clean the sample data
#' Create filters: non-alphanumeric's, url's, repeated letters(+3x)
#+ Data Cleaning
replace_reg <- "[^[:alpha:][:space:]]*"
replace_url <- "http[^[:space:]]*"
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"  

#' Clean the sample. Cleaning is separted from tidying so `unnest_tokens` function can be used for words,
#' and ngrams.
clean_sample <-  repo_sample %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(text = str_replace_all(text, replace_url, "")) %>%
  mutate(text = str_replace_all(text, replace_aaa, "")) %>% 
  mutate(text = iconv(text, "ASCII//TRANSLIT"))

rm(list = c("repo_sample"))

# swear and stop words
data("stop_words")
swear_words <- read_delim("E:/cursos/online courses/Data Science Program/Data science capstone/final/en_US/swearWords.csv", delim = ",", col_names = FALSE)
swear_words <- as.data.frame(t(swear_words), row.names = "")
swear_words <- unnest_tokens(swear_words, word, V1)

#' ## Create all n-grams
#+ Ngrams 

#' unigrams
#' tidy_repo <- clean_sample %>%
#' unnest_tokens(word,text) %>%
#' anti_join(swear_words) %>%
#' anti_join(stop_words)

#' Bigrams
bigram_repo <- clean_sample  %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

#' Trigrams
trigram_repo <- clean_sample  %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

#' Quadgrams
quadgram_repo <- clean_sample  %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4)

#' Quintgrams
quintgram_repo <- clean_sample  %>%
  unnest_tokens(quintgram, text, token = "ngrams", n = 5)

#' Sextgrams
sextgram_repo <- clean_sample  %>%
  unnest_tokens(sextgram, text, token = "ngrams", n = 6)


#' ## Reduce n-grams files
#+ ReduceNgrams 
#' Bigrams
bigram_cover <- bigram_repo %>%
  count(bigram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("bigram_repo"))

#' Trigrams
trigram_cover <- trigram_repo %>%
  count(trigram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("trigram_repo"))

#' Quadgrams
quadgram_cover <- quadgram_repo %>%
  count(quadgram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("quadgram_repo"))

#' Quintgrams
quintgram_cover <- quintgram_repo %>%
  count(quintgram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("quintgram_repo"))

#' Sextgrams
sextgram_cover <- sextgram_repo %>%
  count(sextgram) %>%  
  filter(n > 10) %>%
  arrange(desc(n))  
rm(list = c("sextgram_repo"))

#' ## What does the distribution on ngrams look like?
#+ DistyPlot
disty <- data_frame(ngram = c(rep("bigrams",  nrow(bigram_cover)),
                             rep("trigrams",  nrow(trigram_cover)),
                             rep("quadgrams", nrow(quadgram_cover)),
                             rep("quintgrams", nrow(quintgram_cover)),
                             rep("sextgrams",  nrow(sextgram_cover))),
                    number = c(bigram_cover$n,  trigram_cover$n, 
                              quadgram_cover$n, quintgram_cover$n,
                              sextgram_cover$n))
disty
disty$ngram <- as.factor(disty$ngram)
ggplot(data = disty, aes(y = number, x = reorder(ngram, -number))) +
  geom_boxplot() + scale_y_log10() +
  xlab("ngram")
ggsave("./Ngrams.png")

sextgram_cover %>%
  na.omit() %>%
  top_n(15, n) %>%
  mutate(sextgram = reorder(sextgram, n)) %>%
  ggplot(aes(sextgram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Sextgrams")
ggsave("./Ngrams/sextgrams.png")

quintgram_cover %>%
  na.omit() %>%
  top_n(15, n) %>%
  mutate(quintgram = reorder(quintgram, n)) %>%
  ggplot(aes(quintgram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Quintgrams")
ggsave("./Ngrams/quintgrams.png")

quadgram_cover %>%
  na.omit() %>%
  top_n(15, n) %>%
  mutate(quadgram = reorder(quadgram, n)) %>%
  ggplot(aes(quadgram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Quadgrams")
ggsave("./Ngrams/quadgrams.png")

trigram_cover %>%
  na.omit() %>%
  top_n(15, n) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Trigrams")
ggsave("./Ngrams/trigrams.png")

bigram_cover %>%
  na.omit() %>%
  top_n(15, n) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Bigrams")
ggsave("./Ngrams/bigrams.png")

#' ## Separate words
#+ NgramWords 
bi_words <- bigram_cover %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bi_words

tri_words <- trigram_cover %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ")
tri_words

quad_words <- quadgram_cover %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ")
quad_words

quint_words <- quintgram_cover %>%
  separate(quintgram, c("word1", "word2", "word3", "word4", "word5"), sep = " ")
quint_words

sext_words <- sextgram_cover %>%
  separate(sextgram, c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")
sext_words


#' Save data for the Shiny App
saveRDS(bi_words, "./clean_repos/2version/bi_words_fast.rds")
saveRDS(tri_words, "./clean_repos/2version/tri_words_fast.rds")
saveRDS(quad_words,"./clean_repos/2version/quad_words_fast.rds")
saveRDS(quint_words,"./clean_repos/2version/quint_words_fast.rds")
saveRDS(sext_words,"./clean_repos/2version/sext_words_fast.rds")
#' 
#' -------------
#'  
#' ## Session info
sessionInfo()
