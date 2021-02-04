#' ---
#' title: 'Task 05: Prediction Model'
#' author: "Mark Blackmore"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---

#' Setup
setwd("E:/cursos/online courses/Data Science Program/Data science capstone/prediction_model_2")
go <- Sys.time()
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

#' Load Training Data
bi_words <- readRDS("./clean_repos/2version/bi_words_fast.rds")
tri_words  <- readRDS("./clean_repos/2version/tri_words_fast.rds")
quad_words <- readRDS("./clean_repos/2version/quad_words_fast.rds")
quint_words <- readRDS("./clean_repos/2version/quint_words_fast.rds")
sext_words <- readRDS("./clean_repos/2version/sext_words_fast.rds")

#' Create Ngram Matching Functions
bigram <- function(input_words){
  num <- length(input_words)
  filter(bi_words, 
         word1==input_words[num]) %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 2)) %>%
    as.character() -> out
  ifelse(out =="character(0)", "?", return(out))
}

trigram <- function(input_words){
  num <- length(input_words)
  filter(tri_words, 
         word1==input_words[num-1], 
         word2==input_words[num])  %>% 
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 3)) %>%
    as.character() -> out
  ifelse(out=="character(0)", bigram(input_words), return(out))
}

quadgram <- function(input_words){
  num <- length(input_words)
  filter(quad_words, 
         word1==input_words[num-2], 
         word2==input_words[num-1], 
         word3==input_words[num])  %>% #Para encontrar la predicción, primero filtra por las tres palabras anteriores
    top_n(1, n) %>%
    filter(row_number() == 1L) %>%
    select(num_range("word", 4)) %>%
    as.character() -> out
  ifelse(out=="character(0)", trigram(input_words), return(out))
}

#' Create User Input and Data Cleaning Function; Calls the matching functions
ngrams <- function(input){
  # Create a dataframe
  input <- tibble(text = input)
  # Clean the Inpput
  replace_reg <- "[^[:alpha:][:space:]]*" ##Quite todo excepto letras, espacios y números
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  # Find word count, separate words, lower case
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  input_words <- tolower(input_words)
  # Call the matching functions
  out <- ifelse(input_count == 1, bigram(input_words), 
                ifelse (input_count == 2, trigram(input_words), quadgram(input_words)))
  # Output
  return(out)
}

#' User Input and Program Ouput
input <- "In case of a"
ngrams(input)

ngrams("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd")

#' Time it
stop <- Sys.time()
(how_long <- stop - go)

#' -------------
#'  
#' ## Session info
sessionInfo()


#wth is algorithm doing? 
  input <- "go on a romantic"
  # Create a dataframe
  input <- tibble(text = input)
  # Clean the Inpput
  replace_reg <- "[^[:alpha:][:space:]]*" ##Quite todo excepto letras, espacios y números
  input <- input %>%
    mutate(text = str_replace_all(text, replace_reg, ""))
  # Find word count, separate words, lower case
  input_count <- str_count(input, boundary("word"))
  input_words <- unlist(str_split(input, boundary("word")))
  input_words <- tolower(input_words)
  # Call the matching functions
  out <- ifelse(input_count == 1, bigram(input_words), 
                ifelse (input_count == 2, trigram(input_words), quadgram(input_words)))
 
  
#next, input_words goes to one of the functions 
    num <- length(input_words)
    x <- filter(bi_words, 
                word1==input_words[num]) %>% #Para encontrar la predicción, primero filtra por las tres palabras anteriores
      top_n(1, n) %>%
      filter(row_number() == 1L) %>%
      select(num_range("word", 2)) %>%
      as.character() -> out
    ifelse(out=="character(0)", trigram(input_words), return(out))
  

