#' topic_model.R
#'
#' @lachlandeer 
#' 
#' Estimate a topic model from the airline tweets data
#' for students to work on describing the results.
#'
#'

# --- Libraries --- #

library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(tidytext)
library(textstem)
library(stm)

# --- Load Data --- #

tweets <- read_csv("data/tweets.csv")

# --- Clean Data --- # 

tweets_cleaned <-
  tweets %>%
  mutate(
        # remove links
         text = str_remove_all(text, "https\\S*"),
         text = str_remove_all(text, "http\\S*"),
         text = str_remove_all(text, "t.co*"),
        # remove mentions
         text = str_remove_all(text, "@\\S*"),
        # remove annoying html stuff
         text = str_remove_all(text, "amp"),
         text = str_remove_all(text, "&S*"),
         text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
         text = str_replace_all(text, "<a(.*?)>", " "),
         text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
         text = str_replace_all(text, "&#[:digit:]+;", " "),
         text = str_remove_all(text, "<[^>]*>"),
        # remove numbers
          text = str_remove_all(text, "[:digit:]"),
        # remove excess whitespace
          text = str_squish(text),
          text = str_trim(text)
         ) %>%
    rowid_to_column("id")

# tweets need more than 3 words
tweets_cleaned <-
    tweets_cleaned %>% 
    filter(count_words(text) > 3)

# --- Tidy Negative Tweets --- #
tidy_neg <- 
    tweets_cleaned %>%
    filter(airline_sentiment == "negative") %>% 
    select(id, text) %>%
    unnest_tokens(word, text)

my_stop_words <- tibble(
    word = c(
        "jetblue", "southwestair", "virginamerica",
        "usairways", "americanair", "airline", "united",
        "flight", "southwest", "fly", "flighted", 
        "aa"
    ),
    lexicon = "airlines"
)

tidy_neg <- 
    tidy_neg %>%
    mutate(word = lemmatize_words(word)) %>%
    anti_join(stop_words) %>%
    anti_join(my_stop_words)
    
# throw away uncommon words? < 25
word_counts_agg <- 
    tidy_neg %>%
    group_by(word) %>%
    count()  %>%
    filter(n > 25)

tidy_neg <- 
    tidy_neg %>%
    filter(word %in% word_counts_agg$word)

# doc word counts
word_counts <- 
    tidy_neg %>%
    count(id, word) %>%
    ungroup()

# cast to matrix
tweets_dtm <- 
    word_counts %>%
    cast_sparse(id, word, n)

# --- Topic Model --- #
library(stm)

tweets_lda <- 
    stm(tweets_dtm, 
        K = 8, 
        seed = 123456789
    )

# --- Save it ---!
library(rlist)

list.save(tweets_lda, "data/complaints_topics.Rds")

# --- Assign labels to tweets --- #

tweets_gamma <- 
    tidy(tweets_lda, 
         matrix = "gamma",
         document_names = rownames(tweets_dtm)
    ) %>%
    mutate(document = as.integer(document))

df_topics <-
    tweets_cleaned %>%
    filter(airline_sentiment == "negative") %>% 
    select(id, text, airline) %>%
    inner_join(tweets_gamma, by = c("id" = "document")) %>%
    group_by(id) %>%
    slice_max(gamma)

write_csv(df_topics, "data/complaint_tweets.csv")
