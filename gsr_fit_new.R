# final code for gsr
# using STM
# bigrams, lemma, tidytext to tokenise

library(tidyverse) # loads the tidyverse suite
library(magrittr) # for the pipe functionality
library(tidytext) # for the power of the tidytext
library(quanteda) # to convert things into dfm
library(stm) # the main topic modelling package
library(textstem) # for lemmatising

# modelling initialisation
ngram <- 2
stemlem <- "l"
brand_replace <- TRUE

# load GSR from the working directory
gsr <- read_csv("gsr_new_new.csv", col_types = cols(weekending = col_date(format = "%Y-%m-%d")))

# get some columns from GSR and set up as a new tibble
gsr_new <- tibble(gsr$review_id, gsr$weekending, gsr$trading_title_code, gsr$overall_rating, gsr$customer_service_sat, gsr$recommend, gsr$review_text)

# cleanup
rm(gsr)

# rename columns - didnt work without doing this, maybe it didnt like $ in column name?
colnames(gsr_new) <- c("review_id", "weekending", "brand", "rating", "csat", "recommend", "review_text")

# next bit commented out as we're not using csat and recommend
# insert bit that looks at sparseness of csat and recommend
# remove those reviews where either csat or recommend are blank
# gsr_reviews %<>% filter(!(is.na(csat))) %>% 
#  filter(!(is.na(recommend)))

# subset for complete weeks
gsr_new %<>% filter(weekending >= '2017-10-13' & weekending <= '2017-11-17')

# fork all cleaning onto review_scrub here
# remove all non-UTF8 characters
gsr_new$review_scrub <- gsr_new$review_text %>% 
  iconv("UTF-8", "UTF-8", sub='')

# lower case
gsr_new$review_scrub %<>% tolower()

# i've created a csv file with our brand names
# replace all instances of brand names with the word brand
if(brand_replace == TRUE) {
  brands <- read_csv("brands.csv")
  gsr_new$review_scrub <- brands$brand %>% 
    paste(collapse = "|") %>% 
    gsub("brand", gsr_new$review_scrub)
  # clean up
  rm(brands)
}

# expose the negations by swapping "n't" with " not"
gsr_new$review_scrub <- gsub("n't", " not", gsr_new$review_scrub)

# remove all non-alphan here but keep apostrophes
gsr_new$review_scrub <- gsub("[^[:alpha:] ']", "", gsr_new$review_scrub)

# change some variable types over
gsr_new$brand %<>% as.factor()
gsr_new$review_id %<>% as.character()
gsr_new$recommend <- ifelse(gsr_new$recommend == "Yes",1,0) %>% 
  as.logical()

# lemmatise the review_scrub
if(stemlem == "l") {
  gsr_new$review_scrub <- lemmatize_strings(gsr_new$review_scrub)
}

if(stemlem == "s") {
  # insert stemming code
}
# let's get out the stop words we want. read in the r_stop file
# then split those stop words into unigram and bigram lists
r_stop <- read_csv("r_stop.csv")
uni_stop <- r_stop %>% filter(ngram == 1)
bi_stop <- r_stop %>% filter(ngram == 2)

# clean up
rm(r_stop)

# let's do a unigram tokenise, remove stopwords
gsr_new_uni <- gsr_new %>%
  unnest_tokens(word, review_scrub) %>%
  anti_join(uni_stop) %>%
  count(review_id, word, sort = TRUE) %>%
  ungroup()

# cleanup
rm(uni_stop)

# now we need to create a lookup table of multiple (3+) characters to delete from the word list
# the annoying thing is 3e, 4e and 5e are actually a shoe width size or i'd do this in a filter above

gsr_new_multi <- gsr_new_uni %>%
  select(word) %>%
  filter(str_detect(word, "([A-Za-z])\\1{2,}")) %>%
  filter(!(word=="eee")) %>%
  filter(!(word=="eeee")) %>%
  filter(!(word=="eeeee"))

# ok now that's done anti-join it back onto gsr_uni to remove repeat character tokens
gsr_new_uni %<>% anti_join(gsr_new_multi)

# cleanup
rm(gsr_new_multi)

# remove all entries which are under 3 characters long
gsr_new_uni %<>% filter(nchar(word) >= 3)

# based on http://norvig.com/mayzner.html remove all entries over 12 characters long (2.5% of English)
gsr_new_uni %<>% filter(nchar(word) <= 12)

# remove all entries where word is NA, i assume this is due to the anti_join.
gsr_new_uni %<>% filter(!is.na(word))

# now let's put that back together again
gsr_new_uni_stop <- gsr_new_uni %>% 
  group_by(review_id) %>% 
  mutate(review_scrub = paste(word, collapse = " "))

# let's tidy up the data frame, removing nulls and dupes
gsr_new_uni_stop$word <- NULL
gsr_new_uni_stop$n <- NULL
gsr_new_uni_stop <- unique(gsr_new_uni_stop)
gsr_new_tokens <- gsr_new_uni_stop

# cleanup
# rm(gsr_uni)

if(ngram > 1) {
  
  # now bigram tokenise the stop word removed gsr, apply stop-word bigrams
  gsr_new_ngrams <- gsr_new_uni_stop %>% 
    unnest_tokens(word, review_scrub, token = "ngrams", n = ngram) %>%
    anti_join(bi_stop, by = c("word" = "word")) %>% 
    count(review_id, word) %>%
    ungroup()
  
  # remove any that have now been na'ed
  gsr_new_ngrams %<>% filter(!is.na(word))
  gsr_new_ngrams %<>% filter(!is.na(review_id))
  
  # sort out the review_id
  # gsr_bigrams$review_id %<>% as.character()
  
  # now lets truncate gsr_reviews based on what's left in gsr_bigrams
  # this is because we need to use the gsr_final as our meta_data in the stm
  gsr_new_tokens <- gsr_new_ngrams
} 
gsr_new_final <- gsr_new %>% 
  semi_join(gsr_new_tokens, by = c("review_id" = "review_id"))
gsr_new_final %<>% filter(!is.na(review_id))

# cleanup
rm(bi_stop)
rm(gsr_new_uni_stop)
rm(gsr_new_uni)
rm(gsr_new_ngrams)

# you should now be left with three tibbles
# gsr_reviews which is the full original data plus a processed and lemmatised var
# gsr_tokens which is the tokenisation at the level requested
# gsr_final which is the filtered gsr_reviews for only those review_id where 

# add all variables to a list and save this off, jic
# setup <- list(ngram, stemlem, brand_replace)
#names(setup) <- c("ngram","stemlem","brand_replace")

#initial_vars <- list(setup, gsr_reviews, gsr_tokens, gsr_final)
#names(initial_vars) <- c("setup", "gsr_reviews","gsr_tokens","gsr_final")

# turn the tidy df into a quatenda::dfm
gsr_new_dfm <- gsr_new_tokens %>% 
  cast_dfm(review_id, word, n)

# now import what we need from the original model
# you'll need to replace the file bit with whichever model you've done

load(file = "gsr_stm_n2_k0.rda")
gsr_dfm <- gsr_stm_n2_k0$initial_vars$gsr_tokens %>% cast_dfm(review_id, word, n)
gsr_final <- gsr_stm_n2_k0$initial_vars$gsr_final
gsr_stm_model <- gsr_stm_n2_k0$gsr_stm_model

# use dfm from the model to align the new dfm (THIS IS ANNOYING)
gsr_new_dfm_fit <- dfm_select(gsr_new_dfm, gsr_dfm, valuetype = "fixed", verbose = TRUE)

# convert to stm
gsr_new_stm <- quanteda::convert(gsr_new_dfm_fit, to = c("stm"), docvars = gsr_new_final)

# tidy up
rm(gsr_new_dfm)
rm(gsr_new_tokens)
rm(setup)

# score up. this can take quite a while
new_score <- fitNewDocuments(model = gsr_stm_model, 
                             documents = gsr_new_stm$documents, 
                             newData = gsr_new_stm$meta, 
                             origData = gsr_final, 
                             prevalence = ~ rating,
                             prevalencePrior = "Covariate")


# this then needs some processsing unfortunately

k <- gsr_stm_model$settings$dim$K
scored <- make.dt(new_score, meta = gsr_new_stm$meta)
topics_new <- colnames(scored[,2:(k+1)])[max.col(scored[,2:(k+1)], ties.method = 'first')] %>%
  as.tibble()
gsr_new_topics <- cbind(scored[,(k+2):length(scored)], topics_new)
colnames(gsr_new_topics) <- c("review_id","weekending","brand","rating","csat","recommend","review_text","review_scrub","topic")

# the below tbl is the new data plus the topic
gsr_new_topics$topic <- gsub("Topic", "", gsr_new_topics$topic) %>%
  as.integer()

# distribution over time
topic_new_dist <- gsr_new_topics %>% 
  group_by(weekending, topic) %>% 
  summarise(n = n())

