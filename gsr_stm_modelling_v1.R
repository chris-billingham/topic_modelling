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
gsr <- read_csv("gsr_new.csv", col_types = cols(weekending = col_date(format = "%Y-%m-%d")))

# get some columns from GSR and set up as a new tibble
gsr_reviews <- tibble(gsr$review_id, gsr$weekending, gsr$trading_title_code, gsr$overall_rating, gsr$customer_service_sat, gsr$recommend, gsr$review_text)

# cleanup
rm(gsr)

# rename columns - didnt work without doing this, maybe it didnt like $ in column name?
colnames(gsr_reviews) <- c("review_id", "weekending", "brand", "rating", "csat", "recommend", "review_text")

# next bit commented out as we're not using csat and recommend
# insert bit that looks at sparseness of csat and recommend
# remove those reviews where either csat or recommend are blank
# gsr_reviews %<>% filter(!(is.na(csat))) %>% 
#  filter(!(is.na(recommend)))

# subset for complete weeks
gsr_reviews %<>% filter(weekending >= '2015-03-04' & weekending <= '2017-10-06')

# fork all cleaning onto review_scrub here
# remove all non-UTF8 characters
gsr_reviews$review_scrub <- gsr_reviews$review_text %>% 
  iconv("UTF-8", "UTF-8", sub='')

# lower case
gsr_reviews$review_scrub %<>% tolower()

# i've created a csv file with our brand names
# replace all instances of brand names with the word brand
if(brand_replace == TRUE) {
  brands <- read_csv("brands.csv")
gsr_reviews$review_scrub <- brands$brand %>% 
  paste(collapse = "|") %>% 
  gsub("brand", gsr_reviews$review_scrub)
# clean up
rm(brands)
}

# expose the negations by swapping "n't" with " not"
gsr_reviews$review_scrub <- gsub("n't", " not", gsr_reviews$review_scrub)

# remove all non-alphan here but keep apostrophes
gsr_reviews$review_scrub <- gsub("[^[:alpha:] ']", "", gsr_reviews$review_scrub)

# change some variable types over
gsr_reviews$brand %<>% as.factor()
gsr_reviews$review_id %<>% as.character()
gsr_reviews$recommend <- ifelse(gsr_reviews$recommend == "Yes",1,0) %>% 
  as.logical()

# lemmatise the review_scrub
if(stemlem == "l") {
  gsr_reviews$review_scrub <- lemmatize_strings(gsr_reviews$review_scrub)
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
gsr_uni <- gsr_reviews %>%
  unnest_tokens(word, review_scrub) %>%
  anti_join(uni_stop) %>%
  count(review_id, word, sort = TRUE) %>%
  ungroup()

# cleanup
rm(uni_stop)

# now we need to create a lookup table of multiple (3+) characters to delete from the word list
# the annoying thing is 3e, 4e and 5e are actually a shoe width size or i'd do this in a filter above

gsr_multi <- gsr_uni %>%
  select(word) %>%
  filter(str_detect(word, "([A-Za-z])\\1{2,}")) %>%
  filter(!(word=="eee")) %>%
  filter(!(word=="eeee")) %>%
  filter(!(word=="eeeee"))

# ok now that's done anti-join it back onto gsr_uni to remove repeat character tokens
gsr_uni %<>% anti_join(gsr_multi)

# cleanup
rm(gsr_multi)

# remove all entries which are under 3 characters long
gsr_uni %<>% filter(nchar(word) >= 3)

# based on http://norvig.com/mayzner.html remove all entries over 12 characters long (2.5% of English)
gsr_uni %<>% filter(nchar(word) <= 12)

# remove all entries where word is NA, i assume this is due to the anti_join.
gsr_uni %<>% filter(!is.na(word))

# now let's put that back together again
gsr_uni_stop <- gsr_uni %>% 
  group_by(review_id) %>% 
  mutate(review_scrub = paste(word, collapse = " "))

# let's tidy up the data frame, removing nulls and dupes
gsr_uni_stop$word <- NULL
gsr_uni_stop$n <- NULL
gsr_uni_stop <- unique(gsr_uni_stop)
gsr_tokens <- gsr_uni

# cleanup
# rm(gsr_uni)

if(ngram > 1) {

# now bigram tokenise the stop word removed gsr, apply stop-word bigrams
gsr_ngrams <- gsr_uni_stop %>% 
  unnest_tokens(word, review_scrub, token = "ngrams", n = ngram) %>%
  anti_join(bi_stop, by = c("word" = "word")) %>% 
  count(review_id, word) %>%
  ungroup()

# remove any that have now been na'ed
gsr_ngrams %<>% filter(!is.na(word))
gsr_ngrams %<>% filter(!is.na(review_id))

# sort out the review_id
# gsr_bigrams$review_id %<>% as.character()

# now lets truncate gsr_reviews based on what's left in gsr_bigrams
# this is because we need to use the gsr_final as our meta_data in the stm
gsr_tokens <- gsr_ngrams
} 
gsr_final <- gsr_reviews %>% 
  semi_join(gsr_tokens, by = c("review_id" = "review_id"))
gsr_final %<>% filter(!is.na(review_id))

# cleanup
rm(bi_stop)
rm(gsr_uni_stop)
rm(gsr_uni)
rm(gsr_ngrams)

# you should now be left with three tibbles
# gsr_reviews which is the full original data plus a processed and lemmatised var
# gsr_tokens which is the tokenisation at the level requested
# gsr_final which is the filtered gsr_reviews for only those review_id where 

# add all variables to a list and save this off, jic
setup <- list(ngram, stemlem, brand_replace)
names(setup) <- c("ngram","stemlem","brand_replace")

initial_vars <- list(setup, gsr_reviews, gsr_tokens, gsr_final)
names(initial_vars) <- c("setup", "gsr_reviews","gsr_tokens","gsr_final")

# turn the tidy df into a quatenda::dfm
gsr_dfm <- gsr_tokens %>% 
  cast_dfm(review_id, word, n)

# convert to stm
gsr_stm <- quanteda::convert(gsr_dfm, to = c("stm"), docvars = gsr_final)

# tidy up
rm(gsr_dfm)
rm(gsr_final)
rm(gsr_tokens)
rm(setup)

# now run the model
gsr_stm_model <- stm(gsr_stm$documents,
                     gsr_stm$vocab,
                     K=0, 
                     prevalence = ~ rating + brand,
                     max.em.its = 500,
                     data = gsr_stm$meta,
                     reportevery = 1,
                     seed = 1979)

# add the model to the initial vars and save of to working directory
gsr_stm_n2_k0 <- list(initial_vars, gsr_stm, gsr_stm_model)
names(gsr_stm_n2_k0) <- c("initial_vars", "gsr_stm", "gsr_stm_model")
save(gsr_stm_n2_k0, file = "gsr_stm_n2_k0.rda")
