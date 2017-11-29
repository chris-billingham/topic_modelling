# skip_gram test

skip_4 <- tokens(gsr_uni_stop$review_scrub, ngrams = 2, skip = 0:4, concatenator = " ")

skip_dfm <- dfm(skip_4)

gsr_final <- gsr_reviews %>% 
  semi_join(gsr_uni_stop, by = c("review_id" = "review_id"))
gsr_final %<>% filter(!is.na(review_id))

skip_stm <-quanteda::convert(skip_dfm, to = c("stm"), docvars = gsr_final)

gsr_stm_skip_model <- stm(skip_stm$documents,
                          skip_stm$vocab,
                          K=0,
                          prevalence = ~ rating + brand,
                          max.em.its = 3,
                          data = skip_stm$meta,
                          reportevery = 1,
                          seed = 1979)