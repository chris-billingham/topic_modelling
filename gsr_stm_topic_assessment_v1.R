# obviously you'd change the file you're about to load to the model you want to assess

load(file = "gsr_stm_n2_k0.rda")

# let's split out some good stuff
gsr <- gsr_stm_model
gsr_stm <- gsr_stm_n2_k0$gsr_stm
gsr_f <- initial_vars$gsr_final
k <- gsr_stm_n2_k0$gsr_stm_model$k

# this first bunch are stolen from julia silge 

# beta distribution - most likely words per topic
td_beta <- tidy(gsr, matrix = "beta", document_names = gsr_f$review_id)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  filter(between(topic, 1, 10)) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# gamma distribution - what likelihood is each document each topic
td_gamma <- tidy(gsr_stm_model, matrix = "gamma", document_names = gsr_f$review_id)

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) + 
  geom_histogram(show.legend = FALSE, bins = 100) + 
  facet_wrap(~ topic) +
  scale_y_log10()

# these are cool things
# the 1:58 is 1:k

# this bit can take a while. if you're using different covariates change the formula
prep <- estimateEffect( ~ rating + brand, gsr_stm_model, meta = initial_vars$gsr_final, uncertainty = "Global")

# this is the "formula" for each topic, here capped at 1:10, must run prep first
summary(prep, topics=c(21:30))

# this looks at expected topic proportions by topic
plot.STM(gsr_stm_model, type = "summary")

# this looks at the influence of the covariate on the topic proportions, must run prep first
plot(prep, covariate = "rating", topics = 22, method = c("continuous"))

# this looks at the correlation plot between topics
corr <- topicCorr(gsr, method = c("huge"))
plot(corr)

# plots semantic coherence and exclusivity for the topics
topicQuality(model = gsr, documents = gsr_stm$documents)

# find the top scored document for each topic
findThoughts(gsr, texts = gsr_f$review_text, topics = 4, n=5)

# highest prob (same as beta), frex, lift and score for each topic by tokens
labelTopics(gsr)

dt <- make.dt(gsr, gsr_f)

ktopics <- searchK(gsr_stm$documents, 
                   gsr_stm$vocab, 
                   prevalence = ~ rating + brand, 
                   K = c(50:60))

