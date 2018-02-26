# obviously you'd change the file you're about to load to the model you want to assess

load(file = "gsr_stm_n2_k0.rda")

# let's split out some good stuff
gsr <- gsr_stm_model
gsr_stm <- gsr_stm_n2_k0$gsr_stm
gsr_f <- gsr_final
k <- gsr_stm_model$settings$dim$K

# this first bunch are stolen from julia silge 

# beta distribution - most likely words per topic
td_beta <- tidy(gsr_stm_model, matrix = "beta", document_names = gsr_stm$meta$review_id)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  filter(between(topic, 1, 20)) %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# gamma distribution - what likelihood is each document each topic
td_gamma <- tidy(gsr_stm_model, matrix = "gamma", document_names = gsr_stm$meta$review_id)

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) + 
  geom_histogram(show.legend = FALSE, bins = 100) + 
  facet_wrap(~ topic) +
  scale_y_log10()

# these are cool things
# the 1:58 is 1:k

# this bit can take a while. if you're using different covariates change the formula
prep <- estimateEffect( ~ rating + brand, gsr_stm_model, meta = gsr_f, uncertainty = "Global")

# this is the "formula" for each topic, here capped at 1:10, must run prep first
summary(prep, topics=c(1:5))

# this looks at expected topic proportions by topic
plot.STM(gsr_stm_model, type = "summary")

# this looks at the influence of the covariate on the topic proportions, must run prep first
plot(prep, covariate = "rating", topics = 18, method = c("continuous"))

# this looks at the correlation plot between topics
corr <- topicCorr(gsr_stm_model, method = c("huge"), cutoff = 0)
plot(corr)

# plots semantic coherence and exclusivity for the topics
topicQuality(model = gsr, documents = gsr_stm$documents, M = 2)

# find the top scored document for each topic
findThoughts(gsr, texts = gsr_f$review_text, topics = 51, n=5)

# highest prob (same as beta), frex, lift and score for each topic by tokens
labelTopics(gsr)

sageLabels(gsr)

dt <- make.dt(gsr, gsr_f)

ktopics <- searchK(gsr_stm$documents, 
                   gsr_stm$vocab, 
#                   prevalence = ~ rating, 
#                   content = ~ brand,
                   K = t)

model_out <- selectModel(gsr_stm$documents,
            gsr_stm$vocab,
            K = 55,
            prevalence = ~ rating + brand,
            data = gsr_stm$meta,
            to.disk = TRUE)

t <- seq(3,15,by = 2)
