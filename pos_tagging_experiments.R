# devtools::install_github("matthewjdenny/SpeedReader")
# there is a lot of fannying around to get the thing above working properly

library(SpeedReader) 
library(tidyverse)

review <- gsr_reviews$review_text %>% 
  iconv("UTF-8","UTF-8", sub = '')
review <- tolower(review)
brands <- read_csv("data/brands.csv")

reviews2 <- brands$brand %>%
  paste(collapse = "|") %>%
  gsub("brand", review)


pos_tag <- corenlp(reviews2)
pos_tag_df <- bind_rows(pos_tag)


nouns <- pos_tag_df %>% 
  filter(grepl("NN", POS))

summary_n <- nouns %>% 
  group_by(lemma, NER) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

adj <- pos_tag_df %>% 
  filter(grepl("JJ", POS))
summary_adj <- adj %>% 
  group_by(lemma) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# next day delivery tests

sum(grepl("next day delivery",reviews2))
sum(grepl("next day",reviews2))
sum(grepl("day delivery",reviews2))
next_day <- reviews2[which(grepl("next day", reviews2))]

# ranking of nouns
summary_n$rank_noun <- as.numeric(row.names(summary_n))

pos_tag_df2 <- pos_tag_df %>% filter(grepl("NN",POS)) %>% inner_join(summary_n)

table <- pos_tag_df2 %>% group_by(document) %>%
  filter(rank_noun == min(rank_noun))

# frequency table and pct top 100 nouns
save <- as.data.frame(table(table$rank_noun))
sum(save$Freq[1:100])/sum(save$Freq)

# visual
table %>% ggplot(aes(rank_noun)) + geom_histogram(binwidth = 1) + xlim(c(0,100)) + ylim(c(0,500))


# let's check some of the verbs
pos_tag_df %>% filter(word == "delivered")
pos_tag_df %>% filter(word == "missing")
pos_tag_df %>% filter(word == "delivered")


