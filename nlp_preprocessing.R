library(tidyverse)
library(tidytext)
library(textstem)
library(glue)
library(textclean)
library(sjmisc)

load("tweets.rda")

data("stop_words")

# read in senticnet5 corpus
senticnet <- read.table("senticnet5.txt", header = TRUE)

senticnet %>%
  mutate(CONCEPT = str_replace_all(CONCEPT, "_", " ")) %>%
  rename(word = CONCEPT) -> senticnet

# read in SlangSD corpus
slangSD <- read.delim("SlangSD.txt") %>%
  rename(word = 'a.',
         score = 'X.1')# improve the reading?

tweets_from_bots_humans2 %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% str_remove_all(stop_words$word, "'")) -> word_tokens

save(word_tokens, file = "tweets_tokenized.rda")

word_tokens %>%
  select(user_id, status_id, word, mentions_screen_name, hashtags, mentions_user_id, lang, human_bot) -> new_tokens


new_tokens %>%
  mutate(is_hashtag = ifelse(str_detect(word, "^#"), 1, 0),
         is_tag_user = ifelse(str_detect(word, "^@"), 1, 0),
         is_maga = ifelse(str_detect(word, "^#maga$"), 1, 0),
         is_Trump = ifelse(str_detect(word, regex("^Trump$", ignore_case = TRUE)), 1, 0),
         is_Pence = ifelse(str_detect(word, regex("^Pence$", ignore_case = TRUE)), 1, 0),
         is_Biden = ifelse(str_detect(word, regex("^Biden$", ignore_case = TRUE)), 1, 0),
         is_Harris = ifelse(str_detect(word, regex("^Harris$", ignore_case = TRUE)), 1, 0),
         is_Clinton = ifelse(str_detect(word, regex("^Clinton$", ignore_case = TRUE)), 1, 0),
         is_Pelosi = ifelse(str_detect(word, regex("^Pelosi$", ignore_case = TRUE)), 1, 0),
         is_Donald = ifelse(str_detect(word, regex("^Donald$", ignore_case = TRUE)), 1, 0),
         is_Mike = ifelse(str_detect(word, regex("^Mike$", ignore_case = TRUE)), 1, 0),
         is_Joe = ifelse(str_detect(word, regex("^Joe$", ignore_case = TRUE)), 1, 0),
         is_Kamala = ifelse(str_detect(word, regex("^Kamala$", ignore_case = TRUE)), 1, 0),
         is_Hillary = ifelse(str_detect(word, regex("^Hillary$", ignore_case = TRUE)), 1, 0),
         is_Nancy = ifelse(str_detect(word, regex("^Nancy$", ignore_case = TRUE)), 1, 0),
         is_President = ifelse(str_detect(word, regex("^President$", ignore_case = TRUE)), 1, 0),
         is_election = ifelse(str_detect(word, regex("^election$", ignore_case = TRUE)), 1, 0),
         has_emoji = ifelse(replace_emoji(word) == word, 0, 1)) -> new_tokens

new_tokens %>%
  left_join(senticnet) %>%
  rename(sentic_polarity = POLARITY,
         sentic_score = INTENSITY) %>%
  left_join(slangSD) %>%
  rename(slang_score = score) -> new_tokens

new_tokens %>%
  select(- sentic_polarity) %>%
  group_by(status_id) %>%
  summarise(across(.cols = is_hashtag:slang_score, sum, .names = "{.col}_sum")) -> summary_stats_tokens

tweets_from_bots_humans2 %>%
  left_join(summary_stats_tokens) -> tweets_from_bots_humans2

tweets_from_bots_humans2 %>%
  group_by(user_id) %>%
  add_tally() %>%
  ungroup() -> tweets_from_bots_humans2

tweets_from_bots_humans2 %>%
  mutate(poli_word_sum = is_maga_sum + is_Trump_sum + is_Pence_sum + is_Biden_sum + is_Harris_sum + is_Clinton_sum + is_Pelosi_sum + is_Donald_sum + is_Mike_sum + is_Joe_sum + is_Kamala_sum + is_Hillary_sum + is_Nancy_sum + is_President_sum + is_election_sum,
         account_age = Sys.time() - account_created_at,
         is_reply = ifelse(is.na(reply_to_status_id) == FALSE, 1, 0),
         has_url = ifelse(is.na(urls_url) == FALSE | is.na(ext_media_url) == FALSE, 1, 0)) %>%
  group_by(user_id) %>%
  mutate(percent_retweet = length(is_retweet[is_retweet == TRUE])/n,
         ave_poli_word_per_tweet = (sum(poli_word_sum))/n,
         ave_hashtag_per_tweet = (sum(is_hashtag_sum))/n,
         ave_tags_per_tweet = (sum(is_tag_user_sum))/n,
         med_sentic = median(sentic_score_sum, na.rm = TRUE),
         ave_sentic = mean(sentic_score_sum, na.rm = TRUE),
         med_slang = median(slang_score_sum, na.rm = TRUE),
         ave_slang = mean(slang_score_sum, na.rm = TRUE),
         percent_reply = length(is_reply[is_reply == 1])/n,
         percent_urls = length(has_url[has_url == 1])/n) %>%
  ungroup() -> tweets_from_bots_humans2


tweets_from_bots_humans2 %>%
  select(human_bot, user_id, screen_name, n, followers_count,
         friends_count, account_age, percent_retweet, percent_reply,
         percent_urls, ave_poli_word_per_tweet, ave_hashtag_per_tweet, ave_tags_per_tweet,
         med_sentic, ave_sentic, med_slang, ave_slang) -> preprocessing_results

summary(preprocessing_results)
  
preprocessing_results$ave_poli_word_per_tweet[is.na(preprocessing_results$ave_poli_word_per_tweet)] = 0
preprocessing_results$ave_hashtag_per_tweet[is.na(preprocessing_results$ave_hashtag_per_tweet)] = 0
preprocessing_results$ave_tags_per_tweet[is.na(preprocessing_results$ave_tags_per_tweet)] = 0
preprocessing_results$med_sentic[is.na(preprocessing_results$med_sentic)] = 0
preprocessing_results$med_slang[is.na(preprocessing_results$med_slang)] = 0
preprocessing_results$ave_sentic[is.na(preprocessing_results$ave_sentic)] = 0
preprocessing_results$ave_slang[is.na(preprocessing_results$ave_slang)] = 0
preprocessing_results$human_bot[preprocessing_results$human_bot == "human"] = 0
preprocessing_results$human_bot[preprocessing_results$human_bot == "political_Bot"] = 1

preprocessing_results %>%
  unique() ->preprocessing_results
  

save(preprocessing_results, file = "preprocessed_tweet_stats.rda")
  


