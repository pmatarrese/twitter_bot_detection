########
# Preprocessing
########

load("../data/tweets_raw_labeled.rda")
load("../data/tweets_users_raw_labeled.rda")

# Load stop words from tidytext package
data("stop_words")

# Load in senticnet5 and SlangSD sentiment corpora
senticnet <- read.table("../data/senticnet5.txt", header = TRUE)

senticnet %>%
  mutate(CONCEPT = str_replace_all(CONCEPT, "_", " ")) %>%
  rename(word = CONCEPT) -> senticnet

slangSD <- read.delim("../data/SlangSD.txt") %>%
  rename(word = 'a.',
         score = 'X.1')

# Tokenize tweet text for sentiment and other analysis
tweets %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% str_remove_all(stop_words$word, "'")) -> tweets_tokenized


# Create summary stats from words column
tweets_tokenized %>%
  select(user_id, status_id, word) %>%
  mutate(is_hashtag = ifelse(str_detect(word, "^#"), 1, 0),  # get hashtag count
         is_tag_user = ifelse(str_detect(word, "^@"), 1, 0), # get tagged user count
         is_maga = ifelse(str_detect(word, "^#maga$"), 1, 0),
         is_Trump = ifelse(str_detect(word, regex("^Trump$", ignore_case = TRUE)), 1, 0),
         is_Pence = ifelse(str_detect(word, regex("^Pence$", ignore_case = TRUE)), 1, 0),
         is_Biden = ifelse(str_detect(word, regex("^Biden$", ignore_case = TRUE)), 1, 0),
         is_Harris = ifelse(str_detect(word, regex("^Harris$", ignore_case = TRUE)), 1, 0),         # This section is for identifying any
         is_Clinton = ifelse(str_detect(word, regex("^Clinton$", ignore_case = TRUE)), 1, 0),       # obvious political terms given the 
         is_Pelosi = ifelse(str_detect(word, regex("^Pelosi$", ignore_case = TRUE)), 1, 0),         # current events in US politics
         is_Donald = ifelse(str_detect(word, regex("^Donald$", ignore_case = TRUE)), 1, 0),
         is_Mike = ifelse(str_detect(word, regex("^Mike$", ignore_case = TRUE)), 1, 0),
         is_Joe = ifelse(str_detect(word, regex("^Joe$", ignore_case = TRUE)), 1, 0),
         is_Kamala = ifelse(str_detect(word, regex("^Kamala$", ignore_case = TRUE)), 1, 0),
         is_Hillary = ifelse(str_detect(word, regex("^Hillary$", ignore_case = TRUE)), 1, 0),
         is_Nancy = ifelse(str_detect(word, regex("^Nancy$", ignore_case = TRUE)), 1, 0),
         is_President = ifelse(str_detect(word, regex("^President$", ignore_case = TRUE)), 1, 0),
         is_election = ifelse(str_detect(word, regex("^election$", ignore_case = TRUE)), 1, 0),
         has_emoji = ifelse(replace_emoji(word) == word, 0, 1)) %>%
  left_join(senticnet) %>%
  rename(sentic_polarity = POLARITY,          # This is to try to get
         sentic_score = INTENSITY) %>%        # average and median scores for
  left_join(slangSD) %>%                      # sentiments of tweets based on the 
  rename(slang_score = score) %>%             # two corpora
  select(- sentic_polarity) %>%
  group_by(status_id) %>%
  summarise(across(.cols = is_hashtag:slang_score, sum, .names = "{.col}_sum")) -> summary_stats_tokens

# Join summary stats to original tweets
tweets %>%
  left_join(summary_stats_tokens) -> tweets

# Final summary of stats down to one row per user
tweets %>%
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
  ungroup() %>%
  select(human_bot, user_id, screen_name, n, followers_count,
         friends_count, account_age, percent_retweet, percent_reply,
         percent_urls, ave_poli_word_per_tweet, ave_hashtag_per_tweet, ave_tags_per_tweet,
         med_sentic, ave_sentic, med_slang, ave_slang) %>%
  unique() -> tweets

# Do some manual data correction
tweets$human_bot[tweets$screen_name == "pmatarrese"] <- 0 # I am not a bot
tweets[is.na(tweets)] <- 0 # convert 'NA' values to 0 on sentiment scores

# Process tweet user data 

tweets_users %>%
  mutate(name_has_emoji = ifelse(replace_emoji(name) == name, 0, 1),
         name_num_non_letters = str_count(name, "[^a-zA-Z0-9\\s\\r\\n\\d]"),
         name_ratio_symb_letters = ifelse(name_num_non_letters/str_count(name) < 1, name_num_non_letters/str_count(name), 1),
         description_has_emoji = ifelse(replace_emoji(description) == description, 0, 1),
         description_num_non_letters = str_count(description, "[^a-zA-Z0-9\\s\\r\\n\\d]"),
         description_ratio_symb_letters = ifelse(description_num_non_letters/str_count(description) < 1, description_num_non_letters/str_count(description), 1),
         followers_friends_ratio = followers_count/friends_count,
         fav_status_ratio = favourites_count/statuses_count) -> tweets_users

# Combine user data with tweet summary stats
tweets %>%
  left_join(tweets_users) %>%
  mutate(ave_status_per_day = statuses_count/as.numeric(account_age)) -> tweets

# Pare down to desired columns, and make sure data types are correct

tweets %>%
  select(human_bot, user_id, screen_name, n, followers_count, friends_count, followers_friends_ratio,
         account_age, percent_reply, percent_retweet, percent_urls, ave_poli_word_per_tweet,
         ave_hashtag_per_tweet, ave_status_per_day, ave_tags_per_tweet, ave_sentic, med_sentic,
         ave_slang, med_slang, name_has_emoji, name_num_non_letters, name_ratio_symb_letters,
         description_has_emoji, description_num_non_letters, description_ratio_symb_letters,
         statuses_count, favourites_count, fav_status_ratio) %>%
  mutate(followers_friends_ratio = case_when(followers_friends_ratio == Inf ~ 9999,
                                             is.na(followers_friends_ratio) == TRUE ~ 0,
                                             !(followers_friends_ratio == Inf) & is.na(followers_friends_ratio) == FALSE ~ followers_friends_ratio),
         account_age = as.numeric(account_age),
         ave_status_per_day = ifelse(is.na(ave_status_per_day) == TRUE, 0, ave_status_per_day),
         name_has_emoji = ifelse(is.na(name_has_emoji) == TRUE, 0, name_has_emoji),
         name_num_non_letters = ifelse(is.na(name_num_non_letters) == TRUE, 0, name_num_non_letters),
         name_ratio_symb_letters = ifelse(is.na(name_ratio_symb_letters) == TRUE, 0, name_ratio_symb_letters),
         description_has_emoji = ifelse(is.na(description_has_emoji) == TRUE, 0, description_has_emoji),
         description_num_non_letters = ifelse(is.na(description_num_non_letters) == TRUE, 0, description_num_non_letters),
         description_ratio_symb_letters = ifelse(is.na(description_ratio_symb_letters) == TRUE, 0, description_ratio_symb_letters),
         statuses_count = ifelse(is.na(statuses_count) == TRUE, 1, statuses_count),
         favourites_count = ifelse(is.na(favourites_count) == TRUE, 0, favourites_count),
         fav_status_ratio = ifelse(is.na(fav_status_ratio) == TRUE, 0, fav_status_ratio)) -> tweets

# Save processed training data sets
save(tweets, file = "../data/tweets_processed.rda")
