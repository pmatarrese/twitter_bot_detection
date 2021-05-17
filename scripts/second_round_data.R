########
# Unsupervised learning
########

# Download random sampling of tweets based off of certain hashtags
query = "(#election OR #vote OR #maga OR #trump OR #biden) lang:en"
unlabeled_tweets <- search_tweets(q = query,
                                  n = 50000,
                                  retryonratelimit = TRUE)

save(unlabeled_tweets, file = "unlabeled_hashtag_tweets.rda")

# Create list users with a sample size of 2500
unlabeled_tweets_users <- users_data(unlabeled_tweets) %>%
  unique() %>%
  select(user_id, verified)

unlabeled_tweets_users %>%
  filter(verified == TRUE) -> unlabeled_users_verified
unlabeled_tweets_users %>%
  filter(verified == FALSE) -> unlabeled_users_unverified

unlabeled_users_unverified[sample(length(unlabeled_users_unverified$user_id), 2249),] -> unlabeled_users_unverified

unlabeled_users_unverified %>%
  bind_rows(unlabeled_users_verified) -> unlabeled_tweets_users

# Download timeline data for sampled users
unlabeled_tweets <- get_timeline_unlimited(as.vector(unlabeled_tweets_users$user_id), 100)

unlabeled_tweets_users <- users_data(unlabeled_tweets)

# Save unlabeled dataset and unlabeled user data for records
save(unlabeled_tweets, file = "unlabeled_raw_tweets.rda")
save(unlabeled_tweets_users, file = "unlabeled_raw_users.rda")

# Process unlabeled tweets to have the same columns as the training set
unlabeled_tweets %>%
  filter((lang == "en")) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word %in% str_remove_all(stop_words$word, "'")) %>%
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
  rename(sentic_polarity = POLARITY,          
         sentic_score = INTENSITY) %>%        
  left_join(slangSD) %>%                      
  rename(slang_score = score) %>%             
  select(- sentic_polarity) %>%
  group_by(status_id) %>%
  summarise(across(.cols = is_hashtag:slang_score, sum, .names = "{.col}_sum")) -> unlabeled_tokens_summary

unlabeled_tweets_users %>%
  mutate(name_has_emoji = ifelse(replace_emoji(name) == name, 0, 1),
         name_num_non_letters = str_count(name, "[^a-zA-Z0-9\\s\\r\\n\\d]"),
         name_ratio_symb_letters = ifelse(name_num_non_letters/str_count(name) < 1, name_num_non_letters/str_count(name), 1),
         description_has_emoji = ifelse(replace_emoji(description) == description, 0, 1),
         description_num_non_letters = str_count(description, "[^a-zA-Z0-9\\s\\r\\n\\d]"),
         description_ratio_symb_letters = ifelse(description_num_non_letters/str_count(description) < 1, description_num_non_letters/str_count(description), 1),
         followers_friends_ratio = followers_count/friends_count,
         fav_status_ratio = favourites_count/statuses_count) -> unlabeled_tweets_users

as.vector(unlabeled_tokens_summary$status_id) -> tweets_to_keep

as.vector(unique(unlabeled_tweets$user_id[unlabeled_tweets$status_id %in% tweets_to_keep])) -> users_to_keep

unlabeled_tweets_users %>%
  filter(user_id %in% users_to_keep) %>%
  distinct(user_id, .keep_all = TRUE) -> unlabeled_tweets_users

unlabeled_tweets %>%
  filter(status_id %in% tweets_to_keep) %>%
  distinct(status_id, .keep_all = TRUE) %>%
  group_by(user_id) %>%
  add_tally() %>%
  ungroup() %>%
  left_join(unlabeled_tokens_summary) %>%
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
  select(verified, user_id, screen_name, n, followers_count,
         friends_count, account_age, percent_retweet, percent_reply,
         percent_urls, ave_poli_word_per_tweet, ave_hashtag_per_tweet, ave_tags_per_tweet,
         med_sentic, ave_sentic, med_slang, ave_slang) %>%
  left_join(unlabeled_tweets_users) %>%
  mutate(ave_status_per_day = statuses_count/as.numeric(account_age)) %>%
  select(verified, user_id, screen_name, n, followers_count, friends_count, followers_friends_ratio,
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
         fav_status_ratio = ifelse(is.na(fav_status_ratio) == TRUE, 0, fav_status_ratio)) %>%
  distinct(user_id, .keep_all = TRUE) -> unlabeled_tweets

# Final manual cleaning of data
unlabeled_tweets %>%
  mutate(ave_sentic = ifelse(is.na(ave_sentic) == TRUE, 0, ave_sentic),
         med_sentic = ifelse(is.na(med_sentic) == TRUE, 0, med_sentic),
         ave_slang = ifelse(is.na(ave_slang) == TRUE, 0, ave_slang),
         med_slang = ifelse(is.na(med_slang) == TRUE, 0, med_slang)) -> unlabeled_tweets

# Save processed unlabeled tweet stats
save(unlabeled_tweets, file = "unlabeled_tweets_processed.rda")
save(unlabeled_tweets_users, file = "unlabeled_users_processed.rda")

# Extract list of verified user_id's to compare results to
verified_humans <- as.vector(unlabeled_tweets$user_id[unlabeled_tweets$verified == TRUE])

########
# Run classifiers against unlabeled tweets
########

# Prepare unlabeled tweets for testing
unlabeled_tweets %>%
  mutate(human_bot = ifelse(verified == TRUE, 0, 1)) %>%
  select(human_bot, everything(), -verified) -> unlabeled_for_testing

unlabeled_for_testing %>%
  mutate(human_bot = as.factor(human_bot)) -> unlabeled_for_testing

######## Logistic Regression

# Predict labels based on trained model
log.reg.unlabeled.prob <- predict(log.reg.model,
                                  data.frame(unlabeled_for_testing %>% select(-user_id, -screen_name, -n)),
                                  type = "response")
log.reg.unlabeled.yes.pred <- 1*(log.reg.unlabeled.prob > 0.9)
table(unlabeled_for_testing$human_bot, log.reg.unlabeled.yes.pred)

######## KNN

# Create testing set using new data
knn.unlabeled.testing.X <- unlabeled_for_testing[,5:28]

# Conduct KNN using labeled training data and unlabeled testing data
knn.unlabeled.results <- knn(knn.training.X, knn.unlabeled.testing.X, knn.training.Y$human_bot, 1)
table(unlabeled_for_testing$human_bot, knn.unlabeled.results)

######## Classification Tree

# Predict labels using tree.model on unlabeled data
tree.unlabeled.pred <- predict(tree.model, unlabeled_for_testing, type = "class")
table(unlabeled_for_testing$human_bot, tree.unlabeled.pred)

