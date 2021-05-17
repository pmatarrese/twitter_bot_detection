########
# Results
########

# Create data frame with results from different classifiers on unlabeled dataset

results_tbl <- as_tibble(data.frame(cbind(as.vector(unlabeled_for_testing$user_id),
                                          as.vector(unlabeled_for_testing$screen_name),
                                          as.vector(unlabeled_for_testing$human_bot),
                                          log.reg.unlabeled.yes.pred,
                                          knn.unlabeled.results,
                                          tree.unlabeled.pred))) %>%
  rename(user_id = V1,
         screen_name = V2,
         human_bot = V3,
         logistic_reg = log.reg.unlabeled.yes.pred,
         knn_results = knn.unlabeled.results,
         class_tree_results = tree.unlabeled.pred) %>%
  mutate(knn_results = ifelse(knn_results == 1, 0, 1),
         class_tree_results = ifelse(class_tree_results == 1, 0, 1))

# Save results table
save(results_tbl, file = "../data/results_20201201.rda")

# Gather dataset for comparison analysis
suspected_bots_compare <- as.vector(results_tbl$user_id[as.numeric(results_tbl$logistic_reg) + as.numeric(results_tbl$knn_results) + as.numeric(results_tbl$class_tree_results) >= 2])
known_bots_compare <- as.vector(as.character(known_bots$X1[sample(length(known_bots$X1), length(suspected_bots_compare), replace = TRUE)]))
known_humans_compare <- as.vector(as.character(verified_accounts$X1[sample(length(verified_accounts$X1), length(suspected_bots_compare))]))

analysis_users <- as_tibble(data.frame(cbind(suspected_bots_compare, known_bots_compare, known_humans_compare))) %>%
  pivot_longer(cols = everything(), names_to = "account_type", values_to = "user_id")

analysis_users$account_type[analysis_users$account_type == "suspected_bots_compare"] <- "suspected_bot"
analysis_users$account_type[analysis_users$account_type == "known_bots_compare"] <- "known_bot"
analysis_users$account_type[analysis_users$account_type == "known_humans_compare"] <- "known_human"

analysis_users %>%
  mutate(account_type = as.factor(account_type)) -> analysis_users

# Save sampled comparison users
save(analysis_users, file = "../data/comparison_sample_users.rda")

# Download tweet timelines for analysis dataset
analysis_tweets <- get_timeline_unlimited(analysis_users$user_id, 75)
analysis_users_data <- users_data(analysis_tweets)

# Save raw analysis tweets
save(analysis_tweets, file = "../data/analysis_tweets_raw.rda")
save(analysis_users_data, file = "../data/analysis_users_raw.rda")

# Process analysis dataset
analysis_tweets %>%
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
  summarise(across(.cols = is_hashtag:slang_score, sum, .names = "{.col}_sum")) -> analysis_summary

analysis_users_data %>%
  mutate(name_has_emoji = ifelse(replace_emoji(name) == name, 0, 1),
         name_num_non_letters = str_count(name, "[^a-zA-Z0-9\\s\\r\\n\\d]"),
         name_ratio_symb_letters = ifelse(name_num_non_letters/str_count(name) < 1, name_num_non_letters/str_count(name), 1),
         description_has_emoji = ifelse(replace_emoji(description) == description, 0, 1),
         description_num_non_letters = str_count(description, "[^a-zA-Z0-9\\s\\r\\n\\d]"),
         description_ratio_symb_letters = ifelse(description_num_non_letters/str_count(description) < 1, description_num_non_letters/str_count(description), 1),
         followers_friends_ratio = followers_count/friends_count,
         fav_status_ratio = favourites_count/statuses_count) -> analysis_users_data

analysis_tweets %>%
  distinct(status_id, .keep_all = TRUE) %>%
  group_by(user_id) %>%
  add_tally() %>%
  ungroup() %>%
  left_join(analysis_summary) %>%
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
  left_join(analysis_users_data) %>%
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
  distinct(user_id, .keep_all = TRUE) -> analysis_tweets_processed

analysis_tweets_processed %>%
  left_join(analysis_users) %>%
  select(account_type, everything()) -> analysis_tweets_processed

# Save processed analysis set
save(analysis_tweets_processed, file = "../data/analysis_tweet_processed.rda")

########
# Generate Plots/Tables
########

# Show example tweet from known human, known bot, and suspected bot
analysis_tweets %>%
  left_join(analysis_users) %>%
  select(account_type, user_id, screen_name, text) %>%
  group_by(user_id) %>%
  mutate(tweet_num = row_number()) %>%
  ungroup() %>%
  filter(tweet_num == sample(length(max(tweet_num)), 1)) %>%
  select(-tweet_num) %>%
  kable(caption = "One tweet from a known bot, known, human, and suspected bot.") %>%
  kable_styling() %>%
  save_kable("../plots/sample_tweets.png")

# Plot of classification rates for labeled data
class_rates_tbl %>%
  ggplot(aes(x = as.factor(class_methods), y = class_rates)) +
  geom_col(fill = "#1DA1F2") +
  labs(title = "Classification Rates of Different Classifiers",
       subtitle = "Logistic Regression seems to have performed the best",
       x = "Classification Method",
       y = "Classification Rate")

ggsave(filename = "plot_class_rates.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)



# Show unlabeled results with only "verified" users
results_tbl %>%
  filter(human_bot == 0) %>%
  kable(caption = "The three classifiers at least predicted the known humans (verified accounts) fairly accurately.") %>%
  kable_styling() %>%
  save_kable("../plots/unlabeled_results_humans.png")

# Show account age difference in labeled set
tweets %>%
  ggplot(aes(x = account_age, fill = human_bot)) +
  geom_histogram() +
  facet_wrap(~ human_bot, scales = "free_y") +
  scale_fill_manual(labels = c("Human", "Bot"),
                    values = c("#1DA1F2", "red"),
                    name = "Human or Bot?") +
  labs(title = "Account Ages (in days) of Training Dataset",
       x = "Account Age",
       y = "Count")

ggsave(filename = "plot_labeled_account_ages.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Fav vs Status count in training data
tweets %>%
  ggplot(aes(x = favourites_count, y = statuses_count, color = human_bot)) +
  geom_point() +
  facet_wrap(~ human_bot) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  labs(title = "Favorite count vs Status Count in Training Data",
       x = "Favorites count (log scale)",
       y = "Status count (log scale)")

ggsave(filename = "plot_labeled_fav_vs_status.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

tweets %>%
  ggplot(aes(x = favourites_count, y = statuses_count, color = human_bot)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  labs(title = "Favorite count vs Status Count in Training Data",
       x = "Favorites count (log scale)",
       y = "Status count (log scale)")

ggsave(filename = "plot_labeled_fav_vs_status2.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Fav/Status Ratio vs Ave Statuses per Day in training data
tweets %>%
  ggplot(aes(y = ave_status_per_day, x = fav_status_ratio, color = human_bot)) +
  geom_point() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  scale_x_log10() +
  facet_wrap(~ human_bot) +
  labs(title = "Favorite/Status ratio vs Ave. Statuses per Day in Training\nData",
       x = "Favorite/Status Ratio (Log scale)",
       y = "Ave. Status per Day")

ggsave(filename = "plot_labeled_ratio_vs_status_per_day.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

tweets %>%
  ggplot(aes(y = ave_status_per_day, x = fav_status_ratio, color = human_bot)) +
  geom_point() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  scale_x_log10() +
  labs(title = "Favorite/Status ratio vs Ave. Statuses per Day in Training\nData",
       x = "Favorite/Status Ratio (Log scale)",
       y = "Ave. Status per Day")

ggsave(filename = "plot_labeled_ratio_vs_status_per_day2.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# Unlabeled

# Log reg
unlabeled_tweets %>%
  left_join(results_tbl) %>%
  ggplot(aes(x = favourites_count, y = statuses_count, color = logistic_reg)) +
  geom_point() +
  facet_wrap(~ logistic_reg) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  labs(title = "Favorite count vs Status Count in Unlabeled Data",
       x = "Favorites count (log scale)",
       y = "Status count (log scale)")

ggsave(filename = "plot_unlabeled_log_fav_vs_status.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

unlabeled_tweets %>%
  left_join(results_tbl) %>%
  ggplot(aes(x = favourites_count, y = statuses_count, color = logistic_reg)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  labs(title = "Favorite count vs Status Count in Unlabeled Data",
       x = "Favorites count (log scale)",
       y = "Status count (log scale)")

ggsave(filename = "plot_unlabeled_log_fav_vs_status2.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

unlabeled_tweets %>%
  left_join(results_tbl) %>%
  ggplot(aes(y = ave_status_per_day, x = fav_status_ratio, color = logistic_reg)) +
  geom_point() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  scale_x_log10() +
  facet_wrap(~ logistic_reg) +
  labs(title = "Favorite/Status ratio vs Ave. Statuses per Day in Unlabeled\nData",
       x = "Favorite/Status Ratio (Log scale)",
       y = "Ave. Status per Day")

ggsave(filename = "plot_unlabeled_log_ratio_vs_status_per_day.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

unlabeled_tweets %>%
  left_join(results_tbl) %>%
  ggplot(aes(y = ave_status_per_day, x = fav_status_ratio, color = logistic_reg)) +
  geom_point() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  scale_x_log10() +
  labs(title = "Favorite/Status ratio vs Ave. Statuses per Day in Unlabeled\nData",
       x = "Favorite/Status Ratio (Log scale)",
       y = "Ave. Status per Day")

ggsave(filename = "plot_unlabeled_log_ratio_vs_status_per_day2.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# KNN
unlabeled_tweets %>%
  left_join(results_tbl) %>%
  ggplot(aes(x = favourites_count, y = statuses_count, color = as.factor(knn_results))) +
  geom_point() +
  facet_wrap(~ as.factor(knn_results)) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  labs(title = "Favorite count vs Status Count in Unlabeled Data",
       x = "Favorites count (log scale)",
       y = "Status count (log scale)")

ggsave(filename = "plot_unlabeled_knn_fav_vs_status.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

unlabeled_tweets %>%
  left_join(results_tbl) %>%
  ggplot(aes(x = favourites_count, y = statuses_count, color = as.factor(knn_results))) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  labs(title = "Favorite count vs Status Count in Unlabeled Data",
       x = "Favorites count (log scale)",
       y = "Status count (log scale)")

ggsave(filename = "plot_unlabeled_knn_fav_vs_status2.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

unlabeled_tweets %>%
  left_join(results_tbl) %>%
  ggplot(aes(y = ave_status_per_day, x = fav_status_ratio, color = as.factor(knn_results))) +
  geom_point() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  scale_x_log10() +
  facet_wrap(~ as.factor(knn_results)) +
  labs(title = "Favorite/Status ratio vs Ave. Statuses per Day in Unlabeled\nData",
       x = "Favorite/Status Ratio (Log scale)",
       y = "Ave. Status per Day")

ggsave(filename = "plot_unlabeled_knn_ratio_vs_status_per_day.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

unlabeled_tweets %>%
  left_join(results_tbl) %>%
  ggplot(aes(y = ave_status_per_day, x = fav_status_ratio, color = as.factor(knn_results))) +
  geom_point() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  scale_x_log10() +
  labs(title = "Favorite/Status ratio vs Ave. Statuses per Day in Unlabeled\nData",
       x = "Favorite/Status Ratio (Log scale)",
       y = "Ave. Status per Day")

ggsave(filename = "plot_unlabeled_knn_ratio_vs_status_per_day2.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)


# Class Tree
unlabeled_tweets %>%
  left_join(results_tbl) %>%
  ggplot(aes(x = favourites_count, y = statuses_count, color = as.factor(class_tree_results))) +
  geom_point() +
  facet_wrap(~ as.factor(class_tree_results)) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  labs(title = "Favorite count vs Status Count in Unlabeled Data",
       x = "Favorites count (log scale)",
       y = "Status count (log scale)")

ggsave(filename = "plot_unlabeled_tree_fav_vs_status.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

unlabeled_tweets %>%
  left_join(results_tbl) %>%
  ggplot(aes(x = favourites_count, y = statuses_count, color = as.factor(class_tree_results))) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  labs(title = "Favorite count vs Status Count in Unlabeled Data",
       x = "Favorites count (log scale)",
       y = "Status count (log scale)")

ggsave(filename = "plot_unlabeled_tree_fav_vs_status2.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

unlabeled_tweets %>%
  left_join(results_tbl) %>%
  ggplot(aes(y = ave_status_per_day, x = fav_status_ratio, color = as.factor(class_tree_results))) +
  geom_point() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  scale_x_log10() +
  facet_wrap(~ as.factor(class_tree_results)) +
  labs(title = "Favorite/Status ratio vs Ave. Statuses per Day in Unlabeled\nData",
       x = "Favorite/Status Ratio (Log scale)",
       y = "Ave. Status per Day")

ggsave(filename = "plot_unlabeled_tree_ratio_vs_status_per_day.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

unlabeled_tweets %>%
  left_join(results_tbl) %>%
  ggplot(aes(y = ave_status_per_day, x = fav_status_ratio, color = as.factor(class_tree_results))) +
  geom_point() +
  scale_color_manual(labels = c("Human", "Bot"),
                     values = c("#1DA1F2", "red"),
                     name = "Human or Bot?") +
  scale_x_log10() +
  labs(title = "Favorite/Status ratio vs Ave. Statuses per Day in Unlabeled\nData",
       x = "Favorite/Status Ratio (Log scale)",
       y = "Ave. Status per Day")

ggsave(filename = "plot_unlabeled_tree_ratio_vs_status_per_day2.png",
       plot = last_plot(),
       device = "png",
       path = "../plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)
