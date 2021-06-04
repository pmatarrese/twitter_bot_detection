# Required: must have Twitter Developer account (with keys), connected with Rtweet package 
# to be able to download Tweets data

########
# Loading Libraries
########

# Make sure all rate limits reset
#Sys.sleep(1800)

# Remove all variables from the current environment
rm(list = ls())

# For reproducibility
set.seed(8675309)

# Start timer
start_time <- proc.time()

packages <- c("tinytex", "tidytext", "textstem", "glue", "lubridate",
              "textclean", "sjmisc", "kableExtra", "rtweet", "TSA", "class",
              "tree", "MASS", "e1071", "tidyverse")

temp <- lapply(packages, function(x) {if (!x %in% row.names(installed.packages()))
  install.packages(x);})

temp <- lapply(packages, library, character.only = TRUE)

########
# Collecting Data
########

# Load in .tsv files for list of known bots and list of verified users
known_bots <- read_tsv("astroturf.tsv", col_names = FALSE)
verified_accounts <- read_tsv("verified-2019.tsv", col_names = FALSE)

# Combine lists into one dataframe
users_labeled <- bind_rows(known_bots, verified_accounts)

users_labeled %>%
  rename(user_id = X1,
         human_bot = X2) %>%
  mutate(user_id = as.character(user_id)) -> users_labeled # treat numeric as string

users_labeled$human_bot[users_labeled$human_bot == "political_Bot"] <- 1 # label bots as "1"
users_labeled$human_bot[users_labeled$human_bot == "human"] <- 0 # label humans as "0"

users_labeled %>%
  mutate(human_bot = as.factor(human_bot)) -> users_labeled # change data type to factor

# Define function to grab Tweet data from users automatically while avoiding rate cap

get_timeline_unlimited <- function(users, n){
  
  if (length(users) == 0){
    return(NULL)
  }
  
  rl <- rate_limit(query = "get_timeline") # Figure out how many more queries I can run 
                                           # before hitting rate limit
  
  if (length(users) <= rl$remaining){ # if users left < rate limit, run get_timeline for all users
    print(glue("Getting data for {length(users)} users"))
    tweets <- get_timeline(users, n, check = FALSE)  
  }else{
    
    if (rl$remaining > 0){ # if rate limit not reached, run get_timeline for users equal to remaining available queries
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      tweets_first <- get_timeline(users_first, n, check = FALSE)
      rl <- rate_limit(query = "get_timeline")
    }else{
      tweets_first <- NULL
      users_rest <- users # redefine list as remaining users not yet queried
    }
    wait <- rl$reset + 0.1
    print(glue("Waiting for {round(wait,2)} minutes"))
    Sys.sleep(wait * 60) # wait for x minutes until rate limit is reset
    
    tweets_rest <- get_timeline_unlimited(users_rest, n)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  return(tweets)
}

# Collect last 200 tweets from each user from list of known bots and verified accounts
tweets <- get_timeline_unlimited(as.vector(users_labeled$user_id), 198)

# Collect user information for each user from list of known bots and verified accounts


tweets_users <- users_data(tweets)

########
# Preprocessing
########

# Join labels onto tweets
tweets %>%
  left_join(users_labeled) %>%
  group_by(user_id) %>%
  add_tally() %>%
  ungroup() -> tweets

# Pare down to one row per user for tweet user data

tweets_users %>%
  distinct(user_id, .keep_all = TRUE) %>%
  left_join(users_labeled) -> tweets_users

# Save unprocessed data to files for records
save(tweets, file = "tweets_raw_labeled.rda")
save(tweets_users, file = "tweets_users_raw_labeled.rda")

# Load stop words from tidytext package
data("stop_words")

# Load in senticnet5 and SlangSD sentiment corpora
senticnet <- read.table("senticnet5.txt", header = TRUE)

senticnet %>%
  mutate(CONCEPT = str_replace_all(CONCEPT, "_", " ")) %>%
  rename(word = CONCEPT) -> senticnet

slangSD <- read.delim("SlangSD.txt") %>%
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
save(tweets, file = "tweets_processed.rda")

########
# Building Classifiers
########

# Create sample vector for splitting dataset into training and testing
n <- length(tweets$human_bot)
Z <- sample(n, n/4)

######## Linear Discriminant Analysis/Quadratic Discriminant Analysis

# Prep matrices for LDA/QDA
lda.Y <- as.matrix(tweets[,1])
lda.X <- as.matrix(tweets[,5:28])

# Use LDA function against all variables (except for user_id, screen_name, and n)
lda.results <- lda(lda.Y ~ lda.X, CV = TRUE)

# Calculate classification rate for LDA
table(tweets$human_bot, lda.results$class) -> lda_table
lda_table
class.rate.lda <- mean(tweets$human_bot == lda.results$class)

# Compute QDA in similar fashion
qda.results <- qda(lda.Y ~ lda.X, CV = TRUE)

# Calculate QDA classification rate
table(tweets$human_bot, qda.results$class) -> qda_table
qda_table
qda_table[1,1]
class.rate.qda <- (qda_table[1,1] + qda_table[2,2])/(qda_table[1,1] + qda_table[1,2] + qda_table[2,1] + qda_table[2,2]) # for some reason qda() removed some
                                                       # observations and only gave 2306 
                                                       # results.

######## Logistic regression

# Create training and testing datasets
log.reg.training <- tweets[-Z,] %>%
  select(-user_id, -screen_name, -n)
log.reg.testing <- tweets[Z,] %>%
  select(-user_id, -screen_name, -n)

# Train logistic regression model on training data
log.reg.model <- glm(human_bot ~ ., data = log.reg.training, family = binomial)

# Predict classifications for the testing data
log.reg.prob <- predict(log.reg.model, data.frame(log.reg.testing), type = "response")
log.reg.yes.pred <- 1*(log.reg.prob > 0.9)
table(log.reg.testing$human_bot, log.reg.yes.pred) -> log_reg_table
log_reg_table

# Calculate classification rate for logistic regression model
log.reg.training.error.rate <- (log_reg_table[1,1] + log_reg_table[2,2])/(log_reg_table[1,1] + log_reg_table[1,2] + log_reg_table[2,1] + log_reg_table[2,2])
log.reg.testing.error.rate <- log_reg_table[2,2]/(log_reg_table[2,2] + log_reg_table[1,2])
class.rate.log.reg <- log.reg.testing.error.rate

######## KNN

# Create training and testing Y and X dataframes
knn.training.Y <- tweets[-Z, 1]
knn.testing.Y <- tweets[Z, 1]
knn.training.X <- tweets[-Z, 5:28]
knn.testing.X <- tweets[Z, 5:28]

# Find best KNN classifier
class.rate <- rep(0, 50)

for (k in 1:50) {
  knn.results <- knn(knn.training.X, knn.testing.X, knn.training.Y$human_bot, k)
  class.rate[k] <- mean(knn.testing.Y$human_bot == knn.results)
}

plot(class.rate, type = "l") +
  points(x = which.max(class.rate), y = class.rate[which.max(class.rate)])

# Define KNN best results
knn.best <- knn(knn.training.X, knn.testing.X, knn.training.Y$human_bot, which.max(class.rate))
table(knn.testing.Y$human_bot, knn.best) -> knn_table
knn_table
class.rate.knn <- mean(knn.testing.Y$human_bot == knn.best)

######## Classification Tree

# Create training and testing datasets
tree.training <- tweets[-Z,]
tree.testing <- tweets[Z,]

# Model a tree based on training data
tree.model <- tree(human_bot ~ . - user_id - screen_name - n, data = tree.training)

# Calculate predictions and classification error rate based on tree model
tree.pred <- predict(tree.model, tweets, type = "class")
table(tweets$human_bot[Z], tree.pred[Z]) -> class_tree_table
class_tree_table
mean(tweets$human_bot[Z] == tree.pred[Z]) -> orig_tree_rate

# See if tree can be optimized by pruning based on mis-classification
tree.cv <- cv.tree(tree.model, FUN = prune.misclass)
tree.cv
plot(tree.cv)
tree.pruned <- prune.misclass(tree.model, best = tree.cv$size[which.min(tree.cv$dev)]) # the mis-classification rate is minimized
                                                                                       # at multiple tree sizes, so we choose the
                                                                                       # smallest tree (which is size 6)

# Calculate the classification rate of the pruned tree
pruned.tree.pred <- predict(tree.pruned, tweets, type = "class")
table(tweets$human_bot[Z], pruned.tree.pred[Z])
mean(tweets$human_bot[Z] == pruned.tree.pred[Z]) -> pruned_tree_rate
pruned_tree_rate
orig_tree_rate

orig_tree_rate > pruned_tree_rate # the pruned tree performs worse, so we will
                                  # stick with the original tree

class.rate.tree <- mean(tweets$human_bot[Z] == tree.pred[Z])

######## Support Vector Classifier

# Create SVM with all observations
svm.model <- svm(human_bot ~ . - user_id - screen_name - n, data = tweets)
summary(svm.model)

# Tune based on kernel and cost
svm.tuning <- tune(svm, human_bot ~ . - user_id - screen_name - n, data = tweets,
                   ranges = list(cost = 10^seq(-3,3), kernel = c("linear", "polynomial", "radial", "sigmoid")))
summary(svm.tuning)
svm.tuning$best.parameters

# Conduct cross-validation with optimized SVM
svm.optimum.training <- svm(human_bot ~ . - user_id - screen_name - n, data = tweets[-Z,],
                            cost = svm.tuning$best.parameters$cost, kernel = as.character(svm.tuning$best.parameters$kernel))

# Calculate predicted class and classification rate based on trained SVM
svm.pred <- predict(svm.optimum.training, data = tweets)
table(svm.pred[Z], tweets$human_bot[Z]) -> svm_table
svm_table
class.rate.svm <- (svm_table[1,1] + svm_table[2,2])/(svm_table[1,1] + svm_table[1,2] + svm_table[2,1] + svm_table[2,2])

# Compare classification rates
class_methods <- c("LDA", "QDA", "logistic_reg", "KNN", "class_tree", "SVM")
class_rates <- c(class.rate.lda, class.rate.qda, class.rate.log.reg, class.rate.knn, class.rate.tree, class.rate.svm)

class_rates_tbl <- as_tibble(data.frame(cbind(class_methods, class_rates)))

class_rates_tbl %>%
  kable() %>%
  kable_styling() %>%
  save_kable("plots/labeled_class_rates.png")

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
unlabeled_tweets <- get_timeline_unlimited(as.vector(unlabeled_tweets_users$user_id), 198)

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
save(results_tbl, file = "results_20201201.rda")

# Gather dataset for comparison analysis
suspected_bots_compare <- as.vector(results_tbl$user_id[as.numeric(results_tbl$logistic_reg) + as.numeric(results_tbl$knn_results) + as.numeric(results_tbl$class_tree_results) >= 2])
known_bots_compare <- as.vector(as.character(known_bots$X1[sample(length(known_bots$X1), length(suspected_bots_compare))]))
known_humans_compare <- as.vector(as.character(verified_accounts$X1[sample(length(verified_accounts$X1), length(suspected_bots_compare))]))

analysis_users <- as_tibble(data.frame(cbind(suspected_bots_compare, known_bots_compare, known_humans_compare))) %>%
  pivot_longer(cols = everything(), names_to = "account_type", values_to = "user_id")

analysis_users$account_type[analysis_users$account_type == "suspected_bots_compare"] <- "suspected_bot"
analysis_users$account_type[analysis_users$account_type == "known_bots_compare"] <- "known_bot"
analysis_users$account_type[analysis_users$account_type == "known_humans_compare"] <- "known_human"

analysis_users %>%
  mutate(account_type = as.factor(account_type)) -> analysis_users

# Save sampled comparison users
save(analysis_users, file = "comparison_sample_users.rda")

# Download tweet timelines for analysis dataset
analysis_tweets <- get_timeline_unlimited(analysis_users$user_id, 198)
analysis_users_data <- users_data(analysis_tweets)

# Save raw analysis tweets
save(analysis_tweets, file = "analysis_tweets_raw.rda")
save(analysis_users_data, file = "analysis_users_raw.rda")

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
save(analysis_tweets_processed, file = "analysis_tweet_processed.rda")

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
  save_kable("plots/sample_tweets.png")

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
       path = "plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)



# Show unlabeled results with only "verified" users
results_tbl %>%
  filter(human_bot == 0) %>%
  kable(caption = "The three classifiers at least predicted the known humans (verified accounts) fairly accurately.") %>%
  kable_styling() %>%
  save_kable("plots/unlabeled_results_humans.png")

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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
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
       path = "plots/",
       width = 8,
       height = 6,
       units = "in",
       dpi = 320)

# End timer
end_time <- proc.time()

# How long does the script take to run
end_time - start_time