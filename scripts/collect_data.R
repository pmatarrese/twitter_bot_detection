########
# Collecting Data
########

# Load in .tsv files for list of known bots and list of verified users
known_bots <- read_tsv("../data/astroturf.tsv", col_names = FALSE)
verified_accounts <- read_tsv("../data/verified-2019.tsv", col_names = FALSE)

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

# Collect last 100 tweets from each user from list of known bots and verified accounts
tweets <- get_timeline_unlimited(as.vector(users_labeled$user_id), 100)

# Collect user information for each user from list of known bots and verified accounts

tweets_users <- users_data(tweets)

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
save(tweets, file = "../data/tweets_raw_labeled.rda")
save(tweets_users, file = "../data/tweets_users_raw_labeled.rda")
