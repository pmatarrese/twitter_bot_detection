library(tidyverse)
library(rtweet)
library(glue)
library(textclean)

known_bots <- read_tsv("astroturf.tsv", col_names = FALSE)
verified_accounts <- read_tsv("verified-2019.tsv", col_names = FALSE)

users_labeled <- bind_rows(known_bots, verified_accounts)

users_labeled <- users_labeled %>%
  rename(user_id = X1,
         human_bot = X2) %>%
  mutate(user_id = as.character(user_id))


#test <- get_timelines(user = as.vector(users_labeled$user_id))

#test2 <- test %>%
#  left_join(users_labeled, by = "user_id")

get_timeline_unlimited <- function(users, n){
  
  if (length(users) == 0){
    return(NULL)
  }
  
  rl <- rate_limit(query = "get_timeline")
  
  if (length(users) <= rl$remaining){
    print(glue("Getting data for {length(users)} users"))
    tweets <- get_timeline(users, n, check = FALSE)  
  }else{
    
    if (rl$remaining > 0){
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      tweets_first <- get_timeline(users_first, n, check = FALSE)
      rl <- rate_limit(query = "get_timeline")
    }else{
      tweets_first <- NULL
      users_rest <- users
    }
    wait <- rl$reset + 0.1
    print(glue("Waiting for {round(wait,2)} minutes"))
    Sys.sleep(wait * 60)
    
    tweets_rest <- get_timeline_unlimited(users_rest, n)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  return(tweets)
}

start_time <- proc.time()
tweets_from_bots_humans2 <- get_timeline_unlimited(as.vector(users_labeled$user_id), 100)
end_time <- proc.time()

end_time - start_time

tweets_from_bots_humans2 <- tweets_from_bots_humans2 %>%
  left_join(users_labeled)

save(tweets_from_bots_humans2, file = "tweets.rda")

tweets_from_bots_humans2 %>%
  distinct(user_id, .keep_all = TRUE) %>%
  ggplot(aes(x = account_created_at)) +
  geom_density()

tweets_from_bots_humans2 %>%
  distinct(user_id, .keep_all = TRUE) %>%
  filter(human_bot != "political_Bot") %>%
  ggplot(aes(x = as.Date(account_created_at))) +
  geom_density()

tweets_from_bots_humans2 %>%
  distinct(user_id, .keep_all = TRUE) %>%
  filter(human_bot == "political_Bot") %>%
  ggplot(aes(x = as.Date(account_created_at))) +
  geom_density()


tweets_user_data <- rtweet::users_data(tweets_from_bots_humans2)

tweets_user_data <- tweets_user_data %>%
  distinct(user_id, .keep_all = TRUE) %>%
  left_join(users_labeled)

save(tweets_user_data, file = "tweets_users.rda")

tweets_user_data %>%
  filter(human_bot == "human") %>%
  count(name[])

tweets_user_data %>%
  mutate(name2 = replace_emoji(name)) %>%
  select(name, name2, human_bot) %>%
  filter(name != name2) %>%
  view()


tweets_user_data <- tweets_user_data %>%
  mutate(has_emoji = ifelse(name == replace_emoji(name), 0, 1))

tweets_user_data %>%
  filter(human_bot == "human") %>%
  filter(has_emoji == 1) %>%
  nrow()

tweets_user_data %>%
  filter(human_bot == "human") %>%
  nrow()


tweets_user_data %>%
  filter(human_bot == "political_Bot") %>%
  filter(has_emoji == 1) %>%
  nrow()

tweets_user_data %>%
  filter(human_bot == "political_Bot") %>%
  nrow()
