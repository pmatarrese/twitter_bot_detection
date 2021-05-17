########
# Define functions
########

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
