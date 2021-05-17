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