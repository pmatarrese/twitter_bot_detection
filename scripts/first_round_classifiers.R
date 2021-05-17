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
  save_kable("../plots/labeled_class_rates.png")
