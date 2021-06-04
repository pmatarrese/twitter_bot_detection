

# Show unlabeled results with only "verified" users
results_tbl %>%
  filter(human_bot == 0) %>%
  kable(caption = "The three classifiers at least predicted the known humans (verified accounts) fairly accurately.") %>%
  kable_styling() %>%
  save_kable("unlabeled_results_humans.png")

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

