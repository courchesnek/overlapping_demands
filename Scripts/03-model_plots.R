#load packages
source("Scripts/00-packages.R")

#read in data
breeding_cones <- read.csv("Input/breeding_cones.csv")

#remove all juvenile entries
breeding_cones <- breeding_cones[breeding_cones$age_class != "juvenile", ]

#create groupings to merge variables
breeding_cones$group <- with(breeding_cones, paste(sex, age_class, ifelse(breed_status == 1, "breeding", "non-breeding")))


#convert to factor and set levels and labels as necessary
breeding_cones$group <- factor(breeding_cones$group,
                               levels = c("F yearling breeding", "F yearling non-breeding",
                                          "M yearling breeding", "M yearling non-breeding",
                                          "F adult breeding", "F adult non-breeding",
                                          "M adult breeding", "M adult non-breeding"))


# descriptive statistics --------------------------------------------------
summary_stats <- breeding_cones %>%
  group_by(group) %>%
  summarise(
    mean_cache = mean(log_cache_size_new, na.rm = TRUE),  #mean
    median_cache = median(log_cache_size_new, na.rm = TRUE),  #median
    sd_cache = sd(log_cache_size_new, na.rm = TRUE),  #standard deviation
    min_cache = min(log_cache_size_new, na.rm = TRUE),  #minimum
    max_cache = max(log_cache_size_new, na.rm = TRUE),  #maximum
    n = n())

#save
write.csv(summary_stats, "Output/summary_stats.csv", row.names = FALSE)

#plot
ggplot(breeding_cones, aes(x = group, y = log_cache_size_new)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Cache Size by Sex, Age Class and Breeding Status", y = "Log Cache Size") +
  theme(plot.title = element_text(hjust = 0.5))

#data distribution
hist(breeding_cones$log_cache_size_new, main="Histogram of New Cone Caches", xlab="New Cone Caches")


# two-part model with continuous distribution -----------------------------
#step 1: binary model for zero vs positive outcomes
breeding_cones$cache_present = as.numeric(breeding_cones$log_cache_size_new > 0)
# 
# model_binary <- glm(cache_present ~ group + log_total_cones,
#                       data = breeding_cones,
#                       family = binomial)
# 
# summary(model_binary)
# 
# par(mfrow=c(2,2))
# plot(model_binary)

#without juveniles
model_binary <- glm(cache_present ~ group + log_total_cones, 
                     family = binomial, 
                     data = breeding_cones)

summary(model_binary)

par(mfrow=c(2,2))
plot(model_binary)

#step 2: continuous model for positive outcomes only
#filter for positive caching events
# positive_caches <- breeding_cones %>%
#   filter(log_cache_size_new > 0)
# 
# #linear model for the log of cache size
# model_positives <- lm(log_cache_size_new ~ group + log_total_cones,
#                       data = positive_caches)
# 
# summary(model_positives)
# 
# par(mfrow=c(2,2))
# plot(model_positives)

#without juveniles
positive_caches <- breeding_cones %>%
  filter(log_cache_size_new > 0)

model_positive <- lm(log_cache_size_new ~ group + log_total_cones,
                      data = positive_caches)

summary(model_positive)

par(mfrow=c(2,2))
plot(model_positive)


# generate predictions and plot -------------------------------------------

#first, predict prob caching from binary model
breeding_cones$prob_caching <- predict(model_binary, type = "response", newdata = breeding_cones)

#generate predictions for non-zero cache sizes (linear model)
#note: Predictions will only be applied where caching is likely (e.g., prob_caching > some threshold like 0.5)
breeding_cones$predicted_log_cache_size <- ifelse(breeding_cones$prob_caching > 0.5, 
                                                  predict(model_positive, newdata = breeding_cones, type = "response"), 
                                                  0)

#violin plot - raw
ggplot(positive_caches, aes(x = group, y = log_cache_size_new, fill = group)) +
  geom_violin(trim = FALSE, show.legend = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "New Cones Cached by Sex, Age Class and Breeding Status",
       x = "Group",
       y = "Log New Cones Cached") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


#violin plot - predicted
ggplot(breeding_cones, aes(x = group, y = predicted_log_cache_size, fill = group)) +
  geom_violin(trim = FALSE, show.legend = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Predicted New Cones Cached by Sex, Age Class and Breeding Status",
       x = "Group",
       y = "Predicted Log New Cones Cached") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))



























