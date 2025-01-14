#load packages
source("Scripts/00-packages.R")

#read in data
breeding_cones <- read.csv("Input/breeding_cones.csv")

#create groupings to merge variables
breeding_cones$group <- with(breeding_cones, ifelse(age_class == "juvenile",
                                                    "juvenile",
                                                    paste(sex, age_class, ifelse(breed_status == 1, "breeding", "non-breeding"))))

#convert to factor and set levels in a logical order
breeding_cones$group <- factor(breeding_cones$group,
                               levels = c("juvenile",
                                          "F yearling breeding", "F yearling non-breeding",
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

model_binary <- glm(cache_present ~ group + log_total_cones,
                      data = breeding_cones,
                      family = binomial)

summary(model_binary)

par(mfrow=c(2,2))
plot(model_binary)

#step 2: continuous model for positive outcomes only
#filter for positive caching events
positive_caches <- breeding_cones %>%
  filter(log_cache_size_new > 0)

#fit the zero-inflated Gamma (ZIG) model
model_zig <- gamlss(
  log_cache_size_new ~ group + log_total_cones,
  family = ZAGA(),
  data = positive_caches
)

#model summary
summary(model_zig)

# generate predictions and plot -------------------------------------------
#generate predictions for binary model first
breeding_cones$prob_caching <- predict(model_binary, newdata = breeding_cones, type = "response")

#subset breeding_cones for positive caching events
positive_caches <- breeding_cones[breeding_cones$prob_caching > 0.5, ]

#predict the cache size using the ZIG model
positive_caches$predicted_cache_size <- predict(model_zig, newdata = positive_caches, type = "response")

#box-plot
ggplot(breeding_cones, aes(x = group, y = predicted_cache_size, fill = group)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 1.5, color = "black") +  # Box plot with outliers
  stat_summary(fun = mean, geom = "point", shape = 18, size = 1.5, color = "black") +  # Mean points
  labs(title = "Predicted New Cones Cached by Sex, Age Class, and Breeding Status",
       x = "Group", y = "Predicted Log New Cones Cached") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3", "#a6761d")) +
  guides(fill = "none")

#effect plot
predicted_summary <- aggregate(
  predicted_cache_size ~ group,
  data = breeding_cones,
  FUN = function(x) c(
    mean = mean(x, na.rm = TRUE),
    lower_ci = mean(x, na.rm = TRUE) - 1.96 * sd(x, na.rm = TRUE) / sqrt(length(x)),
    upper_ci = mean(x, na.rm = TRUE) + 1.96 * sd(x, na.rm = TRUE) / sqrt(length(x))
  )
)

predicted_summary <- do.call(data.frame, predicted_summary)
colnames(predicted_summary) <- c("group", "mean_cache_size", "lower_ci", "upper_ci")

ggplot(predicted_summary, aes(x = group, y = mean_cache_size)) +
  geom_point(size = 4, color = "blue") +  # Mean points
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "blue") +  # Error bars
  labs(
    title = "Effect of Group on Predicted New Cone Cache Size",
    x = "Group",
    y = "Predicted Log New Cone Cache Size"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


