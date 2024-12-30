#load packages
source("Scripts/00-packages.R")

#read in data
breeding_cones <- read.csv("Input/breeding_cones.csv")

#reorder age_class factor levels
breeding_cones <- breeding_cones %>%
  mutate(age_class = factor(age_class, levels = c("juvenile", "yearling", "adult")))

#check the levels to confirm the order
levels(breeding_cones$age_class)

# descriptive statistics --------------------------------------------------
summary_stats <- breeding_cones %>%
  group_by(age_class) %>%
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
ggplot(breeding_cones, aes(x = age_class, y = log_cache_size_new)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Cache Size by Age Class", y = "Log Cache Size") +
  theme(plot.title = element_text(hjust = 0.5))

#data distribution
hist(breeding_cones$log_cache_size_new, main="Histogram of New Cone Caches", xlab="New Cone Caches")






# two-part model with continuous distribution -----------------------------
#step 1: binary model for zero vs positive outcomes
breeding_cones$cache_present = as.numeric(breeding_cones$log_cache_size_new > 0)

model_binary <- glm(cache_present ~ sex * age_class * breed_status + log_total_cones,
                      data = breeding_cones,
                      family = binomial(link = "logit"))

summary(model_binary)

par(mfrow=c(2,2))
plot(model_binary)

#evaluate the binary model
#predicted probabilities
predicted_prob <- predict(model_binary, type = "response")

#binarize predictions based on a threshold (usually 0.5)
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)

#actual classes
actual_class <- breeding_cones$cache_present

#confusion Matrix
table(Predicted = predicted_class, Actual = actual_class)

#ROC Curve
roc_curve <- roc(actual_class, predicted_prob)
plot(roc_curve, main="ROC Curve")
auc(roc_curve)

#everything looks good

#step 2: continuous model for positive outcomes only
#filter for positive caching events
positive_caches <- breeding_cones %>%
  filter(log_cache_size_new > 0)

#linear model for the log of cache size
model_positives <- lm(log_cache_size_new ~ sex * age_class * breed_status + log_total_cones,
                      data = positive_caches)

summary(model_positives)

par(mfrow=c(2,2))
plot(model_positives)


# generate predictions and plot -------------------------------------------
##predict the probability that caching occurs
breeding_cones$prob_cache = predict(model_binary, type = "response")

#create a subset where caching is likely (prob_cache > 0.5)
positive_caches <- breeding_cones[breeding_cones$prob_cache > 0.5, ]

#ensure the linear model is fit on the subset where cache_present is true
positive_caches$predicted_cache_size_new = predict(model_positives, newdata = positive_caches, type = "response")

#add a final predicted value column to the original dataset
breeding_cones$final_predicted_cache_size = ifelse(breeding_cones$prob_cache > 0.5,
                                                     predict(model_positives, newdata = subset(breeding_cones, prob_cache > 0.5)),
                                                     0)

#plot predicted probability of caching, colored by breeding status
##ensure breed_status is a factor
breeding_cones$breed_status <- factor(breeding_cones$breed_status, levels = c(0, 1))

# ggplot(breeding_cones, aes(x = age_class, y = prob_cache, color = breed_status, fill = breed_status)) +
#   geom_boxplot(position = position_dodge(width = 0.8)) +  # Positioning the boxes next to each other
#   labs(title = "Predicted Probability of Caching New Cones by Age Class and Breeding Status",
#        x = "Age Class", y = "Predicted Probability of Caching") +
#   scale_color_manual(values = c("red", "blue")) +  # Customize colors for breeding vs non-breeding
#   scale_fill_manual(values = c("red", "blue")) +  # Same color scheme for filling
#   theme_minimal()

ggplot(breeding_cones, aes(x = age_class, y = final_predicted_cache_size, color = breed_status, fill = breed_status)) +
  geom_boxplot(aes(color = breed_status), 
               fill = NA,               
               outlier.shape = 16,      
               outlier.size = 3) +      
  labs(title = "Predicted Cache Size (New Cones) by Age Class and Breeding Status",
       x = "Age Class", y = "Predicted Cache Size (log_cache_size_new)") +
  scale_color_manual(values = c("red", "blue")) +  
  scale_fill_manual(values = c("red", "blue")) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) 

#add sex differences
#create an interaction term for detailed categorization
breeding_cones$group <- interaction(breeding_cones$sex, breeding_cones$age_class, sep=" ")

breeding_cones$group <- factor(breeding_cones$group,
                               levels = c("F juvenile", "M juvenile", "F yearling", "M yearling", "F adult", "M adult"),
                               labels = c("F Juvenile", "M Juvenile", "F Yearling", "M Yearling", "F Adult", "M Adult"))

# Plot with interaction of sex, age class, and breeding status
ggplot(breeding_cones, aes(x = group, y = final_predicted_cache_size, color = breed_status, fill = breed_status)) +
  geom_boxplot(position = position_dodge(width = 0.8), 
               fill = NA,               # No fill inside boxes (only outlines)
               outlier.shape = 16,      # Customize outlier shape
               outlier.size = 3) +      # Adjust outlier size for better visibility
  labs(title = "Predicted Cache Size (New Cones) by Sex and Age Class",
       x = "Group", y = "Predicted Cache Size (log_cache_size_new)",
       color = "Breeding Status", fill = "Breeding Status") +  # Custom legend title
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Non-Breeding", "Breeding")) +  # Custom labels for colors
  scale_fill_manual(values = c("red", "blue"),
                    labels = c("Non-Breeding", "Breeding")) +  # Same custom labels for fill
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

#interaction plot
interact_plot(model_positives, pred = sex, modx = age_class, mod2 = breed_status, data = positive_caches, plot.points = TRUE)




