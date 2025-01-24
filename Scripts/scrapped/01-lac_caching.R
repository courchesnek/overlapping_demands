#load packages
source("Scripts/00-packages.R")

#connect to database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#breed_status for females -----------------------------------------------
breeding <- tbl(con, "litter") %>%
  collect()

females <- breeding %>%
  dplyr::select(yr, squirrel_id, br, fieldBDate) %>%
  filter(br %in% c(0, 1)) %>%
  mutate(
    fieldBDate = as.Date(fieldBDate, format = "%Y-%m-%d"),
    lac_end = if_else(br == 1, fieldBDate + 70, as.Date(NA))) %>%
  rename(
    breed_status = br,
    year = yr) %>%
  filter(!(breed_status == 1 & is.na(fieldBDate))) %>%
  mutate(
    lac_cache = if_else(
      is.na(lac_end) | is.na(fieldBDate),
      FALSE,
      lac_end > as.Date(paste0(year, "-08-16"))))

#save
write.csv(females, "Input/females.csv", row.names = FALSE)


# add in cache numbers ----------------------------------------------------
#read in table
midden_cones <- read.csv("Input/midden_cones.csv")

female_cones <- females %>%
  left_join(
    midden_cones %>% dplyr::select(year, squirrel_id, log_cache_size_new, log_total_cones),
    by = c("squirrel_id", "year")) %>%
  filter(!is.na(log_cache_size_new) & !is.na(log_total_cones))

#add a cache rate column - to control for cone crop
female_cones <- female_cones %>%
  mutate(cache_rate = log_cache_size_new / log_total_cones)


# model -------------------------------------------------------------------
##filter for positive caching events only (i.e. only want cache size new > 0)
positive_caches <- female_cones %>%
  filter(log_cache_size_new > 0)

positive_caches$lac_cache <- as.logical(positive_caches$lac_cache)

#fit the continuous model
model_positive <- lm(log_cache_size_new ~ lac_cache + log_total_cones, data = positive_caches)

#model summary
summary(model_positive)

#check residuals
par(mfrow = c(2, 2))
plot(model_positive)


# plots -------------------------------------------------------------------
#effect plot
effect_plot(model_positive, pred = lac_cache,
            data = positive_caches,
            interval = TRUE, plot.points = TRUE,
            x.label = "Total Cones (Log)",
            y.label = "Predicted Log Cache Size",
            main.title = "Effect of Total Cones and Lactation Status on Cache Size")

#generate predictions
positive_caches$predicted_cache_size <- predict(model_positive, newdata = positive_caches, type = "response")

#violin plot - predicted
ggplot(positive_caches, aes(x = lac_cache, y = predicted_cache_size, fill = lac_cache)) +
  geom_violin(trim = FALSE, color = "black") +
  geom_boxplot(width = 0.1, fill = "white") +
  geom_jitter(shape = 16, position = position_jitter(0.2), alpha = 0.5) +
  labs(title = "Cache Rate by Lactation Status During Caching Season",
       x = "Lactation Status During Caching Season",
       y = "Cache Rate (Log New Cones Cached per Log Total Cones)") +
  scale_fill_manual(values = c("FALSE" = "#A6CEE3", "TRUE" = "#FDBF6F")) +
  scale_x_discrete(labels = c("FALSE" = "Non-Lactating", "TRUE" = "Lactating")) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

#violin plot - rate
females <- ggplot(positive_caches, aes(x = lac_cache, y = cache_rate, fill = lac_cache)) +
  geom_violin(trim = FALSE, color = "black") +
  geom_boxplot(width = 0.1, fill = "white") +
  geom_jitter(shape = 16, position = position_jitter(0.2), alpha = 0.5) +
  labs(title = "Cache Rate by Lactation Status During Caching Season",
       x = "Lactation Status During Caching Season",
       y = "Cache Rate (Log New Cones Cached per Log Total Cones)") +
  scale_fill_manual(values = c("FALSE" = "#A6CEE3", "TRUE" = "#FDBF6F")) +
  scale_x_discrete(labels = c("FALSE" = "Non-Lactating", "TRUE" = "Lactating")) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

females

#save
ggsave("Output/females.jpeg", plot = females, width = 8, height = 6)



