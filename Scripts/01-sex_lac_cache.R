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


# pull in male cache data -------------------------------------------------
midden_cones <- read.csv("Input/midden_cones.csv")


# prepare data to merge ---------------------------------------------------
#change female sex values to lac non lac in fall
f <- females %>%
  mutate(sex = ifelse(lac_cache == TRUE, "f_lac", "f_non_lac")) %>%
  dplyr::select(-lac_cache, -breed_status, -lac_end, -fieldBDate)  # Remove the old 'lac_cache' column if no longer needed

#merge with midden cones to have three values of sex - f_lac, f_non_lac and M
mid_cones <- midden_cones %>%
  left_join(f %>% dplyr::select(year, squirrel_id, sex), by = c("year", "squirrel_id")) %>%
  mutate(sex = ifelse(sex.x == "F", sex.y, sex.x)) %>%
  dplyr::select(-sex.x, -sex.y, -cache_size_new, -total_cones) %>%
  na.omit()


# model -------------------------------------------------------------------
##filter for positive caching events only (i.e. only want cache size new > 0)
positive_caches <- mid_cones %>%
  filter(log_cache_size_new > 0)

#ensure sex is a factor and has the appropriate levels
positive_caches$sex <- factor(positive_caches$sex, levels = c("M", "f_non_lac", "f_lac"))

#fit the linear model
model_positive <- lm(log_cache_size_new ~ sex + log_total_cones, data = positive_caches)

#model summary
summary(model_positive)

#check residuals
par(mfrow = c(2, 2))
plot(model_positive)


# plot --------------------------------------------------------------------
#effect plot
effect_plot(model_positive, pred = sex,
            data = positive_caches,
            interval = TRUE, plot.points = TRUE,
            x.label = "Sex/Lactation Status",
            y.label = "Predicted Log Cache Size New",
            main.title = "Effect of Total Cones, Sex and Lactation Status on Log Cache Size New")

#generate predictions from model for plot
positive_caches$predicted_log_cache_size <- predict(model_positive, newdata = positive_caches, type = "response")

#violin plot of predictions
ggplot(positive_caches, aes(x = sex, y = predicted_log_cache_size, fill = sex)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9), outlier.shape = NA) +
  geom_jitter(aes(y = predicted_log_cache_size), color = "black", size = 0.5, alpha = 0.6) +
  scale_fill_manual(values = c("M" = "#66c2a5", "f_non_lac" = "#fc8d62", "f_lac" = "#8da0cb")) +
  labs(
    title = "Predicted Log Cache Size by Sex Category",
    x = "Sex Category",
    y = "Predicted Log Cache Size (New)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




