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
  dplyr::select(yr, squirrel_id, br, fieldBDate, grid) %>%
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

females <- females %>%
  filter(grid %in% c("KL", "SU", "CH"))

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


# data summary ------------------------------------------------------------
summary_table <- positive_caches %>%
  group_by(sex) %>%
  summarise(
    n = n(),  # Sample size
    mean_log_cache_size = mean(log_cache_size_new, na.rm = TRUE),
    sd_log_cache_size = sd(log_cache_size_new, na.rm = TRUE),
    mean_log_total_cones = mean(log_total_cones, na.rm = TRUE),
    sd_log_total_cones = sd(log_total_cones, na.rm = TRUE)) %>%
  arrange(factor(sex, levels = c("M", "f_non_lac", "f_lac")))

#save
write.csv(summary_table, file = "Output/data_summary.csv", row.names = FALSE)

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
effect_plot <- effect_plot(
  model = model_positive,
  pred = sex,
  pred.labels = c("Males", "Non-Lactating Females", "Lactating Females"), 
  x.label = "Sex and lactation status during the fall caching season",
  y.label = "Predicted number of new cones cached (log-transformed)",
  main.title = "Effect of sex and lactation status on cone caching, adjusted for cone crop",
  colors = c("M" = "#66c2a5", "f_non_lac" = "#fc8d62", "f_lac" = "#8da0cb"),
  interval = TRUE,
  cat.interval.geom = "errorbar") +
  scale_x_discrete(
    labels = c("M" = "Males", "f_non_lac" = "Non-Lactating Females", "f_lac" = "Lactating Females")) +
  theme_minimal()

effect_plot

#save
ggsave("Output/effect_plot.jpeg", plot = effect_plot, width = 8, height = 6)


