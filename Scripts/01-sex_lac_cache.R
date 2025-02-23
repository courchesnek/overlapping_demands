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
    lac_end = if_else(br == 1, fieldBDate + 70, as.Date(NA)),
    caching_date = as.Date(paste0(yr, "-08-16"))) %>%
  rename(
    breed_status = br,
    year = yr) %>%
  filter(!(breed_status == 1 & is.na(fieldBDate))) %>%
  mutate(
    lac_cache = if_else(!is.na(lac_end) & lac_end > (caching_date + 7), TRUE, FALSE), #add a grace period for those who barely overlap with cache start - not true overlaps
    weaned_before_caching = if_else(breed_status == 1 & !lac_cache, TRUE, FALSE),
    non_breeder = if_else(breed_status == 0, TRUE, FALSE))
    
females <- females %>%
  filter(grid %in% c("KL", "SU", "CH"))

#save
write.csv(females, "Input/females.csv", row.names = FALSE)

# pull in male cache data -------------------------------------------------
midden_cones <- read.csv("Input/midden_cones.csv")

# prepare data to merge ---------------------------------------------------
#change female sex values to lac non lac in fall
f <- females %>%
  mutate(sex = case_when(
    lac_cache == TRUE ~ "f_lac",
    weaned_before_caching == TRUE ~ "f_weaned",
    non_breeder == TRUE ~ "f_non_breeder",
    TRUE ~ NA_character_)) %>%
  dplyr::select(-lac_cache, -weaned_before_caching, -non_breeder, -breed_status, -lac_end, -fieldBDate)

#merge with midden cones to have four values of sex - f_lac, f_weaned, f_non_breeder and M
mid_cones <- midden_cones %>%
  left_join(f %>% dplyr::select(year, squirrel_id, sex), by = c("year", "squirrel_id")) %>%
  mutate(sex = ifelse(sex.x == "F", sex.y, sex.x)) %>%
  dplyr::select(-sex.x, -sex.y, -cache_size_new, -total_cones) %>%
  filter(grid %in% c("KL", "SU", "CH")) %>%
  na.omit()

# model -------------------------------------------------------------------
#create a binary column for caching occurrence
mid_cones$cache_present <- ifelse(mid_cones$log_cache_size_new > 0, 1, 0)

#fit a mixed logistic regression model (binary outcome)
model_binary <- glmer(cache_present ~ sex + (1 | squirrel_id), 
                      data = mid_cones, 
                      family = binomial(link = "logit"),
                      control = glmerControl(optimizer = "bobyqa")) #helps convergence

#model summary
summary(model_binary)

##filter for positive caching events only (i.e. only want cache size new > 0)
positive_caches <- mid_cones %>%
  filter(log_cache_size_new > 0)

#ensure sex is a factor and has the appropriate levels (M as baseline)
positive_caches$sex <- factor(
  positive_caches$sex, 
  levels = c("M", "f_non_breeder", "f_weaned", "f_lac"))

#fit mixed-effects model with squirrel_id as a random intercept
model_positive <- lmer(log_cache_size_new ~ sex + log_total_cones + (1 | squirrel_id), 
                       data = positive_caches, 
                       REML = FALSE)

#model summary
summary(model_positive)

#model reference categories?
contrasts(positive_caches$sex) #males are the reference category

plot(model_positive)  #residual plots
qqnorm(resid(model_positive)); qqline(resid(model_positive))

#get p-values
model_positive_test <- lmer(log_cache_size_new ~ sex + log_total_cones + (1 | squirrel_id),
                            data = positive_caches)

summary(model_positive_test)

#clean up model output to save as csv
model_output <- tidy(model_positive_test)

model_output <- model_output %>%
  dplyr::select(-effect, -group)

model_output <- model_output[-6, ]

model_output <- model_output %>%
  rename(tvalue = statistic,
         pvalue = p.value)

model_output <- model_output %>%
  mutate(across(c(estimate, std.error, tvalue, pvalue), ~round(., 3)))

write.csv(model_output, "Output/model_output.csv", row.names = FALSE)


# compare between females -------------------------------------------------
emm <- emmeans(model_positive, ~ sex)

#compare all pairs of sex categories
pairs(emm, adjust = "tukey")

# data summary ------------------------------------------------------------
summary_table <- positive_caches %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    mean_log_cache_size = mean(log_cache_size_new, na.rm = TRUE),
    sd_log_cache_size = sd(log_cache_size_new, na.rm = TRUE),
    mean_log_total_cones = mean(log_total_cones, na.rm = TRUE),
    sd_log_total_cones = sd(log_total_cones, na.rm = TRUE)) %>%
  arrange(factor(sex, levels = c("M", "f_non_breeder", "f_weaned", "f_lac")))

#save
write.csv(summary_table, file = "Output/data_summary.csv", row.names = FALSE)

#how many years of data?
length(unique(positive_caches$year))

#how many grids?
length(unique(positive_caches$grid))

#how many squirrels?
length(unique(positive_caches$squirrel_id))

# plot --------------------------------------------------------------------
#effect plot
effect_plot <- effect_plot(
  model = model_positive,
  pred = sex,
  x.label = "Sex and lactation status during the fall caching season",
  y.label = "Predicted number of new cones cached (log-transformed)",
  main.title = "Effect of Sex and Lactation Status on Cone Caching, Adjusted by Cone Availability",
  interval = TRUE,
  cat.interval.geom = "errorbar",
  colors = c("M" = "#F8766D", "f_non_breeder" = "#7CAE00", "f_weaned" = "#C77CFF","f_lac" = "#00BFC4")) + 
  scale_x_discrete(
    labels = c("M" = "Males", "f_non_breeder" = "Non-Breeding Females", "f_weaned" = "Weaned Females", "f_lac" = "Lactating Females")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

effect_plot

#save
ggsave("Output/effect_plot.jpeg", plot = effect_plot, width = 8, height = 6)















