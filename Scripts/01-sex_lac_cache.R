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
  filter(!(breed_status == 1 & is.na(fieldBDate))) %>% #remove breeders with no birthdate
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
  dplyr::select(-sex.x, -sex.y) %>%
  filter(grid %in% c("KL", "SU", "CH")) %>%
  na.omit() %>%
  dplyr::select(year, grid, squirrel_id, sex, cache_size_new, log_cache_size_new, total_cones, log_total_cones)

# model -------------------------------------------------------------------
#standardize predictors
mid_cones$total_cones_sc <- scale(mid_cones$total_cones)

#create a binary column for caching occurrence
mid_cones$cache_present <- ifelse(mid_cones$cache_size_new > 0, 1, 0)

#fit the binary model for caching occurrence - which is influenced by cone availability
model_binary <- glmmTMB(cache_present ~ total_cones_sc + (1 | squirrel_id) + (1 | year), 
                      data = mid_cones, 
                      family = binomial(link = "logit"))

summary(model_binary)

#residual plots
sim_res_binary <- simulateResiduals(model_binary, n = 2500)
plot(sim_res_binary)

testOutliers(sim_res_binary, type = "bootstrap") #no extreme outliers

##filter for positive caching events only (i.e. only want cache size new > 0)
positive_caches <- mid_cones %>%
  filter(cache_size_new > 0)

#ensure sex is a factor and has the appropriate levels (M as baseline)
positive_caches$sex <- factor(
  positive_caches$sex, 
  levels = c("M", "f_non_breeder", "f_weaned", "f_lac"))

#fit the gamma model for cache size among those years that squirrels do cache - which is now influenced by sex/lac status
model_positive <- glmmTMB(cache_size_new ~ sex + total_cones_sc + (1 | squirrel_id) + (1 | year), 
                        data = positive_caches, 
                        family = Gamma(link = "log"))

#model_reduced <- glmmTMB(cache_size_new ~ sex + total_cones_sc + (1 | squirrel_id), 
                         #data = positive_caches, 
                         #family = Gamma(link = "log"))

#anova(model_positive, model_reduced) - keep year random effect!!!

summary(model_positive)

#residual plots
sim_res_hurdle <- simulateResiduals(model_positive)
plot(sim_res_hurdle)

testOutliers(sim_res_hurdle, type = "bootstrap") #no extreme outliers

#clean up model output to save as csv
model_output <- tidy(model_positive)

model_output <- model_output %>%
  dplyr::select(-effect, -group, -component)

model_output <- model_output[-6, ]
model_output <- model_output[-6, ]

model_output <- model_output %>%
  rename(zvalue = statistic,
         pvalue = p.value)

model_output <- model_output %>%
  mutate(across(c(estimate, std.error, zvalue, pvalue), ~round(., 4)))

write.csv(model_output, "Output/model_output.csv", row.names = FALSE)

# generate predictions and compare between sexes --------------------------
emm <- emmeans(model_positive, ~ sex, 
               at = list(total_cones_sc = 0), 
               type = "response")

df_emm <- as.data.frame(emm)

##compare between female groups
comp <- pairs(emm, adjust = "tukey", infer = c(TRUE, TRUE))
comp_df <- as.data.frame(comp)

#rename columns
comp_df <- comp_df %>%
  rename(
    comparison = contrast,
    std.error = SE,
    lowerCI = asymp.LCL,
    upperCI = asymp.UCL,
    z.value = z.ratio) %>%
  dplyr::select(-df, -null) %>%
  mutate(across(c(ratio, std.error, lowerCI, upperCI, z.value, p.value), ~round(., 4)))
   
#save
write.csv(comp_df, "Output/groups_comparisons.csv", row.names = FALSE)

# plot predictions -------------------------------------------
effect_plot <- ggplot(df_emm, aes(x = sex, y = response, color = sex, fill = sex, shape = sex)) +
  geom_point(size = 6, stroke = 1.2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3, linewidth = 1.8) +
  scale_x_discrete(
    labels = c(
      "M" = "Males",
      "f_non_breeder" = "Female:Non-Breeder",
      "f_weaned" = "Female:Weaned",
      "f_lac" = "Female:Lactating")) +
  labs(
    x = "Sex",
    y = "Number of New Cones Cached",
    title = "Effect of Sex and Lactation Status on Cone Caching\nAdjusted by Cone Availability") +
  scale_color_manual(
    values = c(
      "M" = "#E69F00", 
      "f_non_breeder" = "#56B4E9", 
      "f_weaned" = "#009E73", 
      "f_lac" = "#CC79A7")) +
  scale_shape_manual(values = c(
      "M"              = 16,  
      "f_non_breeder" = 17, 
      "f_weaned"      = 15, 
      "f_lac"   = 23)) +
  scale_fill_manual(values = c(
    "M"            = "#E69F00", 
    "f_non_breeder"= "#56B4E9", 
    "f_weaned"     = "#009E73", 
    "f_lac"        = "#CC79A7")) +
  theme_minimal(base_size = 25) +
  theme(
    text = element_text(size = 22),
    plot.title = element_text(size = 25, face = "bold", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(size = 18),
    axis.title.x = element_text(margin = margin(t = 15), size = 22),
    axis.title.y = element_text(margin = margin(r = 15), size = 22),
    axis.text.x = element_text(hjust = 0.5, color = "black"),
    axis.text.y = element_text(color = "black"),
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
    legend.position = "none")

effect_plot

#save
ggsave("Output/effect_plot.jpeg", plot = effect_plot, width = 12, height = 7)

# data summary - raw data ------------------------------------------------------------
# 1) Sex-specific caching summaries
cache_by_sex <- positive_caches %>%
  filter(cache_size_new > 0 | !is.na(cache_size_new)) %>%
  group_by(sex) %>%
  summarise(
    n_cache         = sum(!is.na(cache_size_new)),
    mean_cache      = mean(cache_size_new, na.rm = TRUE),
    sd_cache        = sd(cache_size_new, na.rm = TRUE),
    se_cache        = sd_cache / sqrt(n_cache),
    tcrit_cache     = ifelse(n_cache > 1, qt(0.975, df = n_cache - 1), NA_real_),
    ci_low_cache    = mean_cache - tcrit_cache * se_cache,
    ci_high_cache   = mean_cache + tcrit_cache * se_cache,
    min_cache       = ifelse(n_cache > 0, min(cache_size_new, na.rm = TRUE), NA_real_),
    max_cache       = ifelse(n_cache > 0, max(cache_size_new, na.rm = TRUE), NA_real_)) %>%
  mutate(across(c(mean_cache, sd_cache, se_cache, ci_low_cache, ci_high_cache,
                  min_cache, max_cache),
                ~ round(.)))

# 2) year-level cone availability (n = 14)
avail_year <- positive_caches %>%
  distinct(year, total_cones) %>%       # one value per year
  summarise(
    n_years   = n(),
    mean_av   = mean(total_cones, na.rm = TRUE),
    sd_av     = sd(total_cones, na.rm = TRUE),
    se_av     = sd_av / sqrt(n_years),
    tcrit     = ifelse(n_years > 1, qt(0.975, df = n_years - 1), NA_real_),
    ci_lo_av  = mean_av - tcrit * se_av,
    ci_hi_av  = mean_av + tcrit * se_av,
    min_av    = min(total_cones, na.rm = TRUE),
    max_av    = max(total_cones, na.rm = TRUE)) %>%
  mutate(across(c(mean_av, sd_av, se_av, ci_lo_av, ci_hi_av,
                  min_av, max_av),
                ~ round(.)))

#save summary tables
write.csv(cache_by_sex, file = "Output/cache_summary.csv", row.names = FALSE)
write.csv(avail_year, file = "Output/treecones_summary.csv", row.names = FALSE)

#how many years of data?
length(unique(positive_caches$year))
unique(positive_caches$year)

#how many grids?
length(unique(positive_caches$grid))

#how many squirrels?
length(unique(positive_caches$squirrel_id))

# dummy plot for predictions ----------------------------------------------
# dummy_data <- tibble(
#   sex = c("Males", "Female:Non-Breeder", "Female:Weaned", "Female:Lactating"),
#   predicted  = c(3000, 3000, 3000, 1500),
#   lower      = c(2800, 2800, 2800, 1300),
#   upper      = c(3200, 3200, 3200, 1700)) %>%
#   mutate(sex = factor(c("Males", "Female:Non-Breeder", "Female:Weaned", "Female:Lactating"),
#                  levels = c("Males", "Female:Non-Breeder", "Female:Weaned", "Female:Lactating")))
# 
# dummy_plot <- ggplot(dummy_data, aes(x = sex, y = predicted, colour = sex)) +
#   geom_point(size = 4) +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
#   labs(x = "Sex",
#        y = "Number of New Cones Cached",
#        title = "Effect of Sex and Lactation Status on Cone Caching\nAdjusted by Cone Availability") +
#   theme_minimal() +
#   theme(text = element_text(size = 22),
#         plot.title = element_text(size = 25, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 18),
#         axis.title.x = element_text(margin = margin(t=15), size = 22),
#         axis.title.y = element_text(margin = margin(r=15), size = 22),
#         axis.text.x = element_text(hjust = 0.5, color = "black"),
#         axis.text.y = element_text(color = "black"),
#         plot.margin = margin(t = 20, r = 20, b = 10, l = 20),
#         legend.position = "none")
# 
# dummy_plot
# 
# 
# #save
# ggsave("Output/dummy_plot.jpeg", plot = dummy_plot, width = 12, height = 7)
# 
# 





