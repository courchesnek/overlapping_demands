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

# compare model types: 1. zero-inflated gamma model and zero-inflated tweedie ---------------------------------------------------------
model_zi_gamma <- glmmTMB(
  cache_size_new ~ sex + (1 | squirrel_id) + (1 | year),
  ziformula = ~ total_cones,  #predictors for the zero inflation part
  data = mid_cones,
  family = ziGamma(link = "log"))

summary(model_zi_gamma)

#check residuals
sim_res <- simulateResiduals(model_zi_gamma)
plot(sim_res)

#test dispersion
testDispersion(sim_res)

#try polynimal
model_zi_gamma_poly <- glmmTMB(
  cache_size_new ~ sex + (1 | squirrel_id) + (1 | year),
  ziformula = ~ poly(total_cones, 2),   # 2nd-degree polynomial
  data = mid_cones,
  family = ziGamma(link = "log"))

summary(model_zi_gamma_poly)

res_poly <- simulateResiduals(model_zi_gamma_poly)
plot(res_poly)

#try spline
model_zi_gamma_spline <- glmmTMB(
  cache_size_new ~ sex + (1 | squirrel_id) + (1 | year),
  ziformula = ~ ns(total_cones, df = 3),  # natural spline
  data = mid_cones,
  family = ziGamma(link = "log"))

summary(model_zi_gamma_spline)

res_spline <- simulateResiduals(model_zi_gamma_spline)
plot(res_spline)

#try tweedie distribution - best AIC!! = tweedie distribution for non-zeros and zero-inflation component for zeros
model_zi_tweedie <- glmmTMB(
  cache_size_new ~ sex + (1 | squirrel_id) + (1 | year),
  ziformula = ~ total_cones,
  data = mid_cones,
  family = tweedie(link = "log"))

summary(model_zi_tweedie)

res_tweedie <- simulateResiduals(model_zi_tweedie)
plot(res_tweedie)

#try tweedie with polynomial
model_zi_gamma_poly_tweedie <- glmmTMB(
  cache_size_new ~ sex + (1 | squirrel_id) + (1 | year),
  ziformula = ~ poly(total_cones, 2),   # 2nd-degree polynomial
  data = mid_cones,
  family = tweedie(link = "log"))

summary(model_zi_gamma_poly_tweedie)

res_poly_tweedie <- simulateResiduals(model_zi_gamma_poly_tweedie)
plot(res_poly_tweedie)

#try tweedie with spline - convergence issues
#compare all gamma models
AIC(
  model_zi_gamma,
  model_zi_gamma_poly,
  model_zi_gamma_spline,
  model_zi_tweedie,            #best AIC
  model_zi_gamma_poly_tweedie)

# compare model types: 2. Hurdle ---------------------------------------------------------
#standardize predictors
mid_cones$total_cones_sc <- scale(mid_cones$total_cones)

#create a binary column for caching occurrence
mid_cones$cache_present <- ifelse(mid_cones$cache_size_new > 0, 1, 0)

#fit the binary model for caching occurrence - which is influenced by cone availability
model_binary <- glmer(cache_present ~ total_cones_sc + (1 | squirrel_id) + (1 | year), 
                      data = mid_cones, 
                      family = binomial(link = "logit"),
                      control = glmerControl(optimizer = "bobyqa")) #helps convergence

summary(model_binary)

#residual plots
sim_res_binary <- simulateResiduals(model_binary)
plot(sim_res_binary)

##filter for positive caching events only (i.e. only want cache size new > 0)
positive_caches <- mid_cones %>%
  filter(cache_size_new > 0)

#ensure sex is a factor and has the appropriate levels (M as baseline)
positive_caches$sex <- factor(
  positive_caches$sex, 
  levels = c("M", "f_non_breeder", "f_weaned", "f_lac"))

#fit the gamma model for cache size among those years that squirrels do cache - which is now influenced by sex/lac status
model_positive <- glmer(cache_size_new ~ sex + (1 | squirrel_id) + (1 | year), 
                       data = positive_caches, 
                       family = Gamma(link = "log"),
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))

summary(model_positive)

#residual plots
sim_res_hurdle <- simulateResiduals(model_positive)
plot(sim_res_hurdle)






