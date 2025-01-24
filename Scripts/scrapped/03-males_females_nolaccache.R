#load packages
source("Scripts/00-packages.R")

#connect to database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#read in data tables
females <- read.csv("Input/females.csv")
midden_cones <- read.csv("Input/midden_cones.csv")

#filter for females who are NOT lac during caching
f_nolaccache <- females %>%
  filter(lac_cache == FALSE) %>%
  dplyr::select(year, squirrel_id)

#filter midden_cones to keep all males and only females in f_nolaccache
midden_cones_filtered <- midden_cones %>%
  filter(sex == "M" | squirrel_id %in% f_nolaccache$squirrel_id) %>%
  dplyr::select(year, grid, squirrel_id, sex, log_cache_size_new, log_total_cones)

#add a cache rate column - to control for cone crop
midden_cones_filtered <- midden_cones_filtered %>%
  mutate(cache_rate = log_cache_size_new / log_total_cones)

# model -------------------------------------------------------------------
##filter for positive caching events only (i.e. only want cache size new > 0)
positive_caches <- midden_cones_filtered %>%
  filter(log_cache_size_new > 0)

model_positive <- lm(log_cache_size_new ~ sex + log_total_cones, data = positive_caches)

#model summary
summary(model_positive)

#check residuals
par(mfrow = c(2, 2))
plot(model_positive)


# plot --------------------------------------------------------------------
#violin plot
mfnolac <- ggplot(positive_caches, aes(x = sex, y = cache_rate, fill = sex)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.5) +
  labs(title = "Caching Rate by Sex",
       x = "Sex",
       y = "Cache Rate (Log New Cones Cached per Log Total Cones)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("M" = "#F8766D", "F" = "#00BFC4"))

mfnolac

#save
ggsave("Output/mfnolac.jpeg", plot = mfnolac, width = 8, height = 6)



















