#load packages
source("Scripts/00-packages.R")

#connect to database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

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
















