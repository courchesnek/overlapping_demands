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
