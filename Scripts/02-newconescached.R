#load packages
source("Scripts/00-packages.R")

#connection to KRSP databases
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

con_suppl <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                           dbname ="krsp_suppl",
                           username = Sys.getenv("krsp_user"),
                           password = Sys.getenv("krsp_password"))

#pull in tables
##the supplementary tables are not updated in the annual data cleanup, so squirrel_id values must be updated from the historic_squirrel_ids table
historic_ids<- tbl(con, "historic_squirrel_ids") %>% 
  dplyr::select(old_squirrel_id, new_squirrel_id) %>% 
  collect()

flastall <- tbl(con, "flastall2") %>% 
  #flastall2 contains juveniles that were not tagged
  # exclusions
  filter(gr %in% c("SU", "KL", "CH", "AG", "LL", "JO", "RR", "SX")) %>% 
  dplyr::select(squirrel_id, sex, byear) %>% 
  collect()

midden_cones <-tbl(con_suppl, "midden_cones") %>% 
  filter(squirrel_id !="UTS") %>% #remove UTS squirrels from data
  collect() %>% 
  left_join (historic_ids, by=c("squirrel_id" = "old_squirrel_id")) %>% 
  mutate (squirrel_id = ifelse(is.na(new_squirrel_id), squirrel_id, new_squirrel_id),
          date = as.POSIXct(Date, format = '%Y-%m-%d %H:%M:%S')) # date looks to be in datetime format

midden_cones <- midden_cones %>% 
  mutate (squirrel_id = ifelse(squirrel_id == 19851, 19537, squirrel_id),
          squirrel_id = ifelse(squirrel_id == 19911, 11895, squirrel_id))


# calculate cache sizes ---------------------------------------------------
midden_cones <- midden_cones %>%   
  mutate(total_new_2019 = total_newclosed + total_newopen,
         total_new = coalesce(total_new, total_new_2019),
         total_cones = total_old + total_new,
         total_cones2 = total_old + total_newclosed,
         cache_size_total = (total_cones / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_total2 = (total_cones2 / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_new = (total_new / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_new_closed = (total_newclosed / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_new_open = (total_newopen / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         cache_size_old = (total_old / no_quad) * ((pi * (width / 2) * (length / 2)) / area_quad),
         squirrel_id = as.numeric(as.character(squirrel_id))) %>%  # Needed to match variable types.  The as.character is needed otherwise the squirrel_ids get recoded as 1...n
  group_by(squirrel_id, year) %>% # This line of code and the one below removes cases where a squirrel owns more than one midden
  slice(which.max(cache_size_total)) %>% # keeps the midden with more cones
  dplyr::select(year, grid, midden, date, locx, locy, squirrel_id, cache_size_total, cache_size_total2, cache_size_new_open, cache_size_new_closed, cache_size_new, cache_size_old) 

midden_cones <- midden_cones %>% 
  left_join(flastall, by="squirrel_id")

midden_cones <- midden_cones %>%
  dplyr::select(-date, -cache_size_total2, -cache_size_new_open, -cache_size_new_closed, -byear, -cache_size_total, -cache_size_old, -midden, -locx, -locy)

# add in cone crop --------------------------------------------------------
tree_cones <-tbl (con, "cones") %>%
  filter(Year>=1988) %>%
  collect()  %>%
  mutate(Year = as.numeric(Year), 
         NumNew = as.numeric(NumNew),
         cone_index = log(NumNew + 1),
         total_cones = 1.11568 * exp(0.1681 + 1.1891 * log(NumNew + 0.01))) # according to Krebs et al. 2012

#mean cones per year
yearly_cones <- group_by(tree_cones, Year) %>% 
  dplyr::summarize(num_trees = sum(!is.na(NumNew)),
            cone_counts = mean(NumNew, na.rm = TRUE),
            cone_index = mean(cone_index, na.rm = TRUE),
            total_cones = mean(total_cones, na.rm = TRUE))

#must add in data for 1989 because there were no cones but the zeros were not entered
yearly_cones <- rbind(yearly_cones,
                      list(1989L, 0, 0, 0, 0.005525156)) %>%
  rename(year = Year)

#merge with midden_cones
midden_cones <- midden_cones %>%
  left_join(yearly_cones %>% dplyr::select(year, total_cones), by = "year") %>%
  na.omit()

#reorder columns
midden_cones <- midden_cones %>%
  dplyr::select(year, grid, squirrel_id, sex, cache_size_new, total_cones)

# data transformation - since non-normal distribution ---------------------
##log transformations - +1 because data contains zeros and cannot do log(0)
midden_cones$log_cache_size_new <- log(midden_cones$cache_size_new + 1)
midden_cones$log_total_cones <- log(midden_cones$total_cones + 1)

#save
write.csv(midden_cones, "Input/midden_cones.csv", row.names = FALSE)

# males cache exactly how much more? --------------------------------------
# males cache exactly how much more? --------------------------------------
#scale the cache size by the total cones produced in each year
midden_cones$scaled_cache_size <- midden_cones$cache_size_new / midden_cones$total_cones

avg_scaled_cache <- midden_cones %>%
  group_by(sex) %>%
  summarise(mean_scaled_cache = mean(scaled_cache_size, na.rm = TRUE))

ratio_males_to_females <- avg_scaled_cache$mean_scaled_cache[avg_scaled_cache$sex == "M"] / 
  avg_scaled_cache$mean_scaled_cache[avg_scaled_cache$sex == "F"]

ratio_males_to_females
#across years (i.e. cone crops) males cache approximately 2.08 times as many new cones as females do. 
















