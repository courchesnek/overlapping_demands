#load packages
source("Scripts/00-packages.R")

#connect to database
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

#birth years ----------------------------------------------------------
#flastall2 contains juveniles that were not tagged
byears <- tbl(con,"flastall2") %>%
  collect()

#keep only relevant columns
byears <- byears %>%
  dplyr::select(squirrel_id, gr, sex, byear) %>%
  na.omit()

#breeding status females -----------------------------------------------
breeding <- tbl(con, "litter") %>%
  collect()

breeding_f <- breeding %>%
  dplyr::select(squirrel_id, grid, br, yr) %>%
  rename(gr = grid,
         breed_status = br,
         year = yr) %>%
  mutate(sex = "F")

#all breeding status >1 should = 1, just care that she at least ATTEMPTED to breed
breeding_f <- breeding_f %>%
  filter(breed_status != 5) %>%
  mutate(breed_status = ifelse(breed_status > 1, 1, breed_status))

#year as character
breeding_f$year <- as.character(breeding_f$year)

#reorder columns
breeding_f <- breeding_f %>%
  dplyr::select(year, gr, squirrel_id, sex, breed_status)

#breeding status males - trapped as scrotal in a given year = breeding ---------------
##pull in trapping table
trap <- tbl(con,"trapping") %>%
  collect()

breeding_m <- trap %>%
  dplyr::select(squirrel_id, gr, date, sex, rep_con) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"), 
         year = format(date, "%Y")) %>%
  filter(sex == "M",
         rep_con <= 2) %>%
  rename(breed_status = rep_con) %>%
  dplyr::select(-date) %>%
  distinct(squirrel_id, year, .keep_all = TRUE)

#reorder columns
breeding_m <- breeding_m %>%
  dplyr::select(year, gr, squirrel_id, sex, breed_status)

#if male was NEVER trapped as SCR, breed_status = 0; if male was trapped at LEAST ONCE as SCR, breed_status = 1
breeding_m <- breeding_m %>%
  group_by(squirrel_id, year) %>% 
  mutate(breed_status = if_else(any(breed_status == 1), 1, 0)) %>%
  slice_head(n = 1) %>%  
  ungroup()

# join tables together ----------------------------------------------------
#join breeding_m and breeding_f
all_breeding <- bind_rows(breeding_f, breeding_m)

#now join with byears
breeding_age <- all_breeding %>%
  left_join(byears %>% dplyr::select(squirrel_id, byear), by = "squirrel_id") %>%  # Join by squirrel_id and retain only 'byear' from byears
  mutate(
    year = as.numeric(year),
    byear = as.numeric(byear),
      age_class = case_when(
      year - byear == 0 ~ "juvenile",   # born the same year
      year - byear == 1 ~ "yearling",   # 1 year old
      year - byear >= 2 ~ "adult"       # 2 or more years old
    )
  ) %>%
  dplyr::select(-byear)

#save
write.csv(breeding_age, "Input/breeding_age.csv", row.names = FALSE)













