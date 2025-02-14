###############################################################################.
##                      PNA LOCALITIES DATA PREP                             ##
###############################################################################.

# Code to be sourced in to Quarto word script
# Loads required packages, data, sets custom themes, creates function, etc.
# Preps data for each locality

# Written by Cecilia Puech (South Gloucestershire council)
# R 4.2.2
# Original date: 27/01/2025


############################ SECTION 1: SET UP #################################

## Packages 
library(tidyverse) #data manipulation, ggplot, etc.
library(janitor) #clean_names and round_half_up
library(readxl) #read in excel tables
library(lubridate) #working with dates
library(hms) #working with time
library(knitr) #kable tables, inserting images
library(PHEindicatormethods) #rates, confidence intervals, etc
library(here) #project directory
library(tidylog) #log results of tidyverse functions

## BNSSG healthier together colour palette?
palette <- c("#1C1F63", "#10CFFB" , "#9EF101","#8605E4",
               "#045EDA", "#E16CFF", "#008247")
               
## BNSSG ggplot theme
theme_bnssg <- function(base_size = 12, base_family = "sans",base_colour = "black"){theme_bw() %+replace% theme(
  
  #Axis titles & text
  axis.title.x = element_text(size = 16, color = '#1c1f63', face = 'bold', family = "sans", 
                              margin = margin(t = 0, r = 20, b = 0, l = 0)), 
  axis.title.y = element_text(size = 16, color = '#1c1f63', angle = 90, face = 'bold', 
                              family = "sans", margin = margin(t = 0, r = 20, b = 0, l = 0)),
  axis.text = element_text(size = 12,  family = "sans", color = 'black'), 
  
  #Plot box and grid
  panel.border = element_blank(), 
  panel.grid.major.y = element_blank(), 
  panel.grid.major.x = element_line(linetype = 'dotted', size = 1), 
  panel.grid.minor = element_blank(), 
  
  #Legend
  legend.justification='left', 
  legend.direction='horizontal', 
  legend.position = "top", 
  legend.location = "plot",
  legend.text = element_text(size = 12, family = "sans"),
  legend.title = element_text(size = 12, family = "sans", face="bold", color = "grey20"),
  
  # Title
  plot.title = element_text(size = 16, color = '#1c1f63', face="bold", 
                            margin = margin(b = 10, t=10), hjust=0),
  plot.title.position = "plot"
) 
}

# no scientific notation
options(scipen = 999)



######################## SECTION 2: DATA IMPORTS ###############################

## Import Postcode to LSOA 21 to PNA Locality Lookup
pc_lsoa_loc_lookup <- read_csv(here("Data", 
                                    "Postcode - LSOA21 - PNA Localities Lookup.csv"))

# Create version without postcodes
lsoa_loc_lookup <- pc_lsoa_loc_lookup %>% 
  select(-postcode) %>% 
  distinct


## Pharmacies
# Join with lookup to get official PNA Localities
pharmacies <- read_xlsx(here("Data", "20250129 Snapshot CP lists BNSSG.xlsx"),
                        skip = 1) %>% 
  clean_names %>% 
  mutate(postcode = str_remove_all(postcode, " ")) %>% 
  left_join(pc_lsoa_loc_lookup, by = "postcode")


## Dispensing practices
disp_prac <- read_xlsx(here("Data", "20250117 Dispensing Practices BNSSG.xlsx")) %>% 
  clean_names %>% 
  
  select(practice_code, practice_address_main_site, dispensing_address_es) 


## Read in dispensing data (collated in dispensing_data R script)
disp_data <- readRDS(here("Data", "dispensing_data.rds"))

## Populations data
# Loop reading in pops by year of data - source: ONS

# start with empty data frame
pops <- data.frame()

# read in each data sheet and append 
# note this method would have been more useful if more than 2 years available
for (year in c(2021:2022)) {

temp <- read_xlsx(here("Data", "sapelsoasyoatablefinal.xlsx"),
                  sheet = paste0("Mid-", year, " LSOA 2021"),
                  skip = 3) %>% 
  clean_names %>% 
  mutate(year = year) %>% #add column for year
  
  # Join/filter relevant PNA localities
  inner_join(lsoa_loc_lookup)

pops <- bind_rows(pops, temp)

}

# IMD lookup - source: OHID
imd <- read_xlsx(here("Data", "2021-lsoa-imd-lookup.xlsx"),
                 sheet = "IMD lookup",
                 skip = 5) %>% 
  clean_names %>% 
  select("lsoa_2021_code" = lsoa21cd, 
         "imd_quintile_national" = imd2019_quintiles_lsoa21_within_ctry09,
         "imd_quintile_local" = imd2019_quintiles_lsoa21_within_combined_ltla23) %>% 
  
  # Join/filter relevant PNA localities
  inner_join(lsoa_loc_lookup)


# tidy up
rm(temp, year)

###################### SECTION 3: DATA MANIPULATION ############################

## 1) Locality Overview ----

## 1.1 Age/Sex Breakdown ----

age_sex_locality <- pops %>% 
  
  # keep latest year of data
  filter(year == max(year)) %>%
  
  # Join/filter PNA localities
  inner_join(lsoa_loc_lookup) %>% 
  
  # extract age and sex as columns
  pivot_longer(f0:m90) %>% 
  mutate(sex = if_else(str_sub(name, 1, 1) == "f", "Female", "Male"),
         age = as.numeric(str_sub(name, 2))) %>% 
  
  # compute age bands
  mutate(age_band_5y = cut(age, c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 
                                  54, 59, 64, 69, 74, 79, 84, 89, Inf), 
                           
                            c("00-04", "05-09", "10-14", "15-19", "20-24", 
                              "25-29", "30-34", "35-39", "40-44", "45-49",
                              "50-54", "55-59", "60-64", "65-69", "70-74",
                              "75-79", "80-84", "85-89", "90+"), 
                           
                            include.lowest=TRUE)) %>% 
  
  # aggregate to locality
  group_by(lad_2021_code, lad_2021_name, pna_locality, sex, age_band_5y) %>% 
  summarise(pop = sum(value)) %>% 

  # get totals by locality, then % for each age/sex
  group_by(pna_locality) %>% 
  mutate(total_pop = sum(pop)) %>% 
  ungroup %>% 
  mutate(perc = round_half_up(pop/total_pop*100, 2))
  

# repeat on local authority
age_sex_la <- age_sex_locality %>% 
  group_by(lad_2021_code, lad_2021_name, sex, age_band_5y) %>% 
  summarise(pop = sum(pop)) %>% 

  # get totals by LA, then % for each age/sex
  group_by(lad_2021_code) %>% 
  mutate(total_pop = sum(pop)) %>% 
  ungroup %>% 
  mutate(perc = round_half_up(pop/total_pop*100, 2))


## 1.2 Deprivation ----

# Get populations by local IMD quintile in each locality
dep_local <- imd %>% 
  
  # Join LSOA populations
  left_join(filter(pops, 
                   year == max(year))) %>% 
  
  # Count populations by quintile
  # convert IMD to factor and prevent R from deleting rows when grouping (drop = F)
  # This is in case some areas have no LSOAs in a given IMD quintile
  mutate(imd_quintile_local = as.factor(imd_quintile_local)) %>% 
  group_by(pna_locality, imd_quintile_local, .drop = F) %>% 
  summarise(pop = sum(total)) %>% 
  ungroup


# Get populations by national IMD quintile in each locality
dep_national <- imd %>% 
  
  left_join(filter(pops, 
                   year == max(year))) %>% 
  
  mutate(imd_quintile_national = as.factor(imd_quintile_national)) %>% 
  group_by(pna_locality, imd_quintile_national, .drop = F) %>% 
  summarise(pop = sum(total)) %>% 
  ungroup


## 2) Pharmacy Overview ----

## Dispensing practices
disp_prac_main <- disp_prac %>% 
  drop_na(practice_code) %>% 
  
  # extract postcode from address (note: original doc needed manual cleaning of postcodes)
  mutate(
    postcode = str_remove_all(
      str_extract(practice_address_main_site, 
                  "\\b([A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2})\\b"), " ")) %>% 
  
  # join with locality lookup
  left_join(pc_lsoa_loc_lookup) %>% 
  
  group_by(pna_locality) %>% 
  summarise(disp_prac_main = n()) %>% 
  ungroup

disp_prac_branch <- disp_prac %>%
  
  # extract postcode from address (note: original doc needed manual cleaning of postcodes)
  mutate(
    postcode = str_remove_all(
      str_extract(dispensing_address_es, 
                  "\\b([A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2})\\b"), " ")) %>% 
  
  # join to locality
  left_join(pc_lsoa_loc_lookup) %>% 
  
  group_by(pna_locality) %>% 
  summarise(disp_prac_branch = n()) %>% 
  ungroup


## Create a subset for national pharmacy chains
# List of pharmacy chains provided by ChatGPT, may be more, check local data

pharm_chains <- pharmacies %>% 
  rename("name" = trading_name_if_different) %>% 
  filter(name %in%
           c("Boots Pharmacy", "Well Pharmacy", "Rowlands Pharmacy", "Day Lewis Pharmacy", 
             "Cohens Chemist", "Jhoots Pharmacy", "PillBox Chemists", "Tesco Pharmacy", 
             "Asda Pharmacy", "Sainsbury's Pharmacy", "Morrisons Pharmacy", "Lloyds Pharmacy",
             "Superdrug Pharmacy", "Kamsons Pharmacy", "Avicenna Pharmacy")) %>% 
  group_by(pna_locality, name) %>% 
  count %>% 
  ungroup %>% 
  arrange(desc(n))


## Total pharmacies and types, adding dispensing practices
pharm_types <- pharmacies %>% 
  
  # add in pharmacy ownership flags
  mutate(chain = if_else(trading_name_if_different %in% pharm_chains$name, 1, 0),
         independ = if_else(chain == 1, 0, 1)) %>% 
  
  # convert "Yes" to 1 and "No"/NA to 0 for counting
  mutate_at(vars(dac:lateral_flow_dist_service, specialist_medicines_provider), 
            ~replace_na(if_else(. == "Yes", 1, 0), 0)) %>% 
  
  # add column for total and count
  mutate(total = 1) %>% 
  group_by(pna_locality) %>% 
  summarise_at(vars(total, dac:lateral_flow_dist_service, 
                    specialist_medicines_provider, chain, independ), sum) %>% 
  ungroup %>% 
  
  left_join(disp_prac_main) %>% 
  left_join(disp_prac_branch) %>% 
  mutate_all(~replace_na(.,0))


## Subset for 100h pharmacies
pharm_100h <- pharmacies %>%
  filter(x100_hour_contract == "Yes") %>% 
  mutate(name_and_address = paste(trading_name_if_different, 
                                  address,
                                  town,
                                  sep = ", ")) %>% 
  select(pna_locality, name_and_address)


## Pharmacy provision table



## 3) Access to essential services ----

pharm_hours <- pharmacies %>% 
  select(ods_code, pna_locality, opening_hours_monday:opening_hours_sunday) %>% 
  pivot_longer(opening_hours_monday:opening_hours_sunday, 
               names_to = "day_of_week",
               values_to = "hours") %>% 
  
  # extract weekday and convert to factor (for chart)
  mutate(day_of_week = str_to_sentence(str_remove_all(day_of_week, 
                                                      "opening_hours_")),
    day_of_week = factor(day_of_week, 
                         levels = c("Sunday", "Saturday", "Friday", "Thursday",
                                    "Wednesday", "Tuesday", "Monday"))) %>% 
  
  #clean hours data
  mutate(hours = str_squish(hours),
         hours = str_replace_all(hours, " - ", "-"),
         hours = str_replace_all(hours, "- ", "-"),
         hours = str_replace_all(hours, " -", "-"),
         hours = str_to_sentence(hours)) %>%
  
  # split time ranges into separate rows, convert "closed" to NA
  separate_rows(hours, sep = " ") %>%  
  mutate(hours = if_else(hours == "Closed", NA, hours)) %>% 
  
  # split into opening and closing hours
  separate(hours, 
           into = c("open_time", "close_time"), 
           sep = "-", 
           convert = TRUE) %>% # Split into open/close
  
  # pad with 0 if missing
  mutate_at(c("open_time", "close_time"),
            ~str_pad(., width = 5, side = "left", pad = "0")) %>% 
  
  # convert to time
  mutate_at(c("open_time", "close_time"), 
            ~as.POSIXct(parse_time(., format = "%H:%M"), 
                        origin = "1970-01-01")) 

