###############################################################################.
##                      PNA LOCALITIES DATA PREP                             ##
###############################################################################.

# Code to be sourced in to Quarto word script
# Loads required packages, data, sets custom themes, creates function, etc.
# Preps data for each locality

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
library(here) #project directory
library(tidylog) #log results of tidyverse functions
library(english) #convert numbers to text

# set the month that pharmacy data relates to
pharm_data_month <- "May 2025"

# set years for population data
lsoa_pop_year <- 2022
high_lvl_pop_year <- 2023

# number of pharmacies in the South West in the month of data specified above
# provided by Les Riggs at NHS South West Collaborative Commissioning Hub
n_pharm_sw <- 919

## colour palette
palette <- c("#1C1F63", "#10CFFB" , "#9EF101","#8605E4")

## plot custom themes
custom_theme <- function(){theme_minimal() %+replace% theme(
  
  # Default text
  text = element_text(family = "sans"),
  
  #Axis titles & text
  axis.title.x = element_text(size = 14, face = "bold",
                              margin = margin(t = 0, r = 20, b = 0, l = 0)), 
  axis.title.y = element_text(size = 14, face = "bold", angle = 90,
                              margin = margin(t = 0, r = 20, b = 0, l = 0)), 
  axis.text = element_text(size = 12),
  
  #Plot box and grid
  panel.grid = element_blank(),
  panel.grid.major.x = element_line(linetype = "dotted", size = 1, color = "grey80"), 
  
  # Legend
  legend.justification = "left", 
  legend.direction = "horizontal", 
  legend.position = "top", 
  legend.location = "plot",
  legend.title = element_text(face="bold", color = "grey20", size = 12),
  legend.text = element_text(size = 12)
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
pharmacies <- read_xlsx(here("Data", "20250519 Snapshot CP lists BNSSG.xlsx"),
                        skip = 1) %>% 
  clean_names %>% 
  mutate(postcode = str_remove_all(postcode, " ")) %>% 
  left_join(pc_lsoa_loc_lookup, by = "postcode") %>% 
  relocate(ods_code, contractor_name, "name" = trading_name_if_different, icb,
           health_and_wellbeing_board:lsoa_2021_code) %>% 
  #rename flu column
  rename("flu" = flu_24_25)


## Dispensing practices
disp_prac <- read_xlsx(here("Data", "20250117 Dispensing Practices BNSSG.xlsx")) %>% 
  clean_names %>% 
  
  select(practice_code, practice_address_main_site, dispensing_address_es) 


## Read in pharmacy dispensing data for most full year of data (currently 2023/24)
# Each month needs to be imported separately and appended
disp_data <- mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2023-07/Dispensing%20Data%20Apr%2023%20-%20CSV.csv"),
                                month = 1) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2023-08/Dispensing%20Data%20May%2023%20-%20CSV.csv"),
                   month = 2)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2023-09/Dispensing%20Data%20June%2023%20-%20CSV.csv"),
                   month = 3)) %>%   
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2023-10/Dispensing%20Data%20July%2023%20-%20CSV.csv"),
                   month = 4)) %>%  
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2023-11/Dispensing%20Data%20Aug%2023%20-%20CSV.csv"),
                   month = 5)) %>%  
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2023-12/Dispensing%20Data%20Sep%2023%20-%20CSV.csv"),
                   month = 6)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2024-01/Dispensing%20Data%20Oct%2023%20-%20CSV.csv"),
                   month = 7)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2024-02/Dispensing%20Data%20Nov%2023%20-%20CSV.csv"),
                   month = 8)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2024-05/Dispensing%20Data%20Dec%2023%20-%20CSV.csv"),
                   month = 9)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2024-05/Dispensing%20Data%20Jan%2024%20-%20CSV.csv"),
                   month = 10)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2024-05/Dispensing%20Data%20Feb%2024%20v2%20-%20CSV.csv"),
                   month = 11)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2024-07/Dispensing%20Data%20Mar%2024%20-%20CSV%20-%20update.csv"),
                   month = 12)) %>% 
  select(month, ContractorCode, Postcode, 
         NumberofItems, 
         `NumberofNewMedicineService(NMS)interventionsdeclared`) %>% 
  clean_names %>% 
  
  # Join Locality lookup with postcode (not pharmacy codes as these change)
  mutate(postcode = str_remove_all(postcode, " ")) %>% 
  left_join(pc_lsoa_loc_lookup) %>% 
  drop_na(pna_locality) %>% 
  mutate(fy = "2023/24")

## Dispensing data for England and SW - done manually in Excel!
disp_eng_sw <- read_xlsx(here("Data", "dispensing_data_England_SW.xlsx"))

## Latest populations data at LSOA level
#https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates

pops <- read_xlsx(here("Data", "mype_lsoa.xlsx"),
                  sheet = paste0("Mid-", lsoa_pop_year, " LSOA 2021"),
                  skip = 3) %>% 
  clean_names %>% 
  # Join/filter relevant PNA localities
  inner_join(lsoa_loc_lookup)


## Latest populations data for LA, Region, and England
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales

pops_high_lvl <- read_xlsx(here("Data", "mype_la_sw_england.xlsx"),
                             sheet = "MYE2 - Persons",
                             skip = 7) %>% 
  clean_names %>% 
  filter(geography %in% c("County", "Unitary Authority") | 
           name == "ENGLAND" | name == "SOUTH WEST") %>% 
  mutate(name = str_to_title(name), 
         name = str_remove_all(name, ", City Of")) %>% 
  select(code, name, geography, "pop" = all_ages) 
  


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



###################### SECTION 3: DATA MANIPULATION ############################

## 1) Locality Overview ----

## 1.1 Age/Sex Breakdown ----

age_sex_locality <- pops %>% 
  
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
  left_join(pops) %>% 
  
  # Count populations by quintile
  # convert IMD to factor and prevent R from deleting rows when grouping (drop = F)
  # This is in case some areas have no LSOAs in a given IMD quintile
  mutate(imd_quintile_local = as.factor(imd_quintile_local)) %>% 
  group_by(pna_locality, imd_quintile_local, .drop = F) %>% 
  summarise(pop = sum(total)) %>% 
  ungroup


# Get populations by national IMD quintile in each locality
dep_national <- imd %>% 
  
  left_join(pops) %>% 
  
  mutate(imd_quintile_national = as.factor(imd_quintile_national)) %>% 
  group_by(pna_locality, imd_quintile_national, .drop = F) %>% 
  summarise(pop = sum(total)) %>% 
  ungroup


## 2) Pharmacy Overview ----

## 2.1 Pharmacy types summary ----

## Dispensing practices (and branches of practices)
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
  mutate(chain = if_else(name %in% pharm_chains$name, 1, 0),
         independ = if_else(chain == 1, 0, 1)) %>% 
  
  # convert "Yes" to 1 and "No"/NA to 0 for counting
  mutate_at(vars(dac:appliance_usage_reviews, specialist_medicines_provider), 
            ~replace_na(if_else(. == "Yes", 1, 0), 0)) %>% 
  
  # add column for total and count
  mutate(total = 1) %>% 
  group_by(pna_locality) %>% 
  summarise_at(vars(total, dac:appliance_usage_reviews, 
                    specialist_medicines_provider, chain, independ), sum) %>% 
  ungroup %>% 
  
  left_join(disp_prac_main) %>% 
  left_join(disp_prac_branch) %>% 
  mutate_all(~replace_na(.,0))


## Subset for 100h pharmacies
pharm_100h <- pharmacies %>%
  filter(x100_hour_contract == "Yes") %>% 
  mutate(name_and_address = paste(name, 
                                  address,
                                  town,
                                  sep = ", ")) %>% 
  select(pna_locality, name_and_address)


## 2.2 Pharmacy provision table ----

# Get locality populations
loc_pops <- pops %>% 
  
  group_by(pna_locality) %>% 
  summarise(pop = sum(total)) %>% 
  ungroup


# Pharmacies by locality
pharm_rate_loc <- pharmacies %>% 
  group_by(pna_locality) %>% 
  count %>% 
  ungroup %>% 
  left_join(loc_pops) %>% 
  select("area" = pna_locality, "n_pharm" = n, pop) 

# Pharmacies by LA & SW
pharm_rate_high_lvl <- tibble(area = "South West", n_pharm = n_pharm_sw)

pharm_rate_high_lvl <- pharmacies %>% 
  group_by(local_authority) %>% 
  count %>% 
  ungroup %>% 
  rename("area" = local_authority, "n_pharm" = n) %>% 
  bind_rows(pharm_rate_high_lvl) %>% 
  left_join(pops_high_lvl, by = c("area" = "name")) %>% 
  select(-code, -geography)

# Bind areas and calculate rates
pharm_rate <- pharm_rate_loc %>% 
  bind_rows(pharm_rate_high_lvl) %>% 
  
  # calculate rates
  mutate(rate_pharm = round_half_up(n_pharm/pop*100000, 1)) %>% 
  
  # add commas for large numbers
  mutate_if(is.numeric, ~format(., big.mark = ",")) %>% 
  mutate_all(~str_replace(., "NA", "-")) %>% 
  
  select("Area" = area,
         "Population" = pop,
         "Number of pharmacies" = n_pharm,
         "Pharmacies per 100,000 population" = rate_pharm)



## 2.3 Pharmacy dispensing data table ----

# Aggregate data to locality level

disp_data_loc <- disp_data %>%
  
  # get total items by locality and FY
  group_by(fy, pna_locality) %>% 
  summarise(n_items = sum(numberof_items),
            n_nms = sum(numberof_new_medicine_service_nms_interventionsdeclared),
            n_months = last(month)) %>% 
  ungroup %>% 
  
  # keep only most recent complete year
  filter(n_months == 12) %>% 
  filter(fy == last(fy)) %>% 
  
  # add populations
  left_join(loc_pops) %>% 
  
  rename("area" = pna_locality)


# Aggregate again for LAs (latest year only)
disp_data_la <- disp_data %>%
  
  # get total items in last FY
  group_by(fy, local_authority) %>% 
  summarise(n_items = sum(numberof_items),
            n_months = last(month)) %>% 
  ungroup %>% 
  rename("area" = local_authority) %>% 
  filter(n_months == 12) %>% 
  filter(fy == last(fy))


# Bind with England/SW data and add on populations
disp_data_high_lvl <- bind_rows(disp_data_la, disp_eng_sw) %>% 
  left_join(pops_high_lvl, by = c("area" = "name"))

# combine with England/SW data and calculate rates
disp_table <- disp_data_loc %>%
  
  # Add on data for SW and England for latest full year of data
  bind_rows(disp_data_high_lvl) %>% 
  
  # calculate rates
  mutate(rate_items = round_half_up(n_items/pop, 1)) %>% 

  # add commas for large numbers
  mutate_if(is.numeric, ~format(., big.mark = ",")) %>% 
  mutate_all(~str_replace(., "NA", "-")) %>% 
  
  select("Area" = area,
         "Population" = pop,
         "Number of dispensed items" = n_items,
         "Items dispensed per head" = rate_items)




## 3) Access to essential services ----

# Create table for plot
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
  separate_rows(hours, sep = ",") %>%  
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

# Create table for summary bulletpoints
pharm_hours_summary <- pharm_hours %>% 
  drop_na(open_time) %>% 
  group_by(ods_code, pna_locality, day_of_week) %>% 
  summarise(open_time = min(open_time),
            close_time = max(close_time)) %>% 
  ungroup %>% 
  arrange(ods_code, desc(day_of_week)) %>% 
  mutate(open_before8_flag = if_else(open_time < ymd_hms("1970-01-01 08:00:00"), 
                                     1, 0),
         open_past630_flag = if_else(close_time > ymd_hms("1970-01-01 18:30:00"), 
                                     1, 0)) %>% 
  group_by(ods_code, pna_locality) %>% 
  summarise(days_open = n(),
         days_open_before8 = sum(open_before8_flag),
         days_open_past630 = sum(open_past630_flag)) %>% 
  ungroup 

