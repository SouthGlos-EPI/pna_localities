## Reading in dispensing data from NHSBSA and appending into one RDS data file

library(tidyverse)
library(janitor)
library(here)

disp2122 <- mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2021-08/Dispensing%20Data%20Apr%2021%20-%20CSV%20v2.csv"),
                   month = 1) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2021-08/Dispensing%20Data%20May%2021%20%20%E2%80%93%20CSV.csv"),
                   month = 2)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2021-09/Dispensing%20Data%20Jun%2021%20%20%E2%80%93%20CSV.csv"),
                   month = 3)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2021-10/Dispensing%20Data%20Jul%2021%20%20%E2%80%93%20CSV.csv"),
            month = 4)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2021-11/Dispensing%20Data%20Aug%2021%20%E2%80%93%20CSV.csv"),
            month = 5)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2022-01/Dispensing%20Data%20Sep%2021%20%E2%80%93%20CSV.csv"),
            month = 6)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2022-02/Dispensing%20Data%20Oct%2021%20-%20CSV.csv"),
            month = 7)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2022-02/Dispensing%20Data%20Nov%2021%20-%20CSV.csv"),
            month = 8)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2022-03/Dispensing%20Data%20Dec%2021%20%E2%80%93%20CSV.csv"),
            month = 9)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2022-04/Dispensing%20Data%20Jan%2022%20%E2%80%93%20CSV.csv"),
            month = 10)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2022-05/Dispensing%20Data%20Feb%2022%20%E2%80%93%20CSV.csv"),
            month = 11)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2022-06/Dispensing%20Data%20Mar%2022%20%E2%80%93%20CSV.csv"),
            month = 12)) %>% 
  select(month, ContractorCode, Postcode, 
         NumberofItems, 
         `NumberofNewMedicineService(NMS)interventionsdeclared`) %>% 
  clean_names %>% 
  mutate(fy = "2021/22")


disp2223 <- mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2023-07/Dispensing%20Data%20Apr%2023%20-%20CSV.csv"),
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
  mutate(fy = "2022/23")

disp2324 <- mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2024-07/Dispensing%20Data%20Apr%2024%20-%20CSV.csv"),
                   month = 1) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2024-08/Dispensing%20Data%20May%2024%20-%20CSV.csv"),
            month = 2)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2024-09/Dispensing%20Data%20Jun%2024%20-%20CSV.csv"),
            month = 3)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2024-10/Dispensing%20Data%20Jul%2024%20-%20CSV.csv"),
            month = 4)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2024-11/Dispensing%20Data%20Aug%2024%20-%20CSV.csv"),
            month = 5)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2024-12/Dispensing%20Data%20Sep%2024%20-%20CSV.csv"),
            month = 6)) %>% 
  bind_rows(mutate(read_csv("https://www.nhsbsa.nhs.uk/sites/default/files/2025-01/Dispensing%20Data%20Oct%2024%20-%20CSV.csv"),
            month = 7)) %>% 
  select(month, ContractorCode, Postcode, 
         NumberofItems, 
         `NumberofNewMedicineService(NMS)interventionsdeclared`) %>% 
  clean_names %>% 
  mutate(fy = "2023/24")

disp <- bind_rows(disp2122, disp2223) %>% 
  bind_rows(disp2324) %>% 
  mutate(last_updated = Sys.Date())

saveRDS(disp, here("Data", "dispensing_data.rds"))
