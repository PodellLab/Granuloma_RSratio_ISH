# Load required packages
library(tidyverse)

# Read in clean data
ISH_IHC <- read.csv("data/IHC_ISH_clean_data_WIP.csv", 
                    stringsAsFactors = TRUE)  %>%  
  mutate(Timepoint = fct_relevel(Timepoint, c("D56", "D114"))) %>%
  mutate(Vaccination = fct_relevel(Vaccination, c("SAL", "BCG"))) %>%
  mutate(Timepoint = fct_recode(Timepoint, "Day 56" = "D56", "Day 114" = "D114")) %>%
  mutate(Vaccination = fct_recode(Vaccination, "Unvaccinated" = "SAL")) %>% 
  mutate(Vaccination = fct_relevel(Vaccination, c("Unvaccinated", "BCG"))) %>% 
  arrange(Vaccination, Timepoint)

# Get the values for "n" at the top of the table
ISH_IHC %>% 
  group_by(Timepoint, Vaccination) %>% 
  count()

ISH_IHC %>% 
  group_by(Timepoint) %>% 
  count()

# Get the medians within each Timepoint and Vaccination
ISH_IHC %>% 
  group_by(Timepoint, Vaccination) %>% 
  summarize(CD4_count = median(CD4_count), 
            CD8_count = median(CD8_count), 
            B220_count = median(B220_count), 
            DAPI_count = median(DAPI_count), 
            total_host_count = median(total_host_count), 
            rep_tb = median(rep_tb), 
            nonrep_tb = median(nonrep_tb), 
            total_tb = median(total_tb))

# Get the overall medians within each Timepoint 
ISH_IHC %>% 
  group_by(Timepoint) %>% 
  summarize(CD4_count = median(CD4_count), 
            CD8_count = median(CD8_count), 
            B220_count = median(B220_count), 
            DAPI_count = median(DAPI_count), 
            total_host_count = median(total_host_count), 
            rep_tb = median(rep_tb), 
            nonrep_tb = median(nonrep_tb), 
            total_tb = median(total_tb))

# Get percent replicating TB for each Timepoint and Treatment
ISH_IHC %>% 
  group_by(Timepoint, Vaccination) %>% 
  summarize(rep_tb = median(rep_tb), 
            total_tb = median(total_tb), 
            perc_rep = rep_tb / total_tb * 100)

# Get percent replicating TB for each Timepoint 
ISH_IHC %>% 
  group_by(Timepoint) %>% 
  summarize(rep_tb = median(rep_tb), 
            total_tb = median(total_tb), 
            perc_rep = rep_tb / total_tb * 100)
  
