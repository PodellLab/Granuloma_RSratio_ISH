# Load required packages
library(tidyverse)

# Input data that resulted from image analysis performed using Visiopharm 
# image analysis software to identify, count, and locate each immune or 
# bacterial cells
ISHdata <- read.table("data_raw/TB_per_lesion_final_raw.txt", header = TRUE,
                      sep = '\t', fill = TRUE)

# Create a subset of data with only lesions. In some cases, there are 
# identical replicated rows for the same study subject, so limit to one row per 
# subject
lesionISH <- ISHdata %>%
  select(Study.level.3, starts_with(c("l0","l1")),
                -contains("_DAPI")) %>%
  unique()

# Make data tidy by pivoting to a long format. This separates the lesion ID 
# from the marker it is associated with while still maintaining the mouse ID 
# and the count associated with each marker and lesion. Also remove DAPI counts 
# due to poor segmentation of nuclei during image analysis which led to 
# inaccurate counts. Accurate DAPI counts will be used from the IHC data set 
# upon merging the data sets. Then pivot parts wider to have separate columns
# for non-replicating, replicating, and total TB
ISHtidydatalong <- lesionISH %>%
  pivot_longer(!Study.level.3, names_to = "bacteria", values_to = "counts") %>%
  separate(bacteria, c("lesion", "tb"), sep = "_")

ISHtidydatawide <- ISHtidydatalong %>%
  pivot_wider(names_from = "tb", values_from = counts) %>%
  rename(total_tb = "total", nonrep_tb = "23s", rep_tb = "pre") %>%
  dplyr::filter(c(total_tb) != 0)

# Pivot data back to a long format to indicate our counts assigned to each 
# replication status of the bacteria which will be useful for merging our ISH 
# and IHC data sets downstream.
ISHtidydatalong <- ISHtidydatawide %>%
  pivot_longer(!c(Study.level.3, lesion), names_to = "tb", values_to = "counts")

# Read in IHC data. Here we have a similar format as before with our lesion 
# ID_cell type, although due to technical issues with the software, more data 
# was exported than desired during our trial and error period. Here we will 
# follow the same steps as above, while removing the excess columns that are 
# of no use.
IHCdata <- read.table("data_raw/IHC_data.tsv", header = TRUE, sep = '\t',
                      fill = TRUE)

# Multiple columns are present in this data set due to optimization of data 
# export in the image analysis software. Study.level.3 is an arbitrary name 
# assigned by the image analysis software for the name of each mouse. 
# L0# and L1# represent each granuloma in the study associated with the cell 
# count measured, here we separate lesion number and cell type to make the data 
# clearer. Although each mouse has data exported for as many lesions as 13, 
# several mice have much fewer, so we use filter out repetitive counts of 0 
# which aren’t needed.
lesionIHC <- IHCdata %>%
  select(Study.level.3, starts_with("l0"), starts_with("l1")) %>%
  unique()

# Work to tidy the data. Pivot it longer so we can separate lesion and cell
# type information into separate columns. A warning message is expected here
# (cases when "cell" includes more than one underscore, like "L01_total_host").
# Then make wide again, to have separate columns for each cell type.
IHCtidydatalong <- lesionIHC %>%
  pivot_longer(!Study.level.3, names_to = "cell", values_to = "counts") %>%
  separate(cell, c("lesion", "cell_type"), sep = "_")

IHCtidydatawide <- IHCtidydatalong %>%
  pivot_wider(names_from = "cell_type",
              values_from = counts)%>%
  filter(c(total) != 0) %>%
  rename(total_host_count = "total", B220_count = "B220", CD4_count = "CD4",
         CD8_count = "CD8", DAPI_count = DAPI)

# Merge ISH and IHC data, including our DAPI counts from IHC to add to our ISH
# data to be used later.
merged_path_data <- IHCtidydatawide %>%
  inner_join(ISHtidydatawide,
             by = c('Study.level.3' = 'Study.level.3', 'lesion' = 'lesion')) %>%
  rename(mouse = "Study.level.3")

# Add other calculations of interest. For log transformations, use log(x + 1)
# to account for occasional 0 values (log of 0 is undefined).
ISH_IHCmerge <- merged_path_data %>%
  # Calculate percentages
  mutate(B220_percent = ((B220_count / total_host_count) * 100),
         CD4_percent = ((CD4_count / total_host_count) * 100),
         CD8_percent = ((CD8_count / total_host_count) * 100),
         Other_percent = ((DAPI_count / total_host_count) * 100)) %>%
  # Calculate proportions
  mutate(B220_prop = ((B220_count / total_host_count)),
         CD4_prop = ((CD4_count / total_host_count)),
         CD8_prop = ((CD8_count / total_host_count)),
         DAPI_prop = ((DAPI_count / total_host_count))) %>%
  # Calculate counts scaled by 500 (the approximate IQR for each cell type)
  mutate(across(.cols =
                  contains("count"), ~ .x / 500, .names = '{.col}_500')) %>%
  # Calculate RS index
  mutate(RS_index =
           ((total_tb / total_host_count) * (rep_tb / nonrep_tb) * 1000)) %>% 
  # Calculate log-transformed values. Use log(x + 1) to avoid issue with 
  # log(0) = -inf
  mutate(log_rep_tb = log(rep_tb + 1),
         log_nonrep_tb = log(nonrep_tb + 1),
         log_total_tb = log(total_tb + 1),
         log_RS_index = ((log_total_tb / total_host_count) *
                           (log_rep_tb / log_nonrep_tb) * (10000)),
         log_RS_prop = ((log_rep_tb / log_total_tb))) %>%
  # Calculate proportion of replicating TB out of total TB
  mutate(RS_prop = ((rep_tb / total_tb)))

# Separate our timepoints and vaccine statuses based on mouse names assigned 
# during the study, for example SAL_09 indicates an unvaccinated animal 
# (saline) and 09 was a number assigned to our Day 56 timepoint. 
# Here mice numbered 9-16 were assigned to Day 56 and 17-25 were assigned to 
# Day 114.
ISH_IHC <- ISH_IHCmerge %>%
  mutate("Mouse" = mouse) %>%
  tidyr::separate(Mouse, c("Vaccination", "Timepoint"), sep = 3) %>%
  mutate(Timepoint =
           case_when(Timepoint < 17 ~ "D56", Timepoint > 16 ~ "D114")) %>%
  mutate_at(c("Vaccination", "Timepoint"), as.factor)

# Write out tidy data
write.csv(ISH_IHC, "data/IHC_ISH_clean_data_WIP.csv", row.names=FALSE)
