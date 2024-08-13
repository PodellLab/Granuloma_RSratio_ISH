# Load required packages
library(tidyverse)
library(ggbeeswarm)

# Read in the tidy data. Get factor labels for timepoint and vaccination
# in correct order so they'll plot in the right order.
ISH_IHC <- read.csv("data/IHC_ISH_clean_data_WIP.csv", 
                    stringsAsFactors = TRUE) %>% 
  mutate(Timepoint = fct_relevel(Timepoint, c("D56", "D114"))) %>%
  mutate(Vaccination = fct_relevel(Vaccination, c("SAL", "BCG"))) %>% 
  arrange(Vaccination, Timepoint)

# Create a long format that will be easier to plot as facets
ISH_long_immune <-ISH_IHC %>%
  select(mouse, lesion, Timepoint, Vaccination, B220_count,
  CD4_count, CD8_count, DAPI_count) %>% 
  pivot_longer(!c(mouse, lesion, Timepoint, Vaccination), 
               names_to = "cell", values_to = "counts") %>% 
  mutate(Vaccination = fct_recode(Vaccination, 
                                  Unvaccinated = "SAL", BCG = "BCG")) %>% 
  mutate(Timepoint = fct_recode(Timepoint, "Day 56" = "D56", "Day 114" = "D114")) %>% 
  mutate(cell = fct_recode(cell, CD4 = "CD4_count", CD8 = "CD8_count", 
                           Bcell = "B220_count", Other = "DAPI_count")) %>% 
  na.omit()

# Create plot for Figure 3E and save to outputs subdirectory
fig3e <- ggplot(ISH_long_immune, aes(x = Vaccination, y = counts, 
                                     fill = Vaccination)) + 
  geom_boxplot(outlier.shape = NA) +
  facet_grid(cell ~ Timepoint, scales = "free_y") +
  labs(title = "Immune Cell Counts per Granuloma by IHC", 
       x = "",
       y = "Log-Scaled Immune Cell Count") + 
  geom_beeswarm() +
  scale_y_log10() + 
  theme(legend.position = "none")
ggsave("outputs/figure_3e.png",
       fig3e, width = 5, height = 6.5)
