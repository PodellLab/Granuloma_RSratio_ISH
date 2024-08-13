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

# Rearrange data to make plotting easier
ISH_long_rs1 <-ISH_IHC %>%
  select(mouse, lesion, Timepoint, Vaccination, RS_prop) %>% 
  pivot_longer(!c(mouse, lesion, Timepoint, Vaccination), 
               names_to = "cell", 
               values_to = "counts") %>% 
  na.omit()

# Recode some factors to give our preferred label names
ISH_long_rs <- ISH_long_rs1 %>%
  mutate(Vaccination = fct_recode(Vaccination, 
                                  Unvaccinated = "SAL", 
                                  BCG = "BCG")) %>% 
  mutate(Timepoint = fct_recode(Timepoint, Day56 = "D56", Day114 = "D114"))

# Create plot for Figure 2E and save to outputs subdirectory
fig2e <- ggplot(ISH_long_rs, aes(x = Vaccination, y = counts, 
                                 fill = Vaccination)) + 
  geom_boxplot() +
  facet_wrap(~Timepoint) +
  labs(x = "",
       y = "Proportion of Replicating Mtb") +
  ylim(0,1) + 
  geom_beeswarm() + 
  theme(legend.position = "none")
ggsave("outputs/figure_2e.png",
       fig2e, width = 3, height = 3.5)
