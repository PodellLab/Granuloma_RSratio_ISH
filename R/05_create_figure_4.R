# Load required packages
library(tidyverse)
library(ggbeeswarm)
library(jtools)
library(broom.mixed)

# Read in the tidy data. Create columns with cell counts scaled by 500 
# (the approximate interquartile range for CD4, CD8, and B cells). Change
# some factor levels so that the level we want will be the baseline
ISH_IHC <- read.csv("data/IHC_ISH_clean_data_WIP.csv") %>%
  mutate(across(.cols = contains("count"), ~ .x/500, .names = '{.col}_500')) %>% 
  select(contains("count_500"), rep_tb, total_tb, Vaccination, Timepoint) %>% 
  mutate(Timepoint = fct_relevel(Timepoint, c("D56", "D114"))) %>%
  mutate(Vaccination = fct_relevel(Vaccination, c("SAL", "BCG"))) %>% 
  arrange(Vaccination, Timepoint)

# Fit module
fit <- glm(rep_tb ~ CD8_count_500 + CD4_count_500 + B220_count_500 + 
            Timepoint + Vaccination + offset(log(total_tb)), 
          data = ISH_IHC,
          family = quasipoisson(link = "log"))

# Get summary of model (info used in label for Figure 4)
summ(fit, confint = TRUE, digits = 3, exp = TRUE)

# Create plot for Figure 2E and save to outputs subdirectory
fig4 <- plot_summs(fit, exp = TRUE, coefs = c("CD4 count" = "CD4_count_500", 
                                      "CD8 count" = "CD8_count_500",
                                      "B cell count" = "B220_count_500")) + 
  labs(x = "")

ggsave("outputs/figure_4.png",
       fig4, width = 6.5, height = 3)
