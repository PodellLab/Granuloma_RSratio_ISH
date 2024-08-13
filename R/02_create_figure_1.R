# Load required packages
library(tidyverse)
library(ggbeeswarm)
library(ggeasy)
library(ggpubr)

# Read in CFU data
cfu <- read.csv("data_raw/cfu.csv") %>%
  mutate(Timepoint = fct_relevel(Timepoint, c("D56", "D114"))) %>%
  mutate(Vaccination = fct_relevel(Vaccination, c("SAL", "BCG"))) %>%
  mutate(Timepoint = fct_recode(Timepoint, "Day 56" = "D56", 
                                "Day 114" = "D114")) %>%
  mutate(Vaccination = fct_recode(Vaccination, "Unvaccinated" = "SAL")) %>% 
  mutate(Vaccination = fct_relevel(Vaccination, c("Unvaccinated", "BCG"))) %>%
  arrange(Vaccination, Timepoint)

# Create plot for Figure 1A and save to outputs subdirectory
prefix <- c("p < ", "p = ", "p > ")
fig1a <- ggplot(cfu, aes(x = Vaccination, y = Lung_CFU, fill = Vaccination)) +  
  geom_boxplot() + 
  facet_wrap(~ Timepoint) + 
  labs(title = "",
       x = "Vaccination", 
       y = "Log10 CFU") +
  theme(legend.position = "none") +
  geom_beeswarm() + 
  theme(legend.position = "none") +
  theme(strip.text = element_text(size = 9)) +
  easy_change_text(c("text"),"face", "bold") +
  easy_text_size(c("axis.text.x", "axis.text.y"), 8) +
  easy_text_size(c("axis.title.x", "axis.title.y"), 9) +
  easy_plot_title_size(size = 11) +
  easy_center_title() +
  stat_compare_means(label.x.npc = 'middle', method = "t.test", 
                     label.y.npc = 0.95,
                     aes(label = 
                           paste0(scales::label_pvalue(accuracy = 0.01, 
                                                       prefix = prefix)(..p..))))  
  

ggsave(fig1a, filename = "outputs/figure1A.tiff", 
       width = 3.25, height = 2.9, units = 'in', dpi = 1200)
