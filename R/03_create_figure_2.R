# Load required packages
library(tidyverse)
library(ggbeeswarm)
library(ggeasy)

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
  mutate(Timepoint = fct_recode(Timepoint, "Day 56" = "D56", "Day 114" = "D114"))

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

# Read in data for Figure 2F (including CFUS)
ISH_IHC <- read.csv("data/IHC_ISH_clean_data_WIP.csv", 
                    stringsAsFactors = TRUE)  %>%  
  mutate(Timepoint = fct_relevel(Timepoint, c("D56", "D114"))) %>%
  mutate(Vaccination = fct_relevel(Vaccination, c("SAL", "BCG"))) %>%
  arrange(Vaccination, Timepoint) 

cfu <- read_csv("data_raw/cfu.csv")

# Merge the two datasets. Also change mouse, Vaccination, and Timepoint
# to factors and relevel to get the level we want as the baseline.
merged_cfu <- cfu %>% 
  inner_join(ISH_IHC, 
             by = c('mouse' = 'mouse', 'Vaccination' = 'Vaccination', 
                    'Timepoint' = 'Timepoint')) %>%
  mutate(mouse = as.factor(mouse),
         Vaccination = as.factor(Vaccination),
         Timepoint = as.factor(Timepoint)) %>%
  mutate(Timepoint = fct_relevel(Timepoint, c("D56", "D114")), 
         Vaccination = fct_relevel(Vaccination, c("SAL", "BCG"))) %>%
  arrange(Vaccination, Timepoint) %>%
  mutate(log_total_host_count = (log(total_host_count)))

# Change some factor names for better labels on the figure
merged_cfu <- merged_cfu %>%
  mutate(Vaccination = fct_recode(Vaccination, Unvaccinated = "SAL", 
                                  BCG = "BCG")) %>%
  mutate(Timepoint = fct_recode(Timepoint, "Day 56" = "D56", 
                                "Day 114" = "D114")) 

# Create Figure 2F and save it to outputs
fig2f <- ggplot(data = merged_cfu, aes(x = Lung_CFU , y = RS_prop, 
                                       color = Vaccination, group = mouse)) + 
  geom_point(shape = 1, stroke = 0.6, aes(size = total_tb)) +
  scale_x_continuous() +
  facet_wrap(~Timepoint) + 
  geom_line(color = "black", alpha = 0.8, linewidth = 0.15) +
  scale_size(breaks = c(100, 1000, 2000, 4000, 6000, 8000, 9000), 
             range = c(1, 9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylim(0,1) +
  labs(title = "",
       x = "CFU", 
       y = "Proportion of Replicating Mtb") +
  easy_change_text(c("text"),"face", "bold") +
  easy_text_size(c("axis.text.x", "axis.text.y"), 8) +
  easy_text_size(c("axis.title.x", "axis.title.y"), 9) +
  easy_text_size(c("legend.text", "legend.title"), 8) +
  easy_plot_title_size(size = 12) +
  easy_center_title() + 
  easy_add_legend_title(size = "Total Mtb")

ggsave(fig2f, 
       filename="outputs/figure_2f.tiff", 
       width = 6.3, height = 3.7, units = 'in', dpi = 1200)
