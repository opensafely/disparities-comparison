library(here)
library(tidyverse)
library(ggplot2)
library(ggsankey)
library(ggalluvial)

##  older adults 
cohort <- "adults"

#import collated phenotype sensitivity data
df_input <- read_csv(here::here(#"post_check", "output", "collated",
                     "output", "collated",
                     "descriptive", paste0(cohort,
                     "_validation_counts_collated.csv")))

df_test <- df_input %>% 
  filter(population == "rsv_pop", subset == "2021_22") %>% 
  select(-c(population, pct, subset))

df_long <- df_test %>% make_long(, sens_stage, spec_stage, value = rounded)

ggplot(df_long, aes(x = x, 
               next_x = next_x, 
               node = node, 
               next_node = next_node,
               fill = factor(node))) +
  geom_sankey() +
  theme_sankey(base_size = 16)

ggplot(data = df_test,
       aes(axis1 = sens_stage, axis2 = spec_stage, y = rounded)) +
  geom_alluvium(aes(fill = spec_stage)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Sensitive Phenotype", "Specific Phenotype"),
                   expand = c(0.15, 0.05)) +
  theme_void()