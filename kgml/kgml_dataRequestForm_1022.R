library(tidyverse)

read.csv("jesse_KGML_Dataset.xlsx - patch_treatmentToPlot.csv") -> dat

dat %>% 
  filter(experiment == "FERT") %>% 
  summary()


dat %>% 
  filter(experiment == "FERT") %>% 
  select(-year) %>% 
  mutate(plot = plot-2000) %>% 
  expand_grid(
    year = 2021:2025
  ) -> dat_fert_exp

dat %>% 
  filter(experiment == "ncas") %>% 
  select(-year) %>% 
  expand_grid(
    year = 2022:2025
  ) -> dat_ncas_exp

bind_rows(
  dat_fert_exp,
  dat_ncas_exp) %>% 
  write.table("clipboard", sep = "\t", row.names = FALSE)

  