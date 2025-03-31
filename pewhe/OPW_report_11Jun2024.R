


library(tidyverse)
read.csv("UMN OPW Weed Survey - Sheet1.csv",
         skip = 1) -> dat

dat %>% 
  select(Date,Plot,Treatment, X.4) %>% 
  filter(Date == "6/11/2024") %>% 
  rename(quadrat_harvest_possible = X.4) -> dat2
  
dat2 %>% 
  rename(entry_code = Treatment) %>% 
  group_by(entry_code) %>% 
  summarise(yes_count = sum(quadrat_harvest_possible=="yes"),
            no_count = sum(quadrat_harvest_possible=="no")) %>% 
  knitr::kable(caption="Plots with sufficent perennial wheat population for a quadrat harvest in 2024.")
