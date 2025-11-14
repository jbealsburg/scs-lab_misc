# 5Jun2024
# Jesse assessed stands in Waseca
# 0=no plants, 10=perfect stand

# anything below a stand rating = 9 should not be combine harvested due to
# insufficient population in the plot

# data 
# https://docs.google.com/spreadsheets/d/1ZmKH_3KzYk3fSEiAouU_yP-atyN0wzYZn9rYUPajzfU/edit#gid=758100743


library(tidyverse)
read.csv("MASTER Variety Trial 2023 Plant - plant_counts.csv") -> dat

dat %>% 
  filter(year=="2024") %>% 
  select(-c(count_0.5m_1,count_0.5m_2,notes)) -> dat2 

dat2 %>% 
  summarise(mean=round(mean(stand_rating),1),
            sd = round(sd(stand_rating),1))

dat2 %>% 
  group_by(program) %>% 
  summarise(mean=round(mean(stand_rating),1),
            sd = round(sd(stand_rating),1)) %>% 
  arrange(mean)

dat2 %>% 
  group_by(variety) %>% 
  summarise(mean=round(mean(stand_rating),1),
            sd = round(sd(stand_rating),1)) %>% 
  arrange(mean)

dat2 %>% 
  mutate(variety = as.factor(variety)) %>% 
  mutate(variety = fct_reorder(variety,stand_rating)) %>% 
  ggplot(aes(variety,stand_rating)) +
  stat_summary() +
  theme(axis.text.x = element_text(angle = 90))
