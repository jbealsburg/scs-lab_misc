# Variety trial plant count data

library(tidyverse)

# Becker collection on 20Sep2023
read.csv("VarietyTrial_2023_PlotPlan - plant_counts.csv") -> dat

dat %>% 
  pivot_longer(
    cols = c(count_0.5m_1,count_0.5m_2)
  ) %>% 
  mutate(sample = recode(name,
                         "count_0.5m_1" = "a",
                         "count_0.5m_2" = "b")) %>% 
  mutate(vis = ifelse(sample=="b",NA,vis_rating)) %>% 
  mutate(plants_meter = value*2) %>% 
  dplyr::select(-c(name,value)) -> dat2

dat2 %>% 
  filter(site=="Becker") %>% 
  summarise(
    mean = mean(plants_meter),
    sd = sd(plants_meter),
    max=max(plants_meter),
    min = min(plants_meter),
    n=n()
  ) %>% 
  mutate(across(
    .fns = ~round(.,0))) %>% 
  knitr::kable(
    caption = "Plant counts in variety trial at Becker MN on 20Sep2023"
  )

dat2 %>% 
  filter(site=="Becker") %>% 
  group_by(program) %>% 
  summarise(
    mean = mean(plants_meter),
    sd = sd(plants_meter),
    max=max(plants_meter),
    min = min(plants_meter),
    n=n()
  ) %>% 
  mutate(across(
    .cols = 2:6,
    .fns = ~round(.,0))) %>% 
  knitr::kable(
    caption = "Plant counts in variety trial by program at Becker MN on 20Sep2023"
  )
  
dat2 %>% 
  filter(site=="Becker") %>% 
  group_by(line) %>% 
  summarise(
    mean = mean(plants_meter),
    sd = sd(plants_meter),
    max=max(plants_meter),
    min = min(plants_meter),
    n=n()
  ) %>% 
  arrange(desc(mean)) %>% 
  mutate(across(
    .fns = ~round(.,0))) %>% 
  knitr::kable(
    caption = "Plant counts in variety trial by line at Becker MN on 20Sep2023"
  )

dat2 %>% 
  filter(site=="Becker") %>%
  ggplot(
    aes(factor(line),plants_meter)
  ) +
  geom_hline(yintercept = 30,
             linetype=3) +
  stat_summary() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Variety",
       y="Plants per meter",
       caption = "Becker MN 20Sep2023\nPlanted 1Sep2023\nTarget population is 30 plants per meter") 


dat2 %>% 
  filter(site=="Becker") %>%
  ggplot(
    aes(factor(line),vis)
  ) +
  stat_summary() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Variety",
       y="Visual rating\n(0=no plants, 10=lots of plants)",
       caption = "Becker MN 20Sep2023") 

dat2 %>% 
  filter(site=="Becker") %>%
  group_by(line) %>% 
  summarise(
    plants_meter = mean(plants_meter),
    vis_rating = mean(vis,na.rm = T)
  ) %>% 
  arrange(vis_rating) %>% 
  mutate(across(
    .fns = ~round(.x,1)
  )) %>% 
  knitr::kable(caption = "Relationship between plant counts and visual rating")


library(corrplot)

dat2 %>% 
  filter(site=="Becker") %>%
  group_by(line) %>% 
  summarise(
    plants_meter = mean(plants_meter),
    vis_rating = mean(vis,na.rm = T)
  ) %>% 
  mutate(across(
    .fns = ~round(.x,1)
  )) -> c1 

cor(c1$plants_meter,
    c1$vis_rating,
    method = "pearson")
# 77% of increase in visual rating is explained by increase in plant counts

c1 %>% 
  mutate(vis_rating = vis_rating*2) %>% 
  pivot_longer(
    cols = c(plants_meter,vis_rating),
    names_to = "measurement_method"
  ) %>% 
  mutate(line = fct_reorder(factor(line),value)) %>% 
  ggplot(aes(factor(line),
             value,
             col=measurement_method,
             group=measurement_method)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Variety",
       y="",
       caption = "Comparison of visual rating vs. plant counts\nVisual ratings are doubled from original 0-10 scale\n r =0.77") 

  

