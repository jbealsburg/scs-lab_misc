
filename <- "2022 winterkill + yield notes - Sheet1.csv"

read.csv(filename) -> dat

library(tidyverse)


# quick experimental summary ----------------------------------------------

dat %>% 
  glimpse()
# 42 plots

dat %>% 
  distinct(variety.code) %>% 
  tally()
# 14 varieties

dat %>% 
  group_by(winter.kill) %>%
  tally()
# 30 plots survived winter
# this is likely because of standing water over range 1 and 2 in spring


# narrative ---------------------------------------------------------------

# This is Jesse's plot assessment data from 2022 in K8

# At end of july, we were instructed to put bird protection

# At this point, it was clear plots in ranges 1 and 2 were not going to yield
# anything because the plants had died from standing water in spring and still
# didn't have much growth/seed 

# I decided to just harvest whatever seedheads I could from these plots and mow
# them down prior to putting up bird fence since there's no point in protecting
# plots that don't really have any seed

# plots not harvested on 27Jul were plots that didn't have winterkill and might
# produce useful data

dat %>% 
  # glimpse()
  filter(harvested27Jul=="n") -> dat2

# Rabbits also started "timbering" the wheat plants by chewing at the stem. When
# every plot was fully dried down, we harvested every plot with 3 row lengths of
# 24". Some plots did not have enough grain to do this, these plots were marked
# as okyield="y"


dat2 %>% 
  # glimpse()
  filter(okyield18Aug == "y") %>% 
  group_by(variety.code) %>% 
  tally() 
  # tally(n)

# we only have yields for 9 of the 14 varieties that we could compute to an
# kg/ha basis

# this does show that varities 5 and 7 performed well in our environment. Likely
# they were later to mature so missed out on bird damage and rabbit damage

# we can compute seed data for all varieties

# how should we handle this data?
