## -----------------------------------------------------------------------------
#Load packages ----
library(tidyverse)
library(plyr)
library(stringr)


## -----------------------------------------------------------------------------
# dat.gasmet<-read.csv("Data/Gasmet/Raw Data/gasmet_2020_eddyflux.csv", na = "NA")

read.csv(
  "jesse_KGML_Dataset.xlsx - data_daily (2).csv"
) -> dat.gasmet

dat.gasmet %>% 
  mutate(experiment = fct_recode(experiment, 
                           "oreiManure" = "orei_manure")) %>% 
  mutate(site=as.factor(site),
         date = as.POSIXct(date,
                "%m/%d/%Y", tz = ""),
        datetime = as.POSIXct(paste(date, time), "%Y-%m-%d %H:%M:%S", 
                               tz = ""),
        code = paste(experiment, plot, date, site, sep = "_")
                           ) -> dat.add



## -----------------------------------------------------------------------------
dat.time <- dat.add %>%
  arrange(datetime) %>%
  group_by(code) %>%
  dplyr::mutate(ind = 1:n(), #this will label the first measurement of each code with '1'
         change.t = ifelse(ind == 1, #if its the first row...
                           0, #assign a 0. If not, calculate the difference in datetimes
                           as.numeric(difftime(datetime, lag(datetime)))), #should be 20 or 21s
         run.t = cumsum(as.numeric(ifelse(ind != 1, #the ind is not '1' (ie. 1st sample)
                        difftime(datetime, lag(datetime)), #record the sec when sample is made
                        0))), #if ind is '1', then start at 0
         n = n())

#it appears that the for-loop difftime is having trouble differentiating minutes from seconds. so when there is a longer break in time during a measurement period on the order of minutes, it is attributing that change in minutes to be seconds. e.g., 10:15:45 to 10:20:48 is recorded as a value of 5, not 303. The new method shown with column 'delta.t' does not have this issue for some reason. Either way, we may want to find a way to 


## -----------------------------------------------------------------------------
#create a df with codes that have a gap in sampling of >60sec
hi.dt<- dat.time %>%
  filter(change.t > 60) 

#visualize the slopes of the codes that have a long gap in sampling 
dat.time %>%
  filter(code %in% hi.dt$code) %>% 
  mutate(hi.col = ifelse(change.t > 60, "gap",
                         'norm')) %>%
  ggplot(aes(run.t, co2_ppm, color = hi.col)) +
  geom_point(size = 1) +
  facet_wrap(~code)

unique(dat.time$ind) #check if any NA's exist in the ind column. 
unique(dat.time$change.t) #check the values of the time change between each sequential datetime row. This should be 20 or 21 sec, but there are often other values. 
# hist(dat.time$n) #view a histogram of the number of sample points per code

#review key metrics from the dataset
qc <- data.frame(ind.ct = length(which(dat.time$ind == 1)), 
               code.ct = length(unique(dat.time$code)),
               date.ct = length(unique(dat.time$date)),
               max.co2 = max(dat.time$co2),
               min.co2 = min(dat.time$co2),
               max.temp = max(dat.time$celltemp),
               min.temp = min(dat.time$celltemp),
               max.time = max(dat.time$run.t),
               max.n = max(dat.time$n),
               min.n = min(dat.time$n))
qc


## -----------------------------------------------------------------------------
#cut off all datapoints prior to gap
dat.nogap <- dat.time %>%
  group_by(code) %>%
  dplyr::mutate(gap = ifelse(change.t > 60, 
                      'y',
                      'n')) %>%
  slice(which.max(gap == 'y') : n()) %>%
  select(-gap)

#dat.remove <- dat.time %>%
 # filter(code != '8_2023-10-13_v08')


## -----------------------------------------------------------------------------
dat.filt <- dat.nogap %>% #dataframe with cleaned data
  arrange(datetime) %>%
  group_by(code) %>%
  dplyr::mutate(ind = 1:n(), #this will label the first measurement of each code with '1'
         change.t = ifelse(ind == 1, #if its the first row...
                           0, #assign a 0. If not, calculate the difference in datetimes
                           as.numeric(difftime(datetime, lag(datetime)))), #should be 20 or 21s
         run.t = cumsum(as.numeric(ifelse(ind != 1, #the ind is not '1' (ie. 1st sample)
                        difftime(datetime, lag(datetime)), #record the sec when sample is made
                        0))),
         n = n()) #if ind is '1', then start at 0

hist(dat.filt$n)
unique(dat.filt$change.t)
range(dat.filt$run.t) #use this range to inform numbers for 'predict.res' in next step

#remove any codes that have 6 or fewer data points, which is required by the loess function next
dat.filt.out <- dat.filt %>%
  filter(n <= 6)

dat.filt.in <- dat.filt %>%
  filter(n > 6)


## -----------------------------------------------------------------------------
#create a df with a column of desired resolution to predict using loess function. Number of seconds and intervals.
predict.res <- data.frame(run.t = seq(0, 600, 0.1))

nest<- dat.filt.in %>%
  # filter(!is.na(co2_ppm)) %>% 
  mutate(co2=co2_ppm) %>% 
  group_by(code) %>% #create nested dfs grouped by code
  nest() %>%
  dplyr::mutate(loess.mod = map(data, ~stats::loess(co2 ~ run.t, degree = 2, data=.x)), 
         #create loess functions for each code
         pred = map(loess.mod, ~stats::predict(.x, predict.res)), 
         #applying loess function to predict run.t values at the resolution described in predict.res
         pred.df = map(pred, ~data.frame(co2 = .x, pred.run.t = predict.res$run.t)), 
         #a new df with just the high res run.t values and predicted co2
         min.time = map(pred.df, ~slice_min(.data = .x, co2, n = 1))) %>% 
  #returns the row with the minimized co2 
  unnest(min.time) %>% 
  dplyr::mutate(clean.data = map2(.x = data, .y = pred.run.t, ~filter(.x, run.t >= .y))) 
#uses the identified minimum run.t to filter out data prior to the minimum co2 point in a cleaned df

#unnests the cleaned data and makes new count column to help remove additional points
dat.clean <- nest %>%
  select(code, clean.data) %>%
  unnest(clean.data) %>%
  dplyr::mutate(count = 1:n())

cut.off<- nest %>%
  select(code, co2, pred.run.t)


## -----------------------------------------------------------------------------
#plot of pre-cleaned data
dat.filt %>%
  ggplot(aes(run.t, co2_ppm, color = as.factor(plot))) +
  geom_point() +
  facet_wrap(~date)

#plot of post-cleaned data. NOTICE: this uses 'count' on the x-axis so that all points align, whereas run.t would not align.
dat.clean %>%
  ggplot(aes(count, co2, color = as.factor(plot))) +
  geom_point() +
  facet_wrap(~date)

ct.clean <- dat.clean %>%
  group_by(code) %>%
  tally()
hist(ct.clean$n)

ct.clean %>%
  filter(n < 10)

dat.clean %>%
  filter(count > 3 & count < 10) %>% #6 data points would be 2 minute of data
  ggplot(aes(count, co2, color = as.factor(plot))) +
  geom_point() +
  geom_line() +
  facet_wrap(~date)

dat.clean %>%
  filter(count > 3 & count < 10) %>% 
  ggplot(aes(count, co2, color = as.factor(date))) +
  geom_point() +
  geom_line() +
  facet_wrap(~plot)


## -----------------------------------------------------------------------------
dat.slim <- dat.clean %>%
  filter(count > 3 & count < 10) %>% 
  dplyr::mutate(n = n()) %>%
  filter(n > 3) %>%
  ungroup()


## -----------------------------------------------------------------------------
co2.convert <- function(co2, temp) {
  co2*(12/1)*(1/1000000)*(1/0.082057)*(1/(273.15 + temp)) * 1000
}

mydat<- dat.slim %>%
  dplyr::select(code, date, plot, experiment, site, datetime, co2_ppm, run.t, temp_celsius, count) %>%
  dplyr::mutate(co2.c = co2.convert(co2_ppm, temp_celsius))


## -----------------------------------------------------------------------------
regressions <- dlply(mydat, .(code), lm, formula = co2.c ~ run.t)

coefs <- cbind(ldply(regressions, coef), #extracts the code variables of each model, and the 5th coefficient, which is slope
               ldply(regressions, function(x){coef(summary(x))[,2]})[,2:3]) #extracts the standard error of the slope coefficient

str_split_fixed(coefs$code,"_", n=3)[,1] -> coefs$experiment
str_split_fixed(coefs$code,"_", n=3)[,2] -> coefs$plot
str_split_fixed(coefs$code,"_", n=4)[,3] -> coefs$date
str_split_fixed(coefs$code,"_", n=4)[,4] -> coefs$site

colnames(coefs)<-c("code","intercept","slope", "intercept_se","slope_se","experiment", "plot","date","site")


## -----------------------------------------------------------------------------
hist(coefs$slope_se)
boxplot(coefs$slope_se)

limit.pt<- mean(coefs$slope_se) + (sd(coefs$slope_se) * 2) #change the final number based on perference for determining outlier

#filter out data that does not meet the above limit.pt criteria
coefs.clean <- coefs %>%
  filter(slope_se < limit.pt)

#record which slopes did not meet criteria 
coefs.out <- coefs %>%
  filter(slope_se > limit.pt)

#create a reference dataframe that exludes data from the codes just determined to meet criteria for removal based on slope se
mydat.clean<- mydat %>%
  filter(!code %in% coefs.out$code)

#check the correct amount of codes were removed 
length(unique(mydat$code)) #this number...
length(unique(coefs.out$code)) #minus this number...
length(unique(mydat.clean$code)) #should equal this number

#plot this data
mydat.clean %>%
  ggplot(aes(count, co2_ppm, color = as.factor(plot))) +
  geom_point() +
  geom_line() +
  facet_wrap(~date)

mydat.clean %>%
  ggplot(aes(count, co2_ppm, color = as.factor(date))) +
  geom_point() +
  geom_line() +
  facet_wrap(~plot)


## ----eval = FALSE-------------------------------------------------------------
#Rows with 1 degree of freedom will be the slopes. This way we don't also get the residuals.
#pvals<-ldply(regressions, anova)
#pvals2<-subset(pvals, Df=="1")

# Create a table with codes (i.e. plots) where slope WAS NOT significant. 
#pvals3<-subset(pvals2, pvals2[,6]>0.05)

#Find the slopes in the coefs.sum dataframe using these codes from pvals3 (which are not significant) and replace those slopes with 0. Column 3 is the slope, column 5 is the slope standard error. 
#coefs.sum[which(coefs.sum$code %in% pvals3$code),c(3,5)]<-0     
#head(coefs.sum)


## -----------------------------------------------------------------------------
coefs.sum <- coefs.clean


## -----------------------------------------------------------------------------
chamberdims<-data.frame(matrix(ncol=5))
colnames(chamberdims)<-c("clength", "cwidth","carea","cvol","cheight")
chamberdims$clength<-50.165 #cm
chamberdims$cwidth<-13.97
chamberdims$carea<-chamberdims$clength*chamberdims$cwidth
chamberdims$cvol<-6250 #cm3. Known by filling chamber with water. 
chamberdims$cheight<-chamberdims$cvol/chamberdims$carea

#converting co2.c to co2 equivalents. 
c.to.e <- function(x) {
  x * (44/12)
}

#converts ug/cm2/s to g/m2/day. 1 g = 1,000,000 ug. 10,000 cm2 = 1 m2. 86,400 sec = 1 day. 
area.to.gmd <- function(x) {
  x * (1/1000000) * (10000/1) * (86400/1)
}

#converts ug/cm2/s to kg/ha/year. 1 kg = 1,000,000,000 ug. 100,000,000 cm2 = 1 Ha. 31,536,000 sec = 1 year. 
area.to.kghyr <- function(x) {
  x * (1/1e9) * (1e8/1) * (3.1536e7/1) 
}


coefs.sum <- coefs.sum %>%
  dplyr::mutate(co2.e = c.to.e(slope),
         co2.e.area = co2.e * chamberdims$cheight,
         co2.e.gmd = area.to.gmd(co2.e.area),
         co2.e.kghyr = area.to.kghyr(co2.e.area),
         day = as.numeric(strftime(date, format="%j")))

#review the distribution of the final flux values
hist(coefs.sum$co2.e.gmd)


## -----------------------------------------------------------------------------
pre.date <- dat.time %>%
  select(plot, date) %>%
  unique() %>%
  group_by(date) %>%
  tally() %>%
  mutate(date = as.character(date))
    
post.date <- coefs.sum %>%
  select(plot, date) %>%
  unique() %>%
  group_by(date) %>%
  tally()

left_join(pre.date, post.date, by = 'date', suffix = c('.pre', '.post'))

pre.plot <- dat.time %>%
  select(plot, date) %>%
  unique() %>%
  group_by(plot) %>%
  tally() %>%
  mutate(plot = as.character(plot))

post.plot <- coefs.sum %>%
  select(plot, date) %>%
  unique() %>%
  group_by(plot) %>%
  tally()

left_join(pre.plot, post.plot, by = 'plot', suffix = c('.pre', ',post'))



## -----------------------------------------------------------------------------

coefs.sum %>% 
  select(experiment, site, plot, date, co2.e.kghyr) %>% 
  write.csv("kgml_CO2_v1.csv")

# checking CO2 data

coefs.sum %>% 
  # filter(co2.e.kghyr>-10000 &
           # co2.e.kghyr<100000) %>% 
  ggplot(aes(co2.e.gmd)) +
  geom_histogram()



## -----------------------------------------------------------------------------
# I take the CO2 equation and just changed it to N20. That is where the 28 is coming from. 273 is kelvin. 
n2o.convert <- function(n2o, temp) {
  n2o*(28/1)*(1/1000000)*(1/0.082057)*(1/(273.15 + temp)) * 1000
}


mydat<- dat.slim %>%
  dplyr::select(code, date, plot, experiment, site, datetime, co2_ppm, n2o_ppm, run.t, temp_celsius, count) %>%
  dplyr::mutate(co2.c = co2.convert(co2_ppm, temp_celsius)) %>%   dplyr::mutate(n2o.c = n2o.convert(n2o_ppm, temp_celsius))


## -----------------------------------------------------------------------------
regressions_n2o <- dlply(mydat, .(code), lm, formula = n2o.c ~ run.t)

coefs_n2o <- cbind(ldply(regressions_n2o, coef), #extracts the code variables of each model, and the 5th coefficient, which is slope
               ldply(regressions, function(x){coef(summary(x))[,2]})[,2:3]) #extracts the standard error of the slope coefficient

str_split_fixed(coefs_n2o$code,"_", n=3)[,1] -> coefs_n2o$experiment
str_split_fixed(coefs_n2o$code,"_", n=3)[,2] -> coefs_n2o$plot
str_split_fixed(coefs_n2o$code,"_", n=4)[,3] -> coefs_n2o$date
str_split_fixed(coefs_n2o$code,"_", n=4)[,4] -> coefs_n2o$site

colnames(coefs_n2o)<-c("code","intercept","slope", "intercept_se","slope_se","experiment", "plot","date","site")


## -----------------------------------------------------------------------------
hist(coefs_n2o$slope_se)
boxplot(coefs_n2o$slope_se)

limit.pt<- mean(coefs_n2o$slope_se) + (sd(coefs_n2o$slope_se) * 2) #change the final number based on perference for determining outlier

#filter out data that does not meet the above limit.pt criteria
coefs_n2o.clean <- coefs_n2o %>%
  filter(slope_se < limit.pt)

#record which slopes did not meet criteria 
coefs_n2o.out <- coefs_n2o %>%
  filter(slope_se > limit.pt)

#create a reference dataframe that exludes data from the codes just determined to meet criteria for removal based on slope se
mydat.clean<- mydat %>%
  filter(!code %in% coefs.out$code)

#check the correct amount of codes were removed 
length(unique(mydat$code)) #this number...
length(unique(coefs.out$code)) #minus this number...
length(unique(mydat.clean$code)) #should equal this number



## ----eval = FALSE-------------------------------------------------------------
#Rows with 1 degree of freedom will be the slopes. This way we don't also get the residuals.
#pvals<-ldply(regressions, anova)
#pvals2<-subset(pvals, Df=="1")

# Create a table with codes (i.e. plots) where slope WAS NOT significant. 
#pvals3<-subset(pvals2, pvals2[,6]>0.05)

#Find the slopes in the coefs.sum dataframe using these codes from pvals3 (which are not significant) and replace those slopes with 0. Column 3 is the slope, column 5 is the slope standard error. 
#coefs.sum[which(coefs.sum$code %in% pvals3$code),c(3,5)]<-0     
#head(coefs.sum)


## -----------------------------------------------------------------------------
coefs_n2o.clean.sum <- coefs_n2o.clean


## -----------------------------------------------------------------------------
chamberdims<-data.frame(matrix(ncol=5))
colnames(chamberdims)<-c("clength", "cwidth","carea","cvol","cheight")
chamberdims$clength<-50.165 #cm
chamberdims$cwidth<-13.97
chamberdims$carea<-chamberdims$clength*chamberdims$cwidth
chamberdims$cvol<-6250 #cm3. Known by filling chamber with water. 
chamberdims$cheight<-chamberdims$cvol/chamberdims$carea

#converting n2o.c to n2o equivalents. 44 is atomic weight of n2o, 28 is the part that is nitrogen. 
n.to.e <- function(x) {
  x * (44/28)
}

# n2o weight = 44
# n = 14

#converts ug/cm2/s to g/m2/day. 1 g = 1,000,000 ug. 10,000 cm2 = 1 m2. 86,400 sec = 1 day. 
area.to.gmd <- function(x) {
  x * (1/1000000) * (10000/1) * (86400/1)
}

#converts ug/cm2/s to kg/ha/year. 1 kg = 1,000,000,000 ug. 100,000,000 cm2 = 1 Ha. 31,536,000 sec = 1 year. 
area.to.kghyr <- function(x) {
  x * (1/1e9) * (1e8/1) * (3.1536e7/1) 
}

coefs_n2o.sum <- coefs_n2o.clean.sum %>%
  dplyr::mutate(n2o.e = n.to.e(slope),
         n2o.e.area = n2o.e * chamberdims$cheight,
         n2o.e.gmd = area.to.gmd(n2o.e.area),
         n2o.e.kghyr = area.to.kghyr(n2o.e.area),
         n2o.e = n.to.e(slope),
         day = as.numeric(strftime(date, format="%j")))

#review the distribution of the final flux values
hist(coefs_n2o.sum$n2o.e)




## -----------------------------------------------------------------------------
pre.date <- dat.time %>%
  select(plot, date) %>%
  unique() %>%
  group_by(date) %>%
  tally() %>%
  mutate(date = as.character(date))
    
post.date <- coefs.sum %>%
  select(plot, date) %>%
  unique() %>%
  group_by(date) %>%
  tally()

left_join(pre.date, post.date, by = 'date', suffix = c('.pre', '.post'))

pre.plot <- dat.time %>%
  select(plot, date) %>%
  unique() %>%
  group_by(plot) %>%
  tally() %>%
  mutate(plot = as.character(plot))

post.plot <- coefs.sum %>%
  select(plot, date) %>%
  unique() %>%
  group_by(plot) %>%
  tally()

left_join(pre.plot, post.plot, by = 'plot', suffix = c('.pre', ',post'))



## -----------------------------------------------------------------------------

coefs_n2o.sum %>% 
  select(experiment, site, plot, date, n2o.e.kghyr) %>% 
  write.csv("kgml_n2o_v1.csv",
            row.names = F)

coefs.sum %>% 
  select(experiment, site, plot, date, co2.e.kghyr) %>% 
  write.csv("kgml_CO2_v1.csv")

# checking CO2 data

coefs.sum %>% 
  # filter(co2.e.kghyr>-10000 &
           # co2.e.kghyr<100000) %>% 
  ggplot(aes(co2.e.kghyr)) +
  geom_histogram()



## -----------------------------------------------------------------------------
# I take the CO2 equation and just changed it to ch4. That is where the 28 is coming from. 273 is kelvin. 
ch4.convert <- function(ch4, temp) {
  ch4*(12/1)*(1/1000000)*(1/0.082057)*(1/(273.15 + temp)) * 1000
}

dat.slim %>% 
  glimpse()

mydat<- dat.slim %>%
  dplyr::select(code, date, plot, experiment, site, datetime, co2_ppm, n2o_ppm, ch4_ppm, run.t, temp_celsius, count) %>%
  dplyr::mutate(co2.c = co2.convert(co2_ppm, temp_celsius)) %>%   dplyr::mutate(n2o.c = n2o.convert(n2o_ppm, temp_celsius)) %>%   dplyr::mutate(ch4.c = ch4.convert(ch4_ppm, temp_celsius))
  


## -----------------------------------------------------------------------------
regressions_ch4 <- dlply(mydat, .(code), lm, formula = ch4.c ~ run.t)

coefs_ch4 <- cbind(ldply(regressions_ch4, coef), #extracts the code variables of each model, and the 5th coefficient, which is slope
               ldply(regressions, function(x){coef(summary(x))[,2]})[,2:3]) #extracts the standard error of the slope coefficient

str_split_fixed(coefs_ch4$code,"_", n=3)[,1] -> coefs_ch4$experiment
str_split_fixed(coefs_ch4$code,"_", n=3)[,2] -> coefs_ch4$plot
str_split_fixed(coefs_ch4$code,"_", n=4)[,3] -> coefs_ch4$date
str_split_fixed(coefs_ch4$code,"_", n=4)[,4] -> coefs_ch4$site

colnames(coefs_ch4)<-c("code","intercept","slope", "intercept_se","slope_se","experiment", "plot","date","site")


## -----------------------------------------------------------------------------
boxplot(coefs_ch4$slope_se)

limit.pt<- mean(coefs_ch4$slope_se) + (sd(coefs_ch4$slope_se) * 2) #change the final number based on perference for determining outlier

#filter out data that does not meet the above limit.pt criteria
coefs_ch4.clean <- coefs_ch4 %>%
  filter(slope_se < limit.pt)

#record which slopes did not meet criteria 
coefs_ch4.out <- coefs_ch4 %>%
  filter(slope_se > limit.pt)

#create a reference dataframe that exludes data from the codes just determined to meet criteria for removal based on slope se
mydat.clean<- mydat %>%
  filter(!code %in% coefs_ch4.out$code)

#check the correct amount of codes were removed 
length(unique(mydat$code)) #this number...
length(unique(coefs_ch4.out$code)) #minus this number...
length(unique(mydat.clean$code)) #should equal this number



## ----eval = FALSE-------------------------------------------------------------
#Rows with 1 degree of freedom will be the slopes. This way we don't also get the residuals.
#pvals<-ldply(regressions, anova)
#pvals2<-subset(pvals, Df=="1")

# Create a table with codes (i.e. plots) where slope WAS NOT significant. 
#pvals3<-subset(pvals2, pvals2[,6]>0.05)

#Find the slopes in the coefs.sum dataframe using these codes from pvals3 (which are not significant) and replace those slopes with 0. Column 3 is the slope, column 5 is the slope standard error. 
#coefs.sum[which(coefs.sum$code %in% pvals3$code),c(3,5)]<-0     
#head(coefs.sum)


## -----------------------------------------------------------------------------
coefs_ch4.clean.sum <- coefs_ch4.clean


## -----------------------------------------------------------------------------
chamberdims<-data.frame(matrix(ncol=5))
colnames(chamberdims)<-c("clength", "cwidth","carea","cvol","cheight")
chamberdims$clength<-50.165 #cm
chamberdims$cwidth<-13.97
chamberdims$carea<-chamberdims$clength*chamberdims$cwidth
chamberdims$cvol<-6250 #cm3. Known by filling chamber with water. 
chamberdims$cheight<-chamberdims$cvol/chamberdims$carea

#converting ch4.c to ch4 equivalents. 
ch4.to.e <- function(x) {
  x * (16/12)
}

# ch4 weight = 16
# c = 12

#converts ug/cm2/s to g/m2/day. 1 g = 1,000,000 ug. 10,000 cm2 = 1 m2. 86,400 sec = 1 day. 
area.to.gmd <- function(x) {
  x * (1/1000000) * (10000/1) * (86400/1)
}

#converts ug/cm2/s to kg/ha/year. 1 kg = 1,000,000,000 ug. 100,000,000 cm2 = 1 Ha. 31,536,000 sec = 1 year. 
area.to.kghyr <- function(x) {
  x * (1/1e9) * (1e8/1) * (3.1536e7/1) 
}

coefs_ch4.sum <- coefs_ch4.clean.sum %>%
  dplyr::mutate(ch4.e = n.to.e(slope),
         ch4.e.area = ch4.e * chamberdims$cheight,
         ch4.e.gmd = area.to.gmd(ch4.e.area),
         ch4.e.kghyr = area.to.kghyr(ch4.e.area),
         ch4.e = n.to.e(slope),
         day = as.numeric(strftime(date, format="%j")))

#review the distribution of the final flux values
hist(coefs_ch4.sum$ch4.e)




## -----------------------------------------------------------------------------
pre.date <- dat.time %>%
  select(plot, date) %>%
  unique() %>%
  group_by(date) %>%
  tally() %>%
  mutate(date = as.character(date))
    
post.date <- coefs.sum %>%
  select(plot, date) %>%
  unique() %>%
  group_by(date) %>%
  tally()

left_join(pre.date, post.date, by = 'date', suffix = c('.pre', '.post'))

pre.plot <- dat.time %>%
  select(plot, date) %>%
  unique() %>%
  group_by(plot) %>%
  tally() %>%
  mutate(plot = as.character(plot))

post.plot <- coefs.sum %>%
  select(plot, date) %>%
  unique() %>%
  group_by(plot) %>%
  tally()

left_join(pre.plot, post.plot, by = 'plot', suffix = c('.pre', ',post'))



## -----------------------------------------------------------------------------

# coefs_ch4.sum %>% 
#   select(experiment, site, plot, date, ch4.e.gmd) %>% 
#   write.csv("kgml_ch4_v1.csv",
#             row.names = F)
# 
# coefs_n2o.sum %>% 
#   select(experiment, site, plot, date, n2o.e.kghyr) %>% 
#   write.csv("kgml_n2o_v1.csv",
#             row.names = F)
# 
# coefs.sum %>% 
#   select(experiment, site, plot, date, co2.e.kghyr) %>% 
#   write.csv("kgml_CO2_v1.csv")


