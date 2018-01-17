calc_R5d <- function(bw, idstation){
  
# Description of the script -----
### This function uses the full DWD data set and the ID of a station of your interest
### The function returns a list that contains the R5d per year, the average R5d for all years, and the rate of change for this variable. 

# ----- load required packages -------
  library(tidyverse)
  library(tidyr)
  library(lubridate)
  library(dplyr)
  library(readr)
  
# load data ---- 

df2 <- bw %>% mutate(date = ymd(paste(date(date)))) # change date


df3 <- df2 %>% filter(id == idstation) # filter for station

#EXCLUDE NA VALUES-----

na_var <- is.na(df3$RSK) #see if there are NAs

df4 <- df3 %>% 
  mutate(na_var = na_var) %>% 
  mutate(year = year(paste(date(date))))

bad_years <- df4 %>% 
  filter(na_var == TRUE) %>% #, id == idstation) %>% 
  group_by(year) %>% 
  count(na_var) %>% 
  filter(n > 60)
  
df4 <- df4 %>% filter(!(year%in%(bad_years$year)))
df4 %>% filter(id==idstation, na_var ==TRUE) %>% group_by(year) %>% count() 

# R5d - Maximum 5 day precipitation total--------

df5 <- df4 %>%  
  select(date, year, RSK) %>% 
  group_by(year) %>% 
  mutate(R5d = rollapply(RSK, 5, FUN="sum", fill=NA, align="right"))

df6 <- df5 %>% 
  select(year, R5d) %>% 
  group_by(year) %>%
  slice(which.max(R5d))

if(length(df6$year)<10){
  stop("too less yearly data (< 10) --> you failed!")
  }

# Index Value 1 -----

R5d_mean <- mean(df6$R5d) #<- 77.183

# Index Value 2 -----

df7796 <- df6 %>%  filter(year >= 1977, year <= 1996) 
df9716 <- df6 %>%  filter(year >= 1997, year <= 2016) 

df_diff <- abs(((mean(df9716$R5d)/mean(df7796$R5d))*100)-100)
# -> 15.947

return(list(df6$R5d, R5d_mean, df_diff))
}
