# Function to calculate indicator 194 "Tn90"

Tn90 <- function(date, Tmin) {
  
  require(lubridate)
  require(tidyverse)
  
  if(length(date) != length(Tmin))
    stop("'date' and 'Tmin' must be the same length")
  
  if(is.Date(date)==F)
    stop("'date' must be a date")
  
  if(is.numeric(Tmin)==F)
    stop("'Tmin' must be numeric")
  
  BW <- data.frame(date, Tmin)
  
  BW$ny <- year(BW$date)
  
  BW <- BW %>% group_by(ny) %>% filter(sum(is.na(Tmin))<60)
  
  if(length(BW$ny)<10)
    stop("sorry, not enough data for calculation (less than 10 years)")
  
  q <- quantile(BW$Tmin, .9, na.rm=T)
  
  BW_Tn90 <- BW %>% group_by(ny) %>% summarise(Tn90=length(which(Tmin>q))/length(Tmin)*100)
  
  AVGges <- mean(BW_Tn90$Tn90, na.rm = T)
  
  AVG1 <- BW_Tn90 %>% filter(ny>=1977, ny<1997) %>% summarise(AVG=mean(Tn90, na.rm=T))
  AVG2 <- BW_Tn90 %>% filter(ny>=1997, ny<=2016) %>% summarise(AVG=mean(Tn90, na.rm=T))
  rateAVG <- AVG2$AVG/AVG1$AVG
  
  return(list(year=BW_Tn90$ny, Tn90=BW_Tn90$Tn90, Average=AVGges, RateOfChange=rateAVG))
}
