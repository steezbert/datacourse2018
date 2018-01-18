Tn90 <- function(x,y){            
  # function to calculate indicator 194 "Tn90"
  # x = Tmin
  # y = date
  
  # packages required
  require(tidyverse)
  require(lubridate)
 
# ----- breaks -----
  if(length(y) != length(x)) {
    stop("y and x must be the same length")
  }
    
    
  if(!is.numeric(x)){
    stop("x must be 'numeric'")
  }
  
  if(!is.Date(y)) {
    stop("y must be 'date'")
  }   
  
# ----- prepare variables ------
    df <- data.frame(x,y) 
    
    df$year <- year(df$y)
    
# ----- quality check -----   
     df <- df %>% group_by(year) %>% filter(sum(is.na(x))<60)
    
# ----- analysis -----    
    q <- quantile(df$x,0.9,na.rm=TRUE)
    
    Tn90df <- df  %>% group_by(year) %>% 
      summarise(Tn90 = length(which(x > q))/length(x)*100)
    
    Tn90df2 <- Tn90df %>% filter(year >= 1997, year <= 2016)
    
    if(length(Tn90df2$year) < 10) {
      stop("length(y) < 10")
    }
    
    Tn90df2 <- Tn90df2 %>% summarise(AVG1 = mean(Tn90, na.rm=TRUE))
    
    Tn90df3 <- Tn90df %>% filter(year >= 1977, year <= 1996)
    
    if(length(Tn90df3$year) < 10) {
      stop("length(y) < 10")
    }
 
    Tn90df3 <- Tn90df3 %>% summarise(AVG2 = mean(Tn90, na.rm=TRUE))
    
    avg <- mean(Tn90df$Tn90, na.rm = TRUE)
      
    roc <- Tn90df2$AVG1/Tn90df3$AVG2
    
# ----- return -----   
    list <- list(year = Tn90df$year, Tn90 = Tn90df$Tn90, avg = avg, rate_of_change = roc)
    return(list)
}
