library(tidyverse)
library(readr)

# ----- Station id-----
station_id <- 755
id <- tibble(id = station_id)

# ----- data path-----
path <- "C:/Users/user/Desktop/Data_Management/BW_Climate_1977_2016.txt"

# ----- running functions ----- 
#CDD
BW_C <- read_tsv(path)
CDD_result <- CDD(BW_C, station_id)

#ETR
ETR_result <- ETR(BW_C, station_id)

#Fd
fd_result <- fun_frostdays(BW_C, station_id)

#R10
r10_result <- R10(BW_C, station_id)

#R5d
r5d_result <- calc_R5d(BW_C, station_id)

#R95T
r95t_result <- R95T(BW_C, station_id)

#SDII
SDII_result <- SDII(BW_C, station_id)

#Tn90
data <- read.csv(path, sep = "\t", dec = ".", header = TRUE)
data$as_date <- as.Date(data$date, format = "%Y-%m-%d")
station <- subset(data[which(data$id == station_id),])
Tn90_result <- Tn90(station$TNK, station$as_date)

#GSL
data <- read.csv(path, sep = "\t", dec = ".", header = TRUE)
data$as_date <- as.Date(data$date, format = "%Y-%m-%d")
station <- subset(data[which(data$id == station_id),])
GSL_result <- GSL(station$as_date, station$TMK)

#HWDI
data <- read.csv(path, sep = "\t", dec = ".", header = TRUE)
data$as_date <- as.Date(data$date, format = "%Y-%m-%d")
HWDI_result <- HWDI(data, station_id)




# ----- Template to calculate the indices for one station -----
avg_Fd    <- as.numeric(fd_result[1])
ratio_Fd  <- as.numeric(fd_result[2])

avg_ETR    <- as.numeric(ETR_result[1])
ratio_ETR  <- as.numeric(ETR_result[2])

avg_GSL    <- as.numeric(GSL_result[1])
ratio_GSL  <- as.numeric(GSL_result[2])

avg_HWDI    <- as.numeric(HWDI_result[1][1])
ratio_HWDI  <- as.numeric(HWDI_result[2][1])

avg_Tn90    <- as.numeric(Tn90_result[3])
ratio_Tn90  <- as.numeric(Tn90_result[4])

avg_R10    <- as.numeric(r10_result[2])
ratio_R10  <- as.numeric(r10_result[3])

avg_CDD    <- as.numeric(CDD_result[2])
ratio_CDD  <- as.numeric(CDD_result[3])

avg_R5d    <- as.numeric(r5d_result[2])
ratio_R5d  <- as.numeric(r5d_result[3])

avg_SDII    <- as.numeric(SDII_result[2])
ratio_SDII  <- as.numeric(SDII_result[1])

avg_R95T    <- as.numeric(r95t_result[1])
ratio_R95T  <- as.numeric(r95t_result[2])


# ----- Combine the indices -----
avgs <- tibble(avg_Fd, avg_ETR, avg_GSL, avg_HWDI,
               avg_Tn90, avg_R10, avg_CDD, avg_R5d,
               avg_SDII, avg_R95T)

ratios <- tibble(ratio_Fd, ratio_ETR, ratio_GSL, ratio_HWDI,
                 ratio_Tn90, ratio_R10, ratio_CDD, ratio_R5d,
                 ratio_SDII, ratio_R95T)

# ----- Should have the same order as the GoogleSheet to copy&paste-----
result <- bind_cols(id, avgs,ratios)
View(result)
