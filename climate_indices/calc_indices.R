library(tidyverse)

# ----- Station id-----
id <- tibble(id = 9999)

# ----- Template to calculate the indices for one station -----
avg_Fd    <- NA
ratio_Fd  <- NA

avg_ETR    <- NA
ratio_ETR  <- NA

avg_GSL    <- GSL(date,temp)$average
ratio_GSL  <- GSL(date,temp)$ratio

avg_HWDI    <- NA
ratio_HWDI  <- NA

avg_Tn90    <- NA
ratio_Tn90  <- NA

avg_R10    <- NA
ratio_R10  <- NA

avg_CDD    <- NA
ratio_CDD  <- NA

avg_R5d    <- NA
ratio_R5d  <- NA

avg_SDII    <- NA
ratio_SDII  <- NA

avg_R95T    <- NA
ratio_R95T  <- NA


# ----- Combine the indices -----
avgs <- tibble(avg_Fd, avg_ETR, avg_GSL, avg_HWDI,
       avg_Tn90, avg_R10, avg_CDD, avg_R5d,
       avg_SDII, avg_R95T)

ratios <- tibble(ratio_Fd, ratio_ETR, ratio_GSL, ratio_HWDI,
               ratio_Tn90, ratio_R10, ratio_CDD, ratio_R5d,
               ratio_SDII, ratio_R95T)

# ----- Should have the same order as the GoogleSheet to copy&paste-----
result <- bind_cols(id, avgs,ratios)
