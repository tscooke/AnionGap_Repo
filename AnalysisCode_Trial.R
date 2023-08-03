### Test code for consolidating DA2 Na, Cl, CO2 output,
### performing anion gap calculation, and making some simple graphs
library(tidyverse)


setwd(paste0(getwd(),"/../00. Data"))

sodium.csv <- read.csv("SodiumLvl_070323-070323.csv")
chloride.csv <- read.csv("ChlorideLvl_070323-070323.csv")
bicarb.csv <- read.csv("CO2_070323-070323.csv")

clean_csv <- function(csv, test){
  csv <- csv[c(
    "PERFORM_DT_TM",
    "X_ACC",
    "DRAWN_DT_TM",
    "RECEIVED_DT_TM",
    "X_RESULT",
    "X_NORM",
    "X_CAT"
  )]
  names(csv)[which(names(csv) == "X_RESULT")] <- test
  return(csv)
}

glimpse(sodium.csv)
glimpse(chloride.csv)
