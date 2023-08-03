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
  headers <- names(csv)
  names(csv) <- paste0(test, headers)
  return(csv)
}

sodium.clean <- clean_csv(sodium.csv, "Sodium_")
chloride.clean <- clean_csv(chloride.csv, "Chloride_")
bicarb.clean <- clean_csv(bicarb.csv, "Bicarb_")

NaCl.df <- merge(sodium.clean, chloride.clean, by.x = "Sodium_X_ACC", by.y = "Chloride_X_ACC")
for (i in 1:nrow(sodium.csv)) {
  if (sodium.csv[["X_ACC"]][i] != chloride.csv[["X_ACC"]][i]) print(i)
}

combined.df <- merge(NaCl.df, bicarb.clean, by.x = "Sodium_X_ACC", by.y = "Bicarb_X_ACC")

combined.df <- combined.df %>% 
  mutate(
    AnionGap = Sodium_X_RESULT - Chloride_X_RESULT - Bicarb_X_RESULT
  )

hist(
  combined.df$AnionGap,
  breaks = 16
  )

# Two Accession numbers are listed twice, so can't use them as unique identifiers.
# One row has X_CAT as Basic Metabolic Panel, the other row has .CMPP 
# Sodium and Chloride both have this duplication, presumably bicarb does, too.
# When merging sodium.csv and chloride.csv, R duplicates both rows, 
# so that four additional rows are added to NaCl.df
# 2-23-184-03789 
# 2-23-184-04989
# 8 additional rows added to combined.df, presumably through same duplication in bicarb.csv

