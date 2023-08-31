# Code to perform mixed model regression on Anion Gap data

# 1) Read in csv's
setwd(paste0(getwd(), "/.."))
csv.2023 <- read.csv(paste0(
  getwd(), "/00. Data/02. Combined Data/AGAP_0611-081923.csv"
))
csv.2022 <- read.csv(paste0(
  getwd(), "/00. Data/02. Combined Data/AGAP_0612-082022.csv"
))

# 2) THis is still proof of concept, so data doesn't have to be super clean
# 3) Start with simple linear regression, then add layers one at a time to make 
    # that I'm doing things correctly



# 4) Once I have even a couple terms working, can probably reach out to Eldad 
    # to schedule another meeting