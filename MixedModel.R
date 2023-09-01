# Code to perform mixed model regression on Anion Gap data
library(tidyverse)
library(lme4)

# 1) Read in csv's
setwd(paste0(getwd(), "/.."))
csv2023 <- read.csv(paste0(
  getwd(), "/00. Data/02. Combined Data/AGAP_0611-081923.csv"
))
csv2022 <- read.csv(paste0(
  getwd(), "/00. Data/02. Combined Data/AGAP_0612-082022.csv"
))

csvCombined <- bind_rows(csv2022, csv2023)

# 2) Data cleaning
    #[X] NEED to remove all SVCRSC values that aren't UC*C702
    #[X] Recode ENCTYP as dummy variable for 'Emergency', 'Inpatient', 'Outpatient',
      # and 'Private Ambulatory'
    #[X] Figure out why there are extra duplicated rows in the data set

clean.csv <- function(dataframe) {
  
  names(dataframe) <- str_replace_all(names(dataframe), "X_", "")
  
  dataframe$RESULT <- str_trim(dataframe$RESULT, side = "left") %>% 
    as.numeric()
  
  dataframe <- dataframe %>% 
    filter(str_ends(SVCRSC, "C702")) %>% 
    mutate(
      Location = case_when(
        ENCTYP == 'Inpatient' ~ 0,
        ENCTYP == 'Outpatient' ~ 1,
        ENCTYP == 'Emergency' ~ 2,
        ENCTYP == 'Private Ambulatory' ~ 3,
        .default = NA,
        TRUE ~ NA
      )
    )
  
  return(dataframe)
}

df <- clean.csv(csvCombined)

df <- df %>% 
  mutate(
    prePost = if_else(str_detect(PERFORM_DT_TM,"/22 "), 0, 1)
  )

# 3) Start with simple linear regression, then add layers one at a time to make 
    # that I'm doing things correctly
    # [] Add dummy variable for instrument (?) this is unlikely to have big effect
    # [] Include Location in regression

lm.df <- lm(RESULT ~ prePost, data = df)
summary(lm.df)

lmer.df <- lmer(RESULT ~ prePost + (1|MRN) + (Location|MRN), data = df)
summary(lmer.df)


# 4) Once I have even a couple terms working, can probably reach out to Eldad 
    # to schedule another meeting