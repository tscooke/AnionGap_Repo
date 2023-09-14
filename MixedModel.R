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
      # or 'Private Ambulatory'; and exclude everything else
      # Actually, instead specifying ENCTYP/Location as a factor and 
          # letting R take care of dummy coding

clean.csv <- function(dataframe) {
  
  names(dataframe) <- str_replace_all(names(dataframe), "X_", "")
  
  dataframe$RESULT <- str_trim(dataframe$RESULT, side = "left") %>% 
    as.numeric()
  
  dataframe <- dataframe %>% 
    filter(str_ends(SVCRSC, "C702")) %>% 
    filter(ENCTYP == 'Inpatient' |
             ENCTYP == 'Outpatient' |
             ENCTYP == 'Emergency' |
             ENCTYP == 'Private Ambulatory'
    ) %>%
  mutate(
    Location = case_when(
      ENCTYP == 'Inpatient' ~ 'Inpatient',
      ENCTYP == 'Outpatient' ~ 'Outpatient',
      ENCTYP == 'Emergency' ~ 'Emergency',
      ENCTYP == 'Private Ambulatory' ~ 'Outpatient',
    )
  )
  dataframe$Location <- factor(dataframe$Location,
                                levels = c('Outpatient','Emergency','Inpatient'))
  
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

lmer.df2 <- lmer(RESULT ~ prePost + (1|MRN) + ENCTYP, data = df)
summary(lmer.df2)

lmer.df3 <- lmer(RESULT ~ Location*prePost +
                   (1 + Location|MRN) +
                   (1 + prePost|MRN) +
                   (1 + prePost|Location), data = df)

# 4) Once I have even a couple terms working, can probably reach out to Eldad 
    # to schedule another meeting

preMRN <- df$MRN[which(df$prePost == 0)]
postMRN <- df$MRN[which(df$prePost == 1)]
prePostRepeat <- logical()

for (i in 1:length(postMRN)) {
  prePostRepeat <- append(prePostRepeat, postMRN[i] %in% preMRN)
}

InptMRN <- df$MRN[which(df$Location == 'Inpatient')]
OutptMRN <- df$MRN[which(df$Location == 'Outpatient')]
EmergMRN <- df$MRN[which(df$Location == 'Emergency')]

EmergInInpt <- logical()
EmergInOtpt <- logical()
OutptInInpt <- logical()

for (i in 1:length(EmergMRN)) {
  EmergInInpt <- append(EmergInInpt, EmergMRN[i] %in% InptMRN)
}

for (i in 1:length(EmergMRN)) {
  EmergInOtpt <- append(EmergInOtpt, EmergMRN[i] %in% OutptMRN)
}

for (i in 1:length(OutptMRN)) {
  OutptInInpt <- append(OutptInInpt, OutptMRN[i] %in% InptMRN)
}