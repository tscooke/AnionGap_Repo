library(tidyverse)

# CONCLUSION: 
# COL - CCL - Laboratory Turn Around Time Report  
# 

setwd(paste0(getwd(), "/.."))
csv2022 <- read.csv(paste0(
  getwd(), "/00. Data/02. Combined Data/AGAP_0612-082022.csv"
))
tat2022 <- read.csv(paste0(
  getwd(), "/00. Data/00. Test Data/TAT_070322_RC.csv"
))

clean.csv <- function(dataframe) {
  names(dataframe) <- str_replace_all(names(dataframe), "X_", "")
  dataframe$RESULT <- str_trim(dataframe$RESULT, side = "left") %>% 
    as.numeric()
  dataframe <- dataframe %>% 
    filter(!is.na(RESULT)) %>% 
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

clean.tat <- function(dataframe) {
  names(dataframe) <- str_replace_all(names(dataframe), "X_", "")
  return(dataframe)
}

df.2022 <- clean.csv(csv2022)

days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
dates.2022 <- str_sub(df.2022$PERFORM_DT_TM, start = 1, end = 8)
dates.2022 <- dates.2022[!duplicated(dates.2022)]
dateday.2022 <- data.frame(dates.2022, rep(days, length.out = length(dates.2022)))

df.2022 <- df.2022 %>% 
  mutate(
    DayOfWeek = factor(
      dateday.2022[match(str_sub(PERFORM_DT_TM, start = 1, end = 8),
                         dateday.2022[,1]),2],
      levels = days
    ),
    Time = str_sub(PERFORM_DT_TM, start = 10, end = 11),
    Date = str_sub(PERFORM_DT_TM, start = 1, end = 8)
  )

df.2022.rmdup <- df.2022[!duplicated(df.2022$MRN),]


tat.2022 <- clean.tat(tat2022)
tat.2022 <- tat.2022 %>% 
  filter(CAT == "Basic Metabolic Panel") %>% 
  mutate(
    Time = str_sub(COMPLETEDT, start = 10, end = 11),
    Date = str_sub(COMPLETEDT, start = 1, end = 8)
  )

tat.2022.rmdup <- tat.2022[!duplicated(tat.2022$ACC),]
  # Remove duplicated Accession numbers

