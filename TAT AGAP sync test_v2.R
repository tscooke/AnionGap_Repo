library(tidyverse)

# CONCLUSION: 
# COL - CCL - Laboratory Turn Around Time Report  
# 

setwd(paste0(getwd(), "/.."))
csv2022 <- read.csv(paste0(
  getwd(), "/00. Data/02. Combined Data/AGAP_0612-082022.csv"
))
tat2022 <- read.csv(paste0(
  getwd(), "/00. Data/00. Test Data/TAT Columbia_0630-070522.csv"
))
tat2022.stroke <- read.csv(paste0(
  getwd(), "/00. Data/00. Test Data/TAT ColumbiaStroke_0630-070522.csv"
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

clean.tat <- function(dataframe) {
  names(dataframe) <- str_replace_all(names(dataframe), "\\.\\.\\.", "")
  dataframe <- dataframe %>% 
    mutate(
      ACC = str_trim(Accession.NbrFormatted, side = "right")
    )
  return(dataframe)
}

# 2) Confirm that the accession numbers in tat2022 are in df.2022
# 3) Make note of which site has the right accession numbers so can just pull data from that in the future

tat.2022.stroke <- clean.tat(tat2022.stroke)

tat.2022.syncd <- tat.2022[(tat.2022$Accession.NbrFormatted %in% df.2022.rmdup$ACC),]

tat.2022.syncd$Performing.Department %>% table()

# %>% 
#   mutate(
#     Time = str_sub(COMPLETEDT, start = 10, end = 11),
#     Date = str_sub(COMPLETEDT, start = 1, end = 8)
#   )

  # Remove duplicated Accession numbers

