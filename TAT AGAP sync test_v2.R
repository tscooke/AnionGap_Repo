library(tidyverse)

# CONCLUSION: 
# COL - DA2 - RMC Order Volume Columbia is the CORRECT report, per initial testing  
# 

setwd(paste0(getwd(), "/.."))
csv2022 <- read.csv(paste0(
  getwd(), "/00. Data/02. Combined Data/AGAP_0612-082022.csv"
))
tat2022 <- read.csv(paste0(
  getwd(), "/00. Data/02. Combined Data/TAT_0612-082022.csv"
))
tat2022.stroke <- read.csv(paste0(
  getwd(), "/00. Data/00. Test Data/TAT ColumbiaStroke_0630-070522.csv"
))

### CLEAN ANION GAP DATA, REMOVE DUPLICATES

clean.csv <- function(dataframe) {
  names(dataframe) <- str_replace_all(names(dataframe), "X_", "")
  dataframe$RESULT <- str_trim(dataframe$RESULT, side = "left") %>% 
    as.numeric()
  dataframe <- dataframe %>% 
    filter(!is.na(RESULT)) %>% 
    filter(str_ends(SVCRSC, "C702")) %>% 
    filter(!(CAT == ".BMPP" | CAT == ".CMPP")) %>% 
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
# df.2022_v2.rmdup <- df.2022_v2[!duplicated(df.2022_v2$MRN),]

### CLEAN TAT DATA

clean.tat <- function(dataframe) {
  names(dataframe) <- str_replace_all(names(dataframe), "\\.\\.\\.", "")
  dataframe <- dataframe %>% 
    filter(
      Accession.NbrFormatted != "---",
      Order.Status == "Completed"
     # ,Order.Procedure == "Basic Metabolic Panel" | Order.Procedure == "Comprehensive Metabolic Panel"
    ) %>% 
    mutate(
      ACC = str_trim(Accession.NbrFormatted, side = "right")
    )
  return(dataframe)
}

tat.2022 <- clean.tat(tat2022)

tat.2022.rmdup <- tat.2022[!duplicated(tat.2022$ACC),]


test.acc <- c("2-22-222-12524")

tat.2022[which(tat.2022$ACC == test.acc),]

df.2022.rmdup[!(df.2022.rmdup$ACC %in% tat.2022.rmdup$ACC),] %>% group_by(Date) %>% summarize(Count = n())
df.2022.rmdup[!(df.2022.rmdup$MRN %in% tat.2022.rmdup$AliasPerson.MRN),] %>% group_by(Date) %>% summarize(Count = n())
df.2022.rmdup[!(df.2022.rmdup$NAME %in% tat.2022.rmdup$Person.NameFull),] %>% group_by(Date) %>% summarize(Count = n())

df.not.tat <- df.2022.rmdup[!(df.2022.rmdup$ACC %in% tat.2022$ACC),]

glimpse(df.not.tat)
df.not.tat$ENCTYP %>% table(useNA = "ifany")  # 42 Inpt, 2 Outpt, 3 Priv Amp
df.not.tat$CAT %>% table(useNA = "ifany") # 39 are BMP, 2 CMP, 5 Electrolyte Panel, 1 .Stroke BMP


 
### 4524 Outpatient accession numbers are not included in tat.2022 (also 39 inpatient cases, but I care less about that)
  # What else is different about those? Are they all Private Ambulatory?
  # They were logged on the machine, but not on the collected time, in lab time, etc. Maybe they came to the lab a different way? were they stats?
  # ANSWER: CAT for almost all of them is .BMPP or .CMPP ; idk what kind of test that is, 

# After filtering df.2022 to remove .BMPP and .CMPP, accession numbers that are in tat.2022 but not df.2022 
  # decreased to 1816. An improvement, but still higher than I would like. More investigating to be done.


# 1) Remove duplicate accession numbers
#    Remove canceled tests
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

