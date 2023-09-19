# Code to concatentate week-long csv reports into a single data frame

# UPDATE TARGET FOLDER 'targetFolder' BEFORE RUNNING

library(dplyr)
setwd(paste0(getwd(),"/../00. Data"))
list.files()

targetFolder <- '03. Reference Range Data'

csv.to.dataframe <- function(targetFolder) {

  setwd(paste0(getwd(), '/', targetFolder))

  numFiles <- length(list.files())

  combinedCsv <- read.csv(list.files()[1])

  for (i in 1:(numFiles-1)) {
    combinedCsv <- bind_rows(
      combinedCsv,
      read.csv(
        paste0(getwd(),'/',list.files()[i+1])
      )
    )
  }

  write.csv(combinedCsv, file = "combined data.csv", row.names = FALSE)
  setwd('..')
}

clean.ag <- function(dataframe) {
  
  names(dataframe) <- str_replace_all(names(dataframe), "X_", "")
  
  dataframe$RESULT <- str_trim(dataframe$RESULT, side = "left") %>% 
    as.numeric()
  
  return(dataframe)
}