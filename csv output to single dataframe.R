# Code to concatentate week-long csv reports into a single data frame
# Set 'targetFolder' to the folder containing the .csv's that you want to concatenate
# Set 'fileName' to the output file name that you want

library(dplyr)

targetFolder <- '01. Raw Data/03. Creatinine/0611-081923'
fileName <- 'NEW-Creatinine_0611-081923.csv'

clean.df <- function(dataframe) {
  
  names(dataframe) <- str_replace_all(names(dataframe), "X_", "")
  
  dataframe$RESULT <- str_trim(dataframe$RESULT, side = "left") %>% 
    as.numeric()
  
  return(dataframe)
}

csv.to.dataframe <- function(targetFolder, fileName) {

  setwd(paste0(getwd(), '/', targetFolder))

  numFiles <- length(list.files())

  combinedCsv <- clean.df(
    read.csv(list.files()[1])
    )

  for (i in 1:(numFiles-1)) {
    combinedCsv <- bind_rows(
      combinedCsv,
      clean.df(
        read.csv(
          paste0(getwd(),'/',list.files()[i+1])
        )
      )
      
    )
  }

  write.csv(combinedCsv, file = fileName, row.names = FALSE)
  setwd('..')
}

setwd(paste0(getwd(),"/../00. Data"))
csv.to.dataframe(targetFolder, fileName)




