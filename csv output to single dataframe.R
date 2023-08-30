# Code to concatentate week-long csv reports into a single data frame

# UPDATE TARGET FILE PATHWAY 'folderpath' BEFORE RUNNING

setwd(paste0(getwd(),"/../00. Data"))
list.files()

folderpath <- paste0(getwd(), '/061123-081923')

csv.to.dataframe <- function(folderpath) {
  
  
  return(list.files(folderpath))
}