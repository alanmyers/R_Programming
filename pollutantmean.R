
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  FileList = NULL
  AllData = NULL
  
  #
  # Must be a better way?  E.g. x[, pollutant]
  #
  idx <- 2
  if (pollutant == "sulfate") { 
    idx <- 2
  } else if (pollutant == "nitrate") {
    idx <- 3
  } else {
    print ("Warning, no value set using Sulfate")    
  }
  
  for (i in id) {    
    if (i < 10) {
      fname <- sprintf("%s/00%d.csv", directory, i) 
    } else if (i < 100) {
      fname <- sprintf("%s/0%d.csv", directory, i) 
    } else {
      fname = sprintf("%s/%d.csv", directory, i)
    }
    Data = read.csv(fname)    
    AllData = rbind(AllData, Data)    
  }
  res <- sapply(AllData[idx], mean, na.rm=TRUE)
  str = sprintf("%f", res)
  print (str)
 
}