complete <- function(directory, id = 1:332, printit=TRUE) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  df = data.frame()  
  for (i in id) {    
    if (i < 10) {
      fname <- sprintf("%s/00%d.csv", directory, i) 
    } else if (i < 100) {
      fname <- sprintf("%s/0%d.csv", directory, i) 
    } else {
      fname = sprintf("%s/%d.csv", directory, i)
    }
    Data = read.csv(fname) 
    nobs <- nrow(subset(Data, Data[2]>= 0 & Data[3]>= 0))
    
    df = rbind(df, c(i, nobs))
    str = sprintf("Id: %d, nobs: %d", i, nobs)
    if (printit) {
      print(str)    
    }
  }
  colnames(df) <- c("id", "nobs")
  df
}