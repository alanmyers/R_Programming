corr <- function(directory = "specdata", threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  val_results <- complete(directory=directory, printit=FALSE)  
  val_results <- subset(val_results, val_results$nobs > threshold)
  
  # Load the data -- Sample in val_results[1]
  AllData <- data.frame()
  results <- numeric()
  for (i in val_results$id) {    
    if (i < 10) {
      fname <- sprintf("%s/00%d.csv", directory, i) 
    } else if (i < 100) {
      fname <- sprintf("%s/0%d.csv", directory, i) 
    } else {
      fname = sprintf("%s/%d.csv", directory, i)
    }

    Data = read.csv(fname)    
    test_results = cor(Data$sulfate, Data$nitrate, use= "na.or.complete")    
    results <- append(results, test_results)
  }
  results
}