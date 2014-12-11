corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  data <- numeric()
  for (i in 1:332) {
    if (i < 10) {
      filename <- paste(directory, "/00", i, ".csv", sep = "")
    } else if (i < 100) {
      filename <- paste(directory, "/0", i, ".csv", sep = "")
    } else {
      filename <- paste(directory, "/", i, ".csv", sep = "")
    }
    csvData <- read.csv(file = filename)
    
    csvData <- csvData[complete.cases(csvData),]
    
    if (nrow(csvData) > threshold) {
      data <- c(data,cor(csvData[,"sulfate"], csvData[,"nitrate"]))
    }
  }
  data
}