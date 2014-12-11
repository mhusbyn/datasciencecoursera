complete <- function(directory, id = 1:332) {
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
  
  data <- data.frame(id = numeric(), nobs = numeric())
  for (i in id) {
    if (i < 10) {
      filename <- paste(directory, "/00", i, ".csv", sep = "")
    } else if (i < 100) {
      filename <- paste(directory, "/0", i, ".csv", sep = "")
    } else {
      filename <- paste(directory, "/", i, ".csv", sep = "")
    }
    csvData <- read.csv(file = filename)
    
    csvData <- csvData[complete.cases(csvData),]
    
    data <- rbind(data, data.frame(id=i, nobs=nrow(csvData)))
  }
  data
}