complete <- function(directory, id=1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  files <- dir(directory)
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  c_id <- character()
  count <- character()
  counter <- 0
  for (i in id) {
    char.i <- as.character(i)
    char.i.length <- nchar(char.i)
    if (char.i.length == 1) {
      f <- paste("00", char.i, ".csv", sep="")
    } else if (char.i.length == 2) {
      f <- paste("0", char.i, ".csv", sep="")
    } else {
      f <- paster(char.i, ".csv", sep="")
    }
    file.data <- read.csv(paste(directory, f, sep="/"))
    counter <- counter + 1
    c_id[counter] <- i
    count[counter] <- nrow(file.data[complete.cases(file.data),])
  }    
  
  out <- as.data.frame(cbind(c_id, count))
  colnames(out) <- c("id", "nobs")
  out
}