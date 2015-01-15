corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  files <- dir(directory)
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  v = vector()
  counter = 0
  for (f in files) {
    file.data <- read.csv(paste(directory, f, sep="/"))
    complete.data <- file.data[complete.cases(file.data),]
    if (nrow(complete.data) > threshold) {
        c <- cor(complete.data$nitrate, complete.data$sulfate)
        counter <- counter + 1
        v[counter] <- c
    }
  }
  ## Return a numeric vector of correlations
  v
}