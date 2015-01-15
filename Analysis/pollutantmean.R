pollutantmean <- function(directory, pollutant, id=1:332) {
  sum <- 0
  files = dir(directory)
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
    sum = sum + mean(file.data[, pollutant],na.rm = T)
    
  }
  sum / length(id)
}