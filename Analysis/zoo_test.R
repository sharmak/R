
zoo.to.data.frame <- function(x.z, s.name) { 
  # index function is used to find the time series in R function
  # Setting row.names removes the date as row.names
  # setNames is used to set the column names
  setNames(data.frame(index(x.z), x.z, row.names=NULL), c("date", s.name))
}

Quandl.Data <- function(ticker, name) {
  cl1 <- Quandl(ticker)
  cl1.z <- zoo(cl1$Settle, cl1$Date)
  cl1.z.s <-  head(cl1.z)
  cl1.df <- zoo.to.data.frame(cl1.z, name)
  cl1.df.s <- head(cl1.df)
  return(cl1.df.s)
}
appendTest <- function (s, desc, units) {
  if (!exists("TEST")) {
    TEST <<- data.frame(date=as.Date(character()),
                    symbol=character(), 
                    value=double(),
                    description=character(),
                    units=character(),
                    cd=as.Date(character()),
                    stringsAsFactors=FALSE) 
  }
  s.t <- melt(s, id="date", variable.name="symbol")
  s.t$description <- desc
  s.t$units <- units
  s.t$cd <- Sys.Date()
  TEST <<- rbind(TEST, s.t)
}

rm(TEST)
ng2 <- Quandl.Data("CHRIS/CME_NG1", "ng2")
cl2 <- Quandl.Data("CHRIS/CME_CL1", "cl1")
appendTest(cl2, "CL2", "Barrel")
appendTest(ng2, "NG2", "Mmbtu")
TEST

