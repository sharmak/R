data_url = "http://faculty.washington.edu/ezivot/econ424/sbuxPrices.csv"
sbux_df <- read.csv(data_url)
str(sbux_df)
head(sbux_df)
tail(sbux_df)
class(sbux_df$Date)
closing_prices <- sbux_df[, "Adj.Close", drop=F]
index_1 = which(sbux_df$Date == "3/1/1994")
index_2 = which(sbux_df$Date == "3/1/1995")
sbux_df[index_1:index_2, "Adj.Close"]
sbux_price_df <- sbux_df[, "Adj.Close", drop=FALSE]
rownames(sbux_price_df) <- sbux_df$Date
head(sbux_price_df)
price_1 <- index(sbux_price_df["3/1/1994",])
price_2 <- sbux_price_df["3/1/1995",]
plot(sbux_df$Adj.Close, type="l", col="blue", lwd=2, ylab="Adj Close", main="Sbux Closing Price")
n <- nrow(sbux_price_df)
sbux_ret <- (sbux_price_df[2:n, 1] - sbux_price_df[1:n-1, 1]) / sbux_price_df[1:n-1, 1]
class(sbux_ret)
names(sbux_ret) <- sbux_df[2:n,1]
head(sbux_ret)

sbux_prices_df = sbux_df[, "Adj.Close", drop=FALSE]
# Denote n the number of time periods:
n = nrow(sbux_prices_df)
sbux_ret = ((sbux_prices_df[2:n, 1] - sbux_prices_df[1:(n-1), 1])/sbux_prices_df[1:(n-1), 1])

# Compute continuously compounded 1-month returns
sbux_ccret  <-  log(sbux_prices_df[2:n,1]) - log(sbux_prices_df[1:n-1,1])
# Assign names to the continuously compounded 1-month returns
names(sbux_ccret) <- sbux_df[2:n, 1]
names(sbux_ccret) 
head(sbux_ccret)
head(cbind(sbux_ccret, sbux_ret))
plot(sbux_ret, type="l", col="blue", lwd=2, ylab="Return", main="Monthly Retun on SBUX")
abline(h=0)
legend(x="bottomright", legend=c("Simple", "CC"), 
       lty=1, lwd=2, col=c("blue","red"))
lines(sbux_ccret, col="red", lwd=2)