require(XLConnect)
wb = loadWorkbook("ngshistory.xls")
df = readWorksheet(wb, "data")
ts = as.ts(df$Total.Lower.48)
plot(ts)
library(tsoutliers)
o <- tso(ts)
tsoutliers::plot.tsoutliers(o)
o$outliers
o$outliers$ind
df[o$outliers$ind,]