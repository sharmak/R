library(Quandl)
library(ggplot2)

Quandl.auth("XQQrEp2ntQZ4Syg2pMbZ")
wti_ice1 <- Quandl("CHRIS/ICE_T1",   start_date='2014-01-01')
wti_nymex1 <- Quandl("CHRIS/CME_CL1",start_date='2014-01-01')
ggplot() + geom_line(data=wti_ice1, aes(x=Date, y=Settle, color="red")) + geom_line(data=wti_nymex1, aes(x=Date, y=Settle, color="blue")) + xlab("Date") + ylab("Settle")
ggplot() + geom_line(data=wti_ice1, aes(x=Date, y=Volume, color="red")) + geom_line(data=wti_nymex1, aes(x=Date, y=Volume, color="blue")) + xlab("Date") + ylab("Volume")


# 5-day retuns on wti_ice1
wti_ice1.z <- zoo(wti_ice1$Settle, wti_ice1$Date)
wti_nymex1.z <- zoo(wti_nymex1$Settle, wti_nymex1$Date)
wti.z <- na.exclude(merge.zoo(wti_ice1.z, wti_nymex1.z))
wti.z$wti_ice1.5d.return <- wti.z$wti_ice1 - lag(wti.z$wti_ice1, -5) 
wti.z$wti_nymex1.5d.return <-wti.z$wti_nymex1 - lag(wti.z$wti_nymex1, -5) 
wti.z <- na.exclude(wti.z)
# Portfolio with 1000 barrel long WTI crude & 1000 barrel WTI ICE crude short
# how do compute the risk on the trade
# 1. Compute mean run on the portfolio
wti_nymex1.mean.returns <- mean(wti.z$wti_nymex1.5d.return)
wti_ice1.mean.returns <- mean(wti.z$wti_ice1.5d.return)
port_mean_return <- 1000 * wti_nymex1.mean.returns - 1000 * wti_ice1.mean.returns
plot(port_mean_return)
head(port_mean_return)
# [a , b][var(x), cov(x,y)   [a
#         cov(x,y, var(y))]   b ]
# [a*var(x) + b*cov(x,y), a*cov(x,y) + b*var(y)][a b]^T
# a^2*var(x) + 2 * a * b * cov(x,y) + b^2 * var(y)
# 2. Compute variance on the portfolio
wti_nymex1.var.return <- var(wti.z$wti_nymex1.5d.return)
wti_ice1.var.return <- var(wti.z$wti_ice1.5d.return)
wti_ice1_nymex1.cov.return <- cov(wti.z$wti_ice1.5d.return, wti.z$wti_nymex1.5d.return)
port_var <- (1000)^2*wti_ice1.var.return  + 1000^2*wti_nymex1.var.return  - 2 * 1000 * 1000 * wti_ice1_nymex1.cov.return
# Portfolio standard dievation (risk) tells us that there is 68% chance that 
# we can gain/lose this much amount
port_std <- sqrt(port_var)


# Compute 1% VaR
# If we assume that portfolio is normally distributed with mean and std, var represents 
# the maximum loss we can make with 1% (.01) uncertainity.
var_1percent <- qnorm(0.01, port_mean_return, port_std)
