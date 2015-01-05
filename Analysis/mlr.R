class(fancy)
f <- zoo(fancy, as.Date(as.yearmon(index(fancy))))
autoplot(f)
log.fancy <- log(fancy)
m <- tslm(log.fancy~season+trend)
summary(m)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 7.6058604  0.0768740  98.939  < 2e-16 ***
#   season2     0.2510437  0.0993278   2.527 0.013718 *  
#   season3     0.6952066  0.0993386   6.998 1.18e-09 ***
#   season4     0.3829341  0.0993565   3.854 0.000252 ***
#   season5     0.4079944  0.0993817   4.105 0.000106 ***
#   season6     0.4469625  0.0994140   4.496 2.63e-05 ***
#   season7     0.6082156  0.0994534   6.116 4.69e-08 ***
#   season8     0.5853524  0.0995001   5.883 1.21e-07 ***
#   season9     0.6663446  0.0995538   6.693 4.27e-09 ***
#   season10    0.7440336  0.0996148   7.469 1.61e-10 ***
#   season11    1.2030164  0.0996828  12.068  < 2e-16 ***
#   season12    1.9581366  0.0997579  19.629  < 2e-16 ***
#   trend       0.0223930  0.0008448  26.508  < 2e-16 ***
# We can interpret 0.25 for season2 as there is 25% increase 
# in the value from season1
# Similarly there is 195% increase in the value for season12
# as compared to season1
# Around 2% increase in the trend every year.
plot(residuals(m))
plot(fitted(m))
f[months(index(f)) == "January"]
boxplot(f[months(index(f)) == "January"])
m.jan <- f[months(index(f)) == "January"]
plot(m.jan)
coredata(m.jan)
boxplot(coredata(m.jan))
m.jan <- f[months(index(f)) == "December"]
boxplot(coredata(m.jan))
