library(tsdl)
library(forecast)

# Passengers Arrivals Dataset 
# Frequency 15 minutes
d1 = subset(tsdl, description="Passenger arrivals")
autoplot(d1[[1]])
x = ts(d1[[1]], frequency = 62)
y <- decompose(x)
plot(y)

# Daily Min. Temperature, Melbourne
# Frequency Daily
d2 = subset(tsdl, description="Daily minimum temperatures in Melbourne")
autoplot(d2[[1]])
x <- decompose(d2[[1]])
plot(x)

# Market Share 
# Frequency Weekly 
d3 = subset(tsdl, description="Weekly market share")
autoplot(d3[[1]])
x <- decompose(d3[[1]])
plot(x)

# Milk Production
# Frequency Monthly
d4 = subset(tsdl, description="Monthly milk production")
autoplot(d4[[1]])
x <- decompose(d4[[1]])
plot(x)

# Water use
# Frequency Yearly
d5 = subset(tsdl, description="Annual water use")
autoplot(d5[[1]])
