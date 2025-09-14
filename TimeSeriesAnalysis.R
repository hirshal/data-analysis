load("/Users/hirshal/Downloads/pond.RData")

class(X)

plot(X, ylab = "Monthly Pond Water Levels (ft)") # plot time series

plot(X[1:60], type="l", main="Pond Water Level 1966-1971") # plot beginning segment of time series to ascertain any periodicity.
# We see there is annual seasonality (spike at the end of every year)

acf(as.numeric(X), main = "ACF of X") # reinforces belief of periodicity - peaks at 12, 24 etc

# try a linear trend

tt <- 1:length(X)
fit <- lm(X ~ tt)

trend <- fitted(fit)
trend <- ts(trend, start=start(X), end=end(X), frequency=frequency(X))

plot(X, ylab = "Monthly Pond Water Levels (ft)")
lines(trend, col="red")

resids <- ts(residuals(fit, start=start(X), end=end(X),frequency=frequency(X)))
plot(resids, ylab="Residuals")

# linear trend is appropriate. Having just removed this, we now remove periodicity.
# create dummy variable:

n = length(X)
jan = as.numeric((1:n %% 12) == 1)
feb = as.numeric((1:n %% 12) == 2)
mar = as.numeric((1:n %% 12) == 3)
apr = as.numeric((1:n %% 12) == 4)
may = as.numeric((1:n %% 12) == 5)
jun = as.numeric((1:n %% 12) == 6)
jul = as.numeric((1:n %% 12) == 7)
aug = as.numeric((1:n %% 12) == 8)
sep = as.numeric((1:n %% 12) == 9)
oct = as.numeric((1:n %% 12) == 10)
nov = as.numeric((1:n %% 12) == 11)
dec = as.numeric((1:n %% 12) == 0)

# fit model to residuals

fit2 <- lm(resids ~ 0 + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + dec)

fit2 # see there is seasonality - water levels lower in Jul/Aug, higher in Oct/Nov/Dec

seasonality <- ts(fitted(fit2), start = start(X), end = end(X), frequency = frequency(X))
fv <- trend + seasonality
Y <- X - fv # Y now contains detrended and deseasonalised residuals.

plot(X, ylab="Monthly Pond Water Levels (ft)")
lines(fv, col = "red")

plot(Y, main="Y", ylab="Detrended and Deseasonalised Residuals")

acf(as.numeric(Y), main="ACF of Y") # does not cut off, so we do not consider MA(q) model

pacf(as.numeric(Y), main="PACF of Y") # cuts off

# use YW equations to fit AR(p) model to residuals Y
  
ar(Y, aic = F, order.max = 1, method = "yule-walker")
ar(Y, aic = F, order.max = 2, method = "yule-walker")
ar(Y, aic = F, order.max = 3, method = "yule-walker") # fits AR(p) model to Y for each p


# Fit AR(1), AR(2), AR(3) and plot residuals:

par(mfrow = c(3,2))

#AR(1)

acf(as.numeric(Y), plot = F, lag = 1) # α = rho_1 = 0.903
ar1fit <- ar(Y, order = 1, aic = F)
ar1fit #confirms α = 0.903

plot(ar1fit$resid, ylab = "AR(1) Residuals")
acf(as.numeric(ar1fit$resid), na.action = na.omit, main = "")

# acf here has big spike for rho_1, so does not resemble WN

#AR(2)

acf(as.numeric(Y), plot = F, lag = 2) 
ar2fit <- ar(Y, order = 2, aic = F)
ar2fit

plot(ar2fit$resid, ylab = "AR(2) Residuals (Z)")
acf(as.numeric(ar2fit$resid), na.action = na.omit, main = "") 

# we see the acf for AR(2) best resembles white noise, so we choose an AR(2) model to fit Y.
# let Z be the residuals of the AR(2) process above

#AR(3)

acf(as.numeric(Y), plot = F, lag = 3)
ar3fit <- ar(as.numeric(Y), order = 3, aic = F)
ar3fit

plot(ar3fit$resid, ylab = "AR(3) Residuals",type="l")
acf(as.numeric(ar3fit$resid), na.action = na.omit, main = "")

par(mfrow = c(1,1))

Z <- ar2fit$resid

# fit ARIMA model

Z.arimafit <- arima(Y, c(2,0,0))

Z.arimafit

par(mfrow=c(1,2))
plot(Z.arimafit$residuals, ylab="ARIMA(2,0,0) Residuals")
acf(as.numeric(Z.arimafit$residuals), na.action = na.omit, main="")
