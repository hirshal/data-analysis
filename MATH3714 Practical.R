dd <- read.csv("https://teaching.seehuhn.de/data/historic-station-data/data.csv")
library(ggplot2)
library(leaps)
library(gridExtra)

# task 1

m1 <- lm(tmax ~ year, data = dd)
m1

qplot(dd$year, dd$tmax, ylab = "Maximum Temperature", xlab = "Year", main = "Maximum Temperature vs Year") +
  geom_smooth(method = lm, color = "red", se = FALSE)

 #test for hypothesis H_0: beta = 0

X1 <- model.matrix(m1)
n <- nrow(X)
p <- ncol(X) - 1

hat.beta <- coef(m1)
sigma.hat.2 <- sum((fitted(m1) - dd$tmax)^2 / (n-p-1))

T <- hat.beta[2] / sqrt(sigma.hat.2*solve(t(X) %*% X)[2,2])
T

t.crit <- qt(0.975, n-p-1)
abs(T) > t.crit

summary(m1) #check

# reject H_0. There is a significant relationship between tmax and year.

# task 2. Relevant section Sec 9 - Diagnostic Plots

m2 <- lm(tmax ~ year + as.factor(month), data = dd)
m2

summary(m2)

AIC(m1)
AIC(m2)

oldresid <- qplot(fitted(m1), resid(m1), data = dd, colour = month, main = "Model 1: Only Year",
      xlab = "Fitted Value", ylab = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

newresid <- qplot(fitted(m2), resid(m2), data = dd, colour = month, main = "Model 2: Month and Year",
      xlab = "Fitted Value", ylab = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

grid.arrange(oldresid,newresid)

par(mfrow = c(1,2))
qqnorm(resid(m1), main = "Model 1: Only Year")
abline(a=0,b=3, col = "red")
qqnorm(resid(m2), main = "Model 2: Month and Year") #model with month gives straighter line than only year = better fit
abline(a=0,b=1.75, col = "red")
par(mfrow = c(1,1))

# task 3

m3 <- lm(tmax ~ year + as.factor(month) + as.factor(station) + year:as.factor(station), data = dd)
summary(m3)

yearplot <- qplot(year, tmax, data = dd, colour = station, ylab = "Maximum Temperature", xlab = "Year", main = "Maximum Temperature by Year") +
  geom_smooth(method = lm, se = FALSE) #can see all different trendlines for each station

monthplot <- qplot(month, tmax, data = dd, colour = station, ylab = "Maximum Temperature", xlab = "Month", main = "Maximum Temperature by Month") +
  geom_smooth(method = lm, se = FALSE)

grid.arrange(yearplot,monthplot)

ggplot(data = dd, mapping = aes(x = station, y = tmax, fill = station)) +
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 4, size = 3 , color = "white")

subset(dd, station == "Whitby")

# task 4

m4 <- lm(tmax ~ I(year^2) + year + as.factor(month), data = dd)
summary(m4)

quadyearplot <- qplot(fitted(m4), resid(m4), data = dd, colour = month, main = "Model 4: Quadratic Year Term",
      xlab = "Fitted Value", ylab = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

grid.arrange(newresid,quadyearplot)

par(mfrow = c(1,2))
qqnorm(resid(m2), main = "Model 2: Month and Year")
abline(a=0,b=1.75, col = "red")
qqnorm(resid(m4), main = "Model 4: Quadratic Year Term")
abline(a=0,b=1.75, col = "red")
par(mfrow = c(1,1))

