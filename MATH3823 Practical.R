graduates <- read.csv("https://richardpmann.com/MATH3823/Datasets/adelaide-25.csv")

head(graduates)

year <- graduates$year
year_factor <- as.factor(year)

faculty <- graduates$faculty
faculty <- as.factor(faculty)

sex <- graduates$sex
sex <- as.factor(sex)

survive <- graduates$survive
total <- graduates$total

surv_prop <- survive/total

par(mfrow=c(3,1))
boxplot(surv_prop ~ year_factor, xlab = "Year of Graduation", ylab = "Survival Probability", main = "Survival Probability by Year of Graduation")
abline(h=0.676,lty=2,col="black",xlim=c(1938,1942))
abline(h=0.766,lty=2,col="red")
boxplot(surv_prop ~ sex, xlab = "Sex", ylab = "Survival Probability", boxwex = 0.3, main = "Survival Probability by Sex")
boxplot(surv_prop ~ faculty, xlab = "Faculty", ylab = "Survival Probability", boxwex = 0.3, main = "Survival Probability by Faculty")
par(mfrow=c(1,1))

y <- cbind(survive, total-survive)
glm.fit1 <- glm(y ~ year + faculty + sex, family = binomial)

glm.fit1
summary(glm.fit1)
anova(glm.fit1)

eta1 <- predict(glm.fit1, data.frame(sex="M", year = 1941, faculty = "M"))
p1 <- exp(eta1)/(1+exp(eta1)) # inverse logit 
p1 #fitted probability

eta2 <- predict(glm.fit1, data.frame(sex="F", year = 1938, faculty = "E"))
p2 <- exp(eta2)/(1+exp(eta2))
p2

# explore different models

glm.fit2 <- glm(y ~ faculty + sex, family = binomial)
glm.fit2
summary(glm.fit2)

qchisq(0.95,1)
deviance(glm.fit2) - deviance(glm.fit1)

glm.fit3 <- glm(y ~ faculty * sex, family = binomial)
glm.fit3
summary(glm.fit3)

glm.fit_link <- glm(y ~ faculty + sex, family = binomial("cloglog"))
glm.fit_link
summary(glm.fit_link)

## model fit criteria

#deviance
deviance(glm.fit_link)

#residual plot - vary glm.fit for each model fit
plot(year,residuals(glm.fit4,type="deviance"), pch=16)
abline(h=0,lty=2)

plot(faculty,residuals(glm.fit_link,type="deviance"), pch=16)
abline(h=0,lty=2)

## separate M and F data

gradsM <- subset(graduates, sex == "M")
gradsF <- subset(graduates, sex == "F")

yearM <- gradsM$year
year_factorM <- as.factor(yearM)
facultyM <- gradsM$faculty
facultyM <- as.factor(facultyM)
surviveM <- gradsM$survive
totalM <- gradsM$total

yearF <- gradsF$year
year_factorF <- as.factor(yearF)
facultyF <- gradsF$faculty
facultyF <- as.factor(facultyF)
surviveF <- gradsF$survive
totalF <- gradsF$total

yM <- cbind(surviveM, totalM-surviveM)
yF <- cbind(surviveF, totalF-surviveF)

M.glm.fit <- glm(yM ~ facultyM, family = binomial)
M.glm.fit
summary(M.glm.fit)

F.glm.fit <- glm(yF ~ facultyF, family = binomial)
F.glm.fit
summary(F.glm.fit)
