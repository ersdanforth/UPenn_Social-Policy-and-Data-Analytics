
library(knitr)
library(kableExtra)
library(stargazer, quietly = T)
library(tidyverse)

df <- da38062.0001
View(df)

#included variables
summary(df$IMP_PMOSWORK) #months worked
summary(df$QA3_AGE_TC) #age
summary(df$QA9X_TC) #total family pays in rent
summary(df$QI3) #access to internet at home (1 = yes)
summary(df$PERSONX_TC) # number of household 
summary(df$IMP_FEMALE) #gender (1 = female)
summary(df$IMP_RACE) #race (1 = POC)

#coding binary variables
df$IMP_RACE <- ifelse(df$IMP_RACE == '(1) 1. White Non-Hispanic', 0, 1)
table(df$IMP_RACE)
df$QI3 <- ifelse(df$QI3 == '(1) 1. Yes', 1, 0)
table(df$QI3)
df$QA4 <- ifelse(df$QA4 == '(1) 1. Yes', 1, 0)
table(df$QA4)
df$IMP_FEMALE <- ifelse(df$IMP_FEMALE == '(2) 2.Female', 1, 0) 
table(df$IMP_FEMALE)

#renaming variables
df <- df %>%
  rename(workmos = IMP_PMOSWORK,
         age = QA3_AGE_TC,
         rent = QA9X_TC,
         internet = QI3,
         family = PERSONX_TC,
         gender = IMP_FEMALE,
         race = IMP_RACE)

#clean data set
dfR <- na.omit(df[,c("workmos",
                    "age",
                    "rent",
                    "internet",
                    "family",
                    "gender",
                    "race")])

#exploratory data analysis
summary(dfR[,c("workmos","internet","rent","age","family","gender","race")])

hist(dfR$workmos, breaks=25)
table(dfR$workmos == 12 | dfR$workmos == 0) 
1686/(2346)

hist(dfR$rent, breaks=25)
hist(dfR$internet, breaks=25)
hist(dfR$age, breaks=25)
hist(dfR$family, breaks=25)
hist(dfR$gender, breaks=25)
hist(dfR$race, breaks=25)

#model building

#center all continuous variables, square age 
dfR$age_c <- dfR$age - mean(dfR$age)
dfR$age_c2 <- dfR$age_c^2
dfR$rent_c <- dfR$rent - mean(dfR$rent)
dfR$family_c <- dfR$family - mean(dfR$family)

#models
lm1 <- lm(workmos ~ internet, data = dfR)
summary(lm1)
plot(lm1)

lm2 <- lm(workmos ~ internet + rent_c, data = dfR)
summary(lm2)

lm3 <- lm(workmos ~ internet + rent_c + age_c + age_c2, data = dfR)
summary(lm3)

lm4 <- lm(workmos ~ internet + rent_c + age_c + age_c2 + family_c, data = dfR)
summary(lm4)

lm4b <- lm(workmos ~ internet + rent_c + age_c + age_c2 + family_c + gender, data = dfR)
summary(lm4b)
#addition of gender not significant

lm5 <- lm(workmos ~ internet + rent_c + age_c + age_c2 + family_c + race, data = dfR)
summary(lm5)

vif(lm5)
anova(lm1, lm2, lm3, lm4, lm5)
#each addition is significant (except gender)

#testing for linearity
cor(dfR[,c("workmos","internet","rent","age","family","race")], use = "complete.obs")

library(car)
scatterplotMatrix(dfR[,c("workmos","internet","rent","age","family","race")],
                  cex = .5,
                  pch = 16,
                  col = rgb(0,0,0,1/32),
                  diagonal=list(method ="histogram", 
                                breaks = 20),
                  cex.labels = 0.5,
                  regLine=list(method = lm,
                               lty = 1,
                               lwd = 1,
                               col = 1),
                  smooth = list(method = "loessLine",
                                lty.smooth = 2,
                                lwd.smooth = 1,
                                col.smooth = 2,
                                lty.spread = 3,
                                lwd.spread = 1,
                                col.spread = 2))

scatterplot(jitter(workmos) ~ rent, data = dfR)
scatterplot(jitter(workmos) ~ age_c, data = dfR)

#normality of residuals
plot(lm5)
#residuals are relatively consistent around 0, with some curve
#QQ-plot looks bad, residuals curve off
hist(resid(lm5), breaks = 100) #not normally distributed, looks more like bimodal

#homoskedasticity
scatterplot(fitted(lm5), resid(lm5))
#residuals are nearly constant around 0, stay within confidence interval

#outlier diagnostics
#residuals vs. leverage
plot(lm5, which = 5)
which(hatvalues(lm5)>0.012)
#leverage cutoff 3*p/n = 
3*7/2346
table(hatvalues(lm5) > 3*7/2346)

#cook's distance
plot(lm5, which = 4)

dfR[2270,]
dfR[2711,]
dfR[156,]

#variable transformations
dfR$logfam <- pmax(0, log1p(dfR$family))
hist(dfR$logfam, breaks=25)

dfR$logwork <- pmax(0, log1p(dfR$workmos))
hist(dfR$logwork, breaks=25)
summary(dfR$logwork)

dfR$sqrwork <- pmax(0, sqrt(dfR$workmos))
hist(dfR$sqrwork, breaks=25)
summary(dfR$sqrwork)

dfR$logint <- pmax(0, log1p(dfR$internet))
hist(dfR$logint, breaks=25)

dfR$sqrint <- pmax(0, sqrt(dfR$internet))
hist(dfR$sqrint, breaks=25)

#updated models, resulting in no real change in diagnostics or in linearity 
lm5b <- lm(logwork ~ internet + rent_c + age_c + age_c2 + family_c + race, data = dfR)
plot(lm5b)

lm5c <- lm(workmos ~ logint + rent_c + age_c + age_c2 + family_c + race, data = dfR)
plot(lm5c)

scatterplotMatrix(dfR[,c("logwork","internet","rent","age","family","race")],
                  cex = .5,
                  pch = 16,
                  col = rgb(0,0,0,1/32),
                  diagonal=list(method ="histogram", 
                                breaks = 20),
                  cex.labels = 0.5,
                  regLine=list(method = lm,
                               lty = 1,
                               lwd = 1,
                               col = 1),
                  smooth = list(method = "loessLine",
                                lty.smooth = 2,
                                lwd.smooth = 1,
                                col.smooth = 2,
                                lty.spread = 3,
                                lwd.spread = 1,
                                col.spread = 2))

#testing interaction effects
lm6 <- lm(workmos ~ internet + rent_c + age_c + age_c2 + family_c + race +
            internet:age_c + internet:age_c2, data = dfR)
summary(lm6)

lm7 <- lm(workmos ~ internet + rent_c + age_c + age_c2 + family_c + race +
            internet:rent_c, data = dfR)
summary(lm7)

lm8 <- lm(workmos ~ internet + rent_c + age_c + age_c2 + family_c + race +
            internet:family_c, data = dfR)
summary(lm8)

lm9 <- lm(workmos ~ internet + rent_c + age_c + age_c2 + family_c + race +
            internet:race, data = dfR)
summary(lm9)

#visualizing age-internet interaction
newx <- seq(from = min(dfR$age_c), 
            to = max(dfR$age_c),
            length.out = 100)
newdata0 <- data.frame(age_c = newx,
                       age_c2 = newx^2,
                       internet = 0,
                       rent_c = 0,
                       family_c = 0,
                       race = 0)
newdata1 <- data.frame(age_c = newx, 
                       age_c2 = newx^2,
                       internet = 1,
                       rent_c = 0,
                       family_c = 0,
                       race = 0)
newy0 <- predict(lm6, newdata = newdata0) 
newy1 <- predict(lm6, newdata = newdata1) 

plot(jitter(dfR$age_c), dfR$workmos, col = dfR$internet + 4)
lines(newx, newy0, col = 4, lwd = 2) #dark blaue - age, no internet: months worked decreases 
lines(newx, newy1, col = 5, lwd = 2) #light blue - age, with internet: months worked increases, then decreases

#visualizing internet-family
newx <- seq(from = min(dfR$family_c), 
            to = max(dfR$family_c),
            length.out = 100)
newdata0 <- data.frame(age_c = 0,
                       age_c2 = 0,
                       internet = 0,
                       rent_c = 0,
                       family_c = newx,
                       race = 0)
newdata1 <- data.frame(age_c = 0,
                       age_c2 = 0,
                       internet = 1,
                       rent_c = 0,
                       family_c = newx,
                       race = 0)
newy0 <- predict(lm8, newdata = newdata0)
newy1 <- predict(lm8, newdata = newdata1) 

plot(jitter(dfR$family_c), dfR$workmos, col = dfR$internet + 4)
lines(newx, newy0, col = 4, lwd = 2) #dark blue - family increase, no internet: months worked is stable
lines(newx, newy1, col = 5, lwd = 2) #light blue - family increase, with internet: months worked decreases more

#visualizing internet-race
newx <- seq(from = min(dfR$race), 
            to = max(dfR$race),
            length.out = 100)
newdata0 <- data.frame(age_c = 0,
                       age_c2 = 0,
                       internet = 0,
                       rent_c = 0,
                       family_c = 0,
                       race = newx)
newdata1 <- data.frame(age_c = 0,
                       age_c2 = 0,
                       internet = 1,
                       rent_c = 0,
                       family_c = 0,
                       race = newx)
newy0 <- predict(lm9, newdata = newdata0)
newy1 <- predict(lm9, newdata = newdata1) 

plot(jitter(dfR$race), dfR$workmos, col = dfR$internet + 4)
lines(newx, newy0, col = 4, lwd = 2) #dark blue - no internet: negative effect of race
lines(newx, newy1, col = 5, lwd = 2) #yellow - with internet: negative effect of race

#final diagnostics
plot(lm6)
summary(lm6)

#comparison
anova(lm1, lm2, lm3, lm4, lm5, lm6)
stargazer(lm1, lm2, lm3, lm4, lm5, lm6, type = "text")

