
# setup the R enviornment for kniting markdown doc properly
library(knitr)
opts_knit$set(root.dir='../')



chlwq2 <- read.csv("./Data/ChlaWQReplicateAcuteData2.csv")



library(ggplot2)
library(GGally)
library(gridExtra)
library(scatterplot3d)
library(MASS)
library(lubridate)
library(lme4)
library(nlme)
library(vegan)


class(chlwq2$Date)
chlwq2$Time <- factor(chlwq2$Time, levels = c("Pre", "Post")) #making sure pre always comes before post on figures
chlwq2$Chl.a1 <- log(chlwq2$Chl.a)
#transforming chlorophyll data since it is not normal



chlwq2$Date <- as.Date(chlwq2$Date, "%m.%d.%y")
class(chlwq2$Date)
chlwq2$Date
#changing class, so that R understands these are dates


ggpairs(chlwq2, columns = c("Treatment", "Chl.a", "Chl.a1", "Temp"), columnLabels =  c("Treatment", "Chl.a", "Chl.a1","Temp"), mapping = aes(color = Tank))
#looking for relationships in the data

chl <- ggpairs(chlwq2, columns = c("Treatment", "Chl.a", "Time", "Tank",  "pH"), columnLabels =  c("Treatment", "Chl.a", "Time", "Tank",  "pH"), mapping = aes(color = Tank))
chl
#looking for relationships in the data

plot(Chl.a1 ~ Treatment, data = chlwq2) #looking at distribution of transformed data

plot(Chl.a ~ Treatment, data = chlwq2) #looking at distribution untransformed, chl.a & chl.a1 are highly correlated

#Exploring the data more

ggplot(chlwq2, aes(Treatment, Chl.a1)) +
  geom_point(aes(col=Date)) +
facet_wrap(~Tank)

ggplot(chlwq2, aes(Temp, Chl.a1)) +
  geom_point(aes(col=Treatment)) +
  facet_wrap(~Tank)

ggplot(chlwq2, aes(Time, Chl.a1)) +
  geom_point(aes(col=Date)) +
  facet_wrap(~Tank)

main <- lm(Chl.a1 ~ Treatment + Ozone + Temp + Time + pH + Salinity + Height + Tank, data = chlwq)
summary(main)
#first model with everything

main2 <- update(main, . ~ . -pH -Salinity - Height)
summary(main2)
#getting rid of a few variables that weren't significant

main3 <- update(main2, . ~ . + Treatment:Temp + Tank:Treatment )
summary(main3)
#adding a treatment:temperature interaction and tank:treatment interaction

AIC(main)
AIC(main2)
AIC(main3)
BIC(main)
BIC(main2)
BIC(main3)

anova(main, main2, main3) #main3 is the best model

#predict values based on the model and compare them to actual to see how well the model predicts data, compare the two

sim_mod <- lm(Chl.a ~ Treatment + Temp + Tank + Tank:Treatment + Temp:Treatment, data = chlwq2)
sub_mod <- update(sim_mod, . ~ . - Tank - Tank:Treatment)
main_mod <- lm(Chl.a ~ Treatment , data = chlwq2)
summary(sim_mod)
plot(sim_mod)

xrange <- seq(min(chlwq2$Treatment), max(chlwq2$Treatment), length.out = 100)


tanks <- levels(chlwq2$Tank)
plot(Chl.a ~ Treatment, data = chlwq2, ylim = c(0, 15))
for(i in seq_along(tanks)) {
  lines(xrange, 
        predict(sim_mod, newdata = 
                  data.frame(Tank = tanks[i],
                             Treatment = xrange,
                             Temp = mean(chlwq2$Temp))),
        col = i+1)
}
lines(predict(sub_mod, newdata = 
                data.frame(
                  Treatment = xrange,
                  Temp = mean(chlwq2$Temp))),
      lwd = 2)
lines(predict(main_mod, newdata = 
                data.frame(
                  Treatment = xrange)), col='purple', lwd=4)

avg_pred <- predict(sim_mod, newdata = 
                      data.frame(Tank = rep(tanks, each = 100),
                                 Treatment = rep(xrange,
                                                 times = 3),
                                 Temp = mean(chlwq2$Temp)))
avg_pred <- tapply(avg_pred, list(rep(xrange, times = 3)), mean)

lines(xrange, avg_pred, lty = 2, lwd=3)

# obs vs predicted plot

pred <- predict(sim_mod)

plot(chlwq2$Chl.a, pred)
abline(a=0, b=1)

avg_obs <- tapply(chlwq2$Chl.a, list(chlwq2$Treatment), mean)
avg_pred <- tapply(pred, list(chlwq2$Treatment), mean) 

plot(avg_obs, avg_pred)
abline(a=0, b=1)


Change <- read.csv("./Data/PercentChlaChange.csv")

plot(Pchlchange~Treatment, data = Change) #first look at the data's distribution

pchl1 <- lm(Pchlchange ~ Treatment , data = Change)
summary(pchl1) #looking at treatment only

pchl2 <- update(pchl1, .~. + Temp)
summary(pchl2) #adding temperature

pchl3 <- update(pchl2, .~. + Treatment:Temp)
summary(pchl3) #looking for an iteraction effect

pchl4 <- update(pchl3, .~. + Salinity)
summary(pchl4) #salinity is not significant, but gives a better f-stat and adjusted rsquared

pchl5 <- update(pchl4, .~. + Tank)
summary(pchl5) #Adding tank makes treatment not as significant, but makes the fit better overall

AIC(pchl1)
AIC(pchl2)
AIC(pchl3)
AIC(pchl4)
AIC(pchl5)
BIC(pchl1)
BIC(pchl2)
BIC(pchl3)
BIC(pchl4)
BIC(pchl5)

anova(pchl1, pchl2, pchl3, pchl4, pchl5)

shapiro.test(residuals(pchl4))

#predict values based on the model and compare them to actual to see how well the model predicts data, compare the two

sim_mod <- lm(Pchlchange ~ Treatment + Temp + Salinity + Tank + 
                Treatment:Temp, data = Change)
sub_mod <- update(sim_mod, . ~ . - Tank)
main_mod <- lm(Pchlchange ~ Treatment , data = Change)
summary(sim_mod)
plot(sim_mod)

xrange <- seq(min(Change$Treatment), max(Change$Treatment), length.out = 100)
srange <- seq(min(Change$Salinity), max(Change$Salinity), length.out = 100)

tanks <- levels(Change$Tank)
plot(Pchlchange ~ Treatment, data = Change, ylim = c(-80, 30))
for(i in seq_along(tanks)) {
  lines(xrange, 
        predict(sim_mod, newdata = 
                  data.frame(Tank = tanks[i],
                             Treatment = xrange,
                             Temp = mean(Change$Temp), Salinity = srange)),
        col = i+1)
}
lines(predict(sub_mod, newdata = 
                data.frame(
                  Treatment = xrange,
                  Temp = mean(Change$Temp), Salinity = srange)),
      lwd = 2)
lines(predict(main_mod, newdata = 
                data.frame(
                  Treatment = xrange)), col='purple', lwd=4)

avg_pred <- predict(sim_mod, newdata = 
                      data.frame(Tank = rep(tanks, each = 100),
                                 Treatment = rep(xrange,
                                                 times = 3),
                                 Temp = mean(Change$Temp), Salinity = srange))
avg_pred <- tapply(avg_pred, list(rep(xrange, times = 3)), mean)

lines(xrange, avg_pred, lty = 2, lwd=3)

# obs vs predicted plot

pred <- predict(sim_mod)

plot(Change$Pchlchange, pred)
abline(a=0, b=1)

avg_obs <- tapply(Change$Pchlchange, list(Change$Treatment), mean)
avg_pred <- tapply(pred, list(Change$Treatment), mean) 

plot(avg_obs, avg_pred)
abline(a=0, b=1)

acf(chlwq2$Chl.a) #looking for autocorrelation
#appear to have weak autocorrelation, only a few points outside of the 95% confidence interval



