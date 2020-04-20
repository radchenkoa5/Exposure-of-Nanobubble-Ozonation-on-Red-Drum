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

#Looking for trends within the data 
ggplot(chlwq2, aes(Treatment, Chl.a1)) +
  geom_point(aes(col=Date))
facet_wrap(~Tank)

ggplot(chlwq2, aes(Temp, Chl.a1)) +
  geom_point(aes(col=Date)) +
  facet_wrap(~Tank)

ggplot(chlwq2, aes(Time, Chl.a1)) +
  geom_point(aes(col=Date)) +
  facet_wrap(~Tank)

#predict values based on the model and compare them to actual to see how well the model predicts data, compare the two
#Looking at the data untransformed 

sim_mod <- lm(Chl.a ~ Treatment + Temp + Tank + Tank:Treatment + Temp:Treatment, data = chlwq2)
sub_mod <- update(sim_mod, . ~ . - Tank - Tank:Treatment)
main_mod <- lm(Chl.a ~ Treatment , data = chlwq2)
summary(sim_mod)
plot(sim_mod)

xrange <- seq(min(chlwq2$Treatment), max(chlwq2$Treatment), length.out = 100)


tanks <- levels(chlwq2$Tank)
plot(Chl.a ~ Treatment, data = chlwq2, ylim = c(0, 15), xlab = 'Volume Ozonated (%)', ylab = 'Chlorophyll a concentration (ug/L)')
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


#Figure without tank line elements
sim_mod <- lm(Chl.a1 ~ Treatment + Temp + Tank + Treatment:Temp + 
                Treatment:Tank, data = chlwq)
sub_mod <- update(sim_mod, . ~ . - Tank - Tank:Treatment)
main_mod <- lm(Chl.a1 ~ Treatment , data = chlwq2)
summary(sim_mod)


xrange <- seq(min(chlwq2$Treatment), max(chlwq2$Treatment), length.out = 100)


tanks <- levels(chlwq2$Tank)
plot(Chl.a1 ~ Treatment, data = chlwq2, ylim = c(0, 4), xlab = 'Volume Ozonated (%)', ylab = 'Chlorophyll a concentration (ug/L)')
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


#Now recreating this graph with transformed chlorophyll a data
#predict values based on the model and compare them to actual to see how well the model predicts data, compare the two


modsum <- summary(main3)
r2 <- modsum$adj.r.squared
p <- 'p = 1.675e-12'




#predict values based on the model and compare them to actual to see how well the model predicts data, compare the two

sim_mod <- lm(Chl.a1 ~ Treatment + Temp + Tank + Tank:Treatment + 
                Temp:Treatment, data = chlwq2)
sub_mod <- update(sim_mod, . ~ . - Tank - Tank:Treatment)
main_mod <- lm(Chl.a1 ~ Treatment , data = chlwq2)
summary(sim_mod)


xrange <- seq(min(chlwq2$Treatment), max(chlwq2$Treatment), length.out = 100)


tanks <- levels(chlwq2$Tank)
plot(Chl.a1 ~ Treatment, data = chlwq2, ylim = c(0, 4), xlab = 'Volume Ozonated (%)', ylab = 'log(Chlorophyll a concentration (ug/L)')


avg_pred <- predict(sim_mod, newdata = 
                      data.frame(Tank = rep(tanks, each = 100),
                                 Treatment = rep(xrange,
                                                 times = 3),
                                 Temp = mean(chlwq2$Temp)))
avg_pred <- tapply(avg_pred, list(rep(xrange, times = 3)), mean)

lines(xrange, avg_pred, lty = 2, lwd=3)
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 47, y = 4, labels = mylabel)
text(x = 47, y = 3.7, labels = p)


Change <- read.csv("./Data/PercentChlaChange.csv")

#Exploring the percent change in chlorophyll a data
ggplot(Change, aes(Treatment, Pchlchange)) +
  geom_point(aes(col=Temp))
facet_wrap(~Tank)

ggplot(Change, aes(Temp, Pchlchange)) +
  geom_point(aes(col=Treatment)) 


ggplot(Change, aes(Treatment, Pchlchange), xl) +
  geom_point(aes(col=Salinity)) 






#predict values based on the model and compare them to actual to see how well the model predicts data, compare the two
#Looking at the percent change in chlorophyll a now
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


#Removing all individual tank lines on the same figure
#predict values based on the model and compare them to actual to see how well the model predicts data, compare the two

sim_mod <- lm(Pchlchange ~ Treatment + Temp + Salinity + Tank + 
                Treatment:Temp, data = Change)
sub_mod <- update(sim_mod, . ~ . - Tank)
main_mod <- lm(Pchlchange ~ Treatment , data = Change)
summary(sim_mod)


xrange <- seq(min(Change$Treatment), max(Change$Treatment), length.out = 100)
srange <- seq(min(Change$Salinity), max(Change$Salinity), length.out = 100)
Temp = mean(Change$Temp)
tanks <- levels(Change$Tank)

plot(Pchlchange ~ Treatment, data = Change, ylim = c(-80, 30), xlab = 'Volume Ozonated (%)', ylab = 'Mean Change in Chlorophyll a (%)')


lines(predict(pchl3, newdata = 
                data.frame(
                  Treatment = xrange, Temp = mean(Change$Temp)), col='purple', lwd=4))

mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 47, y = 25, labels = mylabel)
text(x = 47, y = 18, labels = p)

#Model does not visually fit data as a percent change as well as it does the transformed chlorophyll a data

#Looking at the interaction of two improtant factors in predicting percent chlorophyll change

ggplot(Change, aes(Treatment, Pchlchange)) +
  geom_point(aes(col=Temp)) +
  labs(x = 'Volume Ozonated (%)', y = 'Change in Chlorophyll a Concentration (%)')

ggplot(Change, aes(Temp, Pchlchange)) +
  geom_point(aes(col=Treatment)) +
  labs(x = 'Temperature (C)', y = 'Change in Chlorophyll a Concentration (%)')

ggplot(Change, aes(Treatment, Pchlchange), xl) +
  geom_point(aes(col=Salinity)) +
  labs(x = 'Volume Ozonated (%)', y = 'Change in Chlorophyll a Concentration (%)')

