library(knitr)
opts_knit$set(root.dir='../')

library(car)

chlwq2 <- read.csv("./Data/ChlaWQReplicateAcuteData2.csv")

class(chlwq2$Date)
chlwq2$Time <- factor(chlwq2$Time, levels = c("Pre", "Post")) #making sure pre always comes before post on figures
chlwq2$Chl.a1 <- log(chlwq2$Chl.a)
#transforming chlorophyll data since it is not normal



chlwq2$Date <- as.Date(chlwq2$Date, "%m.%d.%y")
class(chlwq2$Date)
chlwq2$Date
#changing class, so that R understands these are dates

main <- lm(Chl.a1 ~ Tank + Treatment + Temp + 
             Temp:Treatment + Tank:Treatment + Time + Ozone, 
           data = chlwq2)

acf(chlwq2$Chl.a) #looking for autocorrelation
#appear to have weak autocorrelation, only a few points outside of the 95% confidence interval

chladist <- dist(chlwq2$Chl.a1)
xy_dist <- dist(chlwq2$Date)

max_dist = max(xy_dist) / 2

# plot result
plot(xy_dist, chladist)
abline(lm(chladist ~ xy_dist), lwd=3, col='red')
lines(lowess(xy_dist, chladist), lwd=3, col='pink')
abline(v = max_dist, col='red', lwd=3, lty=2)

obs_cor = cor(xy_dist, chladist)
obs_cor

chlamantel = mantel(xy_dist, chladist)
chlamantel

chl_gls <- gls(Chl.a1 ~ Tank + Treatment + Temp + 
                 Temp:Treatment + Tank:Treatment + Time + Ozone, 
               data = chlwq2)
plot(Variogram (chl_gls))


chl_exp = update(chl_gls, corr=corExp())
# examine fit of error model to the raw model residuals
# note this function defaults to displaying pearson standardized residuals
# resType='p' or resType='pearson'
plot(Variogram(chl_exp, maxDist = max_dist))



# that doesn't look so good because clearly the model does not fit the error 
# very well, it appears that there is a nugget (i.e., non-zero y-intercept)
# Let's examine the normalized residuals in which the residuals are 
# devided by the estimate of the variance-covariance matrix. If the model
# fits well these residuals should be normally distributed.
plot(Variogram(chl_exp, resType='normalized', maxDist = max_dist))




# let's look at the same model but with a nugget
chl_exp_nug = update(chl_exp, corr=corExp(nugget=T))
plot(Variogram(chl_exp_nug, maxDist = max_dist))


plot(Variogram(chl_exp_nug, resType='n', maxDist = max_dist))


# those look like they provide a better fit to the data

# let's examine the rational quadratic error model
chl_rat_nug = update(chl_gls, corr=corRatio(nugget=T))
# examine fit of error model to model residuals
plot(Variogram(chl_rat_nug, maxDist = max_dist))


plot(Variogram(chl_rat_nug, resType='n', maxDist = max_dist))


# this model seems to fit about as a good as the exponential with the nugget

# let's compare the models
anova(chl_gls, chl_exp, chl_exp_nug, chl_rat_nug, test=F)

summary(chl_rat_nug)


anova(main, chl_rat_nug)

#weak autocorrelation, linear model explains it well.