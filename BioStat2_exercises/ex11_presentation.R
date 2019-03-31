#11. A scientist is interested in how genotype of a strawberry plant affects fruit yield. There
#are three levels of genotype (AA, AB, BB) and ten plots of land, three plants per plot. Each of the
#three genotypes is present in each plot.


straw <- read.table("./Datasets/strawbdat.sec", header=T, sep = "")
View(straw)
summary(straw)

#Land as factor!
straw$land<-as.factor(straw$land)

# straw$gtype is automaticaly a factor

#a) Perform an ANOVA, assuming one-way randomized block design.

#1

fit.straw <- aov(yield ~ gtype + land, data = straw)
summary(fit.straw)

#2 alternative
lm.straw <- lm(yield ~ gtype + land, data = straw)
summary(lm.straw)

fit.straw2 <- anova(lm.straw)
fit.straw2

#b) Repeat the analysis of variance without taking into account land effects.

#1
fit.noblock <- aov(yield ~ gtype, data = straw)
summary(fit.noblock)

#2 alternative
lm.noblock <- lm(yield ~ gtype, data = straw)
summary(lm.noblock)

fit.noblock2<- anova(lm.noblock)
fit.noblock2


#c) Compare the results in a) and b). Why are the degrees of freedom different? Which result would
#you use?


summary(fit.straw)
#             Df    Sum Sq    Mean Sq   F value   Pr(>F)   
#gtype        2     289.6     144.82    5.406     0.0145 *
#land         1     7.0       6.98      0.307     0.58437   
#Residuals    18    591.2     26.79 

summary(fit.noblock)
#             Df    Sum Sq    Mean Sq   F value   Pr(>F)   
#straw$gtype  2     289.6     144.82    6.536     0.00484 **
#Residuals    27    598.2     22.16    


summary(lm.straw)
#Adjusted R-squared:  0.124
#p-value:             0.2643

summary(lm.noblock)
#Adjusted R-squared:  0.2763
#p-value:             0.004841

# use the model with less parameters, better r2 and p vlaue. 
# signal is so strong no block is needed in experiment

# other checks

library(car)

par(mfrow = c(1, 2), cex = 0.5)

plot(fitted(lm.straw), resid(lm.straw),
     xlab = "Fitted values", ylab = "Residuals", main = "Tukey-Anscombe plot - lm.straw")
qqPlot(resid(lm.straw), dist = "norm",
       mean = mean(resid(lm.straw)), sd = sd(resid(lm.straw)),
       xlab = "Theoretical quantiles", ylab = "Empirical quantiles",
       main = "Q-Q plot of residuals - lm.straw")

plot(fitted(lm.noblock), resid(lm.noblock),
     xlab = "Fitted values", ylab = "Residuals", main = "Tukey-Anscombe plot")
qqPlot(resid(lm.noblock), dist = "norm",
       mean = mean(resid(lm.noblock)), sd = sd(resid(lm.noblock)),
       xlab = "Theoretical quantiles", ylab = "Empirical quantiles",
       main = "Q-Q plot of residuals")

library(lmtest)
lrtest(lm.noblock, lm.straw)
# Likelihood ratio test
# Model 1: yield ~ gtype
# Model 2: yield ~ gtype + land
#   #Df  LogLik Df Chisq Pr(>Chisq)
# 1   4 -87.460                    
# 2  13 -84.227  9 6.465     0.6926

# From p-value 0.6926 also tels us that adding 'land' will just fit the noise

library(MASS)
# using also the Akaike information criterion and starting from full model backword it comes also to
lm.straw.bw <- stepAIC(lm.straw, direction = "backward", trace = 0)
summary(lm.straw.bw)
# lm(formula = yield ~ gtype, data = straw)
# Residual standard error: 4.707 on 27 degrees of freedom
# Multiple R-squared:  0.3262,	Adjusted R-squared:  0.2763 
# F-statistic: 6.536 on 2 and 27 DF,  p-value: 0.004841
