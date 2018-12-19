#install.packages("MASS")
library("MASS")
library(lmPerm)

#install.packages("car")
library("car")
library(ggplot2)

# 25.
astrodat <- read.table("astrodat.sec", header = T)
mdat <- data.frame(
  group = rep(c("before","after"), each = 26), 
  bef
                     )

# a)
t.test(astrodat$before, astrodat$after, alternative = "two.sided", paired = T)
# sign test, 
SignTest(astrodat$after, astrodat$before, alternative = "two.sided")
SIGN.test(astrodat$after, astrodat$before, alternative = "two.sided")


sign.test <- function(x=0,y=NULL,alternative="two.sided") {
#  browser()
  n <- sum((x-y)!=0)
  T <- sum(x<y)
  if (alternative=="less") {
    p.value<-pbinom(T,n,0.5)
  }
  if (alternative=="greater") {
    p.value<- 1-pbinom(T-1,n,0.5)
  }
  if (alternative=="two.sided") {
    p.value<-2*min(1-pbinom(T-1,n,0.5),pbinom(T,n,0.5))
    }
  list(n=n,alternative=alternative,T=T,p.value=p.value)
}

sign.test(astrodat$after, astrodat$before)

# b)
salt <- astrodat[ astrodat[,"salt"]==1, ]
control <- astrodat[ astrodat[,"salt"]==0, ]

qqPlot(salt$after)
t.test(control$before, control$after, paired = T)
t.test(salt$before, salt$after, paired = T)

t.test(control$before, salt$before, paired = F)


t.test(control$after, salt$after, paired = F)

#aovp(salt$after ~ control$after)
#permutation.test(control$after, salt$after, paired = F)
permTS(salt$after, control$after)

