#install.packages("MASS")
library("MASS")

#install.packages("car")

library(car)
library(ggplot2)

compCDF <- function(fdata, desc, distr, darg1, darg2=NULL) {
  distrName <- as.character(substitute(distr))
  
  #?ks.test(fdata, distrName, darg1, darg2)
  df <- data.frame(fdata)
  ed <- ecdf(df$fdata)
  p <- ggplot(aes(fdata), data = df) + stat_ecdf() + theme_bw() + stat_function(fun = distr, args = list(darg1, darg2), col="steelblue")
  p <- p + labs(title=paste("ECDF and theoretical CDF ", distrName)) + xlab(desc) + ylab("comulative probability")
  #  maxdiffidx <- which.max(abs(ed(dd$fdata) - distr(df$fdata, darg1, darg2)))
  #  maxdiffat <- df$fdata[maxdiffidx]
  #  p <- p + geom_vline(xintercept=maxdiffat, lty=2, col="red")
  p
}

# 19.
d.pet <- read.table("count.txt", header = TRUE)



# a)
fdata <- d.pet$a.v1
lambda <- mean(fdata)

compCDF(d.pet$a.v1, "clicks", ppois, lambda)

len <- length(fdata)
xs <- seq(1,len)
plot(xs, dpois(xs, lambda = lambda) )


# b)
sed(7)

rps <- rpois(len, lambda)
#hist(rps, breaks = 10)
plot(table(rps))
barplot(table(rps))
     
plot(density(rps))

# c)
hist(fdata, breaks = 10)
plot(density(fdata))


fdata <- d.pet$a.v1
getMeanCI <- function(fdata) {
  mu <- mean(fdata)
  sd <- sd(fdata)
  n <- length(fdata)
  fi <- qnorm(0.975)
  
  ci.start <- mu - fi*sd/sqrt(n)
  ci.end <- mu + fi*sd/sqrt(n)
  return(c(ci.start, ci.end))
}

getMeanCI(fdata)
getMeanCI(rps)

qqPlot(rps, dist = "pois", main = "simulated data vs Poisson distribution", xlab = "Theor. quantiles (poisson-distribution)", ylab = "Empirical quantiles", lambda = lambda, envelope=.975)
qqPlot(fdata, dist = "pois", main = "real data vs Poisson distribution", xlab = "Theor. quantiles (poisson-distribution)", ylab = "Empirical quantiles", lambda = lambda, envelope=.975)


        # not better