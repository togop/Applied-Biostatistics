#install.packages("MASS")
library("MASS")

#install.packages("car")
library("car")
library(ggplot2)

# 17.
fracture <- read.table("bone-fracture.csv", sep = ";", header = TRUE)


getMeanCI <- function(fdata) {
  mu <- mean(fdata)
  sd <- sd(fdata)
  n <- length(fdata)
  fi <- qnorm(0.975)
  
  ci.start <- mu - fi*sd/sqrt(n)
  ci.end <- mu + fi*sd/sqrt(n)
  return(c(ci.start, ci.end))
}

getBinCI <- function(p) {
  fi <- qnorm(0.975)
  n <- length(fdata)
  
  ci.start <- p - fi*sqrt(p*(1-p)/n)
  ci.end <- p + fi*sqrt(p*(1-p)/n)
  return(c(ci.start, ci.end))
}

getUnifCI <- function(p) {
  fi <- qnorm(0.975)
  n <- length(fdata)
  
  ci.start <- p - fi*sqrt(p*(1-p)/n)
  ci.end <- p + fi*sqrt(p*(1-p)/n)
  return(c(ci.start, ci.end))
}

histPlot <- function(fdata, desc) {
  #  def.par=par(no.readonly = T)
  #  layout.show(layout(matrix(c(1,1,2,3), nrow=2, ncol=2, byrow = TRUE)))
  
  dmin <- floor(min(fdata))
  dmax <- ceiling(max(fdata))
  size <- length(fdata)
  mu <- mean(fdata)
  
  sd <- sd(fdata)
  sem <- sd(fdata)/sqrt(size)
  maxD <- dnorm( mu, mean = mu, sd = sd)
  
  hist(fdata, freq = F, breaks = 20, xlab = desc, xlim = c(dmin, dmax), main = paste("Histogram of ", desc))
  abline(v = mu, lwd = 2, col = "blue")
  
  lines(c(mu - sem, mu + sem), c(maxD,maxD), lwd = 2, col = "red")
  text(mu + sem, maxD, "SEM", pos = 2, offset = 2)
  
  lines(c(mu - sd, mu + sd), c(maxD/2,maxD/2), lwd = 2, col = "orange")
  text(mu + sd, maxD/2, "SD", pos = 2, offset = 2)
  
  xseq=seq(dmin, dmax, (dmax-dmin)/100)
  lines(xseq, dnorm(xseq, mean = mu, sd = sd), lwd = 2, col = "steelblue")
  
  unif <- dunif(xseq, min = dmin, max = dmax)
  lines(xseq, unif, lwd = 2, col = "green")
  text(dmax, unif[1], "unif", pos = 2, offset = 2)
#  lines(xseq, dpois(xseq, lambda = round(mean(fdata))), lwd = 2, col = "red")
}

compNorm <- function(fdata, desc) {
  # another test of normality using the Shapiro-Wilk-Test
  shp.test <- shapiro.test(fdata)
  spw <- round(shp.test$statistic,3)
  spval <- round(shp.test$p.value,3)
  # W close to 1 and p-value < 0.05 means the data is not normaly distributed
  
  qqPlot(fdata, dist = "norm", main = paste(desc, " vs Normal distribution"), xlab = paste("Theor. quantiles (norm) shapiro W:", spw, " , p-val:", spval), ylab = "Empirical quantiles", envelope=.975)
}
# == qqnorm(fdata, pch = 1, frame = FALSE); qqline(fdata, col = "steelblue", lwd = 2)

compUnif <- function(fdata, desc) {
  qqPlot(fdata, dist = "unif", main = paste(desc, " vs Uniform distribution"), xlab = "Theor. quantiles (unif)", ylab = "Empirical quantiles", envelope=.975)
}

compExp <- function(fdata, desc) {
  qqPlot(fdata, dist = "exp", main = paste(desc, " vs Exponential distribution"), xlab = "Theor. quantiles (exp-distribution)", ylab = "Empirical quantiles", rate = 1/mean(fdata), envelope=.975)
}

compPois <- function(fdata, desc) {
  qqPlot(fdata, dist = "pois", main = paste(desc, " vs Poisson distribution"), xlab = "Theor. quantiles (poisson-distribution)", ylab = "Empirical quantiles", lambda = mean(fdata), envelope=.975)
}

#  qqPlot(fdata, dist = "t", main = paste(desc, " vs Student's T distribution"), xlab = "Theor. quantiles (t-distribution)", ylab = "Empirical quantiles", df = 49)

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

####################################

##  Concentration
fdata <- fracture$conc
desc <- "concentration"

#plot(density(fdata))
#lines(xseq, dnorm(xseq, mean = mean(fdata), sd = sd(fdata)), lwd = 2, col = "steelblue")

histPlot(fdata, desc)

# compare with uniform
compUnif(fdata, desc)
compCDF(fdata, desc, punif, min(fdata), max(fdata))
# doesn't look good fit

# compare with normal
compNorm(fdata, desc)
compCDF(fdata, desc, pnorm, mean(fdata), sd(fdata))
# it looks good fit

# Conclusion: Normal Distribution with confidential interval for the mean or Lambda:
getMeanCI(fdata)

#compExp(fdata, desc)
#compCDF(fdata, desc, pexp, 1/mean(fdata))

##  Difference
fdata <- fracture$dif
desc <- "difference" 

histPlot(fdata, desc)

# compare with normal
compNorm(fdata, desc)
compCDF(fdata, desc, pnorm, mean(fdata), sd(fdata))
# doesn't look very good fit

# compare with uniform
compUnif(fdata, desc)
compCDF(fdata, desc, punif, min(fdata), max(fdata))
# it looks good fit

# Conclusion: Iniform distribution with confident interval (not big sense):
getMeanCI(fdata)

#compExp(fdata, desc)
#compCDF(fdata, desc, pexp, 1/mean(fdata))

## Number of cells
fdata <- fracture$no.cells
desc <- "number of cells"

histPlot(fdata, desc)

# compare with uniform
compUnif(fdata, desc)
compCDF(fdata, desc, punif, min(fdata), max(fdata))

# compare with normal
compNorm(fdata, desc)
compCDF(fdata, desc, pnorm, mean(fdata), sd(fdata))
# it looks good fit

#compExp(fdata, desc)
#compCDF(fdata, desc, pexp, 1/mean(fdata))

# compare with poisson
compPois(fdata, desc)
compCDF(fdata, desc, ppois, mean(fdata))
# it looks even better fit

# Conclusion: Normal or Poisson Distribution with confidential interval for the mean :
getMeanCI(fdata)
# Poissson is prefferred as more parsimoneous
  
## Hit
fdata <- fracture$hit
desc <- "hit"

histPlot(fdata, desc)
# makes no big sense

#qqPlot(fdata, dist = "binom", main = " vs binom distribution", xlab = "Theor. quantiles (binom-distribution)", ylab = "Empirical quantiles", size = 1, prob = mean(fdata))
#var(fdata)

# Ho : E. Bruch guess just by chance
# Ha : E. Bruch guest most of the casses
bintest <- binom.test(length(fdata[fdata == 1]), length(fdata), p = 0.5) 
bintest
bintest$p.value
# P-value < 0.05: Ho rejected, acceppt Ha

# Concliusion: Binomial distribution with estimated p confidential interval:
getBinCI(mean(fdata))
# this not so sure!

#plot(qnbinom(ppoints(fdata), size = 1, mu = mean(fdata)))
#abline(0,1)
