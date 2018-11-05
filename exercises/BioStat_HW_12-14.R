# 12
count <- read.table("count.txt", header = T)

str(count)
class(count)

summary(count)
dim(count)

var(count$a.v1)
var(count[,2])
var(count[,3])
var(count[,4])
quantile(count$a.v1)

defpar = par(no.readonly = F)
par(mfrow = c(1,2))
hist(count[,1], breaks = 25)
boxplot(count[,1])

# Ex 13:

# a) p - error rate  X -> Bin(50, p=.1)

# b)
dbinom(3, 50, .1)

# c)
pbinom(3, 50, .1)

# d) 
pnorm(3, 50*0.1, sqrt(50*0.1*0.9))

# 15

# a)
dbinom(3, 5, .1)
(20/2)* 0.1^3 * .9^2
choose(5,3)* 0.1^3 * .9^2
 
# b)
# expected value: mean
mu  <- 150*0.1
# variance
sd <- 150*0.1*(1-0.1)

# beteween 15 and 20

pbinom(20,150,.1) - pbinom(15,150,.1)
sum(sapply((16:20), function(n) choose(150,n)* 0.1^n * .9^(150-n)))

pnorm((20-mu)/sqrt(sd)) - pnorm((15-mu)/sqrt(sd))

# 16

# a)
set.seed(7)

countInCI <- function(n){ 
  count <- 0
#  n <- 50
  rate <- 2
  for(i in 1:1000){
    sample <- rexp(n, rate)
    emu <- mean(sample)
    esd <- sd(sample)
    esd
    a <- 1 - .1/2
    fi <- qnorm(a,0)
    ci.start <- emu - fi*esd/sqrt(n)
    ci.end <- emu + fi*esd/sqrt(n)
    if (0.5 >= ci.start & 0.5 <= ci.end) {
      count <- count + 1 
    }
  }  
 return(count)
}
countInCI(50)

# b)
countInCI(5)

# 17.
fracture <- read.table("bone-fracture.csv", sep = ";", header = TRUE)

hist(fracture$conc, freq = F, breaks = 10, xlim = c(2.5,4.5) )
xseq=seq(2.5,4.5, .1)
lines(xseq, dnorm(xseq, mean=mean(fracture$conc), sd=sd(fracture$conc)))

qqnorm(fracture$conc)
qqline(fracture$conc)

#install.packages("qqplotr")

#install.packages("EnvStats")
#library("EnvStats")

#install.packages("car")
#library("car")

#install.packages("MASS")
library("MASS")


fdata <- fracture$conc
desc <- "concentration"


qqnorm(fdata, pch = 1, frame = FALSE)
qqline(fdata, col = "steelblue", lwd = 2)

library("car")

compDists <- function(fdata, desc) {
  def.par=par(no.readonly = T)
  layout.show(layout(matrix(c(1,1,2,3,4,5), nrow=3, ncol=2, byrow = TRUE)))
  
  summary(fdata)
  
  dmin <- floor(min(fdata))
  dmax <- ceiling(max(fdata))
  
  #dmin <- min(fdata)
  #dmax <- max(fdata)
  
  hist(fdata, freq = F, breaks = 20, xlab = desc, xlim = c(dmin, dmax), main = paste("Histogram of ", desc))
  xseq=seq(dmin,dmax, (dmax-dmin)/100)
  lines(xseq, dnorm(xseq, mean=mean(fdata), sd=sd(fdata)), lwd = 2, col = "steelblue")
  lines(xseq, dunif(xseq, min = dmin, max = dmax), lwd = 2, col = "green")
  lines(xseq, dpois(xseq, lambda = mean(fdata)), lwd = 2, col = "red")
  
  # another test of normality using the Shapiro-Wilk-Test
  shp.test <- shapiro.test(fdata)
  spw <- round(shp.test$statistic,3)
  spval <- round(shp.test$p.value,3)
  # p-value > 0.05 means the data is stil normaly distributed otherwise not
  
  qqPlot(fdata, dist = "norm", main = "vs Normal distribution", xlab = paste("Theor. quantiles (norm) shapiro W:", spw, " , p-val:", spval), ylab = "Empirical quantiles", envelope=.975)
  qqPlot(fdata, dist = "unif", main = " vs Uniform distribution", xlab = "Theor. quantiles (unif)", ylab = "Empirical quantiles", envelope=.975)
  qqPlot(fdata, dist = "pois", main = " vs Poisson distribution", xlab = "Theor. quantiles (poisson-distribution)", ylab = "Empirical quantiles", lambda = mean(fdata))
  qqPlot(fdata, dist = "exp", main = " vs Exponential distribution", xlab = "Theor. quantiles (exp-distribution)", ylab = "Empirical quantiles")
#  qqPlot(fdata, dist = "t", main = " vs Student's T distribution", xlab = "Theor. quantiles (t-distribution)", ylab = "Empirical quantiles", df = 49)
  
  par(def.par)
}

compDists(fracture$conc, "concentration")

compDists(fracture$dif, "difference")

fdata <- fracture$no.cells
desc <- "number of cells"

compDists(fracture$no.cells, "number of cells")

compDists(fracture$hit, "hit")


fdata <- fracture$hit
desc <- "hit"
qqPlot(fdata, dist = "binom", main = " vs binom distribution", xlab = "Theor. quantiles (binom-distribution)", ylab = "Empirical quantiles", size=1, prob=mean(fdata))
var(fdata)

#?qqPlot
#, param.list=list(mean=mean(fracture$conc), sd=sd(fracture$conc))

#library("EnvStats")
#qqPlot(fracture$conc, dist = "norm", "Compare with Normale distribution", xlab = "Theor. quantiles (norm)", ylab = "Empirical quantiles", envelope=.975)


hist(fracture$dif, freq = F, breaks = 50, xlim = c(-3,3))
qqPlot(fracture$dif, y = NULL, distribution = "norm", "Compare with Normale distribution", xlab = "Theor. quantiles (norm)", ylab = "Empirical quantiles")
qqPlot(fracture$dif, dist = "unif", "estimated parameters of chosen distr", xlab = "Theor. quantiles (unif)", ylab = "Empirical quantiles")

qqPlot(fracture$conc, dist = "binom", param.list=list(size=50), "estimated parameters of chosen distr", xlab = "Theor. quantiles (unif)", ylab = "Empirical quantiles")
#, param.list = list(mean = 0, sd = 1)
ggplot(fracture, aes(x.plot,y.plot)) + geom_smooth(method='lm',formula=conc~dif)

barplot(fracture$conc)



hist(fracture$no.cells, freq = F, breaks = 25, xlim = c(140,210))
