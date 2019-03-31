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
    a <- 1 - .05/2
    fi <- qnorm(a)
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
