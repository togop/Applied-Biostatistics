# Q13
x <- c(1000,2000,3000,10000,500,5000)
y <- c(50,100,150,500,25,250)

lm(x~y)
mx <- mean(x)
my <- mean(y)

sd(x)
cor(y,x)

(b1 <- cov(y,x)/(sd(x)*sd(y)))
(b1 <- cor(y,x)*sd(x)/sd(y))
(b1 <- cov(y,x)/var(y))
(b1 <- sum((y-my)*(x-mx))/sum((y-my)^2))

(r <- cor(y,x))
(r <- cov(y,x)/(sd(x)*sd(y)))

pow2 <- function(x) x^2
pow <- function(x,n) x^n

(r <- b1 * sd(y)/sd(x))
(r <- b1 * sqrt(sum(sapply(y-my, pow2))) / sqrt(sum(sapply(x-mx, pow2))))

# Q15
x <- c(45,12,3,17,32)
y <- c(275, 401, 420, 212,365)

(mx <- mean(x))
(sx <- sd(x))

(my <- mean(y))
(sy <- sd(y))

cov(x,y)/(sx*sy)

cor(x,y)
-2740.04 / (33.38*176.66)

cov(x,y)
sum((x-mx)*(y-my))/4
  
# Q21 sign test
pbinom(8,11,0.5,lower.tail = F)

sum(dbinom(9:11,11,0.5))
1-pbinom(8,11,0.5)

# Q 34
dnorm(2.3)
2*(1-pt(2.3,29))

# wilcoxon-test
bloodflow <- read.table("bloodFlow.csv", header = TRUE, sep = ",")
wilcox.test(bloodflow$Caffeine, bloodflow$Baseline, paired = TRUE, exact = TRUE, alternative = "two.sided", conf.level = 0.95)
X <- bloodflow$Caffeine - bloodflow$Baseline
N <- length(bloodflow$Baseline)
  
X <- c(-1.85,-.25,-.88,-1.46,-1.05,-1.67,-1.74,-.33)
W <- -36
varW <- function(n) n*(n+1)*(2*n+1)/6
N <- 8
(Zw <- (W-0.5)/sqrt(varW(N)))
pnorm(Zw)

# https://newonlinecourses.science.psu.edu/stat414/node/319/
wPri <- function(w, n) (w - (n*(n+1)/4))/sqrt(n*(n+1)*(2*n+1)/24)
(Wpri <- wPri(W,N))
2*pnorm(Wpri)

(Zw <- (W-0.5)*sqrt(N)/varW(N))
wilcox.test(X)

# Exercises
# 8

dp <- function(x,l) exp(-l)*l^x/factorial(x)
dp(1,3.05)
dpois(1,3.05)
1-sum(dpois(0:1,3.05))
1-sum(dpois(0:1,3.05))
1-ppois(1,3.05)

# 13
p <- .1
n <- 50
# c)
sum(dbinom(0:3,n,p))
pbinom(3,n,p)
# 0.2502939
# d)
pnorm(3, mean=n*p, sd=n*p*(1-p))
# 0.3283606
