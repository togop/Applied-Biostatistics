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
