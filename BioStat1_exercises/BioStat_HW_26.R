# 26.

# a)
unpared

# b) 
# one sided test, alternativce: male greater then femal

# c)
males <- c(120, 107, 110, 116, 114, 111, 113, 117, 114, 112)
females <- c(110, 111, 107, 108, 110, 105, 107, 106, 111, 111)

qqPlot(males)
qqPlot(females)

x <- mean(males)
n <- length(males)
sx2 <- sd(males)^2

y <- mean(females)
m <- length(females)
sy2 <- sd(females)^2

# calculate s-pool: Slide 238
sPool <- sqrt(((n-1)*sx2 + (m-1)*sy2)/(n+m-2))
T <- (x - y)/(sPool*sqrt(1/n + 1/m))
# H0: T ??? t(df=n+m???2)
# significance level
a <- 0.05
# rejection interval:
#tn+m???2,1?????/2
t10_0_975 <- qt(1-a/2, n+m-2)
# -inf, -t10_0_975
-t10_0_975
# t10_0_975, +inf
t10_0_975

T

# # check Welch approximation
t.test(males, females, alternative = c("greater"), paired = F, var.equal = T)
#t.test(males, females, alternative = c("two.sided"), paired = F)

# 27.

# a)
set.seed(11)
xy.all <- c(males, females)

n <- length(males)
m <- length(females)

mean.diff <- function(group1) mean(xy.all[group1]) - mean(xy.all[-group1])

# original
(D <- mean.diff(1:n))
mean(males) - mean(females)
# permutated
N <- 1000
D.sample <- replicate(N, mean.diff(sample.int(n+m, size = n)))
quantile(D.sample, c(0.025, 0.975))

sum(abs(D.sample) >= abs(D))/N
(sum(abs(D.sample) >= abs(D)) + 1)/(N + 1)
(sum(D.sample >= D) + 1)/(N + 1)

# b)
library(perm)
permTS(males, females, alternative = "greater", paired = FALSE, conf.level = 0.95, method = "exact.mc")

# c)
t.test(males, females, alternative = "greater")
