library(MASS)
library("car")

compPois <- function(fdata, desc) {
  qqPlot(fdata, dist = "pois", main = paste(desc, " vs Poisson distribution"), xlab = "Theor. quantiles (poisson-distribution)", ylab = "Empirical quantiles", lambda = mean(fdata), envelope=.975)
}

# 28.

countsL <- read.table("counts.txt", header = F)
counts <- as.vector(t(countsL))

lambda <- mean(counts)
# a)
t <- rep(4000, length(counts))
plot(counts)
hist(counts)

ppois(0.975, 5, )

ppois(q = counts-1, lambda = lambda, lower.tail = F)

t.test(counts, mu=5)

for (i in 1:40) { 
    t.test(counts[i], mu=5)
}

t.test(rep(counts[1]), mu=5)

replicate(40)
poisson.test(counts, T=40, r=5)

compPois(counts, "palindromes")

?p.adjust(, "")