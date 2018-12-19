# 20.
# a)
pbinom(0, size = 10, prob = 0.3)

# b)

binom.test(4, n = 10, p = 0.3, alternative = "greater", conf.level = 0.99)
t.test(c(1,1,1,1,0,0,0,0,0,0), mu=0.3, alternative = "greater", conf.level = 0.99)

# c)
binom.power(p.alt=0.6, n=10, p=0.3, alternative="greater")
p <- qbinom(0.01, 10, 0.3, lower.tail = F)
beta <- pbinom(p, 10, 0.3, lower.tail = T)
#power
1 - beta
?pbinom
stats::pbinom(0, 10, 0.3, lower.tail = T)
DelayedArray::pbinom(0, 10, 0.3, lower.tail = T)

?qbinom
stats::qbinom(0.05, 10, 0.3, lower.tail = T)
DelayedArray::qbinom(0.05, 10, 0.3, lower.tail = T)

1 - sum(dbinom(1:8, size=10, prob= 0.6))

# 22
binom.test(20, n = 275, p = 0.075, alternative = "greater", conf.level = 0.95)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ?qbiono
?qbinom
