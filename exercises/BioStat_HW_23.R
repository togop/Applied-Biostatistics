#install.packages("MASS")
library("MASS")

#install.packages("car")
library("car")
library(ggplot2)

# 23.
training <- read.table("training.txt", sep = "\t", header = TRUE)
before <- training$before
after <- training$after
diff <- after - before

# a)
hist(before, breaks = 10)
hist(after, breaks = 10)
qqnorm(before)
qqnorm(after)

qqPlot(before, dist = "norm", mean = mean(before), sd = sd(before))
qqPlot(after, dist = "norm", mean = mean(after), sd = sd(after))
qqPlot(diff, dist = "norm", mean = mean(diff), sd = sd(diff ))
hist(diff, breaks = 10)

# b)
t.test(before, conf.level = 0.95)
t.test(after, conf.level = 0.95)
t.test(diff, conf.level = 0.95)

t.test(after, before, paired = TRUE, alternative = "two.sided", conf.level = 0.95)
t.test(after, before, paired = TRUE, alternative = "two.sided", conf.level = 0.95)
wilcox.test(after, before, paired = TRUE, exact = TRUE, alternative = "two.sided", conf.level = 0.95)
