
length <- seq(0, 12, by=0.1) # lengt measurments

fx_y0 <- function(length) dnorm(length, mean = 7, sd = 1)
fx_y1 <- function(length) dnorm(length, mean = 3, sd = 1)

# a) 
fx_evidence <- function(length, pi_0, pi_1) pi_0*fx_y0(length) + pi_1*fx_y1(length)

plot_class_prob <- function(pi_0, pi_1) {
  plot(length, fx_y0(length), main = "Class-conditional probability",  ylab="prob. density", ylim=c(0,0.5), type="l", lty = 1, col="cyan3")
  lines(length, pi_0*fx_y0(length), type="l", lty = 2, col="cyan3")
  lines(length, fx_y1(length),type="l", lty = 1, col="brown1")
  lines(length, pi_1*fx_y1(length), type="l", lty = 2, col="brown1")
  lines(length, fx_evidence(length, pi_0, pi_1), type="l", lty = 1, col="darkblue")
  decision_boundary <- (log(pi_1/pi_0) + 40) / 8
  abline(v=decision_boundary, lty=2, col="black" )
  text(decision_boundary, 0.5, "decision boundary", cex= 0.7)

  legend("topright", legend = c("sea brass", "salmon", "a posteriori", "evidence"), col=c("cyan3", "brown1", "gray", "darkblue"), lty = c(1, 1, 2, 1), cex = 0.7)
} 

plot_class_prob(0.5, 0.5)

# b) 

plot_class_prob(2/3, 1/3)
