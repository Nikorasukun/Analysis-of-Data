# initial data
n.draws = 1200
means = sds = rep(NA, n.draws)

set.seed(6)
x = rnorm(n=n.draws, mean=1, sd=3)

# initial configuration
means[1] = x[1]
sds[1] = NA

# for method
for (i in 1:n.draws) {
  means[i] = mean(x[1:i])
  sds[i] = sd(x[1:i])
}

# with cumsum
means1 = cumsum(x)/(1:n.draws)

# some checks
means[1:10]
means1[1:10]

sum (abs(means1-means))

# my try on sd with vectorization
ex = cumsum(x^2)/(1:n.draws)
ex2 = (cumsum(x)/(1:n.draws))^2
vx = ex2 - ex
factor = 1:n.draws/(0:(n.draws-1))
vtilde=factor*vx
stilde = sqrt(vtilde)

# professor solution
sds1 = sqrt((cumsum(x^2) / (1:n.draws) - ( cumsum(x) / (1:n.draws) )^2)
            * (1:n.draws)/(0:(n.draws-1)))

# check between my solution and professor's
sum(abs(sds1-stilde), na.rm = T)

# plotting what we've done
windows()
plot(
  means,
  xlab="Number of draws",
  ylab="Mean and Sd",
  main="Law of Large Numbers",
  ylim=c(0,4),
  col = "blue"
)
abline(h=1,col="red",lty=2)
points(sds, col="orange")
abline(h=3, col="red", lty=3)

# Dynamic Plot
windows()
plot(
  means,
  xlab="Number of draws",
  ylab="Mean and Sd",
  main="Law of Large Numbers",
  ylim=c(0,4),
  col = "white"
)
abline(h=c(1,3), col="red", lty=c(2,3))
for(i in seq(3,n.draws, by=2)){
  Sys.sleep(0.01)
  lines((i - 2):i, means[(i - 2):i], col="blue", lwd=2)
  points((i-2):i, sds[(i-2):i], col="orange")
}



