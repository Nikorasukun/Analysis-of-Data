# alpha = P(rej h0 | h0), beta = P(rej h0 | h1)
# 1-alpha is the power of a test, probability of committing type 2 errors
# power = P(rej h0 | h1) = 1-beta
# alpha = false positive, beta = false negative
# alpha is called the size of the test
# 1-beta is called the power of the test
# usually alpha is a fixed value and beta is a function that depends on a variable


# one side, side right tail test
# h0 = mu <= mu0
# h1 = mu > mu0
# X ~ N(mu, sigma^2) where sigma^2 is known
# n is number of observations
# z test, zobs = (xbar - mu0) / ( sqrt(sigma^2/n) ) ~ N(0,1)
# Power(mu) = P(zobs > z_(1-alpha) | H1) =
# = P((xbar - mu0) / (sigma / sqrt(n)) +- mu1 > z_(1-alpha)|H1) =
# = P((xbar - mu0) / (sigma / sqrt(n)) +- mu1 > z_(1-alpha)|mu1) =
# = P((xbar - mu1 + mu1 - mu0) / (sigma / sqrt(n)) +- mu1 > z_(1-alpha)|mu1) =
# let's now call mu1-mu0 = delta #
# also xbar-mu1 / (--) under h1 is standardized, because mu1 is the true mean #
# = P(z > z_(1-alpha)-sqrt(n)*delta/sigma|mu1) =
# = 1 - P(z < z_(1-alpha)-sqrt(n)*delta/sigma|mu1) =
# the second part is just a distr function #
# 1 - fi(z_(1-alpha) - sqrt(n)*delta/sigma)
# delta is the only variable inside the expr, the higher the delta, the more powerful
# the test is. delta = mu1-mu0. if delta>>1, then fi(-)<<1, 1-fi(-) ~ 1

# let's try to translate all of this now
x = seq(-5,15,by=0.01)
y = dnorm(x)
sig.level = 0.05 #alpha
q1 = qnorm(sig.level, lower.tail=FALSE) # z_(1-alpha), could have also written qnorm(1-sig.level)

# plot x vs y
plot(x,y,type="l", xlim=c(-5,15), ylim=c(0,0.46), xlab = "", ylab="")
text(x = 0, y = 0.45, labels = expression("H"[0]))

# filling the alpha area
polygon(
  c(q1, x[x>q1], max(x)),
  c(0, y[x>q1], 0),
  col="gray"
)
text(x=1.95, y=0.03, labels=expression(alpha))

# let us work now with H1 instead of H0
y1 = dnorm(x, mean = 2)
lines(x,y1, xlim=c(-5,15))
text(x=2, y= 0.45, labels=expression("H"[1]))

polygon(
  c(min(x), x[x<q1], q1),
  c(0, y1[x<=q1], 0),
  col="blue", density=25
)
text(x = 1, y= 0.1, labels=expression(beta), cex=1.3)
polygon(
  c(q1, x[x>=q1], max(x)),
  c(0, y1[x>=q1], 0),
  col="red", density=25
)
text(x=2.6, y = 0.1, labels="Power")

# what is alpha
set.seed(12)
M = 10^5
n = 20
rej = numeric(M)

for(i in 1:M) {
  x = rnorm(n, mean=0,sd=1)
  rej[i] = (mean(x)-0) / ( sqrt(1 / n) ) > qnorm(0.95)
}

mean(rej) # this is how you compute the significance level

#About the power function
n = 50 # sample size is equal to 50
sigma = 3 # known value in the population so we can use a z test
sig.level = 0.05 #significance level
delta = 1 #difference between the true value and the one under H0
# \Delta = \mu_1 - \mu_0

# one sided test (right)
# H0 : mu <= mu0
# H1 : mu > mu0

# P(Z > z_{1-alpha} - sqrt(n)*\Delta / \sigma)

pnorm(
  qnorm(sig.level, lower.tail = FALSE) - sqrt(n) * delta / sigma,
  lower.tail = FALSE
)

# can we obtain an estimate of this by simulaton?
set.seed(1)
M = 10^5

rej = numeric(M)

for (i in 1:M) {
  x = rnorm(n, mean=1, sd=3)
  rej[i] = (mean(x) - 0) / (3 / sqrt(n)) > qnorm(0.95)
}

mean(rej)

# power function
# let us try now with many different delta values
deltas = seq(-5, 5, l = 1001)

power.z = pnorm(
  qnorm(sig.level, lower.tail = F) - sqrt(n) * deltas / sigma, lower.tail=F
)


plot(deltas, power.z, type= "l")
abline(h = sig.level, col="red")
abline(v = 0, col="red")
# before 0 h0 wins, after 0 h1 wins
# useful plot to distinguish which hypothesys to reject and which to embrace

# in virtuale teaching materials you can find some more examples of the power on the test
# replicate the plot with ggplot and play with t test function for hyp test

# use ggplot2
require(ggplot2)

df = data.frame(delta = deltas, power = power.z)

ggplot(df, aes(x=delta, y=power))+
  geom_line()+
  geom_hline(yintercept=sig.level, color="red")+
  geom_vline(xintercept=0, color="red")+
  labs(x=expression(Delta), y="Power")


# the t test in R
# H0 : mu = 22
# H1 : mu != 22

set.seed(7)
n = 30
x = rnorm(n, mean = 20, sd = 2)

t.test(x)
t.test(x, mu=22)
t.test(x, mu=22, alternative="l")
t.test(x, mu=22, alternative="g")
t.test(x, mu=22, conf.level = 0.99)

# can't use the t.test without having the data, so it's important to know also
# the other method for when you have sinthetic measures instead of full dataset






