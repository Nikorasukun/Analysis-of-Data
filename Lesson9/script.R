# setting the seed
set.seed(123)

# starting variables
mu = 10
sigma2 = 3
n = 10

# some checks
qnorm(0.95)
qt(0.95, df=10)
qt(0.95, df=500)

# creating our x
x = rnorm(n, mean=mu, sd=sqrt(sigma2))

# our significance level for the tests
alpha = 0.05

# creating variables that will be useful in our next calculations
t.alpha.half = qt(1-alpha/2, df=n-1)
x.bar=mean(x)
s2.tilde = var(x)

# confidence interval
LL = x.bar - t.alpha.half*sqrt(s2.tilde/n)
UL = x.bar + t.alpha.half*sqrt(s2.tilde/n)

# full visualization of the CI
cbind(LL, x.bar, UL) # our true mean is inside


# Simulation Study : sampling distr of the CI

# P(LL<theta<UL)=1-alpha, 1-alpha out of 1 intervals will have theta inside

# either the P is 0 or 1
n.draws = 100

# empty boxes
sample.means = sample.variances = lower.limits = upper.limits = rep(NA, n.draws)
  
for (i in 1:n.draws) {
  x = rnorm(n,mu,sqrt(sigma2))
  
  sample.means[i] = mean(x)
  sample.variances[i] = var(x)
  lower.limits[i] = sample.means[i] - t.alpha.half * sqrt(sample.variances[i]/n)
  upper.limits[i] = sample.means[i] + t.alpha.half * sqrt(sample.variances[i]/n)
  
}

# Compute the coverage probability by simulation
# Also known as the Empirical Coverage

cover = ifelse(lower.limits < mu & upper.limits > mu, 1, 0)
mean(cover)

cols = c("blue", "green")
windows()
plot(1:n.draws,
     sample.means,
     ylim = c(min(lower.limits), max(upper.limits)),
     pch=19,
     )

segments(1:n.draws, lower.limits,
         1:n.draws, upper.limits,
         col = cols[cover+1])

abline(h = mu, col="red")

# Gamma Distribution
xg = seq(0.1, 20, by=0.1)
yg = dgamma(xg, shape=5, scale = 1) # alpha = shape, lamba = scale
plot(xg,yg,type="l")
lines(xg, dexp(xg, rate=1), col="red")

# Paramteres of gamma
alpha.gamma = .1
beta.gamma = 1
mu.gamma = alpha.gamma / beta.gamma

set.seed(13)

n = 30

alpha = 0.05

z.alpha.half = qnorm(1 - alpha/2)

n.draw = 100

for(i in 1:n.draws) {
  x = rgamma(n, alpha.gamma, beta.gamma)
  
  sample.means[i] = mean(x)
  sample.variances[i] = var(x)
  lower.limits[i] = sample.means[i] - z.alpha.half * sqrt(sample.variances[i]/n)
  upper.limits[i] = sample.means[i] + z.alpha.half * sqrt(sample.variances[i]/n)

  }

cover.g = ifelse(lower.limits < mu.gamma & upper.limits > mu.gamma, 1, 0)
mean(cover.g)

cols = c("violet", "green")

windows()
plot(1:n.draws,
     sample.means,
     ylim = c(min(lower.limits), max(upper.limits)),
     pch=19,
)

segments(1:n.draw, lower.limits,
         1:n.draw, upper.limits,
         col = cols[cover.g+1])

abline(h = mu.gamma, col="red")



