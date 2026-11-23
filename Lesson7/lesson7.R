# Exercise 1 #####

n = 40
obsMean = 85000
sd = 8300
expectedMean = 88000
alpha = 0.01

zStat = (obsMean - expectedMean) / (sd / sqrt(n))

# qnorm(0.01, mean=expectedMean, sd=sd, lower.tail = FALSE)

dnorm(zStat)

# 0.03 > 0.01 so not H0 is rejected

# Exercise 2 ######

n2 = 40
obsMean2 = 85000
sampleSd = 8432
expectedMean2 = 88000
alpha = 0.01

tStat = (obsMean2 - expectedMean2) / (sampleSd / sqrt(n2))

dt(tStat, n2-1)

# 0.03 > 0.01 so H0 not rejected

# Exercise 3 #####
n3 = 35
sampleSd2 = 3.5
obsMean3 = 16.2
expectedMean3 = 15
alpha = 0.01

tStat3 = (obsMean3 - expectedMean3) / (sampleSd2 / sqrt(n3))

dt(tStat3, n3-1)
# 0.05 > 0.01 so H0 not rejected



# to create a confidence interval we have mean +- quantT(alpha/2, df) * (sd/sqrtn)

# Correction of Exercise 1 #####
# H_0 : mu >= mu_0
# H_1 : mu < mu_0

mu_0 = 88000
n = 40

xbar = 85000

# The population standard deviation is assumed to be known
sigma = 8300

alpha = 0.01

test.statistics = (xbar - mu_0) / (sigma / sqrt(n))

# Under the gaussianity of the salary distribution
# and under H_0

# two expressions to compute the same value
z.alpha = qnorm(alpha) # critical value
z.alpha = -qnorm(1-alpha) # critical value

# the rejection region
test.statistics < z.alpha # if FALSE, we do not reject; if TRUE, we reject (the H_0)
                          # at a threshold of 1% significance level, we do not have enough evidence to reject
                          # the null hypothesis

# alpha is the probability of committing a type I error
# type I error means rejecting H_0 even though it is true

# to find the p-val
# probability under null hypothesis to get an observation as the one we've observed
pnorm(test.statistics)

# very very close to alpha, so we are not very sure about 1% significance level
# the probability of me observing what I've observed is 1.1%, not really reassuring


# Exercise 2 Solution #####

# Estimate of the sample sd, we cannot use z test we use t test
s.tilde = 8432

test.statistics2 = (xbar - mu_0) / (s.tilde / sqrt(n))
t.alpha = qt(alpha, df=n-1)

test.statistics2 < t.alpha

pt(test.statistics2, n-1)

# also in this case there is a prob of 1.5% toi observe the data that we observed
# so not really realiable or reassuring


# Exercise 3 Solved #####
# H_0 : mu <= mu_0
# H_1 : mu > mu_0

mu_0 = 15
n =35
xbar = 16.2
s.tilde = 3.5

# under gaussi----
test.statistics3 = (xbar - mu_0) / (s.tilde / sqrt(n))

t.alpha = qt(1-alpha, n-1)


# the rejection region is 
test.statistics3 > t.alpha

# p-value
1 - pt(test.statistics3, df = n-1)

pt(test.statistics3, df = n-1, lower.tail = F)

# Confidence Interval
t.alpha.half = qt(1-alpha/2,n-1)

margin.of.error = t.alpha.half * sqrt(s.tilde^2 / n)


UL = xbar + margin.of.error
LL = xbar - margin.of.error

c(LL, xbar, UL)

# lazy way
xbar + c(-1 ,0,1) * margin.of.error

# talking about power of a test and how useful it is
# 1 - beta is the power of a test




