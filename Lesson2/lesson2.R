# number of trials
n = 100

# prob of success
p = 0.01

# support of x
x = 0:n

# the pmf
px = dbinom(x,size=n,prob=p)

# the CDF
Fx = pbinom(x,n,p)

# quantile function
qbinom(0.5, n, p)

# normal RV
dnorm(0)
1/sqrt(2*pi)
dnorm(1) == dnorm(-1)

pnorm(0)
pnorm(1.645)

qnorm(0.975)
qnorm(0.95)


# let's go back to the binomial rv

# random generation from a distribution
set.seed(4)
n.draws = 20000
sample.bin = rbinom(n.draws, n,p)

# compare theoretical vs simulated
n*p
mean(sample.bin)


n*p*(1-p)
var(sample.bin) # divided by n-1

count.50 = mean(sample.bin == 50)
dbinom(50,n,p)

# now the whole pmf
px.sim = rep(NA,n+1)

for(i in 1:(n+1)) {
  count = sample.bin == x[i]
  px.sim[i] = sum(count)/n.draws
} # in one line: px.sim[i] = mean(sample.bin==x[i])

# is this a good approx?

sum(abs(px-px.sim)) # 0.02678

# cumsum
cumsum(1:6)

Fx.sim = cumsum(px.sim)

sum(abs(Fx-Fx.sim)) # 0.04197

# let's check it graphically

windows() #for mac quartz()
par(mfrow=c(1,2)) # 1 row, 2 columns
plot(
  x, px,
  main = "px", ylab=""
)
points(x,px.sim, col="red", pch=3)
plot(
  x,Fx,
  main="Fx",ylab=""
)
points(x,Fx.sim,col="red",pch=3)
legend(60, 0.7, legend = c("True", "Sim"),
       col = c("black", "red"), pch= c(1,3))


# comparison with a normal distribution
par(mfrow=c(1,1))
windows()
plot(
  x, px, type="h", col="blue", xlim=c(-10,100)
)
lines(x, px.sim, type="h", col="red", lwd=2)
curve(
  dnorm(x, mean=n*p ,sd=sqrt(n*p*(1-p))),
  from=-10,
  to=100,
  col="orange",
  add=TRUE
)

# Poisson rv, lamba>30 for approximation, else it's too skewed
# for a Binomial rv, you need to have np>5, n*(1-p)>5 or else it's too skewed
# {remember E[poi] = V[poi] = lamba}

# exercise after the end of this lesson in virtuale, same exercise as we did now in
# class but with a poisson distribution, basically just change the funcs from binom to pois

# he helped with support of pois putting the quantile so that we don't have infinity
# 0.9999999 of observations are under 18, so we use 18 as an upper margin (for Pois(4))

# you're gonna do the exercise here

# remember to create a lesson3.R before next lecture and put the first part of the next
# virtuale file inside because we are gonna use it for starters next lecture








