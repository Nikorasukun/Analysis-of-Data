# CLT part 2 ####

# set-up
a = 0; b = 10

sims = 4000 # montecarlo repetition number
m = c(15,50,800)

E.of.X = (a+b)/2
V.of.X = (b-a)^2/12

Z = matrix(NA, nrow = sims, ncol = length(m))

set.seed(10)

for(j in 1:length(m)) {
  
  for(i in 1:sims) {
    
    samp = runif(n=m[j], min = a, max = b)
    
    sample.mean = mean(samp)
    
    Z[i,j] = (sample.mean - E.of.X) / sqrt(V.of.X / m[j])
    
  }
  
  
}

hist(Z[,1], col="blue")
hist(Z[,2], col="orange")
hist(Z[,3], col="gray")

# install packages
# ggplot2
# ggpubr

require(ggplot2)
library(ggpubr)

# prepare the data
z.data = data.frame(
  val = as.vector(Z),
  samp.size = as.factor(
    rep(
      m, each = sims
    )
  )
)

str(z.data)

# default boxplot
boxplot(Z, range=0, col = 2:4)

# what about ggplot2?
ggplot(z.data, aes(x = val, fill = samp.size))+geom_boxplot(color = "red")+coord_flip()
# fill=samp.size makes it so it automatically recognizes three groups because samp.size is a
# factor and so it plots three different things with different automatic colors

# Histograms
# basic version
ggplot(z.data, aes(x = val, fill = samp.size))+
  labs(x = "z", y = "f(z)")+
  geom_histogram(bins=80)

# second, improved, version
ggplot(z.data, aes(x = val, y = after_stat(density), fill = samp.size))+
  labs(x = "z", y = "f(z)")+
  geom_histogram(bins=100, position = "dodge")

# third, more improved, version
graph1 = ggplot(z.data, aes(x = val, y = after_stat(density), fill = samp.size))+
  labs(x = "z", y = "f(z)")+
  geom_histogram(bins=100, position = "identity", alpha = 1)

# if you used fill in ggplot, you need to use a fill function for the color, while if you
# used something different, you need to select a color filling function matching what you had

graph1_1 = graph1+ggtitle("CLT")+scale_fill_brewer(name="Sample \n sizes", palette="Accent")

graph1_f = graph1_1+facet_grid(samp.size ~. )

# in the exam you will not use ggplot because you dont have access to internet
# learn how to do them with normal R functions for the exam









