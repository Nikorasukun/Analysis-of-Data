# The importance of data visualization

# install.packages("datasauRus")
require(datasauRus)
require(ggplot2)

?datasaurus_dozen

# 13 dataset with the same mean and sd
aggregate(datasaurus_dozen$y, list(datasaurus_dozen$dataset), mean)
aggregate(datasaurus_dozen$y, list(datasaurus_dozen$dataset), sd)

# checking for correlation coefficients
dataset = unique(datasaurus_dozen$dataset)

for(i in dataset) {
  print(
    paste(
      i,
      round(
        cor(
          datasaurus_dozen$x[datasaurus_dozen$dataset == i],
          datasaurus_dozen$y[datasaurus_dozen$dataset == i]
        ) # cor close
        ,3
      ) # round close
    ) # paste close
  ) # print close
} # for close

windows()
ggplot(datasaurus_dozen, aes(x=x, y=y, color = dataset))+
  geom_point()+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol = 3)
  

par(mfrow=c(3,4))
for (i in 9) {
  plot(datasaurus_dozen$x[i], add=TRUE)
  
}

?plot
  
  
  
  
  
  
  
  
  


