population <- rnorm(1e6,100,5)
popmean <- round(mean(population),1)
popmean
popsd <- round(sd(population),1)
popsd
summean <- 0
sumsd <- 0
for (i in 1:1000) {
  index <- sample(1e6,100,replace=FALSE)
  random5 <- population[index]
  summean=summean+mean(random5)
  sumsd=sumsd+sd(random5)
}
meanmean <- summean/1000
meanmean
meansd <- sumsd/1000
meansd
