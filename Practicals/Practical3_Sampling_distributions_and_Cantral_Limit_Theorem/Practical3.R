#####Width of the sampling distribution and sample size#####
population <- rnorm(1e6,100,5)
mean <- c()
std <- c()

semlist <- c()
for (j in 5:100) {
  summean <- 0
  meanlist <- c()
  sumsd <- 0
  sd_sum = 0
  for (i in 1:1000) {
    index <- sample(1e6,j,replace=TRUE)
    randomi <- population[index]
    randommean <- mean(randomi)
    randomsd <- sd(randomi)
    sd_sum = sd_sum + randomsd
    meanlist <- c(meanlist,randommean)
  }
  std<-c(std,sd(meanlist))
  sd_sem = sd_sum/1000
  sem <- sd_sem/sqrt(j)
  semlist <- c(semlist,sem)
}
plot(std)
points(semlist,col="blue")
#####Rolling 1 die#####
result <- c()
for (i in 1:1000) {
  res <- sample(6,1,replace = FALSE)
  result <- c(result,res)
}
hist(result)
hist(result,breaks = 0.5:6.5)
#####Rolling 2 dies#####
result2 <- c()
for (i in 1:1000) {
  res1 <- sample(6,1,replace = FALSE)
  res2 <- sample(6,1,replace = FALSE)
  res12 <- res1+res2
  result2 <- c(result2,res12)
}
hist(result2)
#####Dragon wingspans#####
dragonwing <- read.csv("C:/Users/sissy/Desktop/test Git/ADS2_2019-21/Practicals/Practical3_Sampling_distributions_and_Cantral_Limit_Theorem/dragons.csv")
dragon <- c()
for (j in 1:500) {
  dragon <- c(dragon,dragonwing[j,])
}
hist(dragon)
meanlist <- c()
for (i in 1:1000) {
  index <- sample(500,10,replace=FALSE)
  randomi <- dragon[index]
  randommean <- mean(randomi)
  meanlist <- c(meanlist,randommean)
}
hist(meanlist)
