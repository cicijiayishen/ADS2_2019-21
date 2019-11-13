#Question 5
mean.all <- rnorm(10000,mean = 50,sd = 5)
class.unlucky <- rnorm(26,mean = 40,sd = 8)
class.lucky <- c()
class.size <- round(runif(9999,5,40))
for (i in 1:9999) {
  mean <- mean.all[i]
  size <- class.size[i]
  class <- rnorm(size,mean,10)
  class.lucky <- c(class.lucky,class)
}
hist(class.lucky)
