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
class.allraw <- c(class.unlucky,class.lucky)
q5_poor <- c()
for (j in 1:10000) {
  q5_poor <- c(q5_poor,mean(rnorm(1,40,8)>class.allraw))
}
mean(q5_poor)
hist(q5_poor)
#####The following is the solution provided by teacher#####
allrawvector <- c()
allrawlist <- c()
for (i in 1:10000) {
  sampleclass <- rnorm(round(runif(1,5,40)),rnorm(1,50,5),10)
  allrawvector <- c(allrawvector,sampleclass)
  allrawlist <- c(allrawlist,list(sampleclass))
}
hist(allrawvector)

task5unlucky <- c()
for (i in 1:10000) {
  task5unlucky <- c(task5unlucky,mean(rnorm(1,40,8)>allrawvector)*100)
}
mean(task5unlucky)
hist(task5unlucky)
allnormlist <- c()
for (i in 1:10000) {
  classnorm <- c()
  for (j in i:(length(allrawlist[[i]]))) {
    classnorm <- c(classnorm,mean(allrawlist[[i]][j]>allrawvector)*100)
    allnormlist <- c(allnormlist,list(classnorm))
  }
}
cnt <- 0
for (i in 1:10000) {
  if (mean(sample(task5unlucky,26))>mean(allnormlist[[i]])) {
    cnt=cnt+1
  }
}
cnt
Leonie1 <- mean(64>allrawvector)*100
Leonie2 <- mean(63>allrawvector)*100
Leonie3 <- mean(62>allrawvector)*100
Leonie4 <- mean(59>allrawvector)*100
mean(c(Leonie1,Leonie2,Leonie3,Leonie4))
Sheldon1 <- mean(70>allrawvector)*100
Sheldon2 <- mean(63>allrawvector)*100
Sheldon3 <- mean(61>allrawvector)*100
Sheldon4 <- mean(56>allrawvector)*100
mean(c(Sheldon1,Sheldon2,Sheldon3,Sheldon4))