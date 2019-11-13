#Question 1
mean.list <- c()
for (i in 5) {
  mean.vec <- replicate(10000,mean(runif(26,0,100)))
  prob <- length(which(mean.vec<40))/10000
  prob
  mean.list <- c(mean.list,mean.vec)
}
hist(mean.list)
#Question 2
mean.vec1 <- replicate(10000,mean(runif(26,0,100)))
mean.vec2 <- replicate(10000,mean(runif(26,0,80)))
hist(mean.vec1,xlim = c(0,100),border = "red")
par(new=TRUE)
hist(mean.vec2,xlim = c(0,100),border = "blue")
prob2 <- length(which(mean.vec1<mean.vec2))/10000
prob2
#Question 3
sample <- rnorm(10000,mean = 50,sd = 10)
Leonie <- c(64,63,62,59)
Sheldon <- c(70,63,61,56)
L_1 <- length(which(sample<64))/10000*100
L_2 <- length(which(sample<63))/10000*100
L_3 <- length(which(sample<62))/10000*100
L_4 <- length(which(sample<59))/10000*100
Leonie_team <- sum(L_1,L_2,L_3,L_4)
Leonie_team
S_1 <- length(which(sample<70))/10000*100
S_2 <- length(which(sample<63))/10000*100
S_3 <- length(which(sample<61))/10000*100
S_4 <- length(which(sample<56))/10000*100
Sheldon_team <- sum(S_1,S_2,S_3,S_4)
Sheldon_team
#Question 4
dist_poor <- rnorm(10000,mean = 40,sd = 8)
dist_norm <- rnorm(10000,mean = 50,sd = 10)
hist(dist_poor)
mean(pnorm(rnorm(26,40,8),50,10)*100)
sum <- c()
for (i in 1:1000) {
  sum <- c(sum,length(which(pnorm(rnorm(26,40,8),50,10)>mean(runif(26,0,100)))))
}
mean(sum)
