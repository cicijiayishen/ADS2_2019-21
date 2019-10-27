#####Rolling 3 dies#####
result3 <- c()
for (i in 1:1000) {
  res1 <- sample(6,1,replace = FALSE)
  res2 <- sample(6,1,replace = FALSE)
  res3 <- sample(6,1,replace = FALSE)
  res <- res1+res2+res3
  result3 <- c(result3,res)
}
hist(result3)
#####Rolling 4 dies#####
result4 <- c()
for (i in 1:1000) {
  res1 <- sample(6,1,replace = FALSE)
  res2 <- sample(6,1,replace = FALSE)
  res3 <- sample(6,1,replace = FALSE)
  res4 <- sample(6,1,replace = FALSE)
  res <- res1+res2+res3+res4
  result4 <- c(result4,res)
}
hist(result4)
#####Rolling 5 dies#####
result5 <- c()
for (i in 1:1000) {
  res1 <- sample(6,1,replace = FALSE)
  res2 <- sample(6,1,replace = FALSE)
  res3 <- sample(6,1,replace = FALSE)
  res4 <- sample(6,1,replace = FALSE)
  res5 <- sample(6,1,replace = FALSE)
  res <- res1+res2+res3+res4+res5
  result5 <- c(result5,res)
}
hist(result5)
#####Bean machine#####
res <- c()
for (i in 1:1000) {
  end <- sum(sample(c(0,1),8,replace=TRUE))
  res <- c(res,end)
}
hist(res,axes = F,main = "Bean machine",xlab = "",ylab = "")
#asymmetric
res_a <- c()
for (i in 1:1000) {
  end <- sum(sample(c(0,1),8,replace=TRUE,p=c(0.8,0.2)))
  res_a <- c(res_a,end)
}
hist(res_a,axes = F,main = "Bean machine (asymmetric)",xlab = "",ylab = "")