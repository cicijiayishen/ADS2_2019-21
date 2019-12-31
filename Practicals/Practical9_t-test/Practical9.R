temp <- read.csv("C:/Users/sissy/Desktop/test Git/ADS2_2019-21/Practicals/Practical9_t-test/OrionTemp.csv")
head(temp)
summary(temp)
sd(temp$Temperature)
mysample <- sample(temp$Temperature,10,replace=FALSE)
mysample
t.test(mysample,mu = 37,alternative = "two.sided")
x <- seq(-10,10,0.01)
y <- dt(x,df = 9)
plot(x,y)
sum(y[which(x>1.9454)])*0.01*2
p.values <- NULL
cnt_list <- c()
for (i in 1:1000) {
  cnt <- 0
  for (j in 1:i) {
    sample_loop <- sample(temp$Temperature,10,replace = F)
    res <- t.test(sample_loop,mu = 37,alternative = "two.sided")
    if (res$p.value<=0.05) {
      cnt=cnt+1
    }
    p.values <- c(p.values,res$p.value)
  }
  cnt_list <- c(cnt_list,cnt)
}
plot(cnt_list)
