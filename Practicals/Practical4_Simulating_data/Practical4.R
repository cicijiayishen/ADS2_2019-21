#100 people
data1_ND <- rnorm(100,mean = 143,sd = 48)
data1_TW <- rnorm(100,mean = 143,sd = 48)
df1 <- data.frame(c(1:100),data1_ND,data1_TW)
colnames(df1) <- c("volunteer number","ND","TW")
#10000 people
data2_ND <- rnorm(10000,mean = 143,sd = 48)
data2_TW <- rnorm(10000,mean = 156,sd = 55)
df2 <- data.frame(c(1:10000),data2_ND,data2_TW)
colnames(df2) <- c("volunteer number","ND","TW")
#t-test
t.test(df1$ND,df1$TW,paired = T)
summary(df1)
t.test(df2$ND,df2$TW,paired = T)
summary(df2)
set.seed(1)
index <- sample(10000,100,replace = FALSE)
sp2 <- df2[index,]
t.test(sp2)
#set age and gender
age <- sample(18:80,10000,replace=TRUE)
gender <- sample(c("F","M"),10000,replace = TRUE)
df2 <- cbind(df2,age,gender)
library(ggplot2)
ggplot(df2,aes(x=df2$`volunteer number`,y=df2$ND,col=gender))+geom_boxplot()
ggplot(df2,aes(x=df2$`volunteer number`,y=df2$ND,col=gender))+geom_point()
ggplot(df2,aes(x=df2$`volunteer number`,y=df2$ND,col=age))+geom_point()
write.csv(df1,file="")
write.csv(df2)