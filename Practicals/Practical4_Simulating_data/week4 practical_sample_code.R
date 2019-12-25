## simulate two datasets
dat1 <- data.frame(Patient=1:100,ND=rnorm(100,143,48),TM=rnorm(100,143,48))
dat2 <- data.frame(Patient=1:10000, ND=rnorm(10000,143,48), TM=rnorm(10000,156,55))

summary(dat1)
summary(dat2)


##t-test
t.test(dat1$ND,dat1$TM,paired = T)
res<-t.test(dat2$ND,dat2$TM,paired = T)

dat2.s100 <- dat2[sample(10000,100,replace = F),]
t.test(dat2.s100$ND,dat2.s100$TM,paired = T)

set.seed(42)
dat2.s1 <- dat2[sample(10000,100,replace = F),]
t.test(dat2.s1$ND,dat2.s1$TM,paired = T)

## plot gender and age
age <- sample(18:80, 10000,replace = T)
class(age)
age <- as.factor(age)
class(age)
gender <- sample(c("M","F"), 10000, replace = T)
dat2 <- cbind(dat2,age, gender)

plot(dat2$age, dat2$TM)
boxplot(TM ~ gender, data = dat2)

library(ggplot2)
g <- ggplot(dat2,aes(x= age, y= TM))
g <- g + geom_point()
g

g <- ggplot( dat2,aes(x= gender, y= TM, color=gender))
g <- g + geom_boxplot()
g

t.test(TM ~ gender, data=dat2)

##truncnorm
library("truncnorm")
ND = rtruncnorm(100, 0, 500, 143, 48)

