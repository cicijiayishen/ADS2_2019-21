X <- c(1,2,3,4,5,6)
df_x <- length(X)-1
nx <- length(X)
mu_x <- 3
var_x <- sqrt((1/(nx-1))*sum((X-mean(X))^2))

Y <- c(1,3,5,7)
df_y <- length(Y)-1
ny <- length(Y)
mu_y <- 6
var_y <- sqrt((1/(ny-1))*sum((Y-mean(Y))^2))

t1 <- (abs(mean(X)-mu_x))/sd(X)*sqrt(length(X))
t1
p1 <- pt(X,df = length(X)-1)
p1

#3
s_pooled <- sqrt(df_x/(df_x+df_y)*(var_x^2)+df_y/(df_x+df_y)*(var_y^2))
s_difference <- sqrt(s_pooled^2/nx+s_pooled^2/ny)
t <- (mean(X)-mean(Y)-mu_x+mu_y)/s_difference
p <- 2*pt(abs(t),df_x)

##Task 2##
library(ggplot2)
data("ToothGrowth")
tooth.vc <- ToothGrowth[which(ToothGrowth$supp=="VC"),]
t.test(tooth.vc$len,mu = 8.5)
#2
tooth.oj <- ToothGrowth[which(ToothGrowth$supp=="OJ"),]
dos1.vc <- tooth.vc[which(ToothGrowth$dose==0.5),]
dos1.oj <- tooth.oj[which(ToothGrowth$dose==0.5),]
t.test(dos1.vc$len,dos1.oj$len)
dos2.vc <- tooth.vc[which(ToothGrowth$dose==1),]
dos2.oj <- tooth.oj[which(ToothGrowth$dose==1),]
t.test(dos2.vc$len,dos2.oj$len)
dos3.vc <- tooth.vc[which(ToothGrowth$dose==2),]
dos3.oj <- tooth.oj[which(ToothGrowth$dose==2),]
t.test(dos3.vc$len,dos3.oj$len)

##Task 3##
data(iris)
all <- iris[which(iris$Species %in% c("setosa","versicolor")),]
var.test(Sepal.Length~Species,all)
setosa <- iris[which(iris$Species=="setosa"),]
versicolor <- iris[which(iris$Species=="versicolor"),]
t.test(setosa$Sepal.Length,versicolor$Sepal.Length,var.equal = T)

##Task 4##
bp <- read.csv("C:/Users/sissy/Desktop/test Git/ADS2_2019-21/Practicals/Practical10_t-test/Week10Practical_blood_pressure.csv")
t.test(bp$bp_before,bp$bp_after,paired = T)
