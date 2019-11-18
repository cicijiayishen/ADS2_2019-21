library(ggplot2)
library(dplyr)

#one sample t-test
x=c(1,3,5,6,7)
n=length(x)
var=sqrt((1/(n-1))*sum((x-mean(x))^2))
t.value <- (mean(x)-mu)/var*sqrt(n)
p.value <- 2*pt(-abs(t.value), df=length(x)-1) #two-tailed

#two sample independent t-test
dfx=length(x)-1
dfy=length(y)-1
dftotal=dfx+dfy
var.pool=sqrt(((dfx/dftotal)*(sum((x-mean(x))^2)/(dfx)))+((dfy/dftotal)*(sum((y-mean(y))^2)/(dfy))))
var.difference=sqrt((var.pool^2/length(x))+(var.pool^2/length(y)))
t.value <- (mean(x)-mean(y))/var.difference
p.value <- 2*pt(-abs(t.value), df=length(x) + length(y) -2) #two-tailed

# Welch's unequal variances t-test
#t.value <- (mean(x)-mean(y))/sqrt(var(x)/length(x)+var(y)/length(y))
#df = round((var(x)/length(x) + var(y)/length(y))^2/(var(x)^2/length(x)^2/(length(x)-1) +var(y)^2/length(y)^2/(length(y)-1)))
#p.value <- 2*pt(-abs(t.value), df=df) #two-tailed

#paired sample t-test
length(x) == length(y) #make sure it is paired samples
d <- x - y
n=length(d)
var=sqrt((1/(n-1))*sum((d-mean(d))^2))
t.value <- (mean(d))/var*sqrt(n)
p.value <- 2*pt(-abs(t.value), df=length(x)-1) #two-tailed

##The effort of VC on toothgrowth in pig
library(ggplot2)
data("ToothGrowth")
ToothGrowth.vc=ToothGrowth[which(ToothGrowth$supp=="VC"),]
t.test(ToothGrowth.vc$len, mu=8.5)

TG <- filter(ToothGrowth, dose == 0.5) # change the dose group and see the difference
var.test(len ~ supp, data= TG)
t.test(len ~ supp, data = TG, var.equal =T)

TG <- filter(ToothGrowth, dose == 1) # change the dose group and see the difference
var.test(len ~ supp, data= TG)
t.test(len ~ supp, data = TG, var.equal =T)

TG <- filter(ToothGrowth, dose == 2) # change the dose group and see the difference
var.test(len ~ supp, data= TG)
t.test(len ~ supp, data = TG, var.equal =T)

##The difference of sepal length of iris 
data("iris")
iris <- iris %>% filter(Species %in% c("setosa","versicolor")) %>% select(Sepal.Length, Species)
var.test( Sepal.Length ~ Species,data=iris)
t.test(Sepal.Length ~ Species, data=iris,var.equal = FALSE)


## The treatment on blood pressure
bp <- read.csv("Week10Practical_blood_pressure.csv",header = T,row.names = 1)
t.test(bp$bp_before,bp$bp_after,paired = T)
