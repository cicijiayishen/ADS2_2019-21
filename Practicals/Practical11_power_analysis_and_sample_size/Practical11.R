###Task 1###
country <- rnorm(1000000,mean = 175,sd = 10)
college <- rnorm(10000,mean = 178,sd = 10)
p_list <- c()
cnt <- 0
for (i in 1:100000) {
  mysample <- sample(college,10,replace = F)
  sample_test <- t.test(mysample,mu = 175,alternative = "greater")
  p_list <- c(p_list,sample_test$p.value)
  if (sample_test$p.value > 0.05) {
    cnt <- cnt+1
  }
}
mean(p_list)
cnt/100000
power.t.test(n=10,delta = 3,sd = 10,sig.level = 0.05,type = "one.sample",alternative = "one.sided")
power.t.test(n=50,delta = 3,sd = 10,sig.level = 0.05,type = "one.sample",alternative = "one.sided")
power_list <- c()
for (j in seq(5,100,by = 5)) {
  power_test <- power.t.test(n=j,delta = 3,sd = 10,sig.level = 0.05,type = "one.sample",alternative = "one.sided")
  power_list <- c(power_list,power_test$power)
}
plot(seq(5,100,by = 5),power_list,type = "b",xlab = "Ns",ylab = "Pows")


###Task 2###
population <- rnorm(10000,mean = 130,sd = 30)
count <- 0
for (i in 1:100000) {
  placebo <- rnorm(10,mean = 130,sd = 30)
  drug <- rnorm(10,mean = 117,sd = 30)
  sample_test <- t.test(placebo,drug,alternative = "greater",paired = FALSE)
  if (sample_test$p.value > 0.05) {
    count <- count+1
  }
}
count/100000
power.t.test(n=10,delta = 13,sd=30,sig.level = 0.05,type = "two.sample",alternative = "one.sided")
power.t.test(power = 0.8,delta = 13,sd = 30,sig.level = 0.05,type = "two.sample",alternative = "one.sided")
power.t.test(power = 0.8,delta = 13,sd = 30,sig.level = 0.05,type = "paired",alternative = "one.sided")
alphas <- seq(0.01,0.05,by = 0.01)
Ns <- c()
for (j in alphas) {
  power_test <- power.t.test(power = 0.8,delta = 13,sd = 30,sig.level = j,type = "paired",alternative = "one.sided")
  Ns <- c(Ns,power_test$n)
}
plot(alphas,Ns,type = "b")

Pows <- seq(0.5,0.9,by = 0.1)
Ns <- c()
for (i in Pows) {
  power_test <- power.t.test(power = i,delta = 13,sd = 30,sig.level = 0.05,type = "paired",alternative = "one.sided")
  Ns <- c(Ns,power_test$n)
}
plot(Pows,Ns,type = "b")
