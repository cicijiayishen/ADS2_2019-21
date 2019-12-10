trial <- read.csv("C:/Users/sissy/Desktop/test Git/ADS2_2019-21/Practicals/Practical13_Simulation_Based_Comparison_Of_Two_Means/drug_trial.csv")
library(ggplot2)
ggplot(data=trial,aes(x = treatment,y = pain))+geom_boxplot(aes(treatment,col=treatment))

same_group <- c()
dif_group <- c()
for (i in 1:1000) {
  index1 <- sample(1:nrow(trial),1)
  s1 <- trial[index1,]
  
  trial2 <- trial[-index1,]
  index2 <- sample(1:nrow(trial2),1)
  s2 <- trial[index2,]
  
  if (s1$treatment == s2$treatment) {
    same_group <- c(same_group,abs(s1$pain-s2$pain))
  }
  
  if (s1$treatment != s2$treatment) {
    dif_group <- c(dif_group,abs(s1$pain-s2$pain))
  }
}
mean(dif_group)
mean(same_group)
boxplot(same_group,dif_group,col = c("green","orange"))

mean_same_group <- c()
mean_dif_group <- c()
difference <- c()
for (i in 1:1000) {
  trial$new_treatment <- sample(trial$treatment,44,replace = FALSE)
  same_group <- c()
  dif_group <- c()
  for (i in 1:1000) {
    index1 <- sample(1:nrow(trial),1)
    s1 <- trial[index1,]
    
    trial2 <- trial[-index1,]
    index2 <- sample(1:nrow(trial2),1)
    s2 <- trial[index2,]
    
    if (s1$new_treatment == s2$new_treatment) {
      same_group <- c(same_group,abs(s1$pain-s2$pain))
    }
    
    if (s1$new_treatment != s2$new_treatment) {
      dif_group <- c(dif_group,abs(s1$pain-s2$pain))
    }
  }
  difference <- c(difference,abs(mean(same_group)-mean(dif_group)))
}
difference
hist(difference,col = "pink")