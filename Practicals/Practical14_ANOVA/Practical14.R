trial <- read.csv("C:/Users/sissy/Desktop/test Git/ADS2_2019-21/Practicals/Practical14_ANOVA/drug_trial.csv")
library(ggplot2)
ggplot(data=trial,aes(x = treatment,y = pain))+geom_boxplot(aes(treatment,col=treatment))
model <- aov(pain~treatment,data = trial)
plot(model)
summary(model)
TukeyHSD(model)

mouse <- read.csv("C:/Users/sissy/Desktop/test Git/ADS2_2019-21/Practicals/Practical14_ANOVA/mouse_experiment.csv")
ggplot(data = mouse,aes(x = diet, y = weight_gain)) + geom_boxplot(aes(genotype,col = diet))
model2 <- aov(weight_gain~genotype*diet,data = mouse)
plot(model2)
summary(model2)
a <- TukeyHSD(model2)
a
b <- a$`genotype:diet`
b[b[,4]<0.05,]
