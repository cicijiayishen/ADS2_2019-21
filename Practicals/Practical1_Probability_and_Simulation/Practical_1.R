1+1
chicago <- read.csv("C:/Users/sissy/Desktop/test Git/ADS2_2019-21/Practicals/Practical1_Probability_and_Simulation/Chicago2013.csv")
head(chicago)
#####What countries and how many each#####
table(chicago$Country)
#####Draw a histogram of finishing time#####
hist <- hist(chicago$Time,main = "Histogram of finishing time",xlab="Time")
#####Select 10 people and draw a histogram#####
sample(5,10,replace=TRUE)
index <- sample(85,10,replace=FALSE)
random10 <- chicago[index,]
hist <- hist(random10$Time,main = "Histogram of finishing time",xlab="Time")

test <- rnorm(10,0,1)
test
#####Create a group of 100 people#####
men <- rnorm(45,mean = 172,sd = 7)
women <- rnorm(55,mean = 158.5,sd = 6)
#####Draw boxplot of men and women#####
boxplot(men,main="Boxplot of men's heights")
boxplot(women,main="Boxplot of women's heights")
#####View the whole class#####
class <- c(men,women)
max(class)
min(class)
sum(class>162)

#####Create a class of 26 students#####
birthday <- sample(365,26,replace=TRUE)
length(unique(birthday))
#####Overall probability of a shared birthday#####
m <- c()
for (i in 1:10000) {
  birth <- sample(365,26,replace = TRUE)
  if (length(unique(birth))!=26) {
    m <- c(m,1)
  } else {
      m <- c(m,0)
    }
}
sum <- sum(m)
prob <- sum/10000
#####Bonus: form n=1 to n=50#####
df <- data.frame(n,probability)
for (n in 1:50) {
  m <- c()
  for (i in 1:10000) {
    birth <- sample(365,n,replace = TRUE)
    if (length(unique(birth))!=n) {
      m <- c(m,1)
    } else {
      m <- c(m,0)
    }
  }
  sum <- sum(m)
  prob <- sum/10000
  df <- rbind(df,c(n,prob))
}
df
plot(df)