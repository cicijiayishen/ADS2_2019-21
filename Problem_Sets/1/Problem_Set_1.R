#####Looking at students grades#####
grades <- rnorm(100,mean = 86,sd = 5)
hist(grades,main="Boxplot of students' grades",col = "pink",xlab = "Grades",ylab = "Number of students")
sum(grades>81 & grades<91)
sum(grades>96 | grades<76)
#####Getting good grades#####
#1 Randomly select one of the four answers
pass <- 0
for (i in 1:1000000) {
  correct_answer <- sample(1:4,20,replace = TRUE)
  answer <- sample(1:4,20,replace = TRUE)
  score <- sum(correct_answer==answer)
  if (score>=10) {
    pass=pass+1
  }
}
pass/1000000
#2 Choose A for every question
pass <- 0
for (i in 1:1000000) {
  correct_answer <- sample(1:4,20,replace = TRUE)
  answer <- c(rep(1,20))
  score <- sum(correct_answer==answer)
  if (score>=10) {
    pass=pass+1
  }
}
pass/1000000
#Same number of ABCD
pass <- 0
for (i in 1:1000000) {
  list <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5))
  correct_answer <- sample(list)
  answer <- sample(1:4,20,replace = TRUE)
  score <- sum(correct_answer==answer)
  if (score>=10) {
    pass=pass+1
  }
}
pass/1000000


#Another mathematical way
p_passing = 0
for (s in 10:20){
  p_s = 0.25^s*0.75^(20-s)*choose(20,s)
  p_passing = p_passing+p_s
}
p_passing
