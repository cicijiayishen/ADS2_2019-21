#####generate linear model#####
set.seed(12)
x=runif(1000)
y = 10.8*x+0.6
plot(x,y)
#add some noise
idx=sample(1000,500)
y[idx]=y[idx]+rnorm(length(idx))
plot(x,y)
#####generate DNA sequence#####
DNA_1=paste0(sample(c("A","C","G","T"),1000,rep=TRUE),collapse = "")
DNA_1
DNA_2=paste0(sample(c("A","C","G","T"),1000,rep=TRUE,prob = c(0.25,0.25,0.3,0.2)),collapse = "")
DNA_2
