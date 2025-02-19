setwd("C:/Users/sissy/Desktop/test Git/ADS2_2019-21/Lectures/lecture5_demo/") #make sure to change your own corrected paths.
data <- read.csv("Rdata_diamonds_samples100_mdf.csv")
head(data)

#check data type of ID column
typeof(head(data$ID))
class(data$ID)

#check data type of carat column
class(data$carat)

#check data type of cut column
class(data$cut)

cut.chr <- as.character(data$cut)
head(cut.chr)
class(cut.chr)

var1 <- c(2,3,5,6)
class(var1)
var2 <- c(2.0,3.9,5.1,6.9)
class(var2)
var3 <- c(2L,3L,4L,5L)
class(var3)

#vector & list
var.num <- c(2.0,3.9,5.1,6.9)
print(var.num)
class(var.num)

var.int <- c(2L,3L,5L,6L)
print(var.int)
class(var.int)

var.char <- c("Hello",",","world","!")
print(var.char)
class(var.char)

var.fac <- factor(c("mid","mid","high","low"),levels = c("low","mid","high"))
print(var.fac)
class(var.fac)

data.list <- list(var.num,var.int,var.char,var.fac)
head(data.list)
data.list[[1]]
data.list[[1]][1]
dim(data.list)

#dataframe & matrix
data.matrix <- as.matrix(data)
head(data.matrix)
class(data.matrix[,1])

#data cleaning

##Missing data
head(data)
head(is.na(data))
tail(data)
tail(is.na(data))

apply(is.na(data), 2, which) #find row & col of NA.

dim(data)

data.noNA <- data[complete.cases(data),]
dim(data.noNA)

#Documentation
print(data[!complete.cases(data),])

##Duplicated data
#screen
duplicated(data.noNA)
frw.idx <- which(duplicated(data.noNA))
rvs.idx <- which(duplicated(data.noNA,fromLast = TRUE))
data.noNA[c(frw.idx,rvs.idx),]

#treat
dim(data.noNA)
data.noNA.noDup <- data.noNA[!duplicated(data.noNA),]
dim(data.noNA.noDup)

#Documentation
data.noNA[duplicated(data.noNA),]

##Strange pattern
#screen
data.noNA.noDup <- data.frame(data.noNA.noDup,volume = data.noNA.noDup$x*data.noNA.noDup$y*data.noNA.noDup$z)
head(data.noNA.noDup)
plot(x = data.noNA.noDup$carat,y = data.noNA.noDup$volume,pch = 20, col = "darkgoldenrod4",las = 1, xlab = "carat", ylab = "volume")
text(data.noNA.noDup$carat,data.noNA.noDup$volume, labels = data.noNA.noDup$ID, col = "dimgray", cex = 0.7, pos = 4)

#treat
print(data.noNA.noDup[which(data.noNA.noDup$ID == "7"),])
print(data.noNA.noDup[which(data.noNA.noDup$ID == "28"),])

head(data.noNA.noDup[,9:11])
print(data.noNA.noDup[duplicated(data.noNA.noDup[,9:11],fromLast = "TRUE"),])
fwrd.dup.idx <- which(duplicated(data.noNA.noDup[,9:11]))
rvse.dup.idx <- which(duplicated(data.noNA.noDup[,9:11],fromLast = TRUE))
data.noNA.noDup[c(fwrd.dup.idx,rvse.dup.idx),]

#treat
data.noNA.noDup.noStrg <- data.noNA.noDup[-which(data.noNA.noDup$ID == 7 | data.noNA.noDup$ID == 28),]
dim(data.noNA.noDup)
dim(data.noNA.noDup.noStrg)

#Documentation


#####Problem Set 5#####
###Correct typos
#screen
summary(data.noNA.noDup.noStrg$cut)
#diagnosis
print(data.noNA.noDup.noStrg[which(data.noNA.noDup.noStrg$cut == "Idea"),])
#treat
data.noNA.noDup.noStrg.notypo <- data.noNA.noDup.noStrg
data.noNA.noDup.noStrg.notypo$cut[which(data.noNA.noDup.noStrg.notypo$cut == "Idea")] = "Ideal"
summary(data.noNA.noDup.noStrg.notypo$cut)
###Find outliers
#screen
plot(x=data.noNA.noDup.noStrg.notypo$carat,y=data.noNA.noDup.noStrg.notypo$price,
     pch=20,col="darkslateblue",
     las=1,xlab="carat",ylab="price",
     main="diamond carat ~ price")
text(data.noNA.noDup.noStrg.notypo$carat, data.noNA.noDup.noStrg.notypo$price,
     labels=data.noNA.noDup.noStrg.notypo$ID,col="dimgray",
     cex= 0.7, pos=4)
#diagnosis
print(data.noNA.noDup.noStrg.notypo[which(data.noNA.noDup.noStrg.notypo$ID=="96"),])
print(data.noNA.noDup.noStrg.notypo[which(data.noNA.noDup.noStrg.notypo$ID=="27"),])
print(data.noNA.noDup.noStrg.notypo[(which(data.noNA.noDup.noStrg.notypo$ID == "96")-2):(which(data.noNA.noDup.noStrg.notypo$ID == "96")+2),])
print(data.noNA.noDup.noStrg.notypo[(which(data.noNA.noDup.noStrg.notypo$ID == "27")-2):(which(data.noNA.noDup.noStrg.notypo$ID == "27")+2),])
#treat
outlier.idx=rep(0,nrow(data.noNA.noDup.noStrg.notypo))
outlier.idx[which(data.noNA.noDup.noStrg.notypo$ID==96)]=1
outlier.idx[which(data.noNA.noDup.noStrg.notypo$ID==27)]=1
data.noNA.noDup.noStrg.notypo.mkOtlr=data.frame(data.noNA.noDup.noStrg.notypo,otlr=outlier.idx)
head(data.noNA.noDup.noStrg.notypo.mkOtlr)
tail(data.noNA.noDup.noStrg.notypo.mkOtlr)
###Bounce question
plot.df=data.noNA.noDup.noStrg.notypo.mkOtlr #the name is too long, let's simplify it a little bit
library(ggplot2)
p=ggplot(plot.df,aes(x=carat,y=price,color=clarity))
p+geom_point()+facet_grid(clarity~.)
