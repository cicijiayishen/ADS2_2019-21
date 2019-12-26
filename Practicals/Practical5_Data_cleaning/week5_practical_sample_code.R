library(tidyr)
##tidy dataset I
#import WNV mosquito data
wnv <- read.csv("WNV_mosquito_test_results.csv", na.strings = "")
summary(wnv)
anyNA(wnv)
#drop incomplete record
wnv <- drop_na(wnv, LOCATION)
anyNA(wnv)
#change the name of the first column and factorize variables 
names(wnv)[names(wnv) == "SEASON.YEAR"] <- "YEAR"
names(wnv)

#convert date type to POSIXlt format
class(wnv$TEST.DATE)
wnv$TEST.DATE <- as.POSIXct(wnv$TEST.DATE, format = "%m/%d/%Y %H:%M:%S" ,tz="America/Chicago")
class(wnv$TEST.DATE)
#convert timezone of daytime type data
dat1 <- wnv$TEST.DATE[1]
dat1
attributes(dat1)
attributes(dat1)$tzone <- "America/Los_Angeles"
dat1

#separate LOCATION column into LATITUDE and LONGITUDE  
wnv$LOCATION <- gsub("[()]","", wnv$LOCATION, perl = T)
wnv <- separate(wnv,LOCATION, into = c("LATITUDE", "LONGITUDE"), sep = ",", remove = F, fill = "left" ,convert = T)
summary(wnv)


## tidy dataset II
#input ELISA data from csv
pgp3 <- read.csv("Tests_PGP3.csv", na.strings = c("","NA"))
summary(pgp3)
#covert SampleID, age.F, sex to readable factor
pgp3$SampleID <- as.character(pgp3$SampleID)
pgp3$age.f <- as.factor(pgp3$age.f)
pgp3$sex <- gsub("1","M",as.character(pgp3$sex))
pgp3$sex <- gsub("2","F",as.character(pgp3$sex))
pgp3$sex <- as.factor(pgp3$sex)
summary(pgp3)

boxplot(data=pgp3, elisa.od~sex)
boxplot(data=pgp3, elisa.pre.od~age.f)
#drop incomplete record
pgp3<- drop_na(pgp3)
summary(pgp3)
boxplot(data=pgp3, elisa.od~age.f)
#reshape the dataframe
pgp3 <- gather(pgp3,key = "time.point", value = "ELISA.od", elisa.od:elisa.pre.od, factor_key = T)
summary(pgp3)
library(ggplot2)
ggplot(data= pgp3, aes(age.f, ELISA.od, color=time.point)) + geom_boxplot()
#reverse the reshaping
pgp3 <- spread(pgp3, key = time.point, value = ELISA.od )
summary(pgp3)
