onesample=t.test(c(15,30,50),mu = 8.9)
print(onesample)
names(onesample)
onesample$statistic
onesample$parameter
onesample$p.value
onesample$stderr
x <- c(102,340,234,332,129)
y <- c(74,56,70,104,11)
twosamplepaired <- t.test(x,y,paired = TRUE,alternative = "greater")
print(twosamplepaired)
names(twosamplepaired)
twosamplepaired$statistic
twosamplepaired$parameter
twosamplepaired$p.value
twosamplepaired$stderr
geneexp <- read.csv("C:/Users/sissy/Desktop/test Git/ADS2_2019-21/Problem_Sets/10/week10_t_test_problemset_testdata.csv")
head(geneexp)
tail(geneexp)
dim(geneexp)
pvalue <- rep(0,nrow(geneexp))
for (i in 1:nrow(geneexp)) {
  pvalue[i]=t.test(geneexp[i,2:5],geneexp[i,6:9],paired = FALSE,alternative = "two.sided")$p.value
}
geneexp <- data.frame(geneexp,pvalue)
geneexp.sig <- geneexp[which(geneexp$pvalue<=0.05),]
head(geneexp.sig)
dim(geneexp.sig)
