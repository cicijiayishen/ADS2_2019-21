library(ggplot2)
data("diamonds")
#####Task 1#####
g <- ggplot(data=diamonds,aes(x=carat,y = price,group=cut))
g1 <- g+geom_point(stat = "identity",aes(colour=cut))
g1
g2 <- g+geom_point(stat="identity",aes(colour=cut,alpha=1/5))
g2
g3 <- g+geom_point(stat="identity",aes(colour=cut,size=0.1))
g3
g4 <- g+geom_point(stat="identity",aes(colour=cut),shape=18)
g4
#####Task 2#####
d <- ggplot(diamonds,aes(carat))+xlim(0,3)
d1 <- d+stat_bin(aes(ymax=..count..),binwidth = 0.1,geom = "area")
d1
d2 <- d+stat_bin(aes(size=..density..),binwidth = 0.1,geom = "point",position = "identity")
d2
d3 <- d+geom_area(stat="bin",aes(y=..count..))
d3
d4 <- d+geom_point(stat = "bin",aes(size=..density..))
d4
#####Task 3#####
p <- ggplot(diamonds,aes(x=clarity,y=carat))+geom_boxplot(aes(color=cut))
p
sm <- p+geom_smooth(aes(group=cut),method = "lm")
sm
p3_1 <- sm+facet_wrap(.~cut)
p3_1
p3_2 <- sm+facet_wrap(.~color)
p3_2
p3_3 <- sm+facet_wrap(cut~color)
p3_3
p4 <- sm+labs(title = "Carats vs clarity box plot")
p4
p5 <- p4+scale_color_brewer()
p5
#####Task 4######
f1_1 <- sm+scale_y_continuous(trans="log10")
f1_1
f1_2 <- f1_1 + ylab("log10 of carat")
f1_2
f2 <- ggplot(diamonds,aes(x=clarity,y=log10(carat)))+geom_boxplot(aes(color=cut))
f2
f3 <- ggplot(diamonds,aes(x=clarity,y=carat))+geom_boxplot(aes(clarity,log10(carat),color=cut))+geom_smooth(aes(clarity,log10(carat),group=cut),method = "lm")
f3
f4 <- p+scale_y_continuous(limits = c(0.3,3))
f4
#####Task 5#####
dia_sample <- diamonds[sample(nrow(diamonds),100,replace = F),]
q <- ggplot(data = dia_sample,aes(x=clarity,y=carat))
q1 <- q+geom_boxplot()+geom_jitter(aes(size=carat,color=price))
q1
q2 <- q+geom_boxplot()+geom_jitter(aes(size=carat,color=price))+scale_color_gradient(limits=c(10000,15000))
q2
q3 <- q+geom_boxplot()+geom_jitter(aes(size=carat,color=price))+scale_color_gradient(limits=c(10000,15000))+scale_size_continuous(limits = c(1.2,2))
q3
#####Task 6#####
s <- ggplot(diamonds,aes(clarity))
s1 <- s+geom_bar(aes(fill=cut),stat = "count", position = "stack")
s1
s2 <- s+geom_bar(aes(fill=cut),stat = "count", position = "fill")
s2
s3 <- s+geom_bar(aes(fill=cut),stat = "count", position = "dodge")
s3
