library(ggplot2)
data("diamonds")

## deal with overploting
d <- ggplot(diamonds, aes(x= carat,y=price,colour=cut))
d + geom_point() 
d + geom_point(size=0.1)
d + geom_point(alpha=1/5, shape = 18)


##rewrite the code
d <- ggplot(diamonds, aes(carat)) + xlim(0, 3)
d + stat_bin(aes(ymax = ..count..), binwidth = 0.1, geom = "area")
d + geom_area(aes(ymax=..count..), binwidth =0.1, stat = "bin")
d + stat_bin(aes(size = ..density..), binwidth = 0.1, geom = "point", position="identity" )
d+ geom_point(aes(size=..density..), position="identity", stat = "bin", binwith=0.1)


##layer by layer
d <- ggplot(diamonds, aes(x= clarity, y= carat))
d + geom_boxplot()
d + geom_boxplot(aes(colour=cut))
sm <- geom_smooth(aes(group=cut,colour=cut),method = "lm")
d1 <- d + geom_boxplot(aes(colour=cut)) + sm
d1
d1 + facet_wrap(~cut)
d1 + facet_wrap(~color, ncol=7)
d1 + facet_grid(cut~color)
d1 + labs(title = "Carats vs clarity box plot", x="clarity",y="carat")
d2 <- d1 + labs(title = "Carats vs clarity box plot", x="clarity",y="carat") + scale_color_brewer(palette = 3)
png(file="Carats vs clarity box plot.png", width = 500, height = 500)
d2
dev.off()

##scale
d1 + scale_y_continuous(trans = "log10" ) #what is the scale range? why?
d1 + scale_y_continuous(trans = "log10" ) + ylab("unit=carat")
d + geom_boxplot(aes(clarity, log10(carat),colour=cut)) # what is the scale range now? what is the unit? give a y-axis title
d + geom_boxplot(aes(clarity, log10(carat),colour=cut)) + ylab("unit=log10(carat)")
d + geom_boxplot(aes(clarity, log10(carat),colour=cut)) + sm
d + geom_boxplot(aes(clarity, log10(carat),colour=cut)) + geom_smooth(aes(y=log10(carat),group=cut,colour=cut),method = "lm")
d1 + scale_y_continuous(trans = "log10" , limits = c(0.3, 3.0))


##jitter plot
dia_sample <- diamonds[sample(nrow(diamonds),100,replace=F),]
ds <- ggplot(data=dia_sample, aes(x=clarity,y=carat))
ds + geom_boxplot() + geom_jitter(aes(size=carat,colour=price)) ##do you see anything related?
ds + geom_boxplot() + geom_jitter(aes(size=carat,colour=price)) + scale_color_gradient(limits= c(10000,15000))
ds + geom_boxplot() + geom_jitter(aes(size=carat,colour=price)) + scale_color_gradient(limits= c(10000,15000)) + scale_size_continuous(limits = c(1.2,2))

##position
d <- ggplot(diamonds, aes(clarity))
d + geom_bar(aes(fill=cut),stat = "count", position = "stack")
d + geom_bar(aes(fill=cut),stat = "count", position = "fill")
d + geom_bar(aes(fill=cut),stat = "count", position = "dodge")


