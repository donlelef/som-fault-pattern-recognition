# Tets for plots

#R version 3.0.0, www.r-project.org
#Eurostat: Real GDP growth rate - volume - percentage change on previous year
#Finland 2002 - 2012
#Date 2013/02/18

x <- c(1.8,2.0,4.1,2.9,4.4,5.3,0.3,-8.5,3.4,2.8,-1.0) 
names(x) <- c(2002:2012)
par(mar=c(3.2,4,1,0),las=1,mgp=c(2.2,1,0),cex=2,cex.axis=.9)
y <- barplot(x,ylim=c(-9,6),xlim=c(.2,14.5),ylab="%",
             space=.3,col="steelblue2",xaxs="i")
text(y,x+.4,format(x,digits=2))
axis(1,c(.2,y+.7),labels=F)
abline(h=0)
