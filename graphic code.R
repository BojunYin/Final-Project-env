#plot data followed by the chapter
#chapter3  Graphics displayed by the DATA distribution
#3-1 artificial data sets to explain data distribution
library(StatDA)
#generate pdf
pdf("fig-3-1.pdf",width=8,height=4)
par(mfrow=c(2,2),mar=c(4,2,1,2))
x1=c(2,11,15,18,19.3,20,21,22.5,24,28,37)
plot(x1,rep(0.1,length(x1)),xlab="Data values",ylab="",cex.lab=1.2,yaxt="n",
     frame.plot=F,xlim=c(0,40),ylim=c(0,1),pch=3)

x2=c(3,5,6.5,7.8,9,10.1,12,27,28.1,29.4,30.8,32,33.1,35)
plot(x2,rep(0.1,length(x2)),xlab="Data values",ylab="",cex.lab=1.2,yaxt="n",
     frame.plot=F,xlim=c(0,40),ylim=c(0,1),pch=3)

x3=c(1,22,29,33,35,36.5,37.5,38.0,38.4,38.8,39)
plot(x3,rep(0.1,length(x3)),xlab="Data values",ylab="",cex.lab=1.2,yaxt="n",
     frame.plot=F,xlim=c(0,40),ylim=c(0,1),pch=3)

x4=c(2,2.5,3.1,3.9,4.9,6.2,8,12,17,25,39)
plot(x4,rep(0.1,length(x4)),xlab="Data values",ylab="",cex.lab=1.2,yaxt="n",
     frame.plot=F,xlim=c(0,40),ylim=c(0,1),pch=3)
dev.off()
#3-2 how to change the symbol of the point?: pch element
pdf("fig-3-2.pdf",width=6,height=7)
par(mfrow=c(3,1))
par(mar=c(5,1,1,1))
stripchart(chorizon$Sc_INAA,method="overplot",ylim=c(0.8,1.2),pch=3,cex=0.45)
mtext("Sc in C-horizon [mg/kg], points overplotted",side=1,line=3,cex=1.1)

stripchart(chorizon$Sc_INAA,method="stack",ylim=c(0.5,225),pch=3,cex=0.45)
mtext("Sc in C-horizon [mg/kg], points stacked",side=1,line=3,cex=1.1)

set.seed(1)
stripchart(chorizon$Sc_INAA,method="jitter",ylim=c(0.8,1.2),pch=3,cex=0.45)
mtext("Sc in C-horizon [mg/kg], points jittered",side=1,line=3,cex=1.1)

dev.off()
#3-3 histogram of Ba
data(chorizon)
Ba=chorizon[,"Ba"]

pdf("fig-3-3.pdf",width=9,height=7)
par(mfrow=c(2,2),mar=c(4,4,3,3))

hist(Ba,main="",xlab="Ba in C-horizon [mg/kg]",ylab="Frequency",cex.lab=1.2)

hist(Ba,main="",xlab="Ba in C-horizon [mg/kg]",ylab="Frequency",xlim=c(0,250),breaks=60,cex.lab=1.2)
text(130,150,paste("Maximum =",max(Ba),"mg/kg"),pos=4)

par(mar=c(7,4,3,3))
hist(Ba,main="",xlab="Ba in C-horizon [mg/kg]",ylab="Frequency",xlim=c(0,250),breaks=100,cex.lab=1.2)
text(120,80,paste("Maximum =",max(Ba),"mg/kg"),pos=4)

hist(log10(Ba),main="",xlab="",ylab="Frequency",cex.lab=1.2)
axis(1,at=log10(a<-sort(c((10^(-50:50))%*%t(c(2,5,10))))),labels=a,line=2.5)
mtext("log10(Ba) and Ba in C-horizon [mg/kg]",line=5.5,cex=1,side=1)
dev.off()
#dev.new()
# Fig. 3.4.: histogram plus one-dim. scatter plot for Sc_INAA
library(StatDA)
data(chorizon)
Sc=chorizon[,"Sc_INAA"]

# Function for histogram plus one-dimensional scatter plot:
hist1 <- function(x,xlab,ylab) {
  par(oma=c(4,3,0,0), mar=c(0,0,0,0))
  layout(matrix(c(1,2),2,1), c(9,9), c(6,1), TRUE)
  h <- hist(x, freq=TRUE, axes=FALSE, xlab="", ylab="", main="")
  axis(2)
  plot(0, 0, type="n", ylim=c(-0.1,1.1), xlim=c(h$breaks[1],h$breaks[length(h$breaks)]),
       axes=FALSE, xlab="", ylab="")
  mtext(ylab,side=2,line=1,outer=TRUE, las=3,cex=1.2)
  box()
  set.seed(1)
  points(x, runif(length(x)),pch=3,cex=0.5)
  axis(1)
  mtext(xlab,side=1,line=3,outer=TRUE, las=1,cex=1.2)
}

pdf("fig-3-4.pdf",width=7,height=5)
par(mar=c(2,2,1,1))
hist1(Sc,xlab="Sc in C-horizon [mg/kg]",ylab="Frequency")
dev.off()


