#Cu, Ni in Moss
# Fig. 5.2.: Growing dot maps: continuously and exponential growing
library(StatDA)
data(moss)
data(kola.background)
el=moss[,"Ni"]
X=moss[,"XCOO"]
Y=moss[,"YCOO"]

# true representation of x and y axis of map for plot
xwid=diff(range(X))/12e4
ywid=diff(range(Y))/12e4

pdf("fig-NiinMoss.pdf",width=2*xwid,height=1*ywid)
par(mfrow=c(1,2),mar=c(1.5,1.5,1.5,1.5))



### NEW MAP:

# take symbols continuously growing
q=c(min(el),min(el)+(max(el)-min(el))/2,max(el))

# symbols and size
symb=c(16,16,16)
ssize=c(0.3,0.9,1.7)

# generate plot with packground
plot(X,Y,frame.plot=FALSE,xaxt="n",yaxt="n",xlab="",ylab="",type="n")
plotbg(map.col=c("gray","gray","gray","gray"),add.plot=T)

# plot symbols:
el01=(el-min(el))/(max(el)-min(el))
el01ssize=el01*(ssize[3]-ssize[1])+ssize[1]
points(X, Y, pch=symb[3], cex=el01ssize)

#Legend
legn=round(q,2)
leg=rep(NA,length(q))
for (i in 1:length(q)){
  leg[i]=roundpretty(legn[i],2)
}
legend("topright",pch=rev(symb),pt.cex=rev(ssize), legend=rev(leg), title="Ni [mg/kg]",
       cex=0.75)

# continuously growing dot legend
text(min(X)+diff(range(X))*5/8,max(Y),"Linearly",cex=0.70)
text(min(X)+diff(range(X))*5/8,max(Y)-diff(range(Y))/25,"growing dots",cex=0.70)

# scalebar
scalebar(761309,7373050,861309,7363050,shifttext=-0.5,shiftkm=4e4,sizetext=0.8)
# North arrow
Northarrow(362602,7818750,362602,7878750,362602,7838750,Alength=0.15,Aangle=15,Alwd=1.3,Tcex=1.6)


### NEW MAP:

# generate plot with packground
plot(X,Y,frame.plot=FALSE,xaxt="n",yaxt="n",xlab="",ylab="",type="n")
plotbg(map.col=c("gray","gray","gray","gray"),add.plot=T)

bubbleFIN(X,Y,el,S=9,s=2,plottitle="",legendtitle="Ni [mg/kg]", text.cex=0.60,
          legtitle.cex=0.70,ndigits=2)


# Percentile legend
text(min(X)+diff(range(X))*5/7,max(Y),"Exponentially",cex=0.70)
text(min(X)+diff(range(X))*5/7,max(Y)-diff(range(Y))/25,"growing dots",cex=0.70)

#scalebar
scalebar(761309,7373050,861309,7363050,shifttext=-0.5,shiftkm=4e4,sizetext=0.8)
# North arrow
Northarrow(362602,7818750,362602,7878750,362602,7838750,Alength=0.15,Aangle=15,Alwd=1.3,Tcex=1.6)


dev.off()
#dev.new()

# Fig. 6.1.: xy-plot of Cu, Ni in Moss
library(StatDA)
data(moss)

# Country
Cu=moss[,"Cu"]
Ni=moss[,"Ni"]

pdf("fig-Cu-Ni-relationship.pdf",width=9,height=4.5)
par(mfrow=c(1,2),mar=c(4,4,2,2))

plot(Cu,Ni,xlab="Cu in Moss [mg/kg]", ylab="Ni in Moss [mg/kg]",pch=3, cex=0.7,cex.lab=1.2)

plot(log10(Cu),log10(Ni),xlab="Cu in Moss [mg/kg]", ylab="Ni in Moss [mg/kg]",
     xaxt="n", yaxt="n", pch=3, cex=0.7,cex.lab=1.2)
axis(1,at=log10(a<-sort(c((10^(-50:50))%*%t(c(2,5,10))))),labels=a)
axis(2,at=log10(a<-sort(c((10^(-50:50))%*%t(c(2,5,10))))),labels=a)
text(log10(9.1),-0.02,moss[30,"ID"])
text(log10(3.3),0.8,moss[31,"ID"])

dev.off()

# Fig. 6.6.: xy-plot with spatial trend
library(StatDA)
data(moss)
data(kola.background)

pdf("fig-NiinMoss-ew.pdf",width=9,height=4.5)
par(mfrow=c(1,2),mar=c(4,4,2,2))

plotbg(map.col=c("gray","gray","gray","gray"), xlab="UTM east [m]", ylab="UTM north [m]",
       cex.lab=1.2)

X=moss[,"XCOO"]
Y=moss[,"YCOO"]
points(X[Y<7600000 & Y>7500000],Y[Y<7600000 & Y>7500000],pch=3,cex=0.7)

x=(X[Y<7600000 & Y>7500000]-753970)/1000
y=log10(moss[Y<7600000 & Y>7500000,"Ni"])

plot(x,y,xlab="Distance from Monchegorsk [km]",ylab="Ni in Moss [mg/kg]", yaxt="n",
     pch=3,cex=0.7, cex.lab=1.2)
axis(2,at=log10(a<-sort(c((10^(-50:50))%*%t(c(2,5,10))))),labels=a)
lines(smooth.spline(x,y),col=1,lwd=1.3)

dev.off()

# xy-plot with spatial distance (Cu NiZap)
library(StatDA)

# need also this package:
library(sgeostat)

data(moss)
data(kola.background)

X=moss[,"XCOO"]
Y=moss[,"YCOO"]
Cu=moss[,"Ni"]

loc = list(x=c(766824.2,755182.3,708614.7,667868.0,354991.9,353536.7,592195.7),
           y=c(7751607,7679137,7635654,7611937,7614572,7908407,7903136))

loc.in=in.polygon(X,Y,loc$x,loc$y) # observations in polygone

ref=c(616307.2, 7716277) # reference point


pdf("fig-NiinMoss-Zapo.pdf",width=9,height=4.5)
par(mfrow=c(1,2),mar=c(4,4,2,2))

plotbg(map.col=c("gray","gray","gray","gray"), xlab="UTM east [m]", ylab="UTM north [m]",
       cex.lab=1.2)

points(X[!loc.in],Y[!loc.in],pch=16,cex=0.3)
points(X[loc.in],Y[loc.in],pch=3,cex=0.7)
points(ref[1],ref[2],pch=16,cex=1)
polygon(loc$x,loc$y,border=1,lwd=1.3)


distanc=sqrt((X[loc.in]-ref[1])^2+(Y[loc.in]-ref[2])^2)

plot(distanc/1000,log10(Cu[loc.in]),xlab="Distance to Nikel/Zapoljarnij [km]",
     ylab="Ni in Moss [mg/kg]", yaxt="n", pch=3,cex=0.7, cex.lab=1.2)
axis(2,at=log10(a<-sort(c((10^(-50:50))%*%t(c(2,5,10))))),labels=a)
lines(smooth.spline(distanc/1000,log10(Cu[loc.in]),spar=0.8),col=1,lwd=1.3)

dev.off()

# Fig. 6.10.: xy-plot with spatial distance (Cu NiZap-Monch)
library(StatDA)

# need also this package:
library(sgeostat)

data(moss)
data(kola.background)
X=moss[,"XCOO"]
Y=moss[,"YCOO"]
Cu=moss[,"Ni"]

loc = list(x=c(590565.1,511872.8,779702.8,861156.3),
           y=c(7824652,7769344,7428054,7510341))

loc.in=in.polygon(X,Y,loc$x,loc$y) # observations in polygone

ref=c(542245.3,7803068) # reference point

pdf("fig-Ni-ZapMon.pdf",width=9,height=4.5)

par(mfrow=c(1,2),mar=c(4,4,2,2))

plotbg(map.col=c("gray","gray","gray","gray"), xlab="UTM east [m]", ylab="UTM north [m]",
       cex.lab=1.2)
points(X[!loc.in],Y[!loc.in],pch=16,cex=0.3)
points(X[loc.in],Y[loc.in],pch=3,cex=0.7)
points(ref[1],ref[2],pch=16,cex=1)
polygon(loc$x,loc$y,border=1,lwd=1.3)


distanc=sqrt((X[loc.in]-ref[1])^2+(Y[loc.in]-ref[2])^2)

plot(distanc/1000,log10(Cu[loc.in]),xlab="Distance from reference point [km]",
     ylab="Ni in Moss [mg/kg]", yaxt="n", pch=3,cex=0.7, cex.lab=1.2)
axis(2,at=log10(a<-sort(c((10^(-50:50))%*%t(c(2,5,10))))),labels=a)
lines(smooth.spline(distanc/1000,log10(Cu[loc.in]),spar=0.8),col=1,lwd=1.3)

dev.off()
