require(ncdf)
library(fields)
library(graphics)
library(jpeg)
require(rgdal)
require(M3)


## This R script makes multipanel plots of MDA8 O3 ratio between the example sources used in the O3 IPT Appendix B illustrative example


my.colors <- colorRampPalette(c("white","gray","purple","deepskyblue2","green","yellow","orange","red","brown"))
#my.colors <- colorRampPalette(c("white","lightgray","yellow","orange","red","brown"))
rev.colors <- colorRampPalette(c("brown","red","orange","yellow","green","deepskyblue2","purple","lightgray","white"))
my.diff.colors <- colorRampPalette(c("blue","deepskyblue2","white","white","orange","red"))

source.open.map <- open.ncdf(con="/Users/kirkbaker/Documents/o3ipt/data/countymap.4NYPA2.GROUPA.ncf", write=FALSE, readunlim=FALSE)
datavar.array.map <- get.var.ncdf(source.open.map,"POP2010")
datavar.array.map[datavar.array.map > 0.0] <- 1

source.open <- open.ncdf(con="/Users/kirkbaker/Documents/o3ipt/data/combine_MDA8_O3.4NYPA2.ipt.1.voc.500.07.ncf", write=FALSE, readunlim=FALSE)

x.orig.km <- att.get.ncdf(source.open,varid=0,attname="XORIG")$value/1000
y.orig.km <- att.get.ncdf(source.open,varid=0,attname="YORIG")$value/1000
x.cell.size <- att.get.ncdf(source.open,varid=0,attname="XCELL")$value/1000
y.cell.size <- att.get.ncdf(source.open,varid=0,attname="YCELL")$value/1000
num.grid.x <- att.get.ncdf(source.open,varid=0,attname="NCOLS")$value
num.grid.y <- att.get.ncdf(source.open,varid=0,attname="NROWS")$value
x.proj.12 <- seq(from=x.orig.km + x.cell.size/2, length=num.grid.x, by=x.cell.size)
y.proj.12 <- seq(from=y.orig.km + y.cell.size/2, length=num.grid.y, by=y.cell.size)


dates <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)

pdf(file="/Users/kirkbaker/Documents/o3ipt/pics/spatial_source1_ratio_part1_window.pdf",width=12,height=12)
#par(mfrow=c(3,3))
par(mfrow=c(4,4), mar=c(3,3,3.5,3.5), oma=c(0,0,0,1))

source.open <- open.ncdf(con="/Users/kirkbaker/Documents/o3ipt/data/combine_MDA8_O3.4NYPA2.ipt.9.nox.500.07.ncf", write=FALSE, readunlim=FALSE)
source.open.credit <- open.ncdf(con="/Users/kirkbaker/Documents/o3ipt/data/combine_MDA8_O3.4NYPA2.ipt.1.voc.500.07.ncf", write=FALSE, readunlim=FALSE)

days <- seq(1,16,by=1)
for (i in 1:length(days)) {

maxval <- 4
datavar.array <- get.var.ncdf(source.open,"O3PROJ",start=c(1,1,1,days[i]),count=c(num.grid.x,num.grid.y,1,1))
baseline.array <- get.var.ncdf(source.open,"O3",start=c(1,1,1,days[i]),count=c(num.grid.x,num.grid.y,1,1))
datavar.array.credit <- get.var.ncdf(source.open.credit,"O3PROJ",start=c(1,1,1,days[i]),count=c(num.grid.x,num.grid.y,1,1))

datavar.array[datavar.array < 0.001 ] <- 0.
datavar.array.credit[datavar.array.credit < 0.001 ] <- 0.
datavar.array.plot <- datavar.array / datavar.array.credit
datavar.array.plot[baseline.array < 60 ] <- 0.
datavar.array.plot[datavar.array.map < 1 ] <- 0.
datavar.array.plot[datavar.array.plot > maxval ] <- maxval
datavar.array.plot[datavar.array.plot < -maxval ] <- -maxval
image.plot(x.proj.12,y.proj.12,datavar.array.plot,xlab="",ylab="",xlim=c(1750,1950),ylim=c(150,400),zlim=c(0,4),axes=T,col=my.colors(50),main="MDA8 O3",cex.main=1.25,cex.axis=0.75,legend.args=list(text="ppb",col="black",cex=0.66,side=1,line=0.4))
temp <- map('county',plot=F)
coords.proj <- project(cbind(temp$x,temp$y),proj="+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000.0 +b=6370000.0")
lines(coords.proj/1000,col="lightgray",lwd=0.5)
temp <- map('state',plot=F)
coords.proj <- project(cbind(temp$x,temp$y),proj="+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000.0 +b=6370000.0")
lines(coords.proj/1000,col="darkgray",lwd=1)
#points(hms26$coords.x,hms26$coords.y,pch=19,cex=0.66,col="green")
ttext <- paste("Source 1 500 TPY VOC - July ",days[i],", 2011",sep="")
mtext(ttext,cex=0.66)
box(col="black")

}

dev.off()


pdf(file="/Users/kirkbaker/Documents/o3ipt/pics/spatial_source1_ratio_part2_window.pdf",width=12,height=12)
par(mfrow=c(4,4), mar=c(3,3,3.5,3), oma=c(0,0,0,1))


days <- seq(17,29,by=1)
for (i in 1:length(days)) {
  
  maxval <- 4
  datavar.array <- get.var.ncdf(source.open,"O3PROJ",start=c(1,1,1,days[i]),count=c(num.grid.x,num.grid.y,1,1))
  baseline.array <- get.var.ncdf(source.open,"O3",start=c(1,1,1,days[i]),count=c(num.grid.x,num.grid.y,1,1))
  datavar.array.credit <- get.var.ncdf(source.open.credit,"O3PROJ",start=c(1,1,1,days[i]),count=c(num.grid.x,num.grid.y,1,1))
  
  datavar.array[datavar.array < 0.001 ] <- 0.
  datavar.array.credit[datavar.array.credit < 0.001 ] <- 0.
  datavar.array.plot <- datavar.array / datavar.array.credit
  datavar.array.plot[baseline.array < 60 ] <- 0.
  datavar.array.plot[datavar.array.map < 1 ] <- 0.
  datavar.array.plot[datavar.array.plot > maxval ] <- maxval
  datavar.array.plot[datavar.array.plot < -maxval ] <- -maxval
  image.plot(x.proj.12,y.proj.12,datavar.array.plot,xlab="",ylab="",xlim=c(1750,1950),ylim=c(150,400),zlim=c(0,4),axes=T,col=my.colors(50),main="MDA8 O3",cex.main=1.25,cex.axis=0.75,legend.args=list(text="ppb",col="black",cex=0.66,side=1,line=0.4))
  temp <- map('county',plot=F)
  coords.proj <- project(cbind(temp$x,temp$y),proj="+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000.0 +b=6370000.0")
  lines(coords.proj/1000,col="lightgray",lwd=0.5)
  temp <- map('state',plot=F)
  coords.proj <- project(cbind(temp$x,temp$y),proj="+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000.0 +b=6370000.0")
  lines(coords.proj/1000,col="darkgray",lwd=1)
  #points(hms26$coords.x,hms26$coords.y,pch=19,cex=0.66,col="green")
  ttext <- paste("Source 1 500 TPY VOC - July ",days[i],", 2011",sep="")
  mtext(ttext,cex=0.66)
  box(col="black")
  
}

dev.off()


