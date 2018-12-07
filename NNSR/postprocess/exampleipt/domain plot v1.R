require(ncdf)
require(fields)
require(M3)
require(rgdal)

my.colors <- colorRampPalette(c("white","deepskyblue2","orange"))
#my.colors <- colorRampPalette(c("white","purple","deepskyblue2","green","yellow","orange","red","brown"))

###

pdf(file="/Users/kirkbaker/Documents/o3ipt/pics/spatial_domain_v1.pdf",width=8,height=8)
par(mfrow=c(1,1))
par(mar=c(3,3,3,3))

#### First
source.open <- open.ncdf(con="/Users/kirkbaker/Documents/o3ipt/data/countymap.4NYPA2.GROUPA.ncf", write=FALSE, readunlim=FALSE)

x.orig.km <- att.get.ncdf(source.open,varid=0,attname="XORIG")$value/1000
y.orig.km <- att.get.ncdf(source.open,varid=0,attname="YORIG")$value/1000
x.cell.size <- att.get.ncdf(source.open,varid=0,attname="XCELL")$value/1000
y.cell.size <- att.get.ncdf(source.open,varid=0,attname="YCELL")$value/1000
num.grid.x <- att.get.ncdf(source.open,varid=0,attname="NCOLS")$value
num.grid.y <- att.get.ncdf(source.open,varid=0,attname="NROWS")$value
x.proj.12 <- seq(from=x.orig.km + x.cell.size/2, length=num.grid.x, by=x.cell.size)
y.proj.12 <- seq(from=y.orig.km + y.cell.size/2, length=num.grid.y, by=y.cell.size)
x.axis <- seq(from=1, length=num.grid.x, by=1)
y.axis <- seq(from=1, length=num.grid.y, by=1)

maxval <- 1
datavar.array <- get.var.ncdf(source.open,"POP2010")
datavar.array.plot <- datavar.array 
datavar.array.plot[datavar.array.plot > 0.0] <- 1


image(x.proj.12,y.proj.12,datavar.array.plot,xlab="",ylab="",zlim=c(0,maxval),axes=T,col=my.colors(30),main=paste(" "))

temp <- map('county',plot=F)
coords.proj <- project(cbind(temp$x,temp$y),proj="+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000.0 +b=6370000.0")
lines(coords.proj/1000,col="darkgray")
temp <- map('state',plot=F)
coords.proj <- project(cbind(temp$x,temp$y),proj="+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000.0 +b=6370000.0")
lines(coords.proj/1000,lwd=2)
points(pmmons$coords.x,pmmons$coords.y,pch=1,cex=1.5,col="black",lwd=2)
points(o3mons$coords.x,o3mons$coords.y,pch=4,cex=1.5,col="black",lwd=2)
points(aeronet$coords.x,aeronet$coords.y,pch=2,cex=1.5,col="black",lwd=2)
points(reno$coords.x,reno$coords.y,pch=3,cex=1.5,col="black",lwd=2)
mtext("4 km model domain (4NYPA2)",col="darkblue",cex=1)
#legend("bottomleft",c("County Group A"),pch=c(22),col=c("black"),cex=1,bg="white")
box(col='black')


dev.off()

