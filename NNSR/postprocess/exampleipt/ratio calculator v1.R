require(ncdf)
library(fields)
require(rgdal)
require(M3)


## This script was used to generate information to support the illustrative example shown in Appendix B of the O3 IPT TGD


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


source.open <- open.ncdf(con="/Users/kirkbaker/Documents/o3ipt/data/combine_MDA8_O3.4NYPA2.ipt.9.nox.500.07.ncf", write=FALSE, readunlim=FALSE)
source.open.credit <- open.ncdf(con="/Users/kirkbaker/Documents/o3ipt/data/combine_MDA8_O3.4NYPA2.ipt.1.voc.500.07.ncf", write=FALSE, readunlim=FALSE)
source.open.map <- open.ncdf(con="/Users/kirkbaker/Documents/o3ipt/data/countymap.4NYPA2.GROUPA.ncf", write=FALSE, readunlim=FALSE)

one <- NULL

days <- seq(1,29,by=1)
for (i in 1:length(days)) {

datavar.array <- get.var.ncdf(source.open,"O3PROJ",start=c(1,1,1,days[i]),count=c(num.grid.x,num.grid.y,1,1))
baseline.array <- get.var.ncdf(source.open,"O3",start=c(1,1,1,days[i]),count=c(num.grid.x,num.grid.y,1,1))
datavar.array.credit <- get.var.ncdf(source.open.credit,"O3PROJ",start=c(1,1,1,days[i]),count=c(num.grid.x,num.grid.y,1,1))
datavar.array.map <- get.var.ncdf(source.open.map,"POP2010")
datavar.array.map[datavar.array.map > 0.0] <- 1

datavar.array[datavar.array < 0.001 ] <- 0.
datavar.array.credit[datavar.array.credit < 0.001 ] <- 0.
datavar.array[baseline.array < 65 ] <- 0.
datavar.array.credit[baseline.array < 65 ] <- 0.
datavar.array[datavar.array.map < 1 ] <- 0.
datavar.array.credit[datavar.array.map < 1 ] <- 0.

cells <- sum(datavar.array.map)
cells

n.proj <- datavar.array
n.credit <- datavar.array.credit
n.proj[datavar.array > 0] <- 1
n.credit[datavar.array.credit > 0] <- 1

projsum <-round(sum(datavar.array),2)
creditsum <- round(sum(datavar.array.credit),2)
ratio <- round( (projsum / creditsum),1)
nproj <- sum(n.proj)
ncredit <- sum(n.credit)
nprojpct <-  round(100*nproj/cells,1)
ncreditpct <- round(100*ncredit/cells,1)

junk <- c(days[i],projsum,creditsum,ratio,nproj,nprojpct,ncredit,ncreditpct)
one <- rbind(one,junk)

}

one


write.table(one,file="/Users/kirkbaker/Documents/o3ipt/example1.csv",row.names=FALSE,col.names=TRUE, sep=",")

