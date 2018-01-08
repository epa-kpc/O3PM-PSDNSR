
### Read in SSIA format files to calculate impacts

a <- "/work/ROMO/lrt/camx/QQGRIDQQ/postp/QQFNAMEQQ"
one <- read.csv(file=a,skip=0,header=T,row.names=NULL)
head(one)

#Find maximum difference (impact)

subone <- subset(one,CONC>0.0) #remove cell days not downwind of the source
maxdiff <- max(subone$CONC)
stats <- quantile(subone$CONC,probs=c(0.90,0.95,0.99,1))
stats

temp <- subset(subone,CONC==maxdiff)
temp
temp$DATE <- NULL
temp$TIME <- NULL
temp$NH3 <- NULL
temp$I <- NULL
temp$J <- NULL
temp$SPECIE <- gsub(" ","",temp$SPECIE,fixed=TRUE)
temp$DOMAIN <- "QQGRIDQQ"
temp$METRIC <- "QQMETRICQQ"

write.table(temp, file='/work/ROMO/users/kpc/merps/merpcalc/out/peakimpact_QQFNAMEQQ.csv', sep=",",row.names=FALSE)
