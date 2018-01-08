
### Read in SSIA format files to calculate impacts

a <- "/work/ROMO/lrt/camx/12US2/postp/ssia_concs.O3.12US2.2011eh_cb6v2_v6_11g+fake500.baseline20.csv"
temp <- read.csv(file=a,skip=1,header=T,row.names=NULL)
colnames(temp)[1] <- "colrow"
colnames(temp)[5] <- "date"
colnames(temp)[6] <- "baseline"
temp$X_ID <- NULL
temp$X_TYPE <- NULL
temp$LAT <- NULL
temp$LONG <- NULL
temp$O3 <- NULL
head(temp)
baseline <- temp

a <- "/work/ROMO/lrt/camx/12US2/postp/ssia_concs.O3.12US2.2011eh_cb6v2_v6_11g+fake500.source20.csv"
temp <- read.csv(file=a,skip=1,header=T,row.names=NULL)
colnames(temp)[1] <- "colrow"
colnames(temp)[5] <- "date"
colnames(temp)[6] <- "altscen"
temp$X_ID <- NULL
temp$X_TYPE <- NULL
temp$LAT <- NULL
temp$LONG <- NULL
temp$O3 <- NULL
head(temp)
altscen <- temp


#Merge files
one <- merge(baseline,altscen,by=c("date","colrow"),all.x=TRUE,all.y=TRUE)
one$diff <- one$altscen - one$baseline
head(one)


#Find maximum difference (impact)

subone <- subset(one,diff>0.0) #remove cell days not downwind of the source
maxdiff <- max(subone$diff)
stats <- quantile(subone$diff,probs=c(0.90,0.95,0.99,1))
stats

temp <- subset(subone,diff==maxdiff)
temp

two <- subset(one,baseline>65 & diff>0.0)
maxdiff2 <- max(two$diff)
stats2 <- quantile(two$diff,probs=c(0.90,0.95,0.99,1))
stats2

temp <- subset(two,diff==maxdiff2)
temp


#write.table(final, file='/work/ROMO/users/kpc/tr_p_2014/tablesO3/ozonecontrib_finaltable_QQCASEQQ.QQRRFTESTQQ.O3N.csv', sep=",",row.names=FALSE)
