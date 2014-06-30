setwd("F:/Allan/R Stuff/RapMin")
setwd("D:/Allan/DropBox/AS_Arima")

cc_ll <- function(df){
  res <- c(0,0)
  ll <- levels(f$confidence.U.)
  for(i in 1:length(ll)){
    #browser()
    r <- sum(f[f$confidence.U.== ll[i],21])
    l <- ll[i]
    res <- rbind(res,c(l,r))
  }
  return(res)
}

# join a data set
dx <- read.csv('as_arima_dax_res.csv',stringsAsFactors=F)
dx$Date <- as.POSIXct(dx$Date,format='%d/%m/%Y') ;dx$Date[20]
tail(dx)
str(dx)
colnames(dx)
dx$confidence.U. <- as.numeric(dx[,20])
dx$confidence.U. <- round(dx[,20],3)
dx1 <- dx[,c(1,2,3,4,5,18,19,20)]
tail(dx1)
dx1$dxpl <- ifelse(dx1$pred=='U',dx1$Close-dx1$Open,dx1$Open-dx1$Close)

dw <- read.csv('as_arima_dow_res.csv',stringsAsFactors=F)
dw$Date <- as.POSIXct(dw$Date,format='%d/%m/%Y') ;dw$Date[20]
tail(dw)
str(dw)
colnames(dw)
dw$confidence.U. <- as.numeric(dw[,20])
dw$confidence.U. <- round(dw[,20],3)
dw1 <- dw[,c(1,2,3,4,5,18,19,20)]
tail(dw1)
dw1$dwpl <- ifelse(dw1$pred=='U',dw1$Close-dw1$Open,dw1$Open-dw1$Close)

rr <- merge(dx1, dw1, by='Date')
rr1 <- merge(dx1[,c(1,7,8,9)], dw1[,c(1,7,8,9)], by='Date')
tail(rr1)

nik <- read.csv('as_arima_nik_res.csv',stringsAsFactors=F)
nik$Date <- as.POSIXct(nik$Date,format='%d/%m/%Y') ;nik$Date[20]
tail(dw)
str(dw)
colnames(dw)
nik$confidence.U. <- as.numeric(nik[,20])
nik$confidence.U. <- round(nik[,20],3)
nik1 <- nik[,c(1,2,3,4,5,18,19,20)]
tail(nik1)
nik1$nikpl <- ifelse(nik1$pred=='U',nik1$Close-nik1$Open,nik1$Open-nik1$Close)

rr2 <- merge(rr1, nik1[,c(1,7,8,9)], by='Date')
rr2$totpl <- rr2$pl + rr2$nikpl

tail(rr2, n=5)
write.csv(rr2,"dxdwnk_res.csv",row.names=FALSE)

f$confidence.U. <- factor(f$confidence.U.)

ll <- levels(f$confidence.U.)
length(ll)
is.factor(f$confidence.U.)

table(f$confidence.U.,f$Win)


rr <- cc_ll(f)

sum(f$confidence.U.== 0.133333333)
sum(f$confidence.U.== 0.2)
colnames(f)

sum(f[f$confidence.U.== 0.133333333,21])
sum(f[f$confidence.U.== 0.266666667,21])
sum(f[f$confidence.U.== 0.333333333,21])
sum(f[f$confidence.U.== 0.4,21])
sum(f[f$confidence.U.== 0.2,21])


f1 <- f[f$confidence.U. == 0.533333333,]
sum(f)
