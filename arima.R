setwd("F:/Allan/R Stuff/RapMin")
setwd("D:/Allan/DropBox/Dmin")

cc_ll <- function(df){
  res <- c(0,0,0,0,0,0)
  ll <- levels(f$confidence.U.)
  for(i in 1:length(ll)){
    #browser()
    df2 <- df[f$confidence.U.== ll[i],]
    #r <- round(sum(df[f$confidence.U.== ll[i],21]),2)
    r <- round(sum(df2[df2$confidence.U.== ll[i],21]),2)
    ct <- nrow(df2)
    win <- sum(df2$pl > 0)
    win_per <- round(win/ct,2) * 100
    av <- round(r / ct,2) 
    l <- round(as.numeric(ll[i]),3)
    res <- rbind(res,c(l,r,ct,av,win,win_per))
  }
  res <- res[-1,]
  colnames(res) <- c('Conf','pl','Trades','av','wins','win %')
  return(res)
}

setwd("D:/Allan/DropBox/AS_Arima")
f <- read.csv('as_arima_dow_res.csv',stringsAsFactors=T)
cc_ll(f)


# Dow
f <- read.csv('as_arima_dow_res.csv',stringsAsFactors=T)
f$pl <- ifelse(f$pred=='U',f$Close-f$Open,f$Open-f$Close)
rr <- cc_ll(f) ; rr
tail(f)
str(f)
colnames(f)

f <- read.csv('as_arima_dow_res.csv',stringsAsFactors=F)
f$confidence.U. <- round(as.numeric(f$confidence.U.),3)
names(f)[20] <- "Conf_U"
f1 <- f[,c(1,2,3,4,5,19,20)];tail(f1)
f1$pl <- ifelse(f1$pred=='U',f1$Close-f1$Open,f1$Open-f1$Close)
tail(f1)
sum(f1$pl)
mean(f1$pl)

f2 <-f1[f1$Conf_U>0.75 | f1$Conf_U<0.3,] ;tail(f2)
sum(f2$pl)
mean(f2$pl)
nrow(f2)
nrow(f2)/nrow(f1)

f2 <-f1[f1$Conf_U>0.7 | f1$Conf_U<0.4,] ;tail(f2)
sum(f2$pl)
mean(f2$pl)
nrow(f2)
nrow(f2)/nrow(f1)

# dx
f <- read.csv('as_arima_dax_res.csv',stringsAsFactors=T)
f$pl <- ifelse(f$pred=='U',f$Close-f$Open,f$Open-f$Close)
rr <- cc_ll(f) ; rr
tail(f)
str(f)
colnames(f)

f <- read.csv('as_arima_dax_res.csv',stringsAsFactors=F)
f$confidence.U. <- round(as.numeric(f$confidence.U.),3)
names(f)[20] <- "Conf_U"
f1 <- f[,c(1,2,3,4,5,19,20)];tail(f1)
f1$pl <- ifelse(f1$pred=='U',f1$Close-f1$Open,f1$Open-f1$Close)
tail(f1)
sum(f1$pl)
mean(f1$pl)

f2 <-f1[f1$Conf_U>0.7 | f1$Conf_U<0.35,] ;tail(f2)
sum(f2$pl)
mean(f2$pl)
nrow(f2)
nrow(f2)/nrow(f1)

f3 <-f1[f1$Conf_U>0.75 | f1$Conf_U<0.3,] ;tail(f2)
sum(f3$pl)
mean(f3$pl)
nrow(f3)
nrow(f3)/nrow(f1)

# cc
f <- read.csv('as_arima_cac_res.csv',stringsAsFactors=T)
f$pl <- ifelse(f$pred=='U',f$Close-f$Open,f$Open-f$Close)
rr <- cc_ll(f) ; rr
tail(f)
str(f)
colnames(f)

f <- read.csv('as_arima_cac_res.csv',stringsAsFactors=F)
f$confidence.U. <- round(as.numeric(f$confidence.U.),3)
names(f)[20] <- "Conf_U"
f1 <- f[,c(1,2,3,4,5,19,20)];tail(f1)
f1$pl <- ifelse(f1$pred=='U',f1$Close-f1$Open,f1$Open-f1$Close)
tail(f1)
sum(f1$pl)
mean(f1$pl)

f2 <-f1[f1$Conf_U>0.7 | f1$Conf_U<0.4,] ;tail(f2)
sum(f2$pl)
mean(f2$pl)
nrow(f2)
nrow(f2)/nrow(f1)

f3 <-f1[f1$Conf_U>0.75 | f1$Conf_U<0.3,] ;tail(f2)
sum(f3$pl)
mean(f3$pl)
nrow(f3)
nrow(f3)/nrow(f1)

# ft
f <- read.csv('as_arima_ftse_res.csv',stringsAsFactors=T)
f$pl <- ifelse(f$pred=='U',f$Close-f$Open,f$Open-f$Close)
rr <- cc_ll(f) ; rr
tail(f)
str(f)
colnames(f)

f <- read.csv('as_arima_ftse_res.csv',stringsAsFactors=F)
f$confidence.U. <- round(as.numeric(f$confidence.U.),3)
names(f)[20] <- "Conf_U"
f1 <- f[,c(1,2,3,4,5,19,20)];tail(f1)
f1$pl <- ifelse(f1$pred=='U',f1$Close-f1$Open,f1$Open-f1$Close)
tail(f1)
sum(f1$pl)
mean(f1$pl)

f2 <-f1[f1$Conf_U>0.7 | f1$Conf_U<0.4,] ;tail(f2)
sum(f2$pl)
mean(f2$pl)
nrow(f2)
nrow(f2)/nrow(f1)

f3 <-f1[f1$Conf_U>0.75 | f1$Conf_U<0.3,] ;tail(f2)
sum(f3$pl)
mean(f3$pl)
nrow(f3)
nrow(f3)/nrow(f1)

# nk
f <- read.csv('as_arima_nik_res.csv',stringsAsFactors=T)
f$pl <- ifelse(f$pred=='U',f$Close-f$Open,f$Open-f$Close)
rr <- cc_ll(f) ; rr
tail(f)
str(f)
colnames(f)

f <- read.csv('as_arima_nik_res.csv',stringsAsFactors=F)
f$confidence.U. <- round(as.numeric(f$confidence.U.),3)
names(f)[20] <- "Conf_U"
f1 <- f[,c(1,2,3,4,5,19,20)];tail(f1)
f1$pl <- ifelse(f1$pred=='U',f1$Close-f1$Open,f1$Open-f1$Close)
tail(f1)
sum(f1$pl)
mean(f1$pl)

f2 <-f1[f1$Conf_U>0.7 | f1$Conf_U<0.4,] ;tail(f2)
sum(f2$pl)
mean(f2$pl)
nrow(f2)
nrow(f2)/nrow(f1)

f3 <-f1[f1$Conf_U>0.75 | f1$Conf_U<0.3,] ;tail(f2)
sum(f3$pl)
mean(f3$pl)
nrow(f3)
nrow(f3)/nrow(f1)

# oz
f <- read.csv('as_arima_oz_res.csv',stringsAsFactors=T)
f$pl <- ifelse(f$pred=='U',f$Close-f$Open,f$Open-f$Close)
rr <- cc_ll(f) ; rr
tail(f)
str(f)
colnames(f)

f <- read.csv('as_arima_oz_res.csv',stringsAsFactors=F)
f$confidence.U. <- round(as.numeric(f$confidence.U.),3)
names(f)[20] <- "Conf_U"
f1 <- f[,c(1,2,3,4,5,19,20)];tail(f1)
f1$pl <- ifelse(f1$pred=='U',f1$Close-f1$Open,f1$Open-f1$Close)
tail(f1)
sum(f1$pl)
mean(f1$pl)

f2 <-f1[f1$Conf_U>0.7 | f1$Conf_U<0.4,] ;tail(f2)
sum(f2$pl)
mean(f2$pl)
nrow(f2)
nrow(f2)/nrow(f1)

f3 <-f1[f1$Conf_U>0.75 | f1$Conf_U<0.3,] ;tail(f2)
sum(f3$pl)
mean(f3$pl)
nrow(f3)
nrow(f3)/nrow(f1)

# --------------------------------------------
#join
# date, pred, conf, pl

setwd("D:/Allan/DropBox/AS_Arima")

dw <- read.csv('as_arima_dow_res.csv',stringsAsFactors=F)
dw$Date <- as.POSIXct(dw$Date,format='%d/%m/%Y') ;dw$Date[20]
dw$confidence.U. <- round(as.numeric(dw$confidence.U.),3)
names(dw)[20] <- "Conf_U"
dw$pl <- ifelse(dw$pred=='U',dw$Close-dw$Open,dw$Open-dw$Close)
dw1 <- dw[,c(1,19,20,21)];tail(dw1)
colnames(dw1) <- c('Date','dwpr','dwconf','dwpl')

dw1 <- dw[,c(1,2,3,4,5,19,20,21)];tail(dw1)
tail(dw1)
write.csv(dw1,"dax_res.csv",row.names=FALSE)  

dx <- read.csv('as_arima_dax_res.csv',stringsAsFactors=F)
dx$Date <- as.POSIXct(dx$Date,format='%d/%m/%Y') ;dx$Date[20]
dx$confidence.U. <- round(as.numeric(dx$confidence.U.),3)
names(dx)[20] <- "Conf_U"
dx$pl <- ifelse(dx$pred=='U',dx$Close-dx$Open,dx$Open-dx$Close)
dx1 <- dx[,c(1,19,20,21)];tail(dx1)
colnames(dx1) <- c('Date','dxpr','dxconf','dxpl')

mg <- merge(dw1,dx1,by='Date')
tail(mg,n=20)

nk <- read.csv('as_arima_nik_res.csv',stringsAsFactors=F)
nk$Date <- as.POSIXct(nk$Date,format='%d/%m/%Y') ;nk$Date[20]
nk$confidence.U. <- round(as.numeric(nk$confidence.U.),3)
names(nk)[20] <- "Conf_U"
nk$pl <- ifelse(nk$pred=='U',nk$Close-nk$Open,nk$Open-nk$Close)
nk1 <- nk[,c(1,19,20,21)];tail(nk1)
colnames(nk1) <- c('Date','nkpr','nkconf','nkpl')

mg <- merge(mg,nk1,by='Date')
tail(mg)

ft <- read.csv('as_arima_ftse_res.csv',stringsAsFactors=F)
ft$Date <- as.POSIXct(ft$Date,format='%d/%m/%Y') ;ft$Date[20]
ft$confidence.U. <- round(as.numeric(ft$confidence.U.),3)
names(ft)[20] <- "Conf_U"
ft$pl <- ifelse(ft$pred=='U',ft$Close-ft$Open,ft$Open-ft$Close)
ft1 <- ft[,c(1,19,20,21)];tail(ft1)
colnames(ft1) <- c('Date','ftpr','ftconf','ftpl')

mg <- merge(mg,ft1,by='Date')
tail(mg)

mean(mg$ftpl)

mg$totpl <- mg$dwpl + mg$dxpl + mg$nkpl
tail(mg)
sum(mg$totpl)
mean(mg$totpl)

f2 <- mg[mg$dwconf>0.7 | mg$dxconf>0.7 | mg$nkconf>0.7 | mg$dwconf<0.4 | mg$dxconf<0.4 | mg$nkconf<0.4,] 
tail(f2,n=15)
sum(f2$totpl)
mean(f2$totpl)
nrow(f2)
nrow(f2)/nrow(mg)

as.character.Date(f2$Date[1630])
nrow(f2)

f2$Date[1630]

f3 <- f2[f2$Date > f2$Date[1620] & f2$Date < f2$Date[1640],]
f3 <- f2[f2$Date > f2$Date[1600] & f2$Date < f2$Date[1620],]
f3 <- f2[f2$Date > f2$Date[1580] & f2$Date < f2$Date[1600],]
f3 <- f2[f2$Date > f2$Date[1560] & f2$Date < f2$Date[1580],]

tail(f3,n=20)
sum(f3$totpl)

# --------------
dw <- read.csv('as_arima_dow_res.csv',stringsAsFactors=F)
dw$Date <- as.POSIXct(dw$Date,format='%d/%m/%Y') ;dw$Date[20]
dw$confidence.U. <- round(as.numeric(dw$confidence.U.),3)
names(dw)[20] <- "Conf_U"
dw$pl <- ifelse(dw$pred=='U',dw$Close-dw$Open,dw$Open-dw$Close)
dw1 <- dw[,c(1,19,20,21)];tail(dw1)
colnames(dw1) <- c('Date','dwpr','dwconf','dwpl')


dw01 <- read.csv('as_01_arima_dow_res.csv',stringsAsFactors=F)
colnames(dw01)
dw01$pl <- as.numeric(dw01$pl)
dw01$Date <- as.POSIXct(dw01$Date,format='%d/%m/%Y') ;dw01$Date[20]
dw02 <- dw01[,c(1,18,19)]
tail(dw02,n=20)

mm <- merge(dw1,dw02,by='Date')
tail(mm,n=20)
sum(dw01$pl)

# -----------------------
f$confidence.U. <- factor(f$confidence.U.)

ll <- levels(f$confidence.U.)
length(ll)
is.factor(f$confidence.U.)

table(f$confidence.U.,f$Win)

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

# --------------------------------------
# add aroon ...
setwd("D:/Allan/DropBox/AS_Arima")

dw <- read.csv('as_arima_dow_res.csv',stringsAsFactors=F)
tail(dw)

dw$Date <- as.POSIXct(dw$Date,format='%d/%m/%Y') ;dw$Date[20]
dw$confidence.U. <- round(as.numeric(dw$confidence.U.),3)
names(dw)[20] <- "Conf_U"
dw$pl <- ifelse(dw$pred=='U',dw$Close-dw$Open,dw$Open-dw$Close)
dw1 <- dw[,c(1,2,3,4,5,18,19,20,21)];tail(dw1)
ar <- aroon(dw1[c(3,4)], n=20)                 #calc Aroon values
dw1 <- cbind(dw1, ar) 
tail(dw1)
sum(dw1$pl,na.rm=T)
mean(dw1$pl,na.rm=T)

dw1$prevAU <- c( NA, dw1$aroonUp[ - length(dw1$aroonUp) ] )

dw1$pred
dd <- dw1[dw1$prevAU > 85 & dw1$pred == 'U',]
tail(dd,n=20)
dd$pl
sum(dd$pl,na.rm=T)
mean(dd$pl,na.rm=T)


f2 <- mg[mg$dwconf>0.7 | mg$dxconf>0.7 | mg$nkconf>0.7 | mg$dwconf<0.4 | mg$dxconf<0.4 | mg$nkconf<0.4,] 
tai
