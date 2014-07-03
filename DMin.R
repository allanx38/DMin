# 1. Add data to csv
# 2. Create TAP files
# 3. Calc DM values
# 4. Generate res file (if necessary)

#setwd("D:/Allan/DropBox/RWorkingDir/Trading/DMin")
setwd("F:/Allan/R Stuff/DMin")
setwd("D:/Allan/DropBox/Dmin")

#source("D:/Allan/DropBox/RWorkingDir/Trading/Dax/DMin_fnc.R")
source("DMin_fnc.R")
library(TTR)

# -------------------------------------------------------------
# 1. Add Data to csv

# read from clipboard
indata <- read.table("clipboard")
colnames(indata) <- c('Mkt','Date','Open','High','Low','Close')
tail(indata,n=7)

#Dax
Dax <- read.csv("Dax_2000.csv")
tail(Dax)
ln <- nrow(Dax)
Dax <- Dax[-ln,]
Dax_2 <- add_line(indata,1,Dax)
tail(Dax_2)
write.csv(Dax_2,"Dax_2000.csv",row.names=FALSE)
#add dummy entry, tomoorw
Dax_2 <- add_line(indata,7,Dax_2)
tail(Dax_2)
write.csv(Dax_2,"Dax_2000.csv",row.names=FALSE)

#CAC
CAC <- read.csv("CAC_2000.csv")
tail(CAC)
ln <- nrow(CAC)
CAC <- CAC[-ln,]
CAC_2 <- add_line(indata,2,CAC)
tail(CAC_2)
write.csv(CAC_2,"CAC_2000.csv",row.names=FALSE)
#add dummy entry, tomoorw
CAC_2 <- add_line(indata,7,CAC_2)
tail(CAC_2)
write.csv(CAC_2,"CAC_2000.csv",row.names=FALSE)

#FTSE
F100 <- read.csv("F100_2000.csv")
tail(F100)
ln <- nrow(F100)
F100 <- F100[-ln,]
F100_2 <- add_line(indata,3,F100)
tail(F100_2)
write.csv(F100_2,"F100_2000.csv",row.names=FALSE)
#add dummy entry, tomoorw
F100_2 <- add_line(indata,7,F100_2)
tail(F100_2)
write.csv(F100_2,"F100_2000.csv",row.names=FALSE)

#N225
Nik <- read.csv("N225_2000.csv")
tail(Nik)
ln <- nrow(Nik)
Nik <- Nik[-ln,]
Nik_2 <- add_line(indata,4,Nik) #4th line fr Nikkei
tail(Nik_2)
write.csv(Nik_2,"N225_2000.csv",row.names=FALSE)
#add dummy entry, tomoorw
Nik_2 <- add_line(indata,7,Nik_2)
tail(Nik_2)
write.csv(Nik_2,"N225_2000.csv",row.names=FALSE)

#Oz
Oz <- read.csv("Oz_2000.csv")
tail(Oz)
ln <- nrow(Oz)
Oz <- Oz[-ln,]
Oz_2 <- add_line(indata,5,Oz) #4th line fr Nikkei
tail(Oz_2)
write.csv(Oz_2,"Oz_2000.csv",row.names=FALSE)
#add dummy entry, tomoorw
Oz_2 <- add_line(indata,7,Oz_2)
tail(Oz_2)
write.csv(Oz_2,"Oz_2000.csv",row.names=FALSE)

#Dow
Dow <- read.csv("Dow_2000.csv")
tail(Dow)
ln <- nrow(Dow)
Dow <- Dow[-ln,]
Dow_2 <- add_line(indata,6,Dow) #4th line fr Nikkei
tail(Dow_2)
write.csv(Dow_2,"Dow_2000.csv",row.names=FALSE)
#add dummy entry, tomoorw
Dow_2 <- add_line(indata,7,Dow_2)
tail(Dow_2)
write.csv(Dow_2,"Dow_2000.csv",row.names=FALSE)

# -------------------------------------------------------------
# 2. Create TAP files

Dax <- read.csv("Dax_2000.csv")
tail(Dax)
addTAInd_prev(Dax,"Dax")

CAC <- read.csv("CAC_2000.csv")
tail(CAC)
addTAInd_prev(CAC,"CAC")

F100 <- read.csv("F100_2000.csv")
tail(F100)
addTAInd_prev(F100,"F100")

Dow <- read.csv("Dow_2000.csv")
tail(Dow)
addTAInd_prev(Dow,"Dow")

N225 <- read.csv("N225_2000.csv")
tail(N225)
addTAInd_prev(N225,"N225")

Oz <- read.csv("Oz_2000.csv")
tail(Oz)
addTAInd_prev(Oz,"Oz")

# -------------------------------------------------------------
# 3. Calc DM values - today

#a. ------------- Dax

Dax_tap <- read.csv("Dax_tap.csv")
Dax_tap$Date[nrow(Dax_tap)]
dx_res <- calc_dm_today(Dax_tap) ;dx_res

#b. ------------ CAC
CAC_tap <- read.csv("CAC_tap.csv")
CAC_tap$Date[nrow(CAC_tap)]
cac_res <- calc_dm_today(CAC_tap) ;cac_res

# c. ------------ FTSE 
F100_tap <- read.csv("F100_tap.csv")
F100_tap$Date[nrow(F100_tap)]
f100_res <- calc_dm_today(F100_tap) ;f100_res

# d. ------------ Dow 
Dow_tap <- read.csv("Dow_tap.csv")
Dow_tap$Date[nrow(Dow_tap)]
nr <- nrow(Dow_tap)
Dow_tap <- Dow_tap[-nr,]
calc_dm_today(Dow_tap)

# e. ------------ N225 
N225_tap <- read.csv("N225_tap.csv")
N225_tap$Date[nrow(N225_tap)]
n225_res <- calc_dm_today(N225_tap) ;n225_res

# f. ------------ Oz 
Oz_tap <- read.csv("Oz_tap.csv")
Oz_tap$Date[nrow(Oz_tap)]
oz_res <- calc_dm_today(Oz_tap) ;oz_res

paste(c('Dax',as.character.Date(Dax_tap$Date[nrow(Dax_tap)]),dx_res))
paste(c('CAC',as.character.Date(CAC_tap$Date[nrow(CAC_tap)]),cac_res))
paste(c('FTSE',as.character.Date(F100_tap$Date[nrow(F100_tap)]),f100_res))
paste(c('Dow',as.character.Date(Dow_tap$Date[nrow(Dow_tap)]),dow_res))
paste(c('N225',as.character.Date(N225_tap$Date[nrow(N225_tap)]),n225_res))
paste(c('Oz',as.character.Date(Oz_tap$Date[nrow(Oz_tap)]),oz_res))

# -----------------------------------------------
source("DMin_fnc.R")
Dax_tap <- read.csv("Dax_tap.csv")
Dax_tap <- read.csv("CAC_tap.csv")
Dax_tap <- read.csv("F100_tap.csv")
Dax_tap <- read.csv("N225_tap.csv")
Dax_tap <- read.csv("Oz_tap.csv")
Dax_tap <- read.csv("Dow_tap.csv")
#Mkt <- Dax_tap
tail(Dax_tap)
nr <- nrow(Dax_tap);nr
Dax_tap <- Dax_tap[1:(nr-3),]
nr <- nrow(Dax_tap);nr

Dax_tap$Date[nrow(Dax_tap)]
nr <- nrow(Dax_tap);nr
f <- Dax_tap$Close[nr] - Dax_tap$Open[nr]
au <- Dax_tap$prev_aroon_up[nr];au
ad <- Dax_tap$prev_aroon_dn[nr] ;ad
os <- Dax_tap$prev_aroon_os[nr];os
df <- Dax_tap$prev_smadiff[nr];df
Dax_tap <- Dax_tap[-nr,]
Dax_tap$Date[nrow(Dax_tap)]
au_df2(Dax_tap, au, df)
ad_df2(Dax_tap, au, df)
os_df2(Dax_tap, au, df)
f

Mkt <- Dax_tap
r <- Mkt[ (Mkt$prev_aroon_up == au) & 
            (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
          c(18) ]
r
r <- r[!is.na(r)]
r
sm <- sum(r,na.rm=T);sm
gt <- sum(r<0);gt
lt <- sum(r>0);lt
res <- c(sm,gt,lt)


# ------------------------------------------------
# 4. results

# to do
# calc rr files, calc DM pl, merge these ...
Dax_tap <- read.csv("Dax_tap.csv")
CAC_tap <- read.csv("CAC_tap.csv")
F100_tap <- read.csv("F100_tap.csv")
Dow_tap <- read.csv("Dow_tap.csv")
N225_tap <- read.csv("N225_tap.csv")
Oz_tap <- read.csv("Oz_tap.csv")

tail(Oz_tap)

Dax_tap <- read.csv("N225_tap.csv")

Mkt <- Dow_tap
nr <- nrow(Mkt);nr
Mkt$Date[nr]

# start with prev:
r_prev_ta(Mkt, nr)

au <- Mkt$prev_aroon_up[nr] ;au
ad <- Mkt$prev_aroon_dn[nr] ;ad
os <- Mkt$prev_aroon_os[nr] ;os
df <- Mkt$prev_smadiff[nr]  ;df
f <- Mkt$pl[nr]
Mkt <- Mkt[-nr,]
nr <- nrow(Mkt);nr
Mkt$Date[nr]
c <- au_df(Mkt,au,df);c
d <- ad_df(Mkt,ad,df);d
e <- os_df(Mkt,os,df);e
e2 <- c+d+e;e2


# cla using yest curr vals
#nr <- nrow(Mkt);nr
#Mkt <- Mkt[-nr,]
#nr <- nrow(Mkt);nr

au <- Mkt$aroonUp[nr] ;au
ad <- Mkt$aroonDn[nr] ;ad
os <- Mkt$oscillator[nr] ;os
df <- Mkt$Diff[nr]  ;df

c <- au_df(Mkt,au,df);c
d <- ad_df(Mkt,ad,df);d
e <- os_df(Mkt,os,df);e
c+ d + e

source('DMin_fnc.R')
calc_dm_today(Mkt)
nr <- nrow(Dax_tap);nr
r_prev_ta(Dax_tap, nr)

Mkt <- Dax_tap
tail(Mkt)

calc_dm_today(Mkt)


Mkt <- Mkt[-nr,]
calc_dm_today(Mkt)

dw_rr <- test(Dow_tap,3636)

c <- au_df2(Mkt,au,df);c
d <- ad_df2(Mkt,ad,df);d
e <- os_df2(Mkt,os,df);e

source("DMin_fnc.R")
N225_tap <- read.csv("N225_tap.csv")
Dax_tap <- read.csv("Dax_tap.csv")
Dow_tap <- read.csv("Dow_tap.csv")

Mkt <- N225_tap
nr <- nrow(Mkt);nr
Mkt <- Mkt[1:(nr-3),]
nr <- nrow(Mkt);nr
Mkt$Date[nr]
tail(Mkt)
Mkt[nr,c(1,2,5)]; Mkt$Close[nr] - Mkt$Open[nr]

au <- Mkt$aroonUp[nr -1] ;au
ad <- Mkt$aroonDn[nr - 1] ;ad
os <- Mkt$oscillator[nr-1] ;os
df <- Mkt$Diff[nr-1]  ;df
f <- Mkt$pl[nr]

Mkta <- Mkt[-nr,]

c <- au_df(Mkta,au,df);c
d <- ad_df(Mkta,ad,df);d
e <- os_df(Mkta,os,df);e
c+ d + e
f
  

Mkta[ (Mkta$prev_aroon_up == au) & 
       (Mkta$prev_smadiff > (df - 10) & Mkta$prev_smadiff < (df + 10)), 
     c(1,18) ]  

Mkta[ (Mkta$prev_aroon_dn == ad) & 
       (Mkta$prev_smadiff > (df - 10) & Mkta$prev_smadiff < (df + 10)), 
     c(1,18) ]

Mkta[ (Mkta$prev_aroon_os == os) & 
       (Mkta$prev_smadiff > (df - 10) & Mkta$prev_smadiff < (df + 10)), 
     c(1,18) ]

source("DMin_fnc.R")
nr <- nrow(Mkt);nr
res <- test3(Mkt,3470)

# ------------------------------
Dax_tap <- read.csv("Dax_tap.csv")
CAC_tap <- read.csv("CAC_tap.csv")
F100_tap <- read.csv("F100_tap.csv")
Dow_tap <- read.csv("Dow_tap.csv")
N225_tap <- read.csv("N225_tap.csv")
Oz_tap <- read.csv("Oz_tap.csv")

tail(Dow_tap)

# calc DM pl
dx_rr <- test(Dax_tap,2000)
dx_rr$pl2 <- ifelse(dx_rr$a4>0,dx_rr$pl,-dx_rr$pl)
dx_rr$wl <- ifelse(dx_rr$pl2>0,1,0)
dx_rr$sm10 <- (SMA(dx_rr["wl"], 10)) * 10
tail(dx_rr,n=10)

cc_rr <- test(CAC_tap,2000)
cc_rr$pl2 <- ifelse(cc_rr$a4>0,cc_rr$pl,-cc_rr$pl)
cc_rr$wl <- ifelse(cc_rr$pl2>0,1,0)
cc_rr$sm10 <- (SMA(cc_rr["wl"], 10)) * 10

ft_rr <- test(F100_tap,2000)
ft_rr$pl2 <- ifelse(ft_rr$a4>0,ft_rr$pl,-ft_rr$pl)
ft_rr$wl <- ifelse(ft_rr$pl2>0,1,0)
ft_rr$sm10 <- (SMA(ft_rr["wl"], 10)) * 10

source('DMin_fnc.R')
dw_rr <- test(Dow_tap,2000)
dw_rr$pl2 <- ifelse(dw_rr$a4>0,dw_rr$pl,-dw_rr$pl)
dw_rr$wl <- ifelse(dw_rr$pl2>0,1,0)
dw_rr$sm10 <- (SMA(dw_rr["wl"], 10)) * 10
tail(dw_rr)
sum(dw_rr$pl2)
write.csv(dw_rr,"Dowtest_res.csv",row.names=FALSE)

mkt1 <- Dow_tap[1000:2000,]
tail(mkt1,2)
lt_row <- nrow(mkt1)
r <- r_prev_ta(mkt1,lt_row)

N225_tap <- read.csv("N225_tap.csv")
ni_rr <- test(N225_tap,2000)
ni_rr$pl2 <- ifelse(ni_rr$a4>0,ni_rr$pl,-ni_rr$pl)
ni_rr$wl <- ifelse(ni_rr$pl2>0,1,0)
ni_rr$sm10 <- (SMA(ni_rr["wl"], 10)) * 10
tail(ni_rr)
tail(N225_tap)

sum(ni_rr$pl2)
sum(ni_rr$pl2 > 0) / ( sum(ni_rr$pl2 < 0) + sum(ni_rr$pl2 > 0) )

oz_rr <- test(Oz_tap,2000)
oz_rr$pl2 <- ifelse(oz_rr$a4>0,oz_rr$pl,-oz_rr$pl)
oz_rr$wl <- ifelse(oz_rr$pl2>0,1,0)
oz_rr$sm10 <- (SMA(oz_rr["wl"], 10)) * 10

tail(dx_rr)
tail(cc_rr)
tail(ft_rr)
tail(dw_rr)
tail(ni_rr)

source('DMin_fnc.R')
Pl_per_a4(dx_rr)
Pl_per_a4(cc_rr)
Pl_per_a4(ft_rr)
as <- Pl_per_a4(dw_rr)
Pl_per_a4(ni_rr)
Pl_per_a4(oz_rr)

sum(oz_rr$pl2)


# Change date format, extract Date and pl, rename pl
dx_rr$Date <- as.POSIXct(dx_rr$Date,format='%d/%m/%Y') ;dx_rr$Date[20]
cc_rr$Date <- as.POSIXct(cc_rr$Date,format='%d/%m/%Y') ;cc_rr$Date[20]
ft_rr$Date <- as.POSIXct(ft_rr$Date,format='%d/%m/%Y') ;ft_rr$Date[20]
#ft_rr$Date <- as.POSIXct(ft_rr$Date,format='%Y-%m-%d') ;ft_rr$Date[20]
dw_rr$Date <- as.POSIXct(dw_rr$Date,format='%d/%m/%Y') ;dw_rr$Date[20]
ni_rr$Date <- as.POSIXct(ni_rr$Date,format='%d/%m/%Y') ;ni_rr$Date[20]
oz_rr$Date <- as.POSIXct(oz_rr$Date,format='%d/%m/%Y') ;oz_rr$Date[20]

tail(oz_rr)
tail(un3)

# merge pairs
un1 <- merge(dx_rr[,c(1,9,11,13)],
             cc_rr[,c(1,9,11,13)], 
             by='Date',all=T)
colnames(un1) <- c('Date', 'DxA4','DxPL','DxSM','CcA4','CcPL','CcSM')

un2 <- merge(ft_rr[,c(1,9,11,13)],
             dw_rr[,c(1,9,11,13)], 
             by='Date',all=T)
colnames(un2) <- c('Date', 'FtA4','FtPL','FtSM','DwA4','DwPL','DwSM')

un3 <- merge(ni_rr[,c(1,9,11,13)],
             oz_rr[,c(1,9,11,13)], 
             by='Date',all=T)
colnames(un3) <- c('Date','NiA4','NikPL','NikSM','OzA4','OzPL','OzSM')

# final merge
un1_2 <- merge(un1,un2,by='Date',all=T)
un_tot <- merge(un1_2,un3,by='Date',all=T)
tail(un_tot)
un_tot[is.na(un_tot)] <- 0   #remove zeros
un_tot$tot <- un_tot$DxPL+un_tot$CcPL+un_tot$FtPL+un_tot$DwPL+un_tot$NikPL+un_tot$OzPL
tail(un_tot,n=10)
head(un_tot,n=10)

write.csv(un_tot,"Dmin_res.csv",row.names=FALSE)

sum(un_tot$tot)

# win rate
p <- un_tot$tot[un_tot$tot>0]
q <- un_tot$tot[un_tot$tot<0]
length(p)/(length(q)+length(p))
sum(p)
sum(q)

# week days
un_tot2 <- un_tot
un_tot2$Date <- as.POSIXct(un_tot2$Date,format='%Y-%m-%d')
un_tot2$wd <- weekdays(un_tot2$Date)

sum(un_tot2[un_tot2$wd=='Monday',8])
p <- un_tot2[un_tot2$tot>0 & un_tot2$wd=='Monday',8]
q <- un_tot2[un_tot2$tot<0 & un_tot2$wd=='Monday',8]
length(p)/(length(q)+length(p))

sum(un_tot2[un_tot2$wd=='Tuesday',8])
sum(un_tot2[un_tot2$wd=='Wednesday',8])
sum(un_tot2[un_tot2$wd=='Thursday',8])
sum(un_tot2[un_tot2$wd=='Friday',8])


# Dow results
tail(dw_rr)
head(dw_rr)
nrow(dw_rr)
#long trade
qq <- with(dw_rr, (dw_rr[a4>0 & a4<100,]) )
qq <- with(dw_rr, (dw_rr[a4>100 & a4<200,]) )
qq <- with(dw_rr, (dw_rr[a4>200 & a4<300,]) )
qq <- with(dw_rr, (dw_rr[a4>300 & a4<400,]) )
qq <- with(dw_rr, (dw_rr[a4>400 & a4<500,]) )
qq <- with(dw_rr, (dw_rr[a4>500,]) )
qq <- with(dw_rr, (dw_rr[a4>0,]) )

nrow(qq)
sum(qq$pl2) / nrow(qq)
sum(qq$pl2>0) / ( sum(qq$pl2<0) + sum(qq$pl2>0) )

#short trade
qq <- with(dw_rr, (dw_rr[a4<0 & a4< -100,]) )
qq <- with(dw_rr, (dw_rr[a4 < -100 & a4 < -200,]) )
qq <- with(dw_rr, (dw_rr[a4 < -200 & a4 < -300,]) )
qq <- with(dw_rr, (dw_rr[a4>300 & a4<400,]) )
qq <- with(dw_rr, (dw_rr[a4>400 & a4<500,]) )
qq <- with(dw_rr, (dw_rr[a4>500,]) )
qq <- with(dw_rr, (dw_rr[a4<0,]) )

nrow(qq)
sum(qq$pl2) / nrow(qq)
sum(qq$pl2>0) / ( sum(qq$pl2<0) + sum(qq$pl2>0) )

source("DMin_fnc.R")
Mkt <- read.csv("N225_tap.csv")
tail(Mkt)
nr <- nrow(Mkt);nr
Mkt <- Mkt[1:(nr-1),]
nr <- nrow(Mkt);nr
# curr ...
au <- Mkt$aroonUp[nr] 
ad <- Mkt$aroonDn[nr] 
os <- Mkt$oscillator[nr] 
df <- Mkt$Diff[nr] 

nn <- GetData(Mkt)
tail(nn)

rr <- r_curr_ta_2(nn, au, ad, os, df) 
rr

N225_tap <- read.csv("N225_tap.csv")
ni_rr <- test2b(N225_tap,3092)
ni_rr$pl2 <- ifelse(ni_rr$a4>0,ni_rr$pl,-ni_rr$pl)
ni_rr$pl2 <- ifelse(ni_rr$a4>0,ni_rr$Close-ni_rr$Open,ni_rr$Open-ni_rr$Close)
ni_rr$wl <- ifelse(ni_rr$pl2>0,1,0)
ni_rr$sm10 <- (SMA(ni_rr["wl"], 10)) * 10
tail(ni_rr)
tail(N225_tap)

sum(ni_rr$pl2)
sum(ni_rr$pl2 > 0) / ( sum(ni_rr$pl2 < 0) + sum(ni_rr$pl2 > 0) )

Mkt <- read.csv("N225_tap.csv")
Mkt <- Mkt[100:3492,]
tail(Mkt)
sum(Mkt$pl)
nr <- nrow(Mkt);nr
nr <- nr - 3
au <- Mkt$prev_aroon_up[nr];au
ad <- Mkt$prev_aroon_dn[nr] ;ad
os <- Mkt$prev_aroon_os[nr] ;os
df <- Mkt$prev_smadiff[nr] ;df

Mkt <- GetData(Mkt)

r <- Mkt[ (Mkt$prev_aroon_up == au) & 
       (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
     c(18) ]

sum(r)
length(r)
sum(r>0) /  length(r)
sum(r<0)
sum(r>0)

sum ( Mkt[ (Mkt$prev_aroon_up == au) & 
             (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
           c(18) ] ,na.rm=T)

Mkt$pl[nr]
Mkt$Date[nr]

nn <- GetData(Mkt)
tail(nn)

rr <- r_curr_ta_2(nn, au, ad, os, df) 
rr

au_df <- function(Mkt, au, df){
  sum ( Mkt[ (Mkt$prev_aroon_up == au) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ] ,na.rm=T)
}

# -------------------------------------------
Nik <- read.csv("N225_2000.csv")
Nik$UD <- ifelse(Nik$Close>Nik$Open,1,0)
Nik$Date <- as.POSIXct(Nik$Date,format='%d/%m/%Y') ;Nik$Date[20]
Dax <- read.csv("Dax_2000.csv")
Dax$UD <- ifelse(Dax$Close>Dax$Open,1,0)
Dax$Date <- as.POSIXct(Dax$Date,format='%d/%m/%Y') ;Dax$Date[20]
CAC <- read.csv("CAC_2000.csv")
CAC$UD <- ifelse(CAC$Close>CAC$Open,1,0)
CAC$Date <- as.POSIXct(CAC$Date,format='%d/%m/%Y') ;CAC$Date[20]

mg <- merge(Nik,
             CAC, 
             by='Date',all=T)
tail(mg)

rr <- mg[mg$UD.x == 1,]
nrow(rr)
rr1 <- mg[mg$UD.x == 1 & mg$UD.y == 1,]
tail(rr)
nrow(rr1)

Dax_t <- read.csv("Dax_tap.csv")
nr <- nrow(Dax_t)
Dax_t <- Dax_t[100:nr,]
tail(Dax_t)
Dax_t$aroonUp
sum(Dax_t[Dax_t$prev_aroon_up == 95,18])

Dax_t <- read.csv("Dax_tap.csv")
ni_rr <- test(Dax_t,3092)
ni_rr$pl2 <- ifelse(ni_rr$a4>0,ni_rr$pl,-ni_rr$pl)
ni_rr$pl2 <- ifelse(ni_rr$a4>0,ni_rr$Close-ni_rr$Open,ni_rr$Open-ni_rr$Close)
ni_rr$wl <- ifelse(ni_rr$pl2>0,1,0)
ni_rr$sm10 <- (SMA(ni_rr["wl"], 10)) * 10
tail(ni_rr)
sum(ni_rr[ni_rr$sm10 > 9,11],na.rm=T)

ni_rr$pl2 <- ifelse(ni_rr$a3>0,ni_rr$pl,-ni_rr$pl)
sum(ni_rr$pl2)

# ------------------------------------------------
# --------- min OL

setwd("D:/Allan/DropBox/Dmin")
fil <- c("Dax_2000.csv",
         "CAC_2000.csv", 
         "F100_2000.csv",
         "Dow_2000.csv",
         "N225_2000.csv",
         "Oz_2000.csv")
nm <- c("Dax", "CAC", "F100", "Dow", "Nik", "Oz")
df10 <- as.data.frame(matrix(seq(11),nrow=1,ncol=11))

minor_HL <- function(fil, nm){
  df <- t(c('a','b'))
  #browser()
  for(i in 1:length(fil)){
    Mkt <- read.csv(fil[i])
    
    ln <- nrow(Mkt)
    st <- ln - 50
    Mkt <- Mkt[st:ln,]
    
    Mkt$OL <- Mkt$Open - Mkt$Low
    Mkt$OH <- Mkt$High -  Mkt$Open
    Mkt$mn <- ifelse(Mkt$OH>Mkt$OL,Mkt$OL,Mkt$OH)
    
    a <- round(quantile(Mkt$mn, probs=0.90),2)
    b <- nm[i]
    df <- rbind(df,t(c(a,b)))
  }
  colnames(df) <- c('Minor Move', 'Mkt')
  df <- df[-1,]
}

res <- minor_HL(fil, nm)
res
