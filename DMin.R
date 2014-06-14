# 1. Add data to csv
# 2. Create TAP files
# 3. Calc DM values
# 4. Generate res file (if necessary)

setwd("D:/Allan/DropBox/RWorkingDir/Trading/DMin")
#setwd("F:/Allan/R Stuff/Dax")

#source("D:/Allan/DropBox/RWorkingDir/Trading/Dax/DMin_fnc.R")
source("DMin_fnc.R")
library(TTR)

# -------------------------------------------------------------
# 1. Add Data to csv

# read from clipboard
indata <- read.table("clipboard")
colnames(indata) <- c('Mkt','Date','Open','High','Low','Close')
tail(indata)

#Dax
Dax <- read.csv("Dax_2000.csv")
tail(Dax)
#ln <- nrow(Dax)
#Dax <- Dax[-3689,]
Dax_2 <- add_line(indata,1,Dax)
tail(Dax_2)
write.csv(Dax_2,"Dax_2000.csv",row.names=FALSE)

#CAC
CAC <- read.csv("CAC_2000.csv")
tail(CAC)
CAC_2 <- add_line(indata,2,CAC)
tail(CAC_2)
write.csv(CAC_2,"CAC_2000.csv",row.names=FALSE)

#FTSE
F100 <- read.csv("F100_2000.csv")
tail(F100)
F100_2 <- add_line(indata,3,F100)
tail(F100_2)
write.csv(F100_2,"F100_2000.csv",row.names=FALSE)

#N225
Nik <- read.csv("N225_2000.csv")
tail(Nik)
Nik_2 <- add_line(indata,4,Nik) #4th line fr Nikkei
tail(Nik_2)
write.csv(Nik_2,"N225_2000.csv",row.names=FALSE)

#Oz
Oz <- read.csv("Oz_2000.csv")
tail(Oz)
Oz_2 <- add_line(indata,5,Oz) #4th line fr Nikkei
tail(Oz_2)
write.csv(Oz_2,"Oz_2000.csv",row.names=FALSE)

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

Dow_t <- read.csv("Dow_2000_t.csv")
tail(Dow_t)
addTAInd_prev(Dow_t,"Dow_t")

N225 <- read.csv("N225_2000.csv")
tail(N225)
addTAInd_prev(N225,"N225")

Oz <- read.csv("Oz_2000.csv")
tail(Oz)
addTAInd_prev(Oz,"Oz")

# -------------------------------------------------------------
# 3. Calc DM values - today

#a. ------------- Dax
#Dax_tap <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Dax_tap.csv")
Dax_tap <- read.csv("Dax_tap.csv")
Dax_tap$Date[nrow(Dax_tap)]
ln <- nrow(Dax_tap) ;ln
dx_res <- r_p_ind(Dax_tap, ln) ;dx_res

#b. ------------ CAC
#CAC_tap <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/CAC_tap.csv")
CAC_tap <- read.csv("../Data/CAC_tap.csv")
CAC_tap$Date[nrow(CAC_tap)]
ln <- nrow(CAC_tap) ;ln
cac_res <- r_p_ind(CAC_tap, ln) ;cac_res

# c. ------------ FTSE 
#F100_tap <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/F100_tap.csv")
F100_tap <- read.csv("F100_tap.csv")
F100_tap$Date[nrow(F100_tap)]
ln <- nrow(F100_tap) ;ln
f100_res <- r_p_ind(F100_tap, ln) ;f100_res

# d. ------------ Dow 
Dow_tap <- read.csv("Dow_tap.csv")
calc_dm_today(Dow_tap)


ln <- nrow(Dow_tap) ;ln
st <- ln-1000;st
Dow_tap <- Dow_tap[st:ln,]
dow_res <- r_curr_ta(Dow_tap, ln-1)

Dow_tap <- Dow_tap[-ln,]
Dow_tap$Date[nrow(Dow_tap)]

ln <- nrow(Dow_tap) ;ln

tail(Dow_tap)
Dow_tap$Date[nrow(Dow_tap)]
ln <- nrow(Dow_tap) ;ln

dow_res <- r_curr_ta(Dow_tap, ln-1) ;dow_res
dow_res <- r_prev_ta(Dow_tap, ln) ;dow_res


# e. ------------ N225 
#N225_tap <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/N225_tap.csv")
N225_tap <- read.csv("../Data/N225_tap.csv")
N225_tap$Date[nrow(N225_tap)]
ln <- nrow(N225_tap) ;ln
n225_res <- r_p_ind(N225_tap, ln) ;n225_res

# f. ------------ Oz 
#Oz_tap <- read.csv("D:/Allan/DropBox/RWorkingDir/Trading/Data/Oz_tap.csv")
Oz_tap <- read.csv("../Data/Oz_tap.csv")
Oz_tap$Date[nrow(Oz_tap)]
ln <- nrow(Oz_tap) ;ln
oz_res <- r_p_ind(Oz_tap, ln) ;oz_res

paste(c('Dax',as.character.Date(Dax_tap$Date[nrow(Dax_tap)]),dx_res))
paste(c('CAC',as.character.Date(CAC_tap$Date[nrow(CAC_tap)]),cac_res))
paste(c('FTSE',as.character.Date(F100_tap$Date[nrow(F100_tap)]),f100_res))
paste(c('Dow',as.character.Date(Dow_tap$Date[nrow(Dow_tap)]),dow_res))
paste(c('N225',as.character.Date(N225_tap$Date[nrow(N225_tap)]),n225_res))
paste(c('Oz',as.character.Date(Oz_tap$Date[nrow(Oz_tap)]),oz_res))

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

tail(Dow_tap)

# calc DM pl
dx_rr <- test(Dax_tap,2000)
dx_rr$pl2 <- ifelse(dx_rr$a4>0,dx_rr$pl,-dx_rr$pl)
dx_rr$wl <- ifelse(dx_rr$pl2>0,1,0)
dx_rr$sm10 <- (SMA(dx_rr["wl"], 10)) * 10
tail(dx_rr)
#sum(dx_rr$pl2)
#sum(dx_rr$pl2 > 0) / ( sum(dx_rr$pl2 < 0) + sum(dx_rr$pl2 > 0) )
#tail(dx_rr)

cc_rr <- test(CAC_tap,2000)
cc_rr$pl2 <- ifelse(cc_rr$a4>0,cc_rr$pl,-cc_rr$pl)
cc_rr$wl <- ifelse(cc_rr$pl2>0,1,0)
cc_rr$sm10 <- (SMA(cc_rr["wl"], 10)) * 10

ft_rr <- test(F100_tap,2000)
ft_rr$pl2 <- ifelse(ft_rr$a4>0,ft_rr$pl,-ft_rr$pl)
ft_rr$wl <- ifelse(ft_rr$pl2>0,1,0)
ft_rr$sm10 <- (SMA(ft_rr["wl"], 10)) * 10

source('DMin_fnc.R')
dw_rr <- test2(Dow_tap,2000)
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

ni_rr <- test(N225_tap,2000)
ni_rr$pl2 <- ifelse(ni_rr$a4>0,ni_rr$pl,-ni_rr$pl)
ni_rr$wl <- ifelse(ni_rr$pl2>0,1,0)
ni_rr$sm10 <- (SMA(ni_rr["wl"], 10)) * 10

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

tail(dw_rr)
tail(un1)

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
