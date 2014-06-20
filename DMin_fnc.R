# functions for DMin
# -------------------------------------------------------------
# 1. Add Data to csv

add_line<- function(indata, line_num, Mkt){
  indata$Date <- as.POSIXct(indata$Date,format='%m/%d/%Y')
  indata$Date <- as.character.Date(indata$Date,format='%d/%m/%Y')
  Mkt_new <- rbind(Mkt,indata[line_num,2:6])
  return(Mkt_new)
}

# 2. Create TAP files

#Add TA, no candlesticks to Data
addTAInd_prev <- function(Mkt, nm){
  Mkt[2:5] <- round(Mkt[2:5])
  #atr
  atr <- ATR(Mkt[,c("High","Low","Close")], n=14)
  Mkt$atr <- round(atr[,"atr"])
  #SMA
  sma <- round(SMA(Mkt["Close"], 10))
  Mkt <- cbind(Mkt, sma)
  Mkt$Diff <- ifelse(!is.na(Mkt$sma), Mkt$Close - Mkt$sma, NA)
  #aroon
  ar <- aroon(Mkt$Close, n=20)
  Mkt <- cbind(Mkt, ar)
  #roc
  Mkt$mom <- round(momentum(Mkt$Close,n=12)) 
  #Add prev
  Mkt$prev_smadiff <- c( NA, Mkt$Diff[ - length(Mkt$Diff) ] )
  Mkt$prev_aroon_up <- c( NA, Mkt$aroonUp[ - length(Mkt$aroonUp) ] )
  Mkt$prev_aroon_dn <- c( NA, Mkt$aroonDn[ - length(Mkt$aroonDn) ] )
  Mkt$prev_aroon_os <- c( NA, Mkt$oscillator[ - length(Mkt$oscillator) ] )
  Mkt$prev_mom <- c( NA, Mkt$mom[ - length(Mkt$mom) ] )
  Mkt$pl <- Mkt$Close - Mkt$Open
  #write csvfile
  #write.csv(Mkt,paste('../Data/', nm, '_tap.csv',sep=""),row.names=FALSE)
  write.csv(Mkt,paste(nm, '_tap.csv',sep=""),row.names=FALSE)
}  
# ------------------------------------------
# 3. Calc DM values - today

# three comparison functions
au_df <- function(Mkt, au, df){
  sum ( Mkt[ (Mkt$prev_aroon_up == au) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ] ,na.rm=T)
}

au_df2 <- function(Mkt, au, df){
  #browser()
  r <- Mkt[ (Mkt$prev_aroon_up == au) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ]
  r <- r[!is.na(r)]
  sm <- sum(r,na.rm=T)
  gt <- sum(r>0,na.rm=T)
  lt <- sum(r<0,na.rm=T)
  res <- c(sm,gt,lt)
  return(res)
}

ad_df <- function(Mkt, ad, df){
  sum ( Mkt[ (Mkt$prev_aroon_dn == ad) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ] ,na.rm=T)
}

ad_df2 <- function(Mkt, ad, df){
  r <- Mkt[ (Mkt$prev_aroon_dn == ad) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ]
  r <- r[!is.na(r)]
  sm <- sum(r,na.rm=T)
  gt <- sum(r>0,na.rm=T)
  lt <- sum(r<0,na.rm=T)
  res <- c(sm,gt,lt)
  return(res)
}

os_df <- function(Mkt, os, df){
  sum ( Mkt[ (Mkt$prev_aroon_os == os) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ] ,na.rm=T)
}

os_df2 <- function(Mkt, os, df){
  r <- Mkt[ (Mkt$prev_aroon_os == os) & 
               (Mkt$prev_smadiff > (df - 10) & Mkt$prev_smadiff < (df + 10)), 
             c(18) ]
  r <- r[!is.na(r)]
  sm <- sum(r,na.rm=T)
  gt <- sum(r>0,na.rm=T)
  lt <- sum(r<0,na.rm=T)
  res <- c(sm,gt,lt)
  return(res)
}

# # applies comp functions to one row, uisng current rows
# predicts tomorrow's dir
r_curr_ta <- function(Mkt, nr){
  au <- Mkt$aroonUp[nr] 
  ad <- Mkt$aroonDn[nr] 
  os <- Mkt$oscillator[nr] 
  df <- Mkt$Diff[nr] 
  c <- au_df(Mkt,au,df)
  d <- ad_df(Mkt,ad,df)
  e <- os_df(Mkt,os,df)
  e2 <- c+d+e
  return(c(c,d,e,e2))
}

# # applies comp functions to one row, uisng current rows
# predicts tomorrow's dir
r_curr_ta_2 <- function(Mkt, au, ad, os, df){
  #au <- Mkt$aroonUp[nr] 
  #ad <- Mkt$aroonDn[nr] 
  #os <- Mkt$oscillator[nr] 
  #df <- Mkt$Diff[nr] 
  c <- au_df(Mkt,au,df)
  d <- ad_df(Mkt,ad,df)
  e <- os_df(Mkt,os,df)
  e2 <- c+d+e
  return(c(c,d,e,e2))
}

# applies comp functions to one row, uisng prev values
# for analysis
r_prev_ta <- function(Mkt, nr){
  #browser()
  au <- Mkt$prev_aroon_up[nr] 
  ad <- Mkt$prev_aroon_dn[nr] 
  os <- Mkt$prev_aroon_os[nr] 
  df <- Mkt$prev_smadiff[nr] 
  f <- Mkt$pl[nr]
  Mkt <- Mkt[-nr,]
  c <- au_df(Mkt,au,df)
  d <- ad_df(Mkt,ad,df)
  e <- os_df(Mkt,os,df)
  e2 <- c+d+e
  
  
  return(c(c,d,e,e2,f))
}

r_prev_ta_2 <- function(Mkt, au, ad, os, df, nr){
  #browser()
  #au <- Mkt$prev_aroon_up[nr] 
  #ad <- Mkt$prev_aroon_dn[nr] 
  #os <- Mkt$prev_aroon_os[nr] 
  #df <- Mkt$prev_smadiff[nr]  
  c <- au_df(Mkt,au,df)
  d <- ad_df(Mkt,ad,df)
  e <- os_df(Mkt,os,df)
  e2 <- c+d+e
  f <- Mkt$pl[nr]
  
  return(c(c,d,e,e2,f))
}

calc_dm_today <- function(Mkt){
  # get last 1000 rows
  nr <- nrow(Mkt)
  #st <- ln-1000
  #Mkt <- Mkt[1500:ln]
  
  #browser()
  au <- Mkt$aroonUp[nr] 
  ad <- Mkt$aroonDn[nr] 
  os <- Mkt$oscillator[nr] 
  df <- Mkt$Diff[nr]  
  
  #Mkt <- Mkt[-nr,]
  
  c <- au_df(Mkt,au,df);c
  d <- ad_df(Mkt,ad,df);d
  e <- os_df(Mkt,os,df);e
  res <- c+d+e
  
  return(res)
}

# ------------------------------------------------
# 4. results and analysis

# test creates a data set from past, removes future obs
test <- function(Mkt,st){
  res <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  res_mkt <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  #browser()
  colnames(res_mkt) <- colnames(Mkt[c(1,2,3,4,5)])
  ln <- nrow(Mkt)
  
  for(i in st:ln){
    mkt1 <- Mkt[1:i,]
    lt_row <- nrow(mkt1)
    r <- r_prev_ta(Mkt,lt_row)
    res <- rbind(res,r)
    res_mkt <- rbind(res_mkt,Mkt[i,c(1,2,3,4,5)])
    #browser()
  }
  colnames(res) <- c('a1','a2','a3','a4','pl')
  res <- cbind(res_mkt,res)
  res <- res[-1,]
  return(res)
}

# testa - use all obs incl future
testa <- function(Mkt,st){
  res <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  res_mkt <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  #browser()
  colnames(res_mkt) <- colnames(Mkt[c(1,2,3,4,5)])
  ln <- nrow(Mkt)
  
  for(i in st:ln){
    #mkt1 <- Mkt[1:i,]
    #lt_row <- nrow(mkt1)
    r <- r_prev(Mkt,i)
    res <- rbind(res,r)
    res_mkt <- rbind(res_mkt,Mkt[i,c(1,2,3,4,5)])
    #browser()
  }
  colnames(res) <- c('a1','a2','a3','a4','pl')
  res <- cbind(res_mkt,res)
  res <- res[-1,]
  return(res)
}

# test2 removes future obs
# uses last 300 obs of new data set
test2 <- function(Mkt,st){
  res <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  res_mkt <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  colnames(res_mkt) <- colnames(Mkt[c(1,2,3,4,5)])
  #browser()
  ln <- nrow(Mkt)
  for(i in st:ln){
    bg <- (i - 1000)
    mkt1 <- Mkt[bg:i,]
    lt_row <- nrow(mkt1)
    mkt1 <- mkt1[-lt_row,]
    r <- r_prev_ta(mkt1,lt_row)
    res <- rbind(res,r)
    res_mkt <- rbind(res_mkt,Mkt[i,c(1,2,3,4,5)])
    #browser()
  }
  colnames(res) <- c('a1','a2','a3','a4','pl')
  res <- cbind(res_mkt,res)
  res <- res[-1,]
  return(res)
}

test2a <- function(Mkt,st){
  res <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  res_mkt <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  colnames(res_mkt) <- colnames(Mkt[c(1,2,3,4,5)])
  #browser()
  ln <- nrow(Mkt)
  for(i in st:ln){
    bg <- (i - 1000)
    mkt1 <- Mkt[bg:i,]
    lt_row <- nrow(mkt1)
    au <- mkt1$prev_aroon_up[lt_row] 
    ad <- mkt1$prev_aroon_dn[lt_row] 
    os <- mkt1$prev_aroon_os[lt_row] 
    df <- mkt1$prev_smadiff[lt_row]
    
    mkt1 <- mkt1[-lt_row,]
    lt_row <- nrow(mkt1)
    r <- r_prev_ta_2(mkt1,au,ad,os,df,lt_row)
    res <- rbind(res,r)
    res_mkt <- rbind(res_mkt,Mkt[i,c(1,2,3,4,5)])
    #browser()
  }
  colnames(res) <- c('a1','a2','a3','a4','pl')
  res <- cbind(res_mkt,res)
  res <- res[-1,]
  return(res)
}

test2b <- function(Mkt,st){
  res <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  res_mkt <- as.data.frame(matrix(seq(5),nrow=1,ncol=5))
  colnames(res_mkt) <- colnames(Mkt[c(1,2,3,4,5)])
  #browser()
  ln <- nrow(Mkt)
  for(i in st:ln){
    #bg <- (i - 1000)
    mkt1 <- Mkt[100:i,]
    lt_row <- nrow(mkt1)
    au <- mkt1$prev_aroon_up[lt_row] 
    ad <- mkt1$prev_aroon_dn[lt_row] 
    os <- mkt1$prev_aroon_os[lt_row] 
    df <- mkt1$prev_smadiff[lt_row]
    
    mkt1 <- mkt1[-lt_row,]
    lt_row <- nrow(mkt1)
    r <- r_prev_ta_2(mkt1,au,ad,os,df,lt_row)
    res <- rbind(res,r)
    res_mkt <- rbind(res_mkt,Mkt[i,c(1,2,3,4,5)])
    #browser()
  }
  colnames(res) <- c('a1','a2','a3','a4','pl')
  res <- cbind(res_mkt,res)
  res <- res[-1,]
  return(res)
}

#for(i in 1:length(fil)){

# analysis functions
# 1. pl per tot of min func = a4
Pl_per_a4 <- function(dw_rr){
  
  res <- as.data.frame(matrix(seq(3),nrow=1,ncol=3))
  colnames(res) <- c('Desc','PL','Win %')
  #long trade
  for(i in seq(0,500,100)){
    qq <- with(dw_rr, (dw_rr[a4>i & a4< i+100,]) )
    nm <- paste('from',as.character(i), 'to', as.character(i+100))
    a <- sum(qq$pl2)
    b <- round(sum(qq$pl2>0) / ( sum(qq$pl2<0) + sum(qq$pl2>0) ),2) * 100 
    r <- c(nm, a, b)
    res <- rbind(res,r)
  }
  #browser()
  for(i in seq(0,-500,-100)){
    qq <- with(dw_rr, (dw_rr[a4<i & a4> i-100,]) )
    nm <- paste('from',as.character(i), 'to', as.character(i-100))
    a <- sum(qq$pl2)
    b <- round(sum(qq$pl2>0) / ( sum(qq$pl2<0) + sum(qq$pl2>0) ),2) * 100
    r <- c(nm, a, b)
    res <- rbind(res,r)
  }
  res <- res[-1,]
  return(res)
}

GetData <- function(Mkt){
  #browser()
  ln <- nrow(Mkt)
  st <- ln-1000
  mkt1 <- Mkt[st:ln,]
  ln2 <- nrow(mkt1)
  mkt1 <- mkt1[-ln2,]
  return(mkt1)
}
