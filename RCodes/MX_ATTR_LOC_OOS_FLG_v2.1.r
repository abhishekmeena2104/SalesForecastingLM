#install.packages("forecast")
#install.packages("tseries")
library(forecast)
library(tseries)
library(TSA)
library(plyr)
library(sqldf)
library(tcltk)
library(ggplot2)
#library(qdap)
library(sqldf)
#library(tsoutliers)

set.seed(20150626)

slope.perc <- 0.1
sales.perc <- 0.25
ssr.cutoff <- 2

options(scipen=999)
windows.oos.ads = "C:\\Users\\abhishek.meena\\Desktop\\IM\\MAX_LDS_R_PROJECT\\SD_MAX_LDS_OOS_FLG_ADS.txt"

input.oos.DF <- read.csv(windows.oos.ads,header=TRUE,sep=",")

input.oos.DF$TRDNG_WK_END_DT <- as.Date(input.oos.DF$TRDNG_WK_END_DT, format = "%d%b%Y")

input.oos.DF[is.na(input.oos.DF)] <- 0

input.oos.DF$UNIQUE_ID = paste(input.oos.DF$STND_TRRTRY_NM,"|",input.oos.DF$KEY)

#input.oos.DF = ddply(input.oos.DF,.(STND_TRRTRY_NM,KEY),transform,CUMM_RTL_QTY=cumsum(STR_SLS))

#min = by(input.oos.DF, input.oos.DF$UNIQUE_ID, function(x) x$CUMM_RTL_QTY[which.min(x$TRDNG_WK_END_DT)])
#max = by(input.oos.DF, input.oos.DF$UNIQUE_ID, function(x) x$CUMM_RTL_QTY[which.max(x$TRDNG_WK_END_DT)])

input_oos_DF = input.oos.DF

wk <-sqldf("select STND_TRRTRY_NM,KEY,min(TRDNG_WK_END_DT) as min,max(TRDNG_WK_END_DT) as max from input_oos_DF group by STND_TRRTRY_NM,KEY")

slope <- sqldf("select 
               STND_TRRTRY_NM,KEY
               , max(CUMM_RTL_QTY) as CUMM_RTL_QTY_MAX
               , min(CUMM_RTL_QTY) as CUMM_RTL_QTY_MIN
               , max(TRDNG_WK_END_DT) as max
               , min(TRDNG_WK_END_DT) as min
               , (max(TRDNG_WK_END_DT)-min(TRDNG_WK_END_DT))/7+1 as WKS
               , 180*atan((max(CUMM_RTL_QTY)-min(CUMM_RTL_QTY))/((max(TRDNG_WK_END_DT)-min(TRDNG_WK_END_DT))/7+1))/3.17 as slope
               from input_oos_DF
               group by 
               STND_TRRTRY_NM,KEY"
)

cv <- sqldf("select 
            STND_TRRTRY_NM,KEY
            , avg(STR_SLS) as avg
            , stdev(STR_SLS) as std
            , stdev(STR_SLS)/avg(STR_SLS) as cv
            from input_oos_DF 
            group by 
            STND_TRRTRY_NM,KEY"
)

cv$cv = as.numeric(cv$cv)
cv[is.na(cv)] <- 0

slope$SLS_VOL <- ifelse(slope$slope <= 30,"Slow",
                        ifelse((slope$slope <= 60 & slope$slope > 30),"Moderate","Fast"))

cv$SLS_VAR <- ifelse(cv$cv >=3,"High",ifelse((cv$cv>=1 & cv$cv <3),"Moderate","Low"))

vol_var <- sqldf("select 
                 a.*
                 , b.avg
                 , b.std
                 , b.cv
                 , b.SLS_VAR
                 from slope a 
                 left join cv b 
                 on a.STND_TRRTRY_NM = b.STND_TRRTRY_NM and a.KEY = b.KEY"
)

vol_var$SLS_VOL_var = paste(vol_var$SLS_VOL,"-",vol_var$SLS_VAR)

input.oos.DF <- sqldf("select 
                      a.*
                      , b.SLS_VAR
                      , b.SLS_VOL 
                      from input_oos_DF a 
                      left join vol_var b 
                      on a.STND_TRRTRY_NM = b.STND_TRRTRY_NM and a.KEY = b.KEY"
)

calc.slope <- function(ref,series) {
  
  n <- (length(series) - ref)
  slope <- c(rep(0,n))
  for (i in 1:n) {
    slope[i] <- atan((series[i+ref]-series[i])/(7*ref))
  }
  slope <- (slope*180)/3.14159
  return(slope)
}

identify.oos <- function(DF) {
  
  slope.DF <- data.frame()
  
  row.no <- nrow(DF)
  n <- min(13,(row.no-1))
  
  for (j in 1:n) {
    ref.slope <- calc.slope(j,DF$CUMM_RTL_QTY)
    if (j > 1) {
      ref.slope <- c(ref.slope,rep(NA,j-1))
    }
    slope.DF = as.matrix(rbind(slope.DF,ref.slope))
    colnames(slope.DF) <- NULL
    rownames(slope.DF) <- NULL
  }
  
  slope.coutoff <- apply(slope.DF,1,quantile,probs=slope.perc,na.rm=TRUE)
  slope.coutoff <- quantile(slope.coutoff,probs=slope.perc,na.rm=TRUE)
  
  oos.element <- function(df.row,cutoff) {
    #oos.list <- which(df.row <= cutoff*0.25)
    oos.list <- which(df.row <= cutoff)
  }
  
  oos.list <- apply(slope.DF,1,oos.element,cutoff=slope.coutoff)
  
  if (length(oos.list) > 1) {
    
    oos.weeks <- function(x,lag) {
      oos <- c(seq(x+1,by=1,length.out=lag))
    }
    
    oos.all.weeks <-  function(i,op.list) {
      oos.row <- op.list[[i]]
      oos.point <- data.frame(sapply(oos.row,oos.weeks,lag=i))
      oos.point <- unique(unlist(oos.point))
    }
    
    oos.list.final <- lapply(1:length(oos.list),oos.all.weeks,oos.list)
    oos.list.final <- sort(unique(unlist(oos.list.final)), decreasing=FALSE)
    
    #sales.cutoff <- quantile(DF[which(DF$TOT_AVL_SOH > 0 & DF$ST_AVL_SOH_LAG > 0),"RTL_QTY"],probs=sales.perc,na.rm=TRUE)
    #sales.median <- quantile(DF[which(DF$TOT_AVL_SOH > 0 & DF$ST_AVL_SOH_LAG > 0),"RTL_QTY"],probs=0.5,na.rm=TRUE)
    
    sales.cutoff <- quantile(DF[-oos.list.final,c("STR_SLS")],probs=sales.perc,na.rm=TRUE)
    sales.median <- quantile(DF[-oos.list.final,c("STR_SLS")],probs=0.5,na.rm=TRUE)
    
    DF$OOS_FLG <- 0
    DF[oos.list.final,c("OOS_FLG")] <- 1
    #DF$ST_OOS_FLG[which(DF$RTL_QTY >= sales.cutoff)] <- 0
    DF$OOS_FLG[which(DF$STR_SLS >= sales.cutoff)] <- 0
    #DF$OOS_FLG[which((DF$STR_SOH/sales.median) >= ssr.cutoff & (DF$STR_SOH_LAG/sales.median) > 0 & DF$TERR_SOH > 0)] <- 0
    DF$OOS_FLG[which((DF$STR_SOH/sales.median) >= ssr.cutoff & DF$TERR_SOH > 0)] <- 0
    #DF$OOS_FLG[which((DF$WH_AVL_SOH_LAG/sales.median) >= ssr.cutoff)] <- 0
    DF$SALES_MEDIAN <- sales.median
    DF$SALES_CUTOFF <- sales.cutoff
  }
  else {
    oos.list.final <- c(0)
    DF$OOS_FLG <- 0
    DF$SALES_MEDIAN <- NA
    DF$SALES_CUTOFF <- NA
    
  }
  return(list(DF$OOS_FLG,DF$SALES_MEDIAN,DF$SALES_CUTOFF,oos.list.final))
}

engine.oos.flg <- function(oos.DF) {
  
  terr <- unique(oos.DF$STND_TRRTRY_NM)
  key <- unique(oos.DF$KEY)
  
  FINAL_WKLY_OOS_FLG <- data.frame()
  
  for(tt in 1:length(terr)) {#length(terr)
    oos.DF.curr.terr01 = oos.DF[which(oos.DF$STND_TRRTRY_NM==as.character(terr[tt])),]
    key <- unique(oos.DF.curr.terr01$KEY)
    
    for(kk in 1:length(key)) {#length(key)
      #k = 1
      oos.DF.curr = oos.DF[which(oos.DF$STND_TRRTRY_NM==as.character(terr[tt]) &
                                   oos.DF$KEY==as.character(key[kk])),]
      
      print(paste(as.character(terr[tt]),as.character(key[kk]),sep="-"))
      #oos.DF.curr$CUMM_RTL_QTY <- cumsum(oos.DF.curr$RTL_QTY)
      
      #oos.DF.curr$TOT_AVL_SOH <- pmax(oos.DF.curr$WH_AVL_SOH,oos.DF.curr$RTL_QTY,oos.DF.curr$ST_AVL_SOH)
      DF.row <- nrow(oos.DF.curr)
      
      if (DF.row > 1) {
        
        #oos.DF.curr$RDC_SOH_LAG <- c(rep(NA,1),oos.DF.curr$RDC_SOH[1:(DF.row-1)]) 
        oos.DF.curr$STR_SOH_LAG <- c(rep(NA,1),oos.DF.curr$STR_SOH[1:(DF.row-1)])
        oos.DF.curr$TERR_SOH_LAG <- c(rep(NA,1),oos.DF.curr$TERR_SOH[1:(DF.row-1)])
        
        #oos.DF.curr$RDC_SOH_LAG[is.na(oos.DF.curr$RDC_SOH_LAG)] <- oos.DF.curr$RDC_SOH[is.na(oos.DF.curr$RDC_SOH_LAG)]
        oos.DF.curr$STR_SOH_LAG[is.na(oos.DF.curr$STR_SOH_LAG)] <- oos.DF.curr$STR_SOH[is.na(oos.DF.curr$STR_SOH_LAG)]
        oos.DF.curr$TERR_SOH_LAG[is.na(oos.DF.curr$TERR_SOH_LAG)] <- oos.DF.curr$TERR_SOH[is.na(oos.DF.curr$TERR_SOH_LAG)]
        
        oos.flg.op <- identify.oos(oos.DF.curr)
        
        oos.DF.curr$OOS_FLG <- oos.flg.op[[1]]
        oos.DF.curr$SALES_CUTOFF <- oos.flg.op[[2]]
        oos.DF.curr$SALES_MEDIAN <- oos.flg.op[[3]]
        
        FINAL_WKLY_OOS_FLG <- rbind.fill(FINAL_WKLY_OOS_FLG,oos.DF.curr)
      }
      else {
        #FINAL_WKLY_OOS_FLG <- rbind.fill(FINAL_WKLY_OOS_FLG,oos.DF.curr)
        print("missing STND_TRRTRY_NM & KEY")
      }
      
      #if (DF.row >= 52) {
      #FINAL_WKLY_OOS_FLG <- rbind.fill(FINAL_WKLY_OOS_FLG,oos.DF.curr[,-which(names(oos.DF.curr) %in% c("WH_AVL_SOH_LAG","WH_AVL_SOH","STR_SOH"))])
      #}
      #else {
      #  oos.DF.curr$OOS_FLG <- 0
      #  FINAL_WKLY_OOS_FLG <- rbind.fill(FINAL_WKLY_OOS_FLG,oos.DF.curr[,-which(names(oos.DF.curr) %in% c("WH_AVL_SOH_LAG","WH_AVL_SOH","STR_SOH"))])
      #}
    }
  }
  return(FINAL_WKLY_OOS_FLG)
}
#oos.DF = input.oos.DF
#oos.DF = input.oos.DF[which(input.oos.DF$SLS_VAR=="High" & input.oos.DF$SLS_VOL =="Slow"),] 

print(Sys.time())

FINAL_WKLY_OOS_FLG <- engine.oos.flg(input.oos.DF)

print(Sys.time())

oos_wks <- sqldf("select 
                 STND_TRRTRY_NM,KEY
                 , avg(SALES_MEDIAN) as SALES_MEDIAN
                 , avg(SALES_CUTOFF) as SALES_CUTOFF
                 , sum(OOS_FLG) as OOS_WK
                 from FINAL_WKLY_OOS_FLG 
                 group by 
                 STND_TRRTRY_NM,KEY"
)

FINAL_OOS_SUMMARY <- sqldf("select 
                           a.STND_TRRTRY_NM,a.KEY
                           , b.SLS_VOL
                           , b.SLS_VAR
                           , b.WKS as TOTAL_WK
                           , a.OOS_WK
                           , a.SALES_MEDIAN
                           , a.SALES_CUTOFF
                           , b.CUMM_RTL_QTY_MIN
                           , b.CUMM_RTL_QTY_MAX
                           , a.OOS_WK/b.WKS as OOS_PER
                           from oos_wks a 
                           left join vol_var b 
                           on a.STND_TRRTRY_NM = b.STND_TRRTRY_NM and a.KEY = b.KEY"
)

EXPORT_OOS_FLG <- FINAL_WKLY_OOS_FLG[,c("STND_TRRTRY_NM","KEY","TRDNG_WK_END_DT","OOS_FLG")]

write.table(EXPORT_OOS_FLG,"C:\\Users\\abhishek.meena\\Desktop\\IM\\MAX_LDS_R_PROJECT\\SD_MAX_CLN_OOS_FLG_ADS.csv", sep=",", row.names=FALSE)
write.table(FINAL_OOS_SUMMARY,"C:\\Users\\abhishek.meena\\Desktop\\IM\\MAX_LDS_R_PROJECT\\SD_MAX_CLN_OOS_SMRY.csv", sep=",", row.names=FALSE)
write.table(EXPORT_OOS_FLG,"C:\\Users\\abhishek.meena\\Desktop\\IM\\MAX_LDS_R_PROJECT\\SD_MAX_CLN_OOS_FLG_ADS.csv", sep=",", row.names=FALSE)
write.table(FINAL_OOS_SUMMARY,"C:\\Users\\abhishek.meena\\Desktop\\IM\\MAX_LDS_R_PROJECT\\SD_MAX_CLN_OOS_SMRY.csv", sep=",", row.names=FALSE)
