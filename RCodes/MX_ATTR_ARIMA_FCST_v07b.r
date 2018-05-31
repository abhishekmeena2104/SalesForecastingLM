
#install.packages("RSNNS")
#install.packages("DataCombine")
library(forecast)
library(tseries)
library(MASS)
library(TSA)
library(RSNNS)
library(plyr)
library(sqldf)
library(tcltk)
library(ggplot2)
library(DataCombine)
library(parallel)
library(matrixStats)
library(lubridate)
library(dplyr)
library(reshape)
library(reshape2)
rand.seed=0 # seed/ value to fix randomness
ratio=80  # to create random train vs test DF
lead=52 # of forward week forecast
seedval=12345

# ARIMA simulation parameters
# ARIMA model will be simulated over below mentioned 4 parameters to select best combination for forecasting
outlier_flg = c(0) # 1-no outlier treatment 2-univariate outlier treatment 3-multivariate outlier treatment
xreg_lag_flg = c(0,1) # 0-no lag term model. 1-lag term model
#tsf_function = c("ANY","sqrt","aecsin","log","inv_sqrt","inverse","boxcox") # types of transformation
tsf_function = c("ANY")
model_type = c("auto") # which R package to use for forecasting
cv.fold = 13 # number of TS terms for cross validation
cv.iteration = 8 # number of cross validation iteration
mape.roundUP <- 10
lead.discount <- 0.3

# binary and real-numbered exogenous variable name detail
binary_lag_var = c("FLG_SU_EOSS","FLG_WN_EOSS","FLG_SP_EOSS","FLG_RAMADAN")
continuous_lag_var = c("FLG_MID_SSN_OFFER","FLG_NATIONAL_DAY","FLG_BTS","FLG_PROMO_CATG1","FLG_PROMO_CATG2","FLG_PROMO_CATG3","FLG_PROMO_CATG4")
#binary_nolag_var = c("PHS_FLG_1","PHS_FLG_2","PHS_FLG_3","PHS_FLG_4","DISCOUNT_PER")
binary_nolag_var = c("DISC_PER","OOS_PER_A","OOS_PER_B","OOS_PER_C","DSP_STR")

hrchy_column <- 13
join_column <- 9
parm_column <- 7

# history & lead period forecasting ADS windows path
windows.hist_TS = "D:/Ordering Code/Data Outfiles/SD_MAX_CLN_FSCT_HADS_TS1.txt"
windows.lead_TS = "D:/Ordering Code/Data Outfiles/SD_MAX_CLN_FSCT_LADS_TS1.txt"

# read data from the path specified
input.DF.forecast <- read.csv(windows.hist_TS,header=TRUE,sep=",")
input.DF.lead <- read.csv(windows.lead_TS,header=TRUE,sep=",")
# 
# input.DF.forecast <- input.DF.forecast[which(input.DF.forecast$KEY=="Children|Boys 2-8|Jeans|Jeans|TBTNOOSDENIM1|BLUE|LIGHT"),]
# input.DF.lead <- input.DF.lead[which(input.DF.lead$KEY=="Children|Boys 2-8|Jeans|Jeans|TBTNOOSDENIM1|BLUE|LIGHT"),]

#input.DF.forecast <- input.DF.forecast[which(input.DF.forecast$STND_TRRTRY_NM=="Egypt"),]
#input.DF.lead <- input.DF.lead[which(input.DF.lead$STND_TRRTRY_NM=="Egypt"),]



# function does outlier treatment univariate or multivariate
ts.outlier.treatment <- function(outlier.ts,out.flg) {
  
  if (out.flg == 1) {
    no_outlier_ts <- ts(outlier.ts)
    AR01.ts.outlier = as.numeric(tsclean(no_outlier_ts))
  }
  else if (out.flg == 2) {
    no_outlier_ts <- ts(outlier.ts)
    AR01.ts.outlier = as.numeric(tsclean(no_outlier_ts))    
  }
  else {
    AR01.ts.outlier <- outlier.ts
  }
  
  return(AR01.ts.outlier)
}

# function creates additional var with lagged term
# lagged var created differently for event/non event exgo var
exgovar.lag <- function(AR01.exgvar) {
  
  if (length(continuous_lag_var) > 0) { # lagged term for non-event exgo var
    
    lag <- 2  # number of lagged term
    
    df.xreg_other <- data.frame(AR01.exgvar[,continuous_lag_var]) # create DF works even if one var
    names(df.xreg_other) <- continuous_lag_var
    
    other.var.mod <- function(xreg.col) { # create lagged term of an exgo var 
      
      other.var.lag <- function(i,xreg.var) { # add "NA" to a vector and convert to DF
        
        xreg.new.var <- c(rep(NA,i),xreg.var[1:(length(xreg.var)-i)]) # adding "NA" 
        xreg.new.var <- data.frame(xreg.new.var)
      }
      
      temp <- sapply(1:lag,other.var.lag,xreg.col) # repeat for lag times. output is a DF
      return(temp)
    }
    
    df.xreg_other_lag <- apply(df.xreg_other,2,other.var.mod) # repeat over all columns
    df.xreg_other <- cbind(df.xreg_other,df.xreg_other_lag) # combine exgo var and lagged terms
  }
  else { # create empty DF if no non-event exgo var
    df.xreg_other <- data.frame(matrix(nrow=nrow(AR01.exgvar),ncol=0)) 
  }
  
  if (length(binary_lag_var) > 0) { # lagged term for event exgo var
    
    df.xreg_binary <- data.frame(AR01.exgvar[,binary_lag_var]) # create DF works even if one var
    
    binary.var.mod <- function(xreg.col) { # create lagged term for event exgo var
      
      temp1 <- ifelse(xreg.col > 0, 1, 0) # convert into binary vector 
      temp2 <- cumsum(temp1) # take cummulative sum
      temp <- temp2 - cummax(temp2 * !temp1) # reset commumlative sum to start at 1
      
      if (max(temp)==0){
        col.num <- 1
      }else {
        col.num <- max(temp)
      }
      
      binary.var.lag <- function(i,xreg.var) { # create var for every event week
        xreg.new.var <- ifelse(xreg.var==i, 1, 0)
        unlist(xreg.new.var) # convert to a vector
      }
      
      temp <- data.frame(sapply(1:col.num,binary.var.lag,temp)) # repeat process for every event week
      
      #if (col.num == 1) {
      #names(temp) <- c("X1")
      #}
      
      return(temp)
    }
    
    df.xreg_binary_lag <- apply(df.xreg_binary,2,binary.var.mod) # repeat process for evey column
    df.xreg_binary <- do.call(cbind.data.frame,df.xreg_binary_lag) # covert apply output to a DF
  }
  else {
    df.xreg_binary <- data.frame(matrix(nrow=nrow(AR01.exgvar),ncol=0)) # empty DF if no event exgo var
  }
  
  if (length(binary_nolag_var) > 0) {
    
    df.xreg_nolag <- data.frame(AR01.exgvar[,binary_nolag_var])
    names(df.xreg_nolag) <- binary_nolag_var
  }
  else {
    df.xreg_nolag <- data.frame(matrix(nrow=nrow(AR01.exgvar),ncol=0))
  }
  
  AR01.exgvar <- cbind(df.xreg_binary,df.xreg_other,df.xreg_nolag) # combine event/non-event exgo var lagged terms DF
  
  return(AR01.exgvar)
}

transform.ts <- function(AR01.tsf.ts,tsf.fn) {
  
  bc.lambda <- 0
  
  if (tsf.fn == "sqrt") {
    AR01.ts.tsf <- sqrt(AR01.tsf.ts)
  }
  else if (tsf.fn == "arcsin") {
    AR01.ts.tsf <- asin(AR01.tsf.ts)
  }
  else if (tsf.fn == "log") {
    AR01.ts.tsf <- log10(AR01.tsf.ts + 1)
  }
  else if (tsf.fn == "inv_sqrt") {
    AR01.ts.tsf <- 1/sqrt(AR01.tsf.ts+0.0000001)
  }
  else if (tsf.fn == "inverse") {
    AR01.ts.tsf <- 1/(AR01.tsf.ts+0.0000001)
  }
  else if (tsf.fn == "boxcox") {
    bc.lambda <- BoxCox.lambda(AR01.tsf.ts)
    if (bc.lambda == 0) {
      AR01.ts.tsf <- log(AR01.tsf.ts)
    }
    else if (bc.lambda >= 0.005 && bc.lambda <= -0.005) {
      AR01.ts.tsf <- (AR01.tsf.ts^bc.lambda - 1)/bc.lambda
    }
    else {
      AR01.ts.tsf <- AR01.tsf.ts
    }
  }
  else {
    AR01.ts.tsf <- AR01.tsf.ts
  } 
  
  return(list(AR01.ts.tsf,bc.lambda))
}

dtransform.ts <- function(AR01.dtsf.ts,tsf.fn,bc.parm) {
  
  if (tsf.fn == "sqrt") {
    AR01.ts.dtsf <- (AR01.dtsf.ts)^2
  }
  else if (tsf.fn == "arcsin") {
    AR01.ts.dtsf <- sin(AR01.dtsf.ts)
  }
  else if (tsf.fn == "log") {
    AR01.ts.dtsf <- (10^(AR01.dtsf.ts))-1
  }
  else if (tsf.fn == "inv_sqrt") {
    AR01.ts.dtsf <- (1/(AR01.dtsf.ts+0.0000001))^2
  }
  else if (tsf.fn == "inverse") {
    AR01.ts.dtsf <- (1/(AR01.dtsf.ts+0.0000001))
  }
  else if (tsf.fn == "boxcox") {
    if (bc.parm == 0) {
      AR01.ts.dtsf <- exp(AR01.dtsf.ts)
    }
    else if (bc.parm >= 0.005 && bc.parm <= -0.005) {
      AR01.ts.dtsf <- (AR01.dtsf.ts*bc.parm + 1)^(1/bc.parm)
    }
    else {
      AR01.ts.dtsf <- AR01.dtsf.ts
    }
  }
  else {
    AR01.ts.dtsf <- AR01.dtsf.ts
  }
  
  return(AR01.ts.dtsf)
}

AR01.train.test.DF <- function(AR01.all.ts,AR01.all.exgvar,leave.out) {
  
  AR01.ts.X <- AR01.all.ts
  
  AR01.ts.XREG <- as.matrix(AR01.all.exgvar)
  colnames(AR01.ts.XREG) <- NULL
  rownames(AR01.ts.XREG) <- c(1:nrow(AR01.ts.XREG))
  
  AR01.train.X <- AR01.ts.X[1:leave.out]
  AR01.test.X <- AR01.ts.X[(leave.out+1):(leave.out+cv.fold)]
  
  AR01.train.XREG <- AR01.ts.XREG[1:leave.out,]
  AR01.test.XREG <- AR01.ts.XREG[(leave.out+1):(leave.out+cv.fold),]
  
  return(list(AR01.ts.X,AR01.ts.XREG,AR01.train.X,AR01.train.XREG,AR01.test.X,AR01.test.XREG))
}

AR01.forecast <- function(AR01.train.X,AR01.train.XREG,model.type) {
  
  zeroVariance <- function(xreg.matrix) {
    xreg.sd <- apply(xreg.matrix, 2, sd, na.rm=T)
    xreg.col <- which(!xreg.sd > 0.0)
    unlist(xreg.col)
  }
  num_zerovar <- length(zeroVariance(AR01.train.XREG))
  xreg_zerovar <- zeroVariance(AR01.train.XREG)
  
  if (num_zerovar == 0) {
    train.XREG_zerovar <- AR01.train.XREG
  }
  else if (num_zerovar == ncol(AR01.train.XREG)) {
    train.XREG_zerovar <- NULL
  }
  else {
    train.XREG_zerovar <- AR01.train.XREG[,-xreg_zerovar]
  }
  
  arima.fit.tryCatch <- function(error.ind) {
    if (error.ind == 'g') {
      model.fit <- auto.arima(AR01.train.X,max.p = 13,max.P = 13,max.d = 52,max.D = 52,xreg=train.XREG_zerovar)
    }
    else {
      model.fit <- auto.arima(AR01.train.X)
    }
    return(model.fit)
  }
  
  AR01.model.auto <- tryCatch({
    
    ind = 'g'
    fit.tryCatch <- arima.fit.tryCatch(ind)
    
  }, warning = function(w){
    
    message(w)
    
  }, error = function(err) {
    
    print(paste("auto.arima error: ",err))
    ind = 'e'
    fit.tryCatch <- arima.fit.tryCatch(ind)
    return(fit.tryCatch)
    
  }, finally={
    
  })
  
  arma.order <- AR01.model.auto$arma[c(1,6,2)]
  arma.seasonal <- AR01.model.auto$arma[c(3,7,4)]
  
  if (model.type == "arimax") {
    AR.model.final = arimax(AR.train.X,
                            order=arma.order,
                            seasonal=list(order=arma.seasonal,period=52),
                            method="ML",
                            xreg=train.XREG_zerovar)
    
    AR.model.fitted <- as.numeric(fitted.values(AR.model.final))
  }
  else {
    AR01.model.final <- AR01.model.auto
    AR01.model.fitted <- as.numeric(AR01.model.auto$fitted)
  }
  
  return(list(AR01.model.final,AR01.model.fitted,arma.order,arma.seasonal,num_zerovar,xreg_zerovar,train.XREG_zerovar))
}

AR01.lead.forecast <- function(AR01.model,lead.XREG,model.type,num.zerovar,xreg.zerovar) {
  
  if (num.zerovar == 0) {
    lead.XREG_zerovar <- lead.XREG
  }
  else if (num.zerovar == ncol(lead.XREG)) {
    lead.XREG_zerovar <- NULL
  }
  else {
    lead.XREG_zerovar <- lead.XREG[,-xreg.zerovar]
  }
  
  arima.predict.tryCatch <- function(error.ind) {
    if (error.ind == 'g') {
      model.predict <- forecast(AR01.model,h=nrow(lead.XREG),xreg=lead.XREG_zerovar)
    }
    else {
      model.predict <- forecast(AR01.model,h=nrow(lead.XREG))
    }
    return(model.predict)
  }
  
  if (model.type == "auto") {
    AR01.lead.fcst <- tryCatch({
      
      ind = 'g'
      predict.tryCatch <- arima.predict.tryCatch(ind)
      
    }, warning = function(w){
      
      message(w)
      
    }, error = function(err) {
      
      print(paste("auto.arima error: ",err))
      ind = 'e'
      predict.tryCatch <- arima.predict.tryCatch(ind)
      return(predict.tryCatch)
      
    }, finally={
      
    })
    
    AR01.lead.fcst <- data.frame(AR01.lead.fcst)[,1]
  }
  else {
    AR01.lead.fcst <- predict(AR01.model,
                              n.ahead=nrow(lead.XREG),
                              newxreg=lead.XREG_zerovar)
    AR01.lead.fcst <- as.numeric(AR01.lead.fcst$pred)
  }
  
  return(AR01.lead.fcst)
}


ES.forecast<-function(es.data,es.data.lead){
  
es_model<-ets(es.data$ETS_RTL_QTY,model="ZNZ",lower=c(0.0001,0.0001,0.0001,0.8), upper=c(0.3,0.2,0.2,0.98))

}

AR01.cv.validation <- function(parl.ts,parl.exgvar,parl.cv,parl.model,parl.nk,parl.bc) {
  
  cv.validation <- data.frame(matrix(ncol=4, nrow=0))
  colnames(cv.validation) <- c("TEST_MAPE_AGGR","TEST_MAPE_WKLY","TRAIN_MAPE_AGGR","TRAIN_MAPE_WKLY")
  
  op.AR01.io <- AR01.train.test.DF(parl.ts,parl.exgvar,parl.cv)
  
  AR01.ts.X <- op.AR01.io[[1]]
  AR01.ts.XREG <- op.AR01.io[[2]]
  AR01.train.X <- op.AR01.io[[3]]
  AR01.train.XREG <- op.AR01.io[[4]]
  AR01.test.X <- op.AR01.io[[5]]
  AR01.test.XREG <- op.AR01.io[[6]]
  
  op.AR01.fcst <- AR01.forecast(AR01.train.X,AR01.train.XREG,parl.model)
  
  AR01.model <- op.AR01.fcst[[1]]
  AR01.fitted.value <- op.AR01.fcst[[2]]
  AR01.arma.order <- op.AR01.fcst[[3]]
  AR01.arma.seasonal <- op.AR01.fcst[[4]]
  num_zerovar <- op.AR01.fcst[[5]]
  xreg_zerovar <- op.AR01.fcst[[6]]
  
  AR01.test.fcst <- AR01.lead.forecast(AR01.model,AR01.test.XREG,parl.model,num_zerovar,xreg_zerovar)
  
  AR01.test.fcst <- dtransform.ts(AR01.test.fcst,parl.nk,parl.bc)
  AR01.test.ts <- dtransform.ts(AR01.test.X,parl.nk,parl.bc)
  
  AR01.train.fcst <- dtransform.ts(AR01.fitted.value,parl.nk,parl.bc)
  AR01.train.ts <- dtransform.ts(AR01.train.X,parl.nk,parl.bc)
  
  AR01.test.ts[AR01.test.ts<=0] <- NA
  AR01.test.fcst[AR01.test.fcst<=0] <- NA
  
  AR01.test.mape.aggr = round(abs(sum(AR01.test.ts,na.rm=T)-sum(AR01.test.fcst,na.rm=T))*100/(sum(AR01.test.ts,na.rm=T)+0.0000001),digit=2)
  AR01.test.mape.wkly = round(median(abs(AR01.test.ts-AR01.test.fcst)*100/(AR01.test.ts+0.0000001),na.rm=T),digit=2)
  
  na.fcst.element <- which(is.na(AR01.train.fcst))
  AR01.train.fcst <- AR01.train.fcst[-na.fcst.element]
  AR01.train.ts <- AR01.train.ts[-na.fcst.element]  
  
  AR01.train.ts[AR01.train.ts<=0] <- NA
  AR01.train.fcst[AR01.train.fcst<=0] <- NA
  
  AR01.train.mape.aggr = round(abs(sum(AR01.train.ts,na.rm=T)-sum(AR01.train.fcst,na.rm=T))*100/(sum(AR01.train.ts,na.rm=T)+0.0000001),digit=2)
  AR01.train.mape.wkly = round(median(abs(AR01.train.ts-AR01.train.fcst)*100/(AR01.train.ts+0.0000001),na.rm=T),digit=2)
  
  cv.validation <- rbind(cv.validation,data.frame(TEST_MAPE_AGGR=AR01.test.mape.aggr,TRAIN_MAPE_AGGR=AR01.train.mape.aggr,TEST_MAPE_WKLY=AR01.test.mape.wkly,TRAIN_MAPE_WKLY=AR01.train.mape.wkly))
  
  return(list(AR01.arma.order,AR01.arma.seasonal,cv.validation))
}

AR.parm.simulation <- function(AR01.parm.ts,AR01.ts.exgvar) {
  
  parm.simulation <- data.frame(matrix(ncol=7, nrow=0))
  colnames(parm.simulation) <- c("OUTLIER_FLG","XREG_LAG_FLG","TSF_FUNCTION","TEST_MAPE_AGGR","TEST_MAPE_WKLY","TRAIN_MAPE_AGGR","TRAIN_MAPE_WKLY")
  
  arma.simulation.val <- data.frame(matrix(ncol=9, nrow=0))
  colnames(arma.simulation.val) <- c("OUTLIER_FLG","XREG_LAG_FLG","TSF_FUNCTION","p","d","q","P","D","Q")
  
  if (nrow(AR01.ts.exgvar)>78){
    clusVal <- sample(26:(length(AR01.parm.ts)-cv.fold),cv.iteration,replace=F)
  }else if (nrow(AR01.ts.exgvar)>=52 & nrow(AR01.ts.exgvar)<=78)
  {
    clusVal <- sample(26:(length(AR01.parm.ts)-cv.fold),cv.iteration,replace=T)
  }
  
  
  
  for (ni in outlier_flg) {#outlier_flg
    for (nj in xreg_lag_flg) {#xreg_lag_flg
      for (nk in tsf_function) {#tsf_function
        
        ts.parm.outlier <- ts.outlier.treatment(AR01.parm.ts,ni)
        
        if (nj == 1 & nrow(AR01.ts.exgvar)>=78) {
          ts.parm.exgvar <- exgovar.lag(AR01.ts.exgvar)
        }
        else {
          ts.parm.exgvar <- AR01.ts.exgvar
        }
        
        op.ts.parm.tsf <- transform.ts(ts.parm.outlier,nk)
        
        ts.parm.tsf <- op.ts.parm.tsf[[1]]
        boxcox.parm <- op.ts.parm.tsf[[2]]
        
        na.rows.DF <- which(is.na(ts.parm.exgvar),arr.ind=TRUE)[,c(1)]
        na.rows.DF <- unique(na.rows.DF)
        
        if (length(na.rows.DF) > 0) {
          ts.parm.exgvar <- ts.parm.exgvar[-na.rows.DF,]
          ts.parm.outlier <- ts.parm.outlier[-na.rows.DF]
          clusVal.mod <- clusVal - length(na.rows.DF)
        }
        else {
          clusVal.mod <- clusVal
        }
        
        op.AR01.parm.simulation <- parLapply(cl,
                                             clusVal.mod,
                                             AR01.cv.validation, 
                                             parl.ts=ts.parm.outlier, 
                                             parl.exgvar=ts.parm.exgvar, 
                                             parl.model=model_type,
                                             parl.nk=nk,
                                             parl.bc=boxcox.parm)
        
        parm.arma.order <- lapply(op.AR01.parm.simulation,'[[',1)
        parm.arma.order <- as.data.frame(do.call("rbind", parm.arma.order))
        parm.arma.order$cat <- paste(parm.arma.order[,1], parm.arma.order[,2] ,parm.arma.order[,3],sep="-")
        parm.arma.order <- table(parm.arma.order$cat)
        parm.arma.order <- as.integer(unlist((strsplit(names(parm.arma.order[parm.arma.order==max(parm.arma.order)]), "-"))))        
        
        parm.arma.seasonal <- lapply(op.AR01.parm.simulation,'[[',2)
        parm.arma.seasonal <- as.data.frame(do.call("rbind", parm.arma.seasonal))
        parm.arma.seasonal$cat <- paste(parm.arma.seasonal[,1], parm.arma.seasonal[,2] ,parm.arma.seasonal[,3],sep="-")
        parm.arma.seasonal <- table(parm.arma.seasonal$cat)
        parm.arma.seasonal <- as.integer(unlist((strsplit(names(parm.arma.seasonal[parm.arma.seasonal==max(parm.arma.seasonal)]), "-"))))        
        
        arma.simulation.val <- rbind(arma.simulation.val,cbind(data.frame(OUTLIER_FLG=ni,
                                                                          XREG_LAG_FLG=nj,
                                                                          TSF_FUNCTION=nk,
                                                                          p=parm.arma.order[1],
                                                                          d=parm.arma.order[2],
                                                                          q=parm.arma.order[3],
                                                                          P=parm.arma.seasonal[1],
                                                                          D=parm.arma.seasonal[2],
                                                                          Q=parm.arma.seasonal[3])))       
        
        parm.cv.mape <- lapply(op.AR01.parm.simulation,'[[',3)
        cv.final.mape <- colMeans(do.call("rbind",parm.cv.mape),na.rm=T)
        
        parm.simulation <- rbind(parm.simulation,cbind(data.frame(OUTLIER_FLG=ni,
                                                                  XREG_LAG_FLG=nj,
                                                                  TSF_FUNCTION=nk)
                                                       ,t(cv.final.mape)))
      }
    }
  }
  
  parm.simulation$TEST_MAPE_AGGR_R <- mape.roundUP*round(parm.simulation$TEST_MAPE_AGGR/mape.roundUP)
  parm.simulation$TRAIN_MAPE_AGGR_R <- mape.roundUP*round(parm.simulation$TRAIN_MAPE_AGGR/mape.roundUP)
  parm.simulation$TEST_MAPE_WKLY_R <- mape.roundUP*round(parm.simulation$TEST_MAPE_WKLY/mape.roundUP)
  parm.simulation$TRAIN_MAPE_WKLY_R <- mape.roundUP*round(parm.simulation$TRAIN_MAPE_WKLY/mape.roundUP)
  
  parm.simulation$OVERFIT.VAL1 <- abs(round(parm.simulation$TEST_MAPE_WKLY_R,0)-round(parm.simulation$TRAIN_MAPE_WKLY_R,0))
  parm.simulation$OVERFIT.VAL2 <- abs(round(parm.simulation$TEST_MAPE_WKLY_R,0)-round(parm.simulation$TEST_MAPE_AGGR_R,0))
  
  sort.parm.simulation <- parm.simulation[with(parm.simulation, order(TEST_MAPE_WKLY_R,OVERFIT.VAL2,OVERFIT.VAL1)),]
  sort.parm.simulation <- sort.parm.simulation[1,]
  
  arma.simulation.val <- arma.simulation.val[which(arma.simulation.val[,1]==sort.parm.simulation[,1] & arma.simulation.val[,2]==sort.parm.simulation[,2] & arma.simulation.val[,3]==sort.parm.simulation[,3]),]
  parm.arma.order <- arma.simulation.val[,4:6]
  parm.arma.seasonal <- arma.simulation.val[,7:9]
  
  return(list(sort.parm.simulation,parm.arma.order,parm.arma.seasonal))
}

AR01.opt.forecast <- function(AR01.hist.ts,AR01.hist.exgvar,AR01.lead.exgvar) {
  
  hist.ts.len <- length(AR01.hist.ts)
  
  AR01.parm.sim.best <- AR.parm.simulation(AR01.hist.ts,AR01.hist.exgvar)
  
  AR01.best.parm <- AR01.parm.sim.best[[1]]
  opt.outlier.flg <- AR01.best.parm[,1]
  opt.exgvar.lag <- AR01.best.parm[,2]
  opt.tsf.func <- AR01.best.parm[,3]
  
  AR01.comb.exgvar = rbind(AR01.hist.exgvar,AR01.lead.exgvar)
  
  ts.opt.outlier <- ts.outlier.treatment(AR01.hist.ts,opt.outlier.flg)
  
  if (opt.exgvar.lag == 1 & nrow(AR01.hist.exgvar)>=72) {
    ts.opt.exgvar <- exgovar.lag(AR01.comb.exgvar)
  }else {
    ts.opt.exgvar <- AR01.comb.exgvar
  
  }
  
 colnames(ts.opt.exgvar) <- c(paste("V",seq(1,ncol(ts.opt.exgvar),b=1),sep=""))
  
  ts.opt.exgvar_h <- ts.opt.exgvar[1:length(ts.opt.outlier),]
  cor <- data.frame(cor(ts.opt.exgvar_h))
  cor[is.na(cor)] <- 0
  cor[upper.tri(cor)] <- 0
  diag(cor) <- 0
  var <- ts.opt.exgvar_h[,!apply(cor,2,function(x) any(abs(x) > 0.95))]
  vars <- dput(as.character(names(var)))
  
  ts.opt.exgvar_h <- ts.opt.exgvar_h[,vars]
  
  ts.opt.exgvar_l <- ts.opt.exgvar[(length(ts.opt.outlier)+1):nrow(ts.opt.exgvar),vars]
  
  ts.opt.exgvar <- rbind(ts.opt.exgvar_h,ts.opt.exgvar_l)
  
  
  
  
  
  
  op.ts.opt.tsf <- transform.ts(ts.opt.outlier,opt.tsf.func)
  
  ts.opt.tsf <- op.ts.opt.tsf[[1]]
  boxcox.opt <- op.ts.opt.tsf[[2]]
  
  ts.hist.exgvar <- ts.opt.exgvar[1:hist.ts.len,]
  
  na.rows.DF <- which(is.na(ts.hist.exgvar),arr.ind=TRUE)[,c(1)]
  na.rows.DF <- unique(na.rows.DF)
  
  if (length(na.rows.DF) > 0) {
    ts.hist.exgvar <- ts.hist.exgvar[-na.rows.DF,]
    ts.opt.tsf <- ts.opt.tsf[-na.rows.DF]
  }
  
  if(nrow(ts.hist.exgvar)>=78){
    op.AR01.opt.io <- AR01.train.test.DF(ts.opt.tsf,ts.hist.exgvar,52)
  }else if (nrow(ts.hist.exgvar)<78){
    op.AR01.opt.io <- AR01.train.test.DF(ts.opt.tsf,ts.hist.exgvar,26)
  }
  
  AR01.ts.opt.X <- op.AR01.opt.io[[1]]
  AR01.ts.opt.XREG <- op.AR01.opt.io[[2]]
  
  op.AR01.opt.fcst <- AR01.forecast(AR01.ts.opt.X,AR01.ts.opt.XREG,model_type)
  
  AR01.opt.model <- op.AR01.opt.fcst[[1]]
  AR01.hist.fcst <- op.AR01.opt.fcst[[2]]
  opt.num_zerovar <- op.AR01.opt.fcst[[5]]
  opt.xreg_zerovar <- op.AR01.opt.fcst[[6]]
  
  ts.lead.exgvar <- ts.opt.exgvar[(hist.ts.len+1):nrow(ts.opt.exgvar),]
  
  AR01.lead.fcst <- AR01.lead.forecast(AR01.opt.model,ts.lead.exgvar,model_type,opt.num_zerovar,opt.xreg_zerovar)
  
  AR01.hist.fcst <- dtransform.ts(AR01.hist.fcst,opt.tsf.func,boxcox.opt)
  AR01.lead.fcst <- dtransform.ts(AR01.lead.fcst,opt.tsf.func,boxcox.opt)
  
  return(list(AR01.best.parm,AR01.hist.fcst,AR01.lead.fcst,AR01.opt.model,names(ts.hist.exgvar),opt.num_zerovar,opt.xreg_zerovar))
}

AR01.forecast.engine <- function(ts.DF,lead.DF,lead,seedval) {
  
  setseed = seedval
  
  ts.DF$OLD_HRCHY = with(ts.DF, KEY)
  lead.DF$OLD_HRCHY = with(lead.DF,KEY)
  
  AR01.territory <- unique(ts.DF$STND_TRRTRY_NM)
  
  AR01.clss_nm <- unique(ts.DF$OLD_HRCHY)
  
  AR01_ATTR_FCST_FINAL <- data.frame()
  AR01_ATTR_FCST_PARM <- data.frame()
  AR01_ATTR_FCST_COEF <- data.frame()
  
  for(t in 1:length(AR01.territory)) {
    
    for(c in 1:length(AR01.clss_nm)) {
      
      print(paste(as.character(AR01.territory[t])," - ",as.character(AR01.clss_nm[c]),sep=""))
      hist_info_ts = ts.DF[which(ts.DF$STND_TRRTRY_NM==as.character(AR01.territory[t]) & 
                                   
                                   ts.DF$OLD_HRCHY==as.character(AR01.clss_nm[c])),]
      
      #hist_info_ts <- cbind(hist_info_ts,hist_info_ts$FLG_EOSS*hist_info_ts$TRDNG_OPTN)
      #names(hist_info_ts)[ncol(hist_info_ts)] <- "OPTN_X_EOSS"
      
      hist_info_ts$TRDNG_WK_STRT_DT <- with(hist_info_ts,as.Date(as.character(TRDNG_WK_STRT_DT),format="%d%b%Y"))
      hist_info_ts$TRDNG_WK_END_DT <- with(hist_info_ts,as.Date(as.character(TRDNG_WK_END_DT),format="%d%b%Y"))
      
   
      
      hist_exgvar <- hist_info_ts[,c(binary_lag_var,continuous_lag_var,binary_nolag_var)]
      
      lead_info_ts = lead.DF[which(lead.DF$STND_TRRTRY_NM==as.character(AR01.territory[t]) & 
                                     
                                     lead.DF$OLD_HRCHY==as.character(AR01.clss_nm[c])),]
      
      empty.exgvar <- data.frame(matrix(ncol=length(hist_exgvar), nrow=0))
      colnames(empty.exgvar) <- colnames(hist_exgvar)
      
      lead.exgvar <- rbind.fill(empty.exgvar,lead_info_ts)
      lead.exgvar[is.na(lead.exgvar)] <- 0
      
      lead.exgvar = lead.exgvar[1:lead,c(binary_lag_var,continuous_lag_var,binary_nolag_var)]
      
      if (nrow(hist_exgvar)>=52) {
        
        TS.start.dt <- min(hist_info_ts$TRDNG_WK_STRT_DT)
        
        hist.ts.format <- ts(hist_info_ts$RTL_QTY, freq=52, start=decimal_date(ymd(TS.start.dt)))
        
        AR01.final.forecast <- AR01.opt.forecast(hist.ts.format,hist_exgvar,lead.exgvar)
        
        AR01.best.parm <- AR01.final.forecast[[1]]
        AR01.hist.fcst <- AR01.final.forecast[[2]]
        AR01.lead.fcst <- AR01.final.forecast[[3]]
        
        hist_info_ts$TIME_FRAME <- c(rep("Hist Period",nrow(hist_info_ts)))
        
        hist_info_ts$AR_ORDR_FCST <- c(rep(NA,(nrow(hist_info_ts)-length(AR01.hist.fcst)))
                                       ,AR01.hist.fcst)
        
        lead_info_ts <- lead_info_ts[1:lead,]
        lead_info_ts$OLD_HRCHY = with(lead_info_ts, KEY)
        
        lead_info_ts <- cbind(lead_info_ts,lead.exgvar)
        lead_info_ts$TIME_FRAME <- c(rep("Lead Period",nrow(lead_info_ts)))
        
        lead_info_ts$AR_ORDR_FCST <- AR01.lead.fcst
        
        lead_info_ts$TRDNG_WK_STRT_DT <- with(lead_info_ts,as.Date(as.character(TRDNG_WK_STRT_DT),format="%d%b%Y"))
        lead_info_ts$TRDNG_WK_END_DT <- with(lead_info_ts,as.Date(as.character(TRDNG_WK_END_DT),format="%d%b%Y"))
        
        AR01_forecast_ts <- rbind.fill(hist_info_ts,lead_info_ts)
        AR01_forecast_ts$ARIMA_FLG <- c(rep(1,nrow(AR01_forecast_ts)))
        
        AR01_forecast_parm <- cbind(hist_info_ts[1,c(1:join_column)],c(AR01.best.parm[1:parm_column]))
        
        AR01.model.coef <- AR01.final.forecast[[4]]$coef
        len.coef <- length(AR01.model.coef)
        temp.val <- length(names(hist_exgvar)) - AR01.final.forecast[[6]]
        
        if (len.coef >= temp.val) {
          if (AR01.final.forecast[[6]] == 0) {
            names.exgvar <- AR01.final.forecast[[5]]
            num.exgvar <- length(names.exgvar)
            
            exgvar.coef <- AR01.model.coef[(len.coef+1-num.exgvar):len.coef]
            names(exgvar.coef) <- names.exgvar
            
            AR01.model.coef <- c(AR01.model.coef[1:(len.coef-num.exgvar)],exgvar.coef)
            AR01.model.coef <- c(hist_info_ts[1,c(1:join_column)],AR01.model.coef)
          }
          else if (AR01.final.forecast[[6]] == length(AR01.final.forecast[[5]])) {
            names.exgvar <- NULL
            num.exgvar <- 0
            exgvar.coef <- AR01.model.coef[1:len.coef]
            
            AR01.model.coef <- c(exgvar.coef)
            AR01.model.coef <- c(hist_info_ts[1,c(1:join_column)],AR01.model.coef)
          }
          else {
            # names.exgvar <- AR01.final.forecast[[5]][-c(AR01.final.forecast[[7]])]
            # num.exgvar <- length(names.exgvar)
            # 
            # exgvar.coef <- AR01.model.coef[(len.coef+1-num.exgvar):len.coef]
            # names(exgvar.coef) <- names.exgvar
            # 
            # AR01.model.coef <- c(AR01.model.coef[1:(len.coef-num.exgvar)],exgvar.coef)
            # AR01.model.coef <- c(hist_info_ts[1,c(1:join_column)],AR01.model.coef)
            AR01.model.coef <- c(hist_info_ts[1,c(1:join_column)])
          }
        }
        else {
          names.exgvar <- NULL
          num.exgvar <- 0
          exgvar.coef <- AR01.model.coef[1:len.coef]
          
          AR01.model.coef <- c(exgvar.coef)
          AR01.model.coef <- c(hist_info_ts[1,c(1:join_column)],AR01.model.coef)
        }
      }
      else {
        
        hist_info_ts$TIME_FRAME <- c(rep("Hist Period",nrow(hist_info_ts)))
        hist_info_ts$AR_ORDR_FCST <- NULL
        
        lead_info_ts <- cbind(lead_info_ts[1:lead,c(1:hrchy_column)],lead.exgvar)
        lead_info_ts$TIME_FRAME <- c(rep("Lead Period",nrow(lead_info_ts)))
        lead_info_ts$AR_ORDR_FCST <- NULL
        
        lead_info_ts$TRDNG_WK_STRT_DT <- with(lead_info_ts,as.Date(as.character(TRDNG_WK_STRT_DT),format="%d%b%Y"))
        lead_info_ts$TRDNG_WK_END_DT <- with(lead_info_ts,as.Date(as.character(TRDNG_WK_END_DT),format="%d%b%Y"))
        
        AR01_forecast_ts <- rbind.fill(hist_info_ts,lead_info_ts)
        AR01_forecast_ts$ARIMA_FLG <- c(rep(0,nrow(AR01_forecast_ts)))
        
        AR01_forecast_parm <- hist_info_ts[1,c(1:join_column)]
        AR01.model.coef <- hist_info_ts[1,c(1:join_column)]
      }
      
      AR01_ATTR_FCST_FINAL <- rbind.fill(AR01_ATTR_FCST_FINAL,AR01_forecast_ts)
      AR01_ATTR_FCST_PARM <- rbind.fill(AR01_ATTR_FCST_PARM,AR01_forecast_parm)
      AR01_ATTR_FCST_COEF <- rbind.fill(AR01_ATTR_FCST_COEF,data.frame(AR01.model.coef))
    }
    
    
  }
  print(Sys.time())
  return(list(AR01_ATTR_FCST_FINAL,AR01_ATTR_FCST_PARM,AR01_ATTR_FCST_COEF))
}

es.validation<-function(hist_data,es.frcst.data){
  
  
  ES01_test_mape_aggr = round(abs(sum(es.frcst.data$RTL_QTY,na.rm=T)-sum(es.frcst.data$ES_FCST,na.rm=T))*100/(sum(es.frcst.data$ETS_RTL_QTY,na.rm=T)+0.0000001),digit=2)
  ES01_test_mape_wkly = round(median(abs(es.frcst.data$RTL_QTY-es.frcst.data$ES_FCST)*100/(es.frcst.data$ETS_RTL_QTY+0.0000001),na.rm=T),digit=2)
  
  forecast_es_mape<-distinct(hist_data,STND_TRRTRY_NM,GRP_NM,
                             DPT_NM,CLSS_NM,SUB_CLSS_NM,
                             PRI_VNDR_PROD_NBR,ITM_CLR,ITM_SHADE,KEY)
  
  
  forecast_es_mape_01<-cbind(forecast_es_mape,ES01_test_mape_aggr,ES01_test_mape_wkly)
  return(forecast_es_mape_01)
}

es.forecast.engine<-function(ts.DF,lead.DF) {
  
  ts.DF$OLD_HRCHY = with(ts.DF, KEY)
  
  lead.DF$OLD_HRCHY = with(lead.DF, KEY)
  
  ES01.territory <- unique(ts.DF$STND_TRRTRY_NM)
  
  ES01.clss_nm <- unique(ts.DF$OLD_HRCHY)
  
  ES01_ATTR_FCST_FINAL <- data.frame()
  
  ES01_ATTR_FCST_PARM <-data.frame()
  
  for(t in 1:length(ES01.territory)) {
    
    for(c in 1:length(ES01.clss_nm)) {
      
      print(paste(as.character(ES01.territory[t])," - ",as.character(ES01.clss_nm[c]),sep=""))
      hist_info_ts = ts.DF[which(ts.DF$STND_TRRTRY_NM==as.character(ES01.territory[t]) & 
                                   
                                   ts.DF$OLD_HRCHY==as.character(ES01.clss_nm[c])),]
      
      lead_info_ts = lead.DF[which(lead.DF$STND_TRRTRY_NM==as.character(ES01.territory[t]) & 
                                     lead.DF$OLD_HRCHY==as.character(ES01.clss_nm[c])),]
      
      hist_info_ts$ETS_RTL_QTY=ifelse(hist_info_ts$ETS_RTL_QTY<=0,.001,hist_info_ts$ETS_RTL_QTY)
      hist_info_ts$TRDNG_WK_END_DT<-as.Date(hist_info_ts$TRDNG_WK_END_DT,format="%d%b%Y")
      lead_info_ts$TRDNG_WK_END_DT<-as.Date(lead_info_ts$TRDNG_WK_END_DT,format="%d%b%Y")
      ES.forecast01=ES.forecast(hist_info_ts,lead_info_ts)
      
      fitted_values=data.frame(as.numeric(ES.forecast01$fitted))
      names(fitted_values)[1]<-'ES_FCST'
      
      forecast_es<-data.frame(cbind(hist_info_ts,fitted_values))
      forecast_es$time_flag<-'Hist'
      
      if(nrow(lead_info_ts)>0){
      fitted_values_lead=data.frame(as.numeric(forecast(ES.forecast01,h=nrow(lead_info_ts))$mean))
      names(fitted_values_lead)[1]<-'ES_FCST'
        forecast_es_future<-data.frame(cbind(lead_info_ts,fitted_values_lead))
        forecast_es_future$time_flag<-'Lead'
      }else{
        forecast_es_future=NULL
      }
      
      ES01_ATTR_FCST_FINAL<-rbind.fill(ES01_ATTR_FCST_FINAL,forecast_es,forecast_es_future)
      
      keepvars<-c("STND_TRRTRY_NM"
                  ,"GRP_NM","DPT_NM","CLSS_NM"
                  ,"SUB_CLSS_NM","PRI_VNDR_PROD_NBR","ITM_CLR","ITM_SHADE"
                  , "TRDNG_WK_END_DT","RTL_QTY","IN_TRNST_QTY","SOH_QTY"
                  ,"STOCK","NET_SALES_AED","GROSS_SALES_AED","ETS_RTL_QTY"
                  ,"ES_FCST","time_flag",'KEY')
      
      
      ES01_ATTR_FCST_FINAL<-ES01_ATTR_FCST_FINAL[keepvars]
      forecast_es_mape_02<-es.validation(hist_info_ts,forecast_es)
      
      par<-data.frame(ES.forecast01$par)
      params<-rownames(par)
      par1<-cbind(params,value=par)
      
      forecast_es_mape_02_1<-cbind(forecast_es_mape_02,method=ES.forecast01$method,par1)
      
      rownames(forecast_es_mape_02_1)<-NULL
      
      ES01_ATTR_FCST_PARM<-rbind.fill(ES01_ATTR_FCST_PARM ,forecast_es_mape_02_1)
      
    }
    
  }
  return(list(ES01_ATTR_FCST_FINAL,ES01_ATTR_FCST_PARM))
  
}
cl <- makeCluster(detectCores())

clusterExport(cl=cl, varlist=c("AR01.train.test.DF",
                               "AR01.forecast",
                               "AR01.lead.forecast",
                               "dtransform.ts",
                               "continuous_lag_var",
                               "binary_lag_var",
                               "binary_nolag_var",
                               "lead",
                               "cv.fold"))

clusterEvalQ(cl, c(library("RSNNS"),
                   library("plyr"),
                   library("tseries"),
                   library("forecast"),
                   library("TSA"),
                   library("MASS")))

print(Sys.time())
op.AR01.Forecast <- AR01.forecast.engine(input.DF.forecast,input.DF.lead,lead,seedval)
op.ES.Forecast <- es.forecast.engine(input.DF.forecast,input.DF.lead)
stopCluster(cl)

AR01_ATTR_FCST_FINAL <- op.AR01.Forecast[[1]]
AR01_ATTR_FCST_PARM <- op.AR01.Forecast[[2]]
AR01_ATTR_FCST_COEF <- op.AR01.Forecast[[3]]
ES01_ATTR_FCST_FINAL <- op.ES.Forecast[[1]]
ES01_ATTR_FCST_PARM <- op.ES.Forecast[[2]]

AR01_ATTR_FCST_FINAL$AR_ORDR_FCST <- ifelse(AR01_ATTR_FCST_FINAL$AR_ORDR_FCST <= 0, 0, AR01_ATTR_FCST_FINAL$AR_ORDR_FCST)
ES01_ATTR_FCST_FINAL$ES_FCST <- ifelse(ES01_ATTR_FCST_FINAL$ES_FCST <= 0, 0, ES01_ATTR_FCST_FINAL$ES_FCST)

# colnames(ES01_ATTR_FCST_PARM)[12]<-'method'
# colnames(ES01_ATTR_FCST_PARM)[10]<-'ES01_test_mape_aggr'
# colnames(ES01_ATTR_FCST_PARM)[11]<-'ES01_test_mape_wkly'


ARES01_ATTR_FCST_FINAL<-sqldf('select a.ES_FCST,b.* from ES01_ATTR_FCST_FINAL a left join  AR01_ATTR_FCST_FINAL  b
                              on a.STND_TRRTRY_NM=b.STND_TRRTRY_NM and a.KEY=b.KEY and a.TRDNG_WK_END_DT=b.TRDNG_WK_END_DT')
ARES01_ATTR_FCST_FINAL01 <- subset(ARES01_ATTR_FCST_FINAL, !duplicated(ARES01_ATTR_FCST_FINAL[,c('STND_TRRTRY_NM','KEY','TRDNG_WK_END_DT')]))

ES01_ATTR_FCST_PARM_01  <- dcast(ES01_ATTR_FCST_PARM, STND_TRRTRY_NM + GRP_NM + DPT_NM + CLSS_NM + SUB_CLSS_NM + PRI_VNDR_PROD_NBR + ITM_CLR + ITM_SHADE + KEY + ES01_test_mape_aggr + ES01_test_mape_wkly + method ~params)

ARES01_ATTR_FCST_PARM <- sqldf('select a.*,b.ES01_test_mape_aggr,b.ES01_test_mape_wkly
                               from AR01_ATTR_FCST_PARM a inner join ES01_ATTR_FCST_PARM_01 b 
                               on a.STND_TRRTRY_NM=b.STND_TRRTRY_NM and a.GRP_NM=b.GRP_NM
                               and a.DPT_NM=b.DPT_NM and a.CLSS_NM=b.CLSS_NM and
                               a.SUB_CLSS_NM=b.SUB_CLSS_NM and a.PRI_VNDR_PROD_NBR=b.PRI_VNDR_PROD_NBR
                               and a.ITM_CLR=b.ITM_CLR and a.ITM_SHADE=b.ITM_SHADE')

ARES01_ATTR_FCST_COEF <- sqldf('select a.*,b.method,b.alpha,b.l
                               from AR01_ATTR_FCST_COEF a inner join ES01_ATTR_FCST_PARM_01 b 
                               on a.STND_TRRTRY_NM=b.STND_TRRTRY_NM and a.GRP_NM=b.GRP_NM
                               and a.DPT_NM=b.DPT_NM and a.CLSS_NM=b.CLSS_NM and
                               a.SUB_CLSS_NM=b.SUB_CLSS_NM and a.PRI_VNDR_PROD_NBR=b.PRI_VNDR_PROD_NBR
                               and a.ITM_CLR=b.ITM_CLR and a.ITM_SHADE=b.ITM_SHADE')

ARES01_ATTR_FCST_FINAL01<-ARES01_ATTR_FCST_FINAL01[which(ARES01_ATTR_FCST_FINAL01$STND_TRRTRY_NM!='NA'),]


write.table(ARES01_ATTR_FCST_FINAL01, "D:/Ordering Code/Data Outfiles/20180525_FCST_ALLTER_MX_CLN_final.csv", sep=",", row.names=FALSE)
write.table(ARES01_ATTR_FCST_COEF, "D:/Ordering Code/Data Outfiles/20180525_FCST_ALLTER_MX_CLN_coeff.csv", sep=",", row.names=FALSE)
write.table(ARES01_ATTR_FCST_PARM, "D:/Ordering Code/Data Outfiles/20180525_FCST_ALLTER_MX_CLN_parm.csv", sep=",", row.names=FALSE)


no_outlier_ts <- ts(input.DF.forecast$RTL_QTY)
AR01.ts.outlier = as.numeric(tsclean(no_outlier_ts))

no_outlier_ts

AR01.ts.outlier
