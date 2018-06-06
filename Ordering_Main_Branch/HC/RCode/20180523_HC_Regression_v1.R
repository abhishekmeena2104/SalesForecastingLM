library(fmsb)
library(car)
library(MASS)
library(plyr)
library(data.table)
library(dplyr)
library(tidyr)

RSQ.cutoff <- 0.4
rm()
regdata <- read.csv('D:/Dimple/Projects/Home Centre/Supply Chain/Forecasting/Data/DB_hc_HSF_hist_input_20180519.csv')
names(regdata) <- toupper(names(regdata))

#regdata <- regdata[which(regdata$STND_TRRTRY_NM=="Bahrain" & regdata$KEY=="Bahrain~Furniture~Dining~Serving&Storage~ServingTrolleys~0400952891502"),]
#regdata <- regdata[which(regdata$STND_TRRTRY_NM=="Bahrain"),]

var <- c("STND_TRRTRY_NM","GRP_NM","DPT_NM","KEY","RTL_QTY","OOS_FLG",
         "OOS_PERC_A","OOS_PERC_B","OOS_PERC_C",
         "STR_DISP_CNT","STR_DISC_CNT","SALE_FLAG","PART_SALE_FLAG",
         "CLUSTER_SALE_FLAG","NATIONAL_DAY_FLAG","GREAT_DEAL_FLAG",
         "RAMADAN_FLAG","ANNIVERSARY_FLAG","RSF_FLAG","DSF_FLAG",
         "PROMO_0_10_FLAG","PROMO_10_20_FLAG","PROMO_20_30_FLAG",
         "PROMO_30_100_FLAG")

reg_var <- c("RTL_QTY","OOS_FLG","OOS_PERC_A","OOS_PERC_B",
             "OOS_PERC_C","SALE_FLAG","PART_SALE_FLAG",
             "CLUSTER_SALE_FLAG","NATIONAL_DAY_FLAG","GREAT_DEAL_FLAG",
             "RAMADAN_FLAG","ANNIVERSARY_FLAG","RSF_FLAG","DSF_FLAG",
             "PROMO_0_10_FLAG","PROMO_10_20_FLAG","PROMO_20_30_FLAG",
             "PROMO_30_100_FLAG")
             
#regdata1 <- regdata[regdata['RTL_QTY']>=0][,var]
regdata1 = regdata
colnames(regdata1)
reg.territory <- unique(regdata1$STND_TRRTRY_NM)
reg.key <- unique(regdata1$KEY)
grp = unique(regdata1$GRP_NM)
dept = unique(regdata1$DPT_NM)

coeff1 = data.frame(matrix(ncol=20,nrow=0))
coeff1 = data.frame()
#names(coeff1)=c("rn","BETA_COEF","RSQR","STND_TRRTRY_NM","KEY","GRP_NM","DPT_NM","BETA_ADJ")

for(t in 1:length(reg.territory)){
  for(c in 1:length(reg.key)){
    
    print(paste(as.character(reg.territory[t])," - ",as.character(reg.key[c]),sep=""))          
    
    reg_data = regdata1[which(regdata1$STND_TRRTRY_NM==as.character(reg.territory[t]) & 
                                regdata1$KEY==as.character(reg.key[c])),]
    
    zeroVariance <- function(xreg.matrix) {
      xreg.sd <- apply(xreg.matrix, 2, sd, na.rm=T)
      xreg.col <- which(!xreg.sd > 0.0)
      unlist(xreg.col)
    }
    num_zerovar <- length(zeroVariance(reg_data[,reg_var]))
    xreg_zerovar <- zeroVariance(reg_data[,reg_var])
    
    no.sls.flg <- is.element("RTL_QTY",names(xreg_zerovar))
    
    #if (nrow(reg_data)>=length(reg_var) & no.sls.flg=='FALSE' & num_zerovar<(length(reg_var)-1)) {
    if (nrow(reg_data)>=52 & no.sls.flg=='FALSE' & num_zerovar<(length(reg_var)-1)) {
      
      step <- stepAIC(lm(RTL_QTY~.,data=reg_data[,reg_var]), direction="both")
      #print("I am here - 1")
      coeff_df<-cbind(data.frame(BETA_COEF=step$coefficients),RSQR=summary(step)$r.squared)
      #print("I am here - 2")
      coeff_df <- setDT(coeff_df, keep.rownames = TRUE)[]
      #print("I am here - 3")
      coeff_dft <- data.frame(t(coeff_df[,2]))
      colnames(coeff_dft) <- coeff_df$rn
      #print("I am here - 4")
      coeff_dft <- cbind(STND_TRRTRY_NM=reg.territory[t],KEY=reg.key[c],GRP_NM=unique(reg_data$GRP_NM),DPT_NM=unique(reg_data$DPT_NM),RSQR=summary(step)$r.squared,coeff_dft)
      #print("I am here - 5")
      coeff1 <- rbind.fill(coeff1,coeff_dft)
    }
    # reg_data$RTL_QTY<-(reg_data$RTL_QTY-min(reg_data$RTL_QTY))/(max(reg_data$RTL_QTY)-min(reg_data$RTL_QTY))
  }
}

coeff1$B0_FLG <- ifelse(coeff1$'(Intercept)'>=1,1,0)
coeff1$OOS_FLG_SIGN <- ifelse(coeff1$OOS_FLG>0,1,0)
coeff1$SIGF_VAR <- apply(coeff1, 1, function(x) sum(is.na(x)))

coeff1 <- coeff1[which(coeff1$RSQR >= RSQ.cutoff),]

coeff1.sort <- coeff1[with(coeff1, order(STND_TRRTRY_NM,GRP_NM,DPT_NM,-B0_FLG,OOS_FLG_SIGN,SIGF_VAR,-RSQR)),]

coeff1.sort <- coeff1.sort[!duplicated(c(coeff1.sort$STND_TRRTRY_NM,coeff1.sort$GRP_NM,coeff1.sort$DPT_NM)),]

ind_var <- colnames(coeff1.sort[,6:ncol(coeff1.sort)])

coeff1 <- gather(coeff1.sort,key="IND_VAR",value="BETA_COEF",ind_var)

coeff1$BETA_ADJ <- coeff1$BETA_COEF/coeff1[which(coeff1$IND_VAR=='(Intercept)'),]$BETA_COEF

coeff1 <- coeff1[with(coeff1,order(STND_TRRTRY_NM,GRP_NM,DPT_NM,IND_VAR)),]

coeff2 <- coeff1[complete.cases(coeff1),]
coeff2 <- coeff2[-which(coeff2$IND_VAR %in% c("B0_FLG","OOS_FLG_SIGN","SIGF_VAR")),]

# +OOS_PER_A+OOS_PER_B+OOS_PER_C+FLG_BTS+FLG_EID+FLG_MID_SSN_OFFER+FLG_NATIONAL_DAY+FLG_RAMADAN+FLG_SP_EOSS+FLG_SU_EOSS+FLG_WN_EOSS+FLG_impct_catg1+FLG_impct_catg2+FLG_impct_catg3+FLG_impct_catg4+DISC_PER

write.table(coeff2,"D:/Dimple/Projects/Home Centre/Supply Chain/Forecasting/Data/DB_hc_HSF_reg_20180519.csv", sep=",", row.names=FALSE)
#write.table(coeff2,"D:/INTEGRATED FORECASTING/SKU/LIFESTYLE/SAS/Import/VS_LS_BATH_REG_COEF.csv", sep=",", row.names=FALSE)
