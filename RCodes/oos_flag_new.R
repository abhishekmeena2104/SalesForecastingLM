
# import data
# install.packages('MASS')
# install.packages('fmsb')
# install.packages('car')
library('fmsb')
library(car)
library(MASS)
library(plyr)
library(data.table)
library(dplyr)
rm()
regdata<-read.csv('C:\\Users\\abhishek.meena\\Desktop\\IM\\MAX_LDS_R_PROJECT\\SD_MAX_LDS_OOS_FLG_ADS.txt')

# binary and real-numbered exogenous variable name detail
binary_lag_var = c("FLG_SU_EOSS","FLG_WN_EOSS","FLG_SP_EOSS","FLG_RAMADAN")

# regdata$RTL_QTY_norm<-(regdata$RTL_QTY-min(regdata$RTL_QTY))/(max(regdata$RTL_QTY)-min(regdata$RTL_QTY))

# continuous_lag_var = c("FLG_MID_SSN_OFFER","FLG_NATIONAL_DAY","FLG_BTS","FLG_impct_catg1","FLG_impct_catg2","FLG_impct_catg3","FLG_impct_catg4")
# #binary_nolag_var = c("PHS_FLG_1","PHS_FLG_2","PHS_FLG_3","PHS_FLG_4","DISCOUNT_PER")
# binary_nolag_var = c("DISC_PER","A","B","C")

var<-c("KEY","GRP_NM","STND_TRRTRY_NM","DPT_NM","RTL_QTY","OOS_PER_A","OOS_PER_B","OOS_PER_C","FLG_BTS","FLG_EID","FLG_MID_SSN_OFFER","FLG_NATIONAL_DAY","FLG_RAMADAN","FLG_SP_EOSS","FLG_SU_EOSS","FLG_WN_EOSS","FLG_PROMO_CATG1","FLG_PROMO_CATG2","FLG_PROMO_CATG3","FLG_PROMO_CATG4","DISC_PER")
reg_var<-c("RTL_QTY","OOS_PER_A","OOS_PER_B","OOS_PER_C","FLG_BTS","FLG_EID","FLG_MID_SSN_OFFER","FLG_NATIONAL_DAY","FLG_RAMADAN","FLG_SP_EOSS","FLG_SU_EOSS","FLG_WN_EOSS","FLG_PROMO_CATG1","FLG_PROMO_CATG2","FLG_PROMO_CATG3","FLG_PROMO_CATG4","DISC_PER")
regdata1<-regdata[which(regdata$RTL_QTY>0),var]

colnames(regdata1)
reg.territory<-unique(regdata1$STND_TRRTRY_NM)
reg.key<-unique(regdata1$KEY)
dept=unique(regdata1$DPT_NM)

coeff1=data.frame(matrix(ncol=8,nrow=0))
names(coeff1)=c("rn","coeff","r_sq","territory","key","grp","dept","beta_adj")


for(t in 1:length(reg.territory)){
  for(c in 1:length(reg.key)){
    
    print(paste(as.character(reg.territory[t])," - ",as.character(reg.key[c]),sep=""))          
    
    reg_data = regdata1[which(regdata1$STND_TRRTRY_NM==as.character(reg.territory[t]) & 
                                regdata1$KEY==as.character(reg.key[c])),]
    
    # reg_data$RTL_QTY<-(reg_data$RTL_QTY-min(reg_data$RTL_QTY))/(max(reg_data$RTL_QTY)-min(reg_data$RTL_QTY))
    
    step <- stepAIC(lm((RTL_QTY)~.,data=reg_data[,reg_var]), direction="both")
    
    coeff_df<-cbind(data.frame(coeff=step$coefficients),r_sq=summary(step)$r.squared,territory=reg.territory[t],key=reg.key[c],grp=unique(reg_data$GRP_NM),dept=unique(reg_data$DPT_NM))
    
    setDT(coeff_df, keep.rownames = TRUE)[]
    
    coeff_df$beta_adj<-coeff_df$coeff/coeff_df[which(coeff_df$rn=='(Intercept)'),]$coeff
    
    coeff1<-rbind(coeff1,coeff_df)
    
  }
}


coeff_max<-coeff1 %>% group_by(territory,dept) %>% summarise(max = max(r_sq))

coeff1<-merge(x=coeff1,y=coeff_max,by=c('territory','dept'),all.x=TRUE)

coeff2<-coeff1[which(coeff1$r_sq==max),]

coeff2<-coeff2[,-c('max')]

# +OOS_PER_A+OOS_PER_B+OOS_PER_C+FLG_BTS+FLG_EID+FLG_MID_SSN_OFFER+FLG_NATIONAL_DAY+FLG_RAMADAN+FLG_SP_EOSS+FLG_SU_EOSS+FLG_WN_EOSS+FLG_impct_catg1+FLG_impct_catg2+FLG_impct_catg3+FLG_impct_catg4+DISC_PER

write.csv(coeff2,'D:\\Ordering Code\\Data Outfiles\\20180524_R_reg_output.csv',row.names = FALSE)


