library('fmsb')
library(car)
library(MASS)
library(plyr)
library(data.table)
library(dplyr)

INTERCEPT = "(Intercept)"

data<-read.csv('D:\\ordering\\MAX_LDS\\DATA\\INPUT\\regCoeffInput.csv')
regVar<-c(INTERCEPT,"OOS_PER_A","OOS_PER_B","OOS_PER_C","FLG_BTS","FLG_EID","FLG_MID_SSN_OFFER","FLG_NATIONAL_DAY","FLG_RAMADAN","FLG_SP_EOSS","FLG_SU_EOSS","FLG_WN_EOSS","FLG_PROMO_CATG1","FLG_PROMO_CATG2","FLG_PROMO_CATG3","FLG_PROMO_CATG4","DISC_PER")
# empty result dataframe
result=data.frame(matrix(ncol=8,nrow=0))
names(result)=c("rn","coeff","r_sq","territory","key","grp","dept","beta_adj")

territoryList = as.vector(unique(data$STND_TRRTRY_NM))
deptList      = as.vector(unique(data$DPT_NM))

# running loop at the territory - dept level
for (territory in territoryList){
  
  for (dept in deptList){
    
    coeffData        = data[data$STND_TRRTRY_NM== territory & data$DPT_NM==dept,] # filtering data at territory-dept level
    allmisscols      = apply(coeffData,2, function(x)all(is.na(x))) 
    colswithallmiss  = names(allmisscols[allmisscols>0]) # the variables with all NA values
   
    #picking up each regression variable and finding its median after adjusting(divinding the coeff by intercept) it
    for (var in regvar){
      if (!(var %in% colswithallmiss)){  #  skipping variable with all NA values
        coeffValues = na.omit(coeffData[,c(var,INTERCEPT)]) # getting coeff values with their corresponding intecepts
        adjCoeffValues = coeffValues[var]/coeffValues[INTERCEPT]
        adjCoeffValues = adjCoeffValues[,var]
        coeffMedian  = median(adjCoeffValues,na.rm = TRUE)
        tempRow = cbind(rn=var,coeff = NA,r_sq = NA,territory=territory,key = NA,grp =as.character(unique(coeffData$GRP_NM))
                    ,dept =as.character(unique(coeffData$DPT_NM)),beta_adj =coeffMedian)
        result = rbind(result,tempRow)
      }
    }
      
  }
}

write.csv(result,'D:\\ordering\\MAX_LDS\\DATA\\OUTPUT\\20180604_R_reg_output.csv',row.names = FALSE)

