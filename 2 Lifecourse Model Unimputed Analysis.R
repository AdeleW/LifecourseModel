#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~2. Lifecourse Model Unimputed data~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Run 1. Lifecourse Model Data Tidying.R

summary(adultchildfamily2)
dim(adultchildfamily2) #4178 x  35

#1. Analysis on all data avaiable (N=4178 Sample) - so sample size is bigger in adult predictor only model, etc
setwd("~/PhD Papers/Lifecourse/JPSP/analysis/Complete Cases")

#Unstandardised results

#----------------------------------------------------------------
#1. Predicting wellbeing using adult and child variables together
#----------------------------------------------------------------

WBbyAdultPredreg<-matrix(NA, nrow=225, ncol=33)
colnames(WBbyAdultPredreg)<-c("Wellbeing outcome", "Predictor", "Partial correlation coefficient", "Standard error", "t-value", "p value", "Observations", "unadjusted R2", "adjusted R2", "F statistic", "F statistic p value", "Wellbeing outcome", "Predictor", "Partial correlation coefficient", "Standard error", "t-value", "p value", "Observations", "unadjusted R2", "adjusted R2", "F statistic", "F statistic p value", "Wellbeing outcome", "Predictor", "Partial correlation coefficient", "Standard error", "t-value", "p value", "Observations", "unadjusted R2", "adjusted R2", "F statistic", "F statistic p value")
WBbyAdultPredreg[1:16,1]<-"ONS1"
WBbyAdultPredreg[1:16,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[17:32,1]<-"ONS2"
WBbyAdultPredreg[17:32,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[33:48,1]<-"ONS3"
WBbyAdultPredreg[33:48,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[49:64,1]<-"ONS4"
WBbyAdultPredreg[49:64,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[65:80,1]<-"wemwbs"
WBbyAdultPredreg[65:80,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[81:96,1]<-"happ"
WBbyAdultPredreg[81:96,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[97:112,1]<-"life"
WBbyAdultPredreg[97:112,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[113:128,1]<-"meaning"
WBbyAdultPredreg[113:128,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[129:144,1]<-"bpn"
WBbyAdultPredreg[129:144,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[145:160,1]<-"autonomy"
WBbyAdultPredreg[145:160,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[161:176,1]<-"relatedness"
WBbyAdultPredreg[161:176,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[177:192,1]<-"competence"
WBbyAdultPredreg[177:192,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[193:208,1]<-"gratitude"
WBbyAdultPredreg[193:208,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[209:224,1]<-"optimism"
WBbyAdultPredreg[209:224,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")

which( colnames(adultchildfamily2)=="ons1" ) #4
head(adultchildfamily2)

#All predictors in model

for(a in 1:14){
  
  model<-lm(adultchildfamily2[,(3+a)]~adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex, data=adultchildfamily2)
  summ<-summary(model)
  WBbyAdultPredreg[(16*a-15):(16*a),3]<-summ$ coefficients[2:17] #Coefficient estimates for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a),4]<-summ$ coefficients[19:34] #Coefficient estimate standard errors for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a),5]<-summ$ coefficients[36:51] #Coefficient estimate t values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a),6]<-summ$ coefficients[53:68] #Coefficient estimate p values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15),7]<-summ$ df  [2] #number of observations
  WBbyAdultPredreg[(16*a-15),8]<- summ$ r.squared #unadjusted r2
  WBbyAdultPredreg[(16*a-15),9]<- summ$ adj.r.squared #adjusted r2
  WBbyAdultPredreg[(16*a-15),10]<- summ$ fstatistic[1] 
  WBbyAdultPredreg[(16*a-15),11]<-  pf(summ$fstatistic[1],summ$fstatistic[2],summ$fstatistic[3],lower.tail=FALSE) 
  
  
  
}


#Only child and family predictors in model


for(a in 1:14){
  
  model<-lm(adultchildfamily2[,(3+a)]~childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex, data=adultchildfamily2)
  summ<-summary(model)
  WBbyAdultPredreg[(16*a-8):(16*a),14]<-summ$ coefficients[2:10] #Coefficient estimates for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-8):(16*a),15]<-summ$ coefficients[12:20] #Coefficient estimate standard errors for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-8):(16*a),16]<-summ$ coefficients[22:30] #Coefficient estimate t values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-8):(16*a),17]<-summ$ coefficients[32:40] #Coefficient estimate p values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-8),18]<-summ$ df  [2] #number of observations
  WBbyAdultPredreg[(16*a-8),19]<- summ$ r.squared #unadjusted r2
  WBbyAdultPredreg[(16*a-8),20]<- summ$ adj.r.squared #adjusted r2
  WBbyAdultPredreg[(16*a-8),21]<- summ$ fstatistic[1] 
  WBbyAdultPredreg[(16*a-8),22]<-  pf(summ$fstatistic[1],summ$fstatistic[2],summ$fstatistic[3],lower.tail=FALSE) 
  
  
  
}


#Only adult predictors in model


for(a in 1:14){
  
  model<-lm(adultchildfamily2[,(3+a)]~adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+Sex, data=adultchildfamily2)
  summ<-summary(model)
  WBbyAdultPredreg[(16*a-15):(16*a-8),25]<-summ$ coefficients[2:9] #Coefficient estimates for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a-8),26]<-summ$ coefficients[11:18] #Coefficient estimate standard errors for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a-8),27]<-summ$ coefficients[20:27] #Coefficient estimate t values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a-8),28]<-summ$ coefficients[29:36] #Coefficient estimate p values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15),29]<-summ$ df  [2] #number of observations
  WBbyAdultPredreg[(16*a-15),30]<- summ$ r.squared #unadjusted r2
  WBbyAdultPredreg[(16*a-15),31]<- summ$ adj.r.squared #adjusted r2
  WBbyAdultPredreg[(16*a-15),32]<- summ$ fstatistic[1] 
  WBbyAdultPredreg[(16*a-15),33]<-  pf(summ$fstatistic[1],summ$fstatistic[2],summ$fstatistic[3],lower.tail=FALSE) 
  
  
  
}

###*****Remember to change the sex coefficient responses!!!!

write.csv(WBbyAdultPredreg, "UnimputedUnstand.csv")

#Standardised

WBbyAdultPredreg<-matrix(NA, nrow=225, ncol=33)
colnames(WBbyAdultPredreg)<-c("Wellbeing outcome", "Predictor", "Partial correlation coefficient", "Standard error", "t-value", "p value", "Observations", "unadjusted R2", "adjusted R2", "F statistic", "F statistic p value", "Wellbeing outcome", "Predictor", "Partial correlation coefficient", "Standard error", "t-value", "p value", "Observations", "unadjusted R2", "adjusted R2", "F statistic", "F statistic p value", "Wellbeing outcome", "Predictor", "Partial correlation coefficient", "Standard error", "t-value", "p value", "Observations", "unadjusted R2", "adjusted R2", "F statistic", "F statistic p value")
WBbyAdultPredreg[1:16,1]<-"ONS1"
WBbyAdultPredreg[1:16,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[17:32,1]<-"ONS2"
WBbyAdultPredreg[17:32,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[33:48,1]<-"ONS3"
WBbyAdultPredreg[33:48,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[49:64,1]<-"ONS4"
WBbyAdultPredreg[49:64,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[65:80,1]<-"wemwbs"
WBbyAdultPredreg[65:80,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[81:96,1]<-"happ"
WBbyAdultPredreg[81:96,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[97:112,1]<-"life"
WBbyAdultPredreg[97:112,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[113:128,1]<-"meaning"
WBbyAdultPredreg[113:128,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[129:144,1]<-"bpn"
WBbyAdultPredreg[129:144,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[145:160,1]<-"autonomy"
WBbyAdultPredreg[145:160,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[161:176,1]<-"relatedness"
WBbyAdultPredreg[161:176,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[177:192,1]<-"competence"
WBbyAdultPredreg[177:192,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[193:208,1]<-"gratitude"
WBbyAdultPredreg[193:208,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[209:224,1]<-"optimism"
WBbyAdultPredreg[209:224,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")

which( colnames(adultchildfamily2)=="ons1" ) #5
head(adultchildfamily2)

#All predictors in model

for(a in 1:14){
  
  model<-lm(scale(adultchildfamily2[,(4+a)])~scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=adultchildfamily2)
  summ<-summary(model)
  WBbyAdultPredreg[(16*a-15):(16*a),3]<-summ$ coefficients[2:17] #Coefficient estimates for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a),4]<-summ$ coefficients[19:34] #Coefficient estimate standard errors for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a),5]<-summ$ coefficients[36:51] #Coefficient estimate t values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a),6]<-summ$ coefficients[53:68] #Coefficient estimate p values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15),7]<-nobs(model) #number of observations
  WBbyAdultPredreg[(16*a-15),8]<- summ$ r.squared #unadjusted r2
  WBbyAdultPredreg[(16*a-15),9]<- summ$ adj.r.squared #adjusted r2
  WBbyAdultPredreg[(16*a-15),10]<- summ$ fstatistic[1] 
  WBbyAdultPredreg[(16*a-15),11]<-  pf(summ$fstatistic[1],summ$fstatistic[2],summ$fstatistic[3],lower.tail=FALSE) 
  
  
  
}


#Only child and family predictors in model


for(a in 1:14){
  
  model<-lm(scale(adultchildfamily2[,(4+a)])~scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=adultchildfamily2)
  summ<-summary(model)
  WBbyAdultPredreg[(16*a-8):(16*a),14]<-summ$ coefficients[2:10] #Coefficient estimates for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-8):(16*a),15]<-summ$ coefficients[12:20] #Coefficient estimate standard errors for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-8):(16*a),16]<-summ$ coefficients[22:30] #Coefficient estimate t values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-8):(16*a),17]<-summ$ coefficients[32:40] #Coefficient estimate p values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-8),18]<-summ$ df  [2] #number of observations
  WBbyAdultPredreg[(16*a-8),19]<- summ$ r.squared #unadjusted r2
  WBbyAdultPredreg[(16*a-8),20]<- summ$ adj.r.squared #adjusted r2
  WBbyAdultPredreg[(16*a-8),21]<- summ$ fstatistic[1] 
  WBbyAdultPredreg[(16*a-8),22]<-  pf(summ$fstatistic[1],summ$fstatistic[2],summ$fstatistic[3],lower.tail=FALSE) 
  
  
  
}


#Only adult predictors in model


for(a in 1:14){
  
  model<-lm(scale(adultchildfamily2[,(4+a)])~scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=adultchildfamily2)
  summ<-summary(model)
  WBbyAdultPredreg[(16*a-15):(16*a-8),25]<-summ$ coefficients[2:9] #Coefficient estimates for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a-8),26]<-summ$ coefficients[11:18] #Coefficient estimate standard errors for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a-8),27]<-summ$ coefficients[20:27] #Coefficient estimate t values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a-8),28]<-summ$ coefficients[29:36] #Coefficient estimate p values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15),29]<-summ$ df  [2] #number of observations
  WBbyAdultPredreg[(16*a-15),30]<- summ$ r.squared #unadjusted r2
  WBbyAdultPredreg[(16*a-15),31]<- summ$ adj.r.squared #adjusted r2
  WBbyAdultPredreg[(16*a-15),32]<- summ$ fstatistic[1] 
  WBbyAdultPredreg[(16*a-15),33]<-  pf(summ$fstatistic[1],summ$fstatistic[2],summ$fstatistic[3],lower.tail=FALSE) 
  
  
  
}

write.csv(WBbyAdultPredreg, "UnimputedStand.csv")



#-------------------------------------
#Supplementary Complete Cases Analysis
#-------------------------------------

#Run standardised models of distal and combined models with only child level predictors

#Standardised

WBbyAdultPredreg<-matrix(NA, nrow=225, ncol=33)
colnames(WBbyAdultPredreg)<-c("Wellbeing outcome", "Predictor", "Partial correlation coefficient", "Standard error", "t-value", "p value", "Observations", "unadjusted R2", "adjusted R2", "F statistic", "F statistic p value", "Wellbeing outcome", "Predictor", "Partial correlation coefficient", "Standard error", "t-value", "p value", "Observations", "unadjusted R2", "adjusted R2", "F statistic", "F statistic p value", "Wellbeing outcome", "Predictor", "Partial correlation coefficient", "Standard error", "t-value", "p value", "Observations", "unadjusted R2", "adjusted R2", "F statistic", "F statistic p value")
WBbyAdultPredreg[1:16,1]<-"ONS1"
WBbyAdultPredreg[1:16,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[17:32,1]<-"ONS2"
WBbyAdultPredreg[17:32,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[33:48,1]<-"ONS3"
WBbyAdultPredreg[33:48,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[49:64,1]<-"ONS4"
WBbyAdultPredreg[49:64,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[65:80,1]<-"wemwbs"
WBbyAdultPredreg[65:80,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[81:96,1]<-"happ"
WBbyAdultPredreg[81:96,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[97:112,1]<-"life"
WBbyAdultPredreg[97:112,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[113:128,1]<-"meaning"
WBbyAdultPredreg[113:128,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[129:144,1]<-"bpn"
WBbyAdultPredreg[129:144,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[145:160,1]<-"autonomy"
WBbyAdultPredreg[145:160,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[161:176,1]<-"relatedness"
WBbyAdultPredreg[161:176,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[177:192,1]<-"competence"
WBbyAdultPredreg[177:192,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[193:208,1]<-"gratitude"
WBbyAdultPredreg[193:208,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")
WBbyAdultPredreg[209:224,1]<-"optimism"
WBbyAdultPredreg[209:224,2]<-c("Log income", "Educational attainment", "Employed", "Good conduct", "Lives with partner", "Self-perceived health", "Emotional health", "Child IQ", "Child externalising", "Child emotional health","Child physical health","Family economic","Parental general health","Parental emotional health","Family behavioural", "Female")

which( colnames(adultchildfamily2)=="ons1" ) #5
head(adultchildfamily2)

#All predictors in model

for(a in 1:14){
  
  model<-lm(scale(adultchildfamily2[,(4+a)])~scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+Sex, data=adultchildfamily2)
  summ<-summary(model)
  WBbyAdultPredreg[(16*a-15):(16*a-4),3]<-summ$ coefficients[2:13] #Coefficient estimates for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a-4),4]<-summ$ coefficients[15:26] #Coefficient estimate standard errors for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a-4),5]<-summ$ coefficients[28:39] #Coefficient estimate t values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a-4),6]<-summ$ coefficients[41:52] #Coefficient estimate p values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15),7]<-nobs(model) #number of observations
  WBbyAdultPredreg[(16*a-15),8]<- summ$ r.squared #unadjusted r2
  WBbyAdultPredreg[(16*a-15),9]<- summ$ adj.r.squared #adjusted r2
  WBbyAdultPredreg[(16*a-15),10]<- summ$ fstatistic[1] 
  WBbyAdultPredreg[(16*a-15),11]<-  pf(summ$fstatistic[1],summ$fstatistic[2],summ$fstatistic[3],lower.tail=FALSE) 
  
  
  
}


#Only child and family predictors in model


for(a in 1:14){
  
  model<-lm(scale(adultchildfamily2[,(4+a)])~scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+Sex, data=adultchildfamily2)
  summ<-summary(model)
  WBbyAdultPredreg[(16*a-8):(16*a-4),14]<-summ$ coefficients[2:6] #Coefficient estimates for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-8):(16*a-4),15]<-summ$ coefficients[8:12] #Coefficient estimate standard errors for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-8):(16*a-4),16]<-summ$ coefficients[14:18] #Coefficient estimate t values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-8):(16*a-4),17]<-summ$ coefficients[20:24] #Coefficient estimate p values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-8),18]<-summ$ df  [2] #number of observations
  WBbyAdultPredreg[(16*a-8),19]<- summ$ r.squared #unadjusted r2
  WBbyAdultPredreg[(16*a-8),20]<- summ$ adj.r.squared #adjusted r2
  WBbyAdultPredreg[(16*a-8),21]<- summ$ fstatistic[1] 
  WBbyAdultPredreg[(16*a-8),22]<-  pf(summ$fstatistic[1],summ$fstatistic[2],summ$fstatistic[3],lower.tail=FALSE) 
  
  
  
}


#Only adult predictors in model


for(a in 1:14){
  
  model<-lm(scale(adultchildfamily2[,(4+a)])~scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=adultchildfamily2)
  summ<-summary(model)
  WBbyAdultPredreg[(16*a-15):(16*a-8),25]<-summ$ coefficients[2:9] #Coefficient estimates for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a-8),26]<-summ$ coefficients[11:18] #Coefficient estimate standard errors for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a-8),27]<-summ$ coefficients[20:27] #Coefficient estimate t values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15):(16*a-8),28]<-summ$ coefficients[29:36] #Coefficient estimate p values for zadultloginc to Sex
  WBbyAdultPredreg[(16*a-15),29]<-summ$ df  [2] #number of observations
  WBbyAdultPredreg[(16*a-15),30]<- summ$ r.squared #unadjusted r2
  WBbyAdultPredreg[(16*a-15),31]<- summ$ adj.r.squared #adjusted r2
  WBbyAdultPredreg[(16*a-15),32]<- summ$ fstatistic[1] 
  WBbyAdultPredreg[(16*a-15),33]<-  pf(summ$fstatistic[1],summ$fstatistic[2],summ$fstatistic[3],lower.tail=FALSE) 
  
  
  
}

setwd("~/Google Drive/PhD Projects/Life course modelling of wellbeing/Analysis/Complete Cases Analysis")
write.csv(WBbyAdultPredreg, "CompleteCasesStandNoFam.csv")


