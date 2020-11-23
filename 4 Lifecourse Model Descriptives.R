#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~Descriptives~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

summary(adultchildfamily2)
dim(adultchildfamily2) #4178 x  35

#Subset with only wellbeing outcome and predictor columns
outpred<-adultchildfamily2[,c("Sex", "ons1", "ons2", "ons3", "ons4", "wemwbs", "happ", "life", "meaning", 
                              "adultloginc", "AQguess2", "employed23", "adultpartner", "adultantis", "selfhealth", "adultemohealth", 
                              "childext", "childemohealth", "childIQ", "childcurrenthealth",
                              "econcomp", "pargenhealth", "paremohealth", "fambehav", "ID")]
dim(outpred) #4178x   30
#Subset data on individuals with complete cases for all predictors and all wellbeing outcomes 
complete<-outpred[complete.cases(outpred),]
dim(complete) #247 x 31
summary(complete)

#Subset with only adult level predictors
adultpred<-adultchildfamily2[,c("Sex", "adultloginc", "AQguess2", "employed23", "adultpartner", "adultantis", "selfhealth", "adultemohealth")]
adultcomplete<-adultpred[complete.cases(adultpred),]
dim(adultcomplete) #2045x  8

#Subset with only child level predictors
childpred<-adultchildfamily2[,c("childext", "childemohealth", "childIQ", "childcurrenthealth")]
childcomplete<-childpred[complete.cases(childpred),]
dim(childcomplete) #1703x  4

#Subset with only family level predictors
familypred<-adultchildfamily2[,c("econcomp", "pargenhealth", "paremohealth", "fambehav")]
familycomplete<-familypred[complete.cases(familypred),]
dim(familycomplete) #559  x4

#Descriptives

#Additional variables needed:
table(maindata$YPC2510) #income band
(maindata$Age3ext) #Age 3 child externalising (sum score)
Age9ext) #Age 9 child externalising (sum score)
Age13ext) #Age 13 child externalising (sum score)
Age3emodiff) #Age 3 child internalising (sum score)
Age9int) #Age 9 child internalising (sum score)
Age13int) #Age 13 child internalising (sum score)
Age3health) #Age 3 #child's health in past year
ks1001num) #Age 8 #child's health in past year
ta1001num) #Age 13 #child's health in past year
b_sc_m) #mother's social class based on occupation 18 weeks gest
pb_sc_p) #partner's social class based on occupation 32 weeks gest
h470) #Family income per week #2 years 9 months - first time asked
summary(maindata$n8130)	#J9a: Average family take-home income per week #8 years
summary(maindata$r9020)	#On average, about how much is the take-home family income each week (include social benefits etc.)? #11 years 2 months
table(maindata$YPC1160) #number of siblings at 23
summary(maindata$f565) #f565 partner currently employed 8 months yes = 1, no = 2
table(maindata$p3080) #p3080 husband/partner currently employed 9 years 2 months yes = 1, no = 2
table(maindata$s3080) #s3080 partner is currently employed 12 years 1 month #yes = 1, no = 2
table(maindata$c645a) #Mother's highest qualification when child is 32 weeks gestation
table(maindata$pb325a) #Father's highest qualification when child is 18 weeks gestation
summary(maindata$b370) #Mother EPDS Age 18 weeks gest (sum score)
hist(maindata$nEPDS) #Mother EPDS Age8 years (sum score) 
summary(maindata$rEPDS) #Mother EPDS Age 11 years (sum score) 
table(maindata$pb261) #father's EPDS 18 weeks gest (sum score)
plEPDS #father's EPDS Age8 years (sum score) 
ppEPDS #father's EPDS Age 11 years (sum score) 
summary(maindata$e310) #mother's Description of health #8 weeks
p1000 #mother's Description of health #9 years
s1000 #mother's Description of health #12 years
pc051 #father's Description of health #8 weeks
pm1000 #father's Description of health #9 years
pq1000 #father's Description of health #12 years
table(maindata$conceivedmarr) #higher score (1) means conceived in marriage (positive)
table(maindata$partogev10) #parents still together = 1, parents not together = 0 #still together at age 10
table(maindata$parcrime) #0 = both parents been in trouble with the law, 1 = 1 parent was in trouble, 2 = no parents in trouble with law #age 12

additional<-maindata[,c("ID", "YPC2510", "incomeMP3", "f8ws112num", "fh6280num", "Age3ext", "Age9ext", "Age13ext", "Age3emodiff", "Age9int", "Age13int", 
                        "Age3health", "ks1001num", "ta1001num", "b_sc_mnum", "pb_sc_pnum", "h470num", "n8130num", "r9020num", "YPC1160", 
                        "f565num", "p3080num", "s3080num", "c645anum", "pb325anum", "b370num", "nEPDS", "rEPDS", "pb261num", "plEPDS",
                        "ppEPDS", "e310num", "p1000num", "s1000num", "pc051num", "pm1000num", "pq1000num", "conceivedmarr", 
                        "partogev10", "parcrime")]
library(psych)

#1. Descriptives of data available for 4222 individuals who returned the 23 questionnaire
dim(additional) # 15243 x   37
dim(adultchildfamily2) #4178x 34
dessample<-merge(additional, adultchildfamily2, by="ID", all.y=TRUE) 
dim(dessample) #4178  x 70
describe1<-describe(dessample)
describe1frame<-as.data.frame(describe1)
describe1frame$meansd<-NA
for (i in 1:nrow(describe1frame)){
  describe1frame$meansd[i]<-paste0(round(describe1frame$mean[i], 2), "(", round(describe1frame$sd[i],2), ")")
}
setwd("~/Google Drive/PhD Projects/Life course modelling of wellbeing/Layard Replication/Complete Cases Model 3times/Final/Descriptives")
write.csv(describe1frame, "4222 descriptives2.csv")
table(dessample$incomeMP3)
# -10   -1    0    1    2    3    4    5    6    7 
#   3  715   516  360  565 1371  714  185   45   19 
table(dessample$AQguess2)
#0    1    2    3    4 
#27   21  310 1121 1458 
table(dessample$employed23)
#0    1 
#332 3542
table(dessample$adultpartner)
#0    1 
#2616 1041
table(dessample$h470num)
#66.66667    149.5    249.5    349.5      600 
#     168      448      872      747      884 
table(dessample$n8130num)
# 66.66667    149.5    249.5    349.5      600 
#       46      210      433      594     1663 
table(dessample$r9020num)
#80 154.5 214.5 264.5 324.5 394.5 454.5 519.5 679.5  1200 
#53   117   133   188   293   300   216   466   634   463 
table(dessample$YPC1160) #number of siblings at 23
dessample$YPC1160num<-ifelse(dessample$YPC1160<0,NA, dessample$YPC1160)
table(dessample$YPC1160num)
describe(dessample$YPC1160num)
table(dessample$f565num)
table(dessample$p3080num)
table(dessample$s3080num)
table(dessample$conceivedmarr) #higher score (1) means conceived in marriage (positive)
table(dessample$partogev10) #parents still together = 1, parents not together = 0 #still together at age 10
table(dessample$parcrime) #0 = both parents been in trouble with the law, 1 = 1 parent was in trouble, 2 = no parents in trouble with law #age 12

#2. Descriptives of complete cases individuals in analysis (256)
dim(additional) # 15243 x   40
dim(complete) #247 x 31
dessampleCC<-merge(additional, complete, by="ID", all.y=TRUE) 
dim(dessampleCC) #256 x 70
describe2<-describe(dessampleCC)
describe2frame<-as.data.frame(describe2)
describe2frame$meansd<-NA
for (i in 1:nrow(describe2frame)){
  describe2frame$meansd[i]<-paste0(round(describe2frame$mean[i], 2), "(", round(describe2frame$sd[i],2), ")")
}
setwd("~/Google Drive/PhD Projects/Life course modelling of wellbeing/Layard Replication/Complete Cases Model 3times/Final/Descriptives")
write.csv(describe2frame, "256CC descriptives2.csv")
table(dessampleCC$incomeMP3)
table(dessampleCC$AQguess2)
table(dessampleCC$employed23)
table(dessampleCC$adultpartner)
table(dessampleCC$h470num)
table(dessampleCC$n8130num)
table(dessampleCC$r9020num)
table(dessampleCC$YPC1160) #number of siblings at 23
dessampleCC$YPC1160num<-ifelse(dessampleCC$YPC1160<0,NA, dessampleCC$YPC1160)
table(dessampleCC$YPC1160num)
describe(dessampleCC$YPC1160num)
table(dessampleCC$f565num)
table(dessampleCC$p3080num)
table(dessampleCC$s3080num)
table(dessampleCC$conceivedmarr) #higher score (1) means conceived in marriage (positive)
table(dessampleCC$partogev10) #parents still together = 1, parents not together = 0 #still together at age 10
table(dessampleCC$parcrime) #0 = both parents been in trouble with the law, 1 = 1 parent was in trouble, 2 = no parents in trouble with law #age 12

#Correlation Matrix
library("Hmisc")

#1. Correlation Matrix of data available for 4222 individuals who returned the 23 questionnaire
summary(adultchildfamily2)
adultchildfamily2cor<-adultchildfamily2
adultchildfamily2cor$ID<-NULL
adultchildfamily2cor$X<-NULL
adultchildfamily2cor$Age<-NULL
adultchildfamily2cor$aln<-NULL
adultchildfamily2cor$qlet<-NULL
summary(adultchildfamily2cor)
adultchildfamily2cor<-adultchildfamily2cor[,c(1,2,3,4, 5, 6, 7, 8, 9, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)]
returnedcorr<-rcorr(as.matrix(adultchildfamily2cor), type = "pearson")
setwd("~/PhD Papers/Lifecourse/New folder")
write.csv(returnedcorr$r, "4222corrR.csv")
write.csv(returnedcorr$P, "4222corrP.csv")
write.csv(returnedcorr$n, "4222corrN.csv")


adultchildfamily2cor<-adultchildfamily2[,c(9, 11, 10, 12)]
returnedcorr<-rcorr(as.matrix(adultchildfamily2cor), type = "pearson")
write.csv(returnedcorr$r, "wbR.csv")
write.csv(returnedcorr$P, "wbP.csv")
write.csv(returnedcorr$n, "wbN.csv")


#2. Correlation Matrix of complete cases individuals in analysis (247)
summary(complete)
completecorr<-complete
completecorr$ID<-NULL
completecorr$X<-NULL
completecorr$Age<-NULL
completecorr$aln<-NULL
completecorr$qlet<-NULL
summary(completecorr)
returnedcorr<-rcorr(as.matrix(completecorr), type = "pearson")
setwd("~/Google Drive/PhD Projects/Life course modelling of wellbeing/Layard Replication/Complete Cases Model 3times/Final/Correlations")
write.csv(returnedcorr$r, "247corrR.csv")
write.csv(returnedcorr$P, "247corrP.csv")
write.csv(returnedcorr$n, "247corrN.csv")

#Descriptives - imputed model

#lm model predicting 1 is a means model 
fit.mi = with(data=adultchildfamily2, exp = lm(wemwbs~1))
summary(fit.mi)
mean(adultchildfamily2$wemwbs, na.rm=TRUE)
#produces the same values
fit.mi = with(data=imputedunstan50, exp = lm(ons1~1))
combFit = pool(fit.mi) 
summary(combFit)
mean(adultchildfamily2$ons1, na.rm=TRUE)
#Same values

#Make matrix to store descriptives
imputeddesc<-matrix(NA, 15, 2)
imputeddesc[,1]<-c("adultloginc", "AQguess2", "employed23", "adultpartner", "adultantis", "selfhealth", "adultemohealth", 
                   "childext", "childemohealth", "childIQ", "childcurrenthealth",
                   "econcomp", "pargenhealth", "paremohealth", "fambehav")

for(i in 1:nrow(imputeddesc)){
fit.mi = with(data=imputedunstan50, exp = lm(get(imputeddesc[i, 1])~1))
combFit = pool(fit.mi) 
imputeddesc[i,2]<-paste0(round(summary(combFit)[1], 2), "(", round(summary(combFit)[2],2), ")") #coefficient (SE)
}
setwd("~/Google Drive/PhD Projects/Life course modelling of wellbeing/Layard Replication/Results/Imputed Models/Descriptives")
write.csv(imputeddesc, "imputeddesc.csv")

#Check with table
fit.mi = with(data=imputedunstan50, exp = lm(paremohealth~1))
combFit = pool(fit.mi) 
paste0(round(summary(combFit)[1], 2), "(", round(summary(combFit)[2],2), ")") #coefficient (SE)

#Find N
imputedunstan50
summary(imputedunstan50)
imputedunstan50complete1 <- complete(imputedunstan50, 1)
summary(imputedunstan50complete1)
dim(imputedunstan50complete1)
4178-28 #income
4178-47  #education level
4178-23  #employment
4178-28  #partnership
4178-42  #antisocial behaviour
4178-39  #self percieved health
4178-41  #emotional problems
4178-33  #childIQ
4178-53  #child externalising
4178-57  #child internalising
4178-51  #child health
4178-87  #socioeconomic
4178-88  #parental general health
4178-86  #parental emotional health
4178-79  #family behavioural

table(imputedunstan50complete1$AQguess2)
table(imputedunstan50complete10$AQguess2)
table(imputedunstan50complete20$AQguess2)
#all different
#gives number in dataset 1 as representation
table(imputedunstan50complete1$AQguess2)
table(imputedunstan50complete1$employed23)
table(imputedunstan50complete1$adultpartner)

#Correlation table - average correlation across 60 imputed dataframes
setwd("~/Google Drive/PhD Projects/Life course modelling of wellbeing/Analysis/Multiple Imputation Analysis/Descriptives/correlation")

#separate the imputed datasets into individual dataframes
#take away the unneccesary variables
#run correlation matrix
#assign correlation matrix (R) to empty matrix
for(i in 1:60){
completedata<-complete(imputedunstan50, i)
completedata$ID<-NULL
completedata$X<-NULL
completedata$Age<-NULL
completedata$aln<-NULL
completedata$qlet<-NULL
corrmatrix<-rcorr(as.matrix(completedata), type = "pearson")
assign(paste0("corrR", i), corrmatrix$r)
}

my.list<-list(corrR1, corrR2, corrR3, corrR4, corrR5, corrR6, corrR7, corrR8, corrR9, corrR10,
              corrR11, corrR12, corrR13, corrR14, corrR15, corrR16, corrR17, corrR18, corrR19, corrR20,
              corrR21, corrR22, corrR23, corrR24, corrR25, corrR26, corrR27, corrR28, corrR29, corrR30,
              corrR31, corrR32, corrR33, corrR34, corrR35, corrR36, corrR37, corrR38, corrR39, corrR40,
              corrR41, corrR42, corrR43, corrR44, corrR45, corrR46, corrR47, corrR48, corrR49, corrR50,
              corrR51, corrR52, corrR53, corrR54, corrR55, corrR56, corrR57, corrR58, corrR59, corrR60)
avcorr<-Reduce("+", my.list) / length(my.list)
write.csv(avcorr, "imputedavcorr.csv")


#Cronbach's alpha
library(ltm)
# Cronbach's alpha for the LSAT data-set
# with a Bootstrap 95% CI
cronbach.alpha(wellbeing23, CI = TRUE, B = 500, na.rm=TRUE)
