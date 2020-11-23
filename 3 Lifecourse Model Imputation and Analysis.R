##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##~~~~~2. Lifecourse Model Multiple Imputation~~~~~##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

#See http://web.maths.unsw.edu.au/~dwarton/missingDataLab.html

#Loading required packages
library(mice)
library(VIM)
library(lattice)
library(psych)

#Change required variables into factors 
adultchildfamily2$ID<-as.factor(adultchildfamily2$ID)
adultchildfamily2$Sex<-as.factor(adultchildfamily2$Sex)
adultchildfamily2$employed23<-as.factor(adultchildfamily2$employed23)
adultchildfamily2$adultpartner<-as.factor(adultchildfamily2$adultpartner)


#Run basic MICE model
init = mice(adultchildfamily2, maxit=0) 
meth = init$method

#The predictor matrix is a square matrix that specifies the variables that are used to impute each incomplete variable.
predM = init$predictorMatrix

#To impute the missing values, mice package use an algorithm in a such a way that use 
#information from other variables in the dataset to predict and impute the missing values. 
#Therefore, you may not want to use a certain variable as predictors.
#Remove identifying variables and others as imputation predictors
#Specifying that in column of the following variables are all 0 so they are never used in the imputation regressions
#The object predM contains the predictor matrix from an initial run of mice with zero iterations, specified by maxit = 0. 
predM[, c("ID", "Age", "aln", "qlet")]=0

#Skip imputation of the wellbeing variables, but the wellbeing variables are still used for prediction
meth[c("ons1", "ons2", "ons3", "ons4", "wemwbs", "happ", "life", "meaning", 
       "bpn", "autonomy", "relatedness", "competence", "gratitude", "optimism", "Sex")]=""

#Now let's specify the methods for imputing the missing values. 
#There are specific methods for continues, binary and ordinal variables.
meth[c("adultloginc", "AQguess2", "adultantis", "selfhealth", "adultemohealth", "childIQ", "childext", "childemohealth", "childcurrenthealth","econcomp", "pargenhealth", "paremohealth", "fambehav")]="pmm" #ordinal variables
meth[c("employed23", "adultpartner")]="logreg" #binary

#Methods:
#Predictive mean modelling: Compared with standard methods based on linear regression and the normal distribution, 
#PMM produces imputed values that are much more like real values. 
#If the original variable is skewed, the imputed values will also be skewed. 
#If the original variable is bounded by 0 and 100, the imputed values will also be bounded by 0 and 100. 
#And if the real values are discrete (like number of children), the imputed values will also be discrete. 
#That's because the imputed values are real values that are "borrowed" from individuals with real data.

#We used Rubin rules to average the variable estimates over 60 imputed or completed datasets.31

set.seed(103)
imputedunstan50 = mice(adultchildfamily2, method=meth, predictorMatrix=predM, m=60, maxit=20)


#Now run analyses with all the imputed output and then pool the results

#Unstandardised
setwd("~/PhD Papers/Lifecourse/JPSP/analysis/Unstandardised")

wbmeasures<-c("ons1", "ons2", "ons3", "ons4", "wemwbs", "happ", "life", "meaning", "bpn", "autonomy", "relatedness", "competence", "gratitude", "optimism")
Predictors<-c("adultloginc", "AQguess2", "employed23", "adultantis", "adultpartner", "selfhealth", "adultemohealth", "childIQ", "childext", "childemohealth", "childcurrenthealth", "econcomp", "pargenhealth", "paremohealth", "fambehav", "Sex")

#OVERALL MODEL

#We also want to find the pooled R square of the entire model as well as the incremental variance explained R2 by each predictor
#So create table to store this information for each wellbeing indicator and model
rsquaretable<-matrix(NA, 19, 57)
rsquaretable[3:19,1]<-c("Overall Model", "Model LESS Adult Income", "Model LESS Adult Education", "Model LESS Adult Employment", "Model LESS Adult Conduct", "Model LESS Adult Partner", "Model LESS Adult Self Reported Health", "Model LESS Adult Emotional Health", "Model LESS Child IQ", "Model LESS Child Externalising", "Model LESS Child Emotional Health", "Model LESS Child Overall Health", "Model LESS Family Economic Composition", "Model LESS Parental General Health", "Model LESS Parental Emotional Health", "Model LESS Family Behavioural", "Model LESS Sex")
rsquaretable[1, 2:5]<-c("ONS1", "ONS1","ONS1","ONS1")
rsquaretable[2, 2:5]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 6:9]<-c("ONS2", "ONS2", "ONS2", "ONS2")
rsquaretable[2, 6:9]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 10:13]<-c("ONS3", "ONS3", "ONS3", "ONS3")
rsquaretable[2, 10:13]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 14:17]<-c("ONS4", "ONS4", "ONS4", "ONS4")
rsquaretable[2, 14:17]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 18:21]<-c("WEMWBS", "WEMWBS", "WEMWBS", "WEMWBS")
rsquaretable[2, 18:21]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 22:25]<-c("Happiness", "Happiness", "Happiness", "Happiness")
rsquaretable[2, 22:25]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 26:29]<-c("Life Satisfaction", "Life Satisfaction", "Life Satisfaction", "Life Satisfaction")
rsquaretable[2, 26:29]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 30:33]<-c("Meaning in Life", "Meaning in Life", "Meaning in Life", "Meaning in Life")
rsquaretable[2, 30:33]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 34:37]<-c("BPN", "BPN","BPN","BPN")
rsquaretable[2, 34:37]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 38:41]<-c("Autonomy", "Autonomy","Autonomy","Autonomy")
rsquaretable[2, 38:41]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 42:45]<-c("Relatedness", "Relatedness","Relatedness","Relatedness")
rsquaretable[2, 42:45]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 46:49]<-c("Competence", "Competence", "Competence", "Competence")
rsquaretable[2, 46:49]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 50:53]<-c("Gratitude", "Gratitude", "Gratitude", "Gratitude")
rsquaretable[2, 50:53]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 54:57]<-c("Optimism", "Optimism", "Optimism", "Optimism")
rsquaretable[2, 54:57]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")

#overall model
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "all", ".csv"))
  rsquaretable[3, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model less adultincome
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSadultinc", ".csv"))
  rsquaretable[4, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model less education
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSadultedu", ".csv"))
  rsquaretable[5, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}


#overall model LESS employed
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSadultemploy", ".csv"))
  rsquaretable[6, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS adultconduct
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultpartner+selfhealth+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSadultcondcut", ".csv"))
  rsquaretable[7, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS partner
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+selfhealth+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSadultpart", ".csv"))
  rsquaretable[8, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS Adult self reported health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSselfhealth", ".csv"))
  rsquaretable[9, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS adult emotional health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSadultemo", ".csv"))
  rsquaretable[10, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS ChildIQ
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSchildIQ", ".csv"))
  rsquaretable[11, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS child externalising
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSchildext", ".csv"))
  rsquaretable[12, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS child emotional health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childext+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSchildemo", ".csv"))
  rsquaretable[13, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS child current health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childext+childemohealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSchildgenhealth", ".csv"))
  rsquaretable[14, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS family econ
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSfamecon", ".csv"))
  rsquaretable[15, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS parental gen health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSfamhealth", ".csv"))
  rsquaretable[16, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS parental emotional health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSfamhealth", ".csv"))
  rsquaretable[17, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}


#overall model LESS family conduct
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSfambehav", ".csv"))
  rsquaretable[18, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS sex
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSsex", ".csv"))
  rsquaretable[19, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

write.csv(rsquaretable, "Rsqallpred.csv")

#CHILD MODEL

#Build R square table for child model
rsquaretable<-matrix(NA, 12, 57)
rsquaretable[3:12,1]<-c("Overall Child Model", "Child Model LESS Child IQ", "Child Model LESS Child Externalising", "Child Model LESS Child Emotional Health", "Child Model LESS Child Overall Health", "Child Model LESS Family Economic Composition", "Child Model LESS Parental General Health", "Child Model LESS Parental Emotional Health", "Child Model LESS Family Behavioural", "Child Model LESS Sex")
rsquaretable[1, 2:5]<-c("ONS1", "ONS1","ONS1","ONS1")
rsquaretable[2, 2:5]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 6:9]<-c("ONS2", "ONS2", "ONS2", "ONS2")
rsquaretable[2, 6:9]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 10:13]<-c("ONS3", "ONS3", "ONS3", "ONS3")
rsquaretable[2, 10:13]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 14:17]<-c("ONS4", "ONS4", "ONS4", "ONS4")
rsquaretable[2, 14:17]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 18:21]<-c("WEMWBS", "WEMWBS", "WEMWBS", "WEMWBS")
rsquaretable[2, 18:21]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 22:25]<-c("Happiness", "Happiness", "Happiness", "Happiness")
rsquaretable[2, 22:25]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 26:29]<-c("Life Satisfaction", "Life Satisfaction", "Life Satisfaction", "Life Satisfaction")
rsquaretable[2, 26:29]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 30:33]<-c("Meaning in Life", "Meaning in Life", "Meaning in Life", "Meaning in Life")
rsquaretable[2, 30:33]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 34:37]<-c("BPN", "BPN","BPN","BPN")
rsquaretable[2, 34:37]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 38:41]<-c("Autonomy", "Autonomy","Autonomy","Autonomy")
rsquaretable[2, 38:41]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 42:45]<-c("Relatedness", "Relatedness","Relatedness","Relatedness")
rsquaretable[2, 42:45]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 46:49]<-c("Competence", "Competence", "Competence", "Competence")
rsquaretable[2, 46:49]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 50:53]<-c("Gratitude", "Gratitude", "Gratitude", "Gratitude")
rsquaretable[2, 50:53]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 54:57]<-c("Optimism", "Optimism", "Optimism", "Optimism")
rsquaretable[2, 54:57]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")

#overall child model
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "child", ".csv"))
  rsquaretable[3, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS ChildIQ
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSchildIQ", ".csv"))
  rsquaretable[4, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS childext
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ childIQ+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSext", ".csv"))
  rsquaretable[5, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model less child emotional health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ childIQ+childext+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSchildemo", ".csv"))
  rsquaretable[6, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS child general health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ childIQ+childext+childemohealth+econcomp+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSchildgenhealth", ".csv"))
  rsquaretable[7, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS family economic composition
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ childIQ+childext+childemohealth+childcurrenthealth+pargenhealth+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSfamecon", ".csv"))
  rsquaretable[8, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS parental general health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSfamhealth", ".csv"))
  rsquaretable[9, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS parental emotional health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ childIQ+childext+childemohealth+childcurrenthealth+econcomp+paremohealth+fambehav+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSfamhealth", ".csv"))
  rsquaretable[10, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS family behavioural
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSfambehav", ".csv"))
  rsquaretable[11, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS sex
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ childIQ+childext+childemohealth+childcurrenthealth+econcomp+pargenhealth+paremohealth+fambehav))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSsex", ".csv"))
  rsquaretable[12, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

write.csv(rsquaretable, "Rsqchildonly.csv")

#ADULT ONLY MODEL

rsquaretable<-matrix(NA, 11, 57)
rsquaretable[3:11,1]<-c("Overall Adult Model", "Adult Model LESS Adult Income", "Adult Model LESS Adult Education", "Adult Model LESS Adult Employment", "Adult Model LESS Adult Conduct", "Adult Model LESS Adult Partner", "Adult Model LESS Adult Self Reported Health", "Adult Model LESS Adult Emotional Health", "Adult Model LESS Sex")
rsquaretable[1, 2:5]<-c("ONS1", "ONS1","ONS1","ONS1")
rsquaretable[2, 2:5]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 6:9]<-c("ONS2", "ONS2", "ONS2", "ONS2")
rsquaretable[2, 6:9]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 10:13]<-c("ONS3", "ONS3", "ONS3", "ONS3")
rsquaretable[2, 10:13]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 14:17]<-c("ONS4", "ONS4", "ONS4", "ONS4")
rsquaretable[2, 14:17]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 18:21]<-c("WEMWBS", "WEMWBS", "WEMWBS", "WEMWBS")
rsquaretable[2, 18:21]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 22:25]<-c("Happiness", "Happiness", "Happiness", "Happiness")
rsquaretable[2, 22:25]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 26:29]<-c("Life Satisfaction", "Life Satisfaction", "Life Satisfaction", "Life Satisfaction")
rsquaretable[2, 26:29]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 30:33]<-c("Meaning in Life", "Meaning in Life", "Meaning in Life", "Meaning in Life")
rsquaretable[2, 30:33]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 34:37]<-c("BPN", "BPN","BPN","BPN")
rsquaretable[2, 34:37]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 38:41]<-c("Autonomy", "Autonomy","Autonomy","Autonomy")
rsquaretable[2, 38:41]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 42:45]<-c("Relatedness", "Relatedness","Relatedness","Relatedness")
rsquaretable[2, 42:45]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 46:49]<-c("Competence", "Competence", "Competence", "Competence")
rsquaretable[2, 46:49]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 50:53]<-c("Gratitude", "Gratitude", "Gratitude", "Gratitude")
rsquaretable[2, 50:53]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 54:57]<-c("Optimism", "Optimism", "Optimism", "Optimism")
rsquaretable[2, 54:57]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")

#overall adult model
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adult", ".csv"))
  rsquaretable[3, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adultincome
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSfaminc", ".csv"))
  rsquaretable[4, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adultedu
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+employed23+adultantis+adultpartner+selfhealth+adultemohealth+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSadultedu", ".csv"))
  rsquaretable[5, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adultemploy
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+adultantis+adultpartner+selfhealth+adultemohealth+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSadultemp", ".csv"))
  rsquaretable[6, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adult conduct
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultpartner+selfhealth+adultemohealth+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSadultconduct", ".csv"))
  rsquaretable[7, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adultpart
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+selfhealth+adultemohealth+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSadultpart", ".csv"))
  rsquaretable[8, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adult self reported general health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+adultemohealth+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSadultgenhealth", ".csv"))
  rsquaretable[9, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adult emotional health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSadultemo", ".csv"))
  rsquaretable[10, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS sex
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(get(wbmeasures[i]) ~ adultloginc+AQguess2+employed23+adultantis+adultpartner+selfhealth+adultemohealth))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSsex", ".csv"))
  rsquaretable[11, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}


write.csv(rsquaretable, "Rsqadultonly.csv")


#Standardised results

setwd("~/PhD Papers/Lifecourse/JPSP/analysis/Standardised")

wbmeasures<-c("ons1", "ons2", "ons3", "ons4", "wemwbs", "happ", "life", "meaning", "bpn", "autonomy", "relatedness", "competence", "gratitude", "optimism")
Predictors<-c("scale(adultloginc)", "scale(AQguess2)", "scale(as.numeric(as.character(employed23)))", "scale(adultantis)", "scale(as.numeric(as.character(adultpartner)))", "scale(selfhealth)", "scale(adultemohealth)", "scale(childIQ)", "scale(childext)", "scale(childemohealth)", "scale(childcurrenthealth)", "scale(econcomp)", "scale(pargenhealth)", "scale(paremohealth)", "scale(fambehav)", "Sex")

#OVERALL MODEL

#We also want to find the pooled R square of the entire model as well as the incremental variance explained R2 by each predictor
#So create table to store this information for each wellbeing indicator and model
rsquaretable<-matrix(NA, 19, 57)
rsquaretable[3:19,1]<-c("Overall Model", "Model LESS Adult Income", "Model LESS Adult Education", "Model LESS Adult Employment", "Model LESS Adult Conduct", "Model LESS Adult Partner", "Model LESS Adult Self Reported Health", "Model LESS Adult Emotional Health", "Model LESS Child IQ", "Model LESS Child Externalising", "Model LESS Child Emotional Health", "Model LESS Child Overall Health", "Model LESS Family Economic Composition", "Model LESS Parental General Health", "Model LESS Parental Emotional Health", "Model LESS Family Behavioural", "Model LESS Sex")
rsquaretable[1, 2:5]<-c("scale(ons1)", "scale(ons1)","scale(ons1)","scale(ons1)")
rsquaretable[2, 2:5]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 6:9]<-c("scale(ons2)", "scale(ons2)", "scale(ons2)", "scale(ons2)")
rsquaretable[2, 6:9]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 10:13]<-c("scale(ons3)", "scale(ons3)", "scale(ons3)", "scale(ons3)")
rsquaretable[2, 10:13]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 14:17]<-c("scale(ons4)", "scale(ons4)", "scale(ons4)", "scale(ons4)")
rsquaretable[2, 14:17]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 18:21]<-c("scale(wemwbs)", "scale(wemwbs)", "scale(wemwbs)", "scale(wemwbs)")
rsquaretable[2, 18:21]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 22:25]<-c("scale(happ)iness", "scale(happ)iness", "scale(happ)iness", "scale(happ)iness")
rsquaretable[2, 22:25]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 26:29]<-c("scale(life) Satisfaction", "scale(life) Satisfaction", "scale(life) Satisfaction", "scale(life) Satisfaction")
rsquaretable[2, 26:29]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 30:33]<-c("scale(meaning) in Life", "scale(meaning) in Life", "scale(meaning) in Life", "scale(meaning) in Life")
rsquaretable[2, 30:33]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 34:37]<-c("scale(bpn)", "scale(bpn)","scale(bpn)","scale(bpn)")
rsquaretable[2, 34:37]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 38:41]<-c("scale(autonomy)", "scale(autonomy)","scale(autonomy)","scale(autonomy)")
rsquaretable[2, 38:41]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 42:45]<-c("scale(relatedness)", "scale(relatedness)","scale(relatedness)","scale(relatedness)")
rsquaretable[2, 42:45]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 46:49]<-c("scale(competence)", "scale(competence)", "scale(competence)", "scale(competence)")
rsquaretable[2, 46:49]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 50:53]<-c("scale(gratitude)", "scale(gratitude)", "scale(gratitude)", "scale(gratitude)")
rsquaretable[2, 50:53]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 54:57]<-c("scale(optimism)", "scale(optimism)", "scale(optimism)", "scale(optimism)")
rsquaretable[2, 54:57]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")


#overall model
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "all", ".csv"))
  rsquaretable[3, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model less adultincome
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSadultinc", ".csv"))
  rsquaretable[4, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model less education
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSadultedu", ".csv"))
  rsquaretable[5, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}


#overall model LESS employed
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSadultemploy", ".csv"))
  rsquaretable[6, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS adultconduct
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSadultcondcut", ".csv"))
  rsquaretable[7, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS partner
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSadultpart", ".csv"))
  rsquaretable[8, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS Adult self reported health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSscale(selfhealth)", ".csv"))
  rsquaretable[9, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS adult emotional health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSadultemo", ".csv"))
  rsquaretable[10, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS scale(childIQ)
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSscale(childIQ)", ".csv"))
  rsquaretable[11, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS child externalising
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSscale(childext)", ".csv"))
  rsquaretable[12, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS child emotional health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSchildemo", ".csv"))
  rsquaretable[13, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS child current health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSchildgenhealth", ".csv"))
  rsquaretable[14, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS family econ
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSfamecon", ".csv"))
  rsquaretable[15, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS parental general health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSscale(famhealth)", ".csv"))
  rsquaretable[16, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS parental emotional health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSscale(famhealth)", ".csv"))
  rsquaretable[17, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS family conduct
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSscale(fambehav)", ".csv"))
  rsquaretable[18, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall model LESS Sex
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "allLESSsex", ".csv"))
  rsquaretable[19, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

write.csv(rsquaretable, "Rsqallpred.csv")

#CHILD MODEL

#Build R square table for child model
rsquaretable<-matrix(NA, 12, 57)
rsquaretable[3:12,1]<-c("Overall Child Model", "Child Model LESS Child IQ", "Child Model LESS Child Externalising", "Child Model LESS Child Emotional Health", "Child Model LESS Child Overall Health", "Child Model LESS Family Economic Composition", "Child Model LESS Parental General Health", "Child Model LESS Parental Emotional Health", "Child Model LESS Family Behavioural", "Child Model LESS Sex")
rsquaretable[1, 2:5]<-c("scale(ons1)", "scale(ons1)","scale(ons1)","scale(ons1)")
rsquaretable[2, 2:5]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 6:9]<-c("scale(ons2)", "scale(ons2)", "scale(ons2)", "scale(ons2)")
rsquaretable[2, 6:9]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 10:13]<-c("scale(ons3)", "scale(ons3)", "scale(ons3)", "scale(ons3)")
rsquaretable[2, 10:13]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 14:17]<-c("scale(ons4)", "scale(ons4)", "scale(ons4)", "scale(ons4)")
rsquaretable[2, 14:17]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 18:21]<-c("scale(wemwbs)", "scale(wemwbs)", "scale(wemwbs)", "scale(wemwbs)")
rsquaretable[2, 18:21]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 22:25]<-c("scale(happ)iness", "scale(happ)iness", "scale(happ)iness", "scale(happ)iness")
rsquaretable[2, 22:25]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 26:29]<-c("scale(Life) Satisfaction", "scale(Life) Satisfaction", "scale(Life) Satisfaction", "scale(Life) Satisfaction")
rsquaretable[2, 26:29]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 30:33]<-c("scale(meaning) in Life", "scale(meaning) in Life", "scale(meaning) in Life", "scale(meaning) in Life")
rsquaretable[2, 30:33]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 34:37]<-c("scale(bpn)", "scale(bpn)","scale(bpn)","scale(bpn)")
rsquaretable[2, 34:37]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 38:41]<-c("scale(autonomy)", "scale(autonomy)","scale(autonomy)","scale(autonomy)")
rsquaretable[2, 38:41]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 42:45]<-c("scale(relatedness)", "scale(relatedness)","scale(relatedness)","scale(relatedness)")
rsquaretable[2, 42:45]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 46:49]<-c("scale(competence)", "scale(competence)", "scale(competence)", "scale(competence)")
rsquaretable[2, 46:49]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 50:53]<-c("scale(gratitude)", "scale(gratitude)", "scale(gratitude)", "scale(gratitude)")
rsquaretable[2, 50:53]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 54:57]<-c("scale(optimism)", "scale(optimism)", "scale(optimism)", "scale(optimism)")
rsquaretable[2, 54:57]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")

#overall child model
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "child", ".csv"))
  rsquaretable[3, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS scale(childIQ)
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSscale(childIQ)", ".csv"))
  rsquaretable[4, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS scale(childext)
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(childIQ)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSext", ".csv"))
  rsquaretable[5, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model less child emotional health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(childIQ)+scale(childext)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSchildemo", ".csv"))
  rsquaretable[6, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS child general health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSchildgenhealth", ".csv"))
  rsquaretable[7, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS family economic composition
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSfamecon", ".csv"))
  rsquaretable[8, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS parental general health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(paremohealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSscale(famhealth)", ".csv"))
  rsquaretable[9, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS parental emotional health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(fambehav)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSscale(famhealth)", ".csv"))
  rsquaretable[10, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}


#overall child model LESS family behavioural
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSscale(fambehav)", ".csv"))
  rsquaretable[11, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall child model LESS sex
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "childLESSscale(sex)", ".csv"))
  rsquaretable[12, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

write.csv(rsquaretable, "Rsqchildpred.csv")

#ADULT ONLY MODEL

rsquaretable<-matrix(NA, 11, 57)
rsquaretable[3:11,1]<-c("Overall Adult Model", "Adult Model LESS Adult Income", "Adult Model LESS Adult Education", "Adult Model LESS Adult Employment", "Adult Model LESS Adult Conduct", "Adult Model LESS Adult Partner", "Adult Model LESS Adult Self Reported Health", "Adult Model LESS Adult Emotional Health", "Adult Model LESS Sex")
rsquaretable[1, 2:5]<-c("scale(ons1)", "scale(ons1)","scale(ons1)","scale(ons1)")
rsquaretable[2, 2:5]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 6:9]<-c("scale(ons2)", "scale(ons2)", "scale(ons2)", "scale(ons2)")
rsquaretable[2, 6:9]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 10:13]<-c("scale(ons3)", "scale(ons3)", "scale(ons3)", "scale(ons3)")
rsquaretable[2, 10:13]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 14:17]<-c("scale(ons4)", "scale(ons4)", "scale(ons4)", "scale(ons4)")
rsquaretable[2, 14:17]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 18:21]<-c("scale(wemwbs)", "scale(wemwbs)", "scale(wemwbs)", "scale(wemwbs)")
rsquaretable[2, 18:21]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 22:25]<-c("scale(happ)iness", "scale(happ)iness", "scale(happ)iness", "scale(happ)iness")
rsquaretable[2, 22:25]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 26:29]<-c("scale(happ) Satisfaction", "scale(happ) Satisfaction", "scale(happ) Satisfaction", "scale(happ) Satisfaction")
rsquaretable[2, 26:29]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 30:33]<-c("scale(meaning) in Life", "scale(meaning) in Life", "scale(meaning) in Life", "scale(meaning) in Life")
rsquaretable[2, 30:33]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 34:37]<-c("scale(bpn)", "scale(bpn)","scale(bpn)","scale(bpn)")
rsquaretable[2, 34:37]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 38:41]<-c("scale(autonomy)", "scale(autonomy)","scale(autonomy)","scale(autonomy)")
rsquaretable[2, 38:41]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 42:45]<-c("scale(relatedness)", "scale(relatedness)","scale(relatedness)","scale(relatedness)")
rsquaretable[2, 42:45]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 46:49]<-c("scale(competence)", "scale(competence)", "scale(competence)", "scale(competence)")
rsquaretable[2, 46:49]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 50:53]<-c("scale(gratitude)", "scale(gratitude)", "scale(gratitude)", "scale(gratitude)")
rsquaretable[2, 50:53]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")
rsquaretable[1, 54:57]<-c("scale(optimism)", "scale(optimism)", "scale(optimism)", "scale(optimism)")
rsquaretable[2, 54:57]<-c("Estimate", "95% CI lower", "95% CI upper", "FMI")

#overall adult model
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adult", ".csv"))
  rsquaretable[3, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adultincome
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSfaminc", ".csv"))
  rsquaretable[4, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adultedu
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSadultedu", ".csv"))
  rsquaretable[5, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adultemploy
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSadultemp", ".csv"))
  rsquaretable[6, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adult conduct
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSadultconduct", ".csv"))
  rsquaretable[7, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adultpart
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(selfhealth)+scale(adultemohealth)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSadultpart", ".csv"))
  rsquaretable[8, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adult self reported general health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(adultemohealth)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSadultgenhealth", ".csv"))
  rsquaretable[9, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS adult emotional health
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+Sex))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSadultemo", ".csv"))
  rsquaretable[10, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

#overall adult model LESS sex
for (i in 1:length(wbmeasures)){
  fit.mi = with(data=imputedunstan50, exp = lm(scale(get(wbmeasures[i])) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)))
  combFit = pool(fit.mi) 
  summ<-summary(combFit)
  write.csv(summ, paste0(wbmeasures[i], "adultLESSsex", ".csv"))
  rsquaretable[11, (4*i-2):(4*i+1)]<-pool.r.squared(fit.mi)[1:4]
}

write.csv(rsquaretable, "Rsqadultonly.csv")

#Check sample size of imputed models
imputedunstan50c1 <- complete(imputedunstan50, 1)

#Overall model single imputed datasets
c1model<-lm(scale(ons1) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #110 observations deleted due to missingness
summary(c1model)$ df  #4051
c1model<-lm(scale(ons2) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #110 observations deleted due to missingness
summary(c1model)$ df  #4051
c1model<-lm(scale(ons3) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #110 observations deleted due to missingness
summary(c1model)$ df  #4051
c1model<-lm(scale(ons4) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #110 observations deleted due to missingness
summary(c1model)$ df  #4051
c1model<-lm(scale(wemwbs) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #111 observations deleted due to missingness
summary(c1model)$ df  #4050
c1model<-lm(scale(happ) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #111 observations deleted due to missingness
summary(c1model)$ df  #4050
c1model<-lm(scale(life) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #111 observations deleted due to missingness
summary(c1model)$ df  #4050
c1model<-lm(scale(meaning) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #111 observations deleted due to missingness
summary(c1model)$ df  #4050
c1model<-lm(scale(bpn) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #111 observations deleted due to missingness
summary(c1model)$ df  #4050
c1model<-lm(scale(autonomy) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #111 observations deleted due to missingness
summary(c1model)$ df  #4050
c1model<-lm(scale(relatedness) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #111 observations deleted due to missingness
summary(c1model)$ df  #4050
c1model<-lm(scale(competence) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #111 observations deleted due to missingness
summary(c1model)$ df  #4050
c1model<-lm(scale(gratitude) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #112 observations deleted due to missingness
summary(c1model)$ df  #4049
c1model<-lm(scale(optimism) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #111 observations deleted due to missingness
summary(c1model)$ df  #4050

#Adult only
#Overall model single imputed datasets
c1model<-lm(scale(ons1) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #85 observations deleted due to missingness
summary(c1model)$ df  #4084
c1model<-lm(scale(ons2) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #85 observations deleted due to missingness
summary(c1model)$ df  #4084
#Overall model single imputed datasets
c1model<-lm(scale(ons3) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #85 observations deleted due to missingness
summary(c1model)$ df  #4084
c1model<-lm(scale(ons4) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #85 observations deleted due to missingness
summary(c1model)$ df  #4084
#Overall model single imputed datasets
c1model<-lm(scale(wemwbs) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #87 observations deleted due to missingness
summary(c1model)$ df  #4082
c1model<-lm(scale(happ) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #87 observations deleted due to missingness
summary(c1model)$ df  #4082
#Overall model single imputed datasets
c1model<-lm(scale(life) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #91 observations deleted due to missingness
summary(c1model)$ df  #4078
c1model<-lm(scale(meaning) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #90 observations deleted due to missingness
summary(c1model)$ df  #4079
#Overall model single imputed datasets
c1model<-lm(scale(bpn) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #95 observations deleted due to missingness
summary(c1model)$ df  #4074
c1model<-lm(scale(autonomy) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #95 observations deleted due to missingness
summary(c1model)$ df  #4074
#Overall model single imputed datasets
c1model<-lm(scale(relatedness) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #91 observations deleted due to missingness
summary(c1model)$ df  #4078
c1model<-lm(scale(competence) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #95 observations deleted due to missingness
summary(c1model)$ df  #4074
#Overall model single imputed datasets
c1model<-lm(scale(gratitude) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #100 observations deleted due to missingness
summary(c1model)$ df  #4069
c1model<-lm(scale(optimism) ~ scale(adultloginc)+scale(AQguess2)+scale(as.numeric(as.character(employed23)))+scale(adultantis)+scale(as.numeric(as.character(adultpartner)))+scale(selfhealth)+scale(adultemohealth)+Sex, data=imputedunstan50c1)
summary(c1model) #94 observations deleted due to missingness
summary(c1model)$ df  #4075

#Child and family only models
c1model<-lm(scale(ons1) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #108 observations deleted due to missingness
summary(c1model)$ df  #4060
c1model<-lm(scale(ons2) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #107 observations deleted due to missingness
summary(c1model)$ df  #4061
c1model<-lm(scale(ons3) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #107 observations deleted due to missingness
summary(c1model)$ df  #4061
c1model<-lm(scale(ons4) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #107 observations deleted due to missingness
summary(c1model)$ df  #4061
c1model<-lm(scale(wemwbs) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #108 observations deleted due to missingness
summary(c1model)$ df  #4060
c1model<-lm(scale(happ) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #108 observations deleted due to missingness
summary(c1model)$ df  #4060
c1model<-lm(scale(life) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #108 observations deleted due to missingness
summary(c1model)$ df  #4060
c1model<-lm(scale(meaning) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #109 observations deleted due to missingness
summary(c1model)$ df  #4059
c1model<-lm(scale(bpn) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #109 observations deleted due to missingness
summary(c1model)$ df  #4059
c1model<-lm(scale(autonomy) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #109 observations deleted due to missingness
summary(c1model)$ df  #4059
c1model<-lm(scale(relatedness) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #108 observations deleted due to missingness
summary(c1model)$ df  #4060
c1model<-lm(scale(competence) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #109 observations deleted due to missingness
summary(c1model)$ df  #4059
c1model<-lm(scale(gratitude) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #110 observations deleted due to missingness
summary(c1model)$ df  #4058
c1model<-lm(scale(optimism) ~ scale(childIQ)+scale(childext)+scale(childemohealth)+scale(childcurrenthealth)+scale(econcomp)+scale(pargenhealth)+scale(paremohealth)+scale(fambehav)+Sex, data=imputedunstan50c1)
summary(c1model) #108 observations deleted due to missingness
summary(c1model)$ df  #4060











