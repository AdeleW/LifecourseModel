###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
###~~~~~~1. Lifecourse Model Data Tidying~~~~~~###
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

setwd("~/PhD Papers/Lifecourse/analysis")
library(psych)
library(ltm)
maindata<-read.csv("lifecoursecompleteshort.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~Taking one random sibling out~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Create unique ID using family ID (aln) and sibling ID (qlet)
dim(maindata) #15445x1403
head(maindata[1:10])
names(maindata)[names(maindata) == "�..aln"] <- "aln"
table(maindata$qlet)
#    A     B 
#15243   202
maindata$ID = paste(maindata$aln, maindata$qlet, sep="")
head(maindata$ID)
dim(maindata) # 15445 x 1476

#Split the sample into singletons and sibling families
#B sibling participants
Bsib<-maindata[maindata$qlet=="B", c("aln", "qlet", "ID")]
head(Bsib, n=10)
tail(Bsib, n=10)
fam2<-as.vector(Bsib$aln)
sibs<-maindata[maindata$aln %in% fam2,] #subset of all individuals who have a sibling also in the study dataset
dim(sibs) #404x 1476
head(sibs[1:5])
#List of random numbers assigned to each family has previously been created and saved as randomsibling.csv
#Recall this for replication
randomsibling<-read.csv("randomsibling.csv")
random<-as.vector(randomsibling$x)
for (i in 1:202){
  sibs$random[2*i-1]<-ifelse(random[i]==1, 0,  1)
  sibs$random[2*i]<-ifelse(random[i]==1, 1, 0)
}
head(sibs[1:5])
head(sibs[1474:1477])
table(sibs$random) #same number of 0 and 1 (since sibling pair in family has to be assigned either 1 or 0)
randomsib<-sibs[sibs$random==1,]
dim(randomsib) #202x 1477 
head(randomsib[1:5])
head(randomsib[1474:1477])
table(randomsib$random)
table(randomsib$qlet) #random number of A's and B's selected
dim(randomsib) #202
duplicated(randomsib$aln) #all false
randomsib$random<-NULL
#Subset all the family ids maindata that are not in Bsib - these are the singleton individuals
singletons<-subset(maindata, !(maindata$aln %in% Bsib$aln))
dim(singletons) #15041x  1476
table(singletons$qlet)
#rbind singletons and random sibling subsets back into maindata
dim(singletons) #15041x  1476
dim(randomsib) #202x 1476
head(singletons[1470:1476], n=1)
head(randomsib[1470:1476], n=1)
head(singletons[1:5], n=1)
head(randomsib[1:5], n=1)
maindata<-rbind(randomsib, singletons)
dim(maindata) # 15243 x 1476
head(maindata[1:5], n=10)
tail(maindata[1:5], n=10)
table(maindata$qlet)
#   A     B 
#15132   111

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~Preparing Adult Predictors~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~Adult socio-economic predictors~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#-----------------------------------------------------
#a. Adult socio-economic predictors: employment
#-----------------------------------------------------

#YPC2450	l1a: Respondent is currently in full-time paid work (30 or more hours a week)
table(maindata$YPC2450)
#-9999   -10    -1     0     1 
#   19 11323   384  1079  2640
maindata$YPC2450num<-ifelse(maindata$YPC2450==1, 1,
                            ifelse(maindata$YPC2450==0, 0,
                                   NA))
table(maindata$YPC2450num)
#YPC2451	l1b: Respondent is currently in part-time paid work (less than 30 hours a week)
table(maindata$YPC2451)
maindata$YPC2451num<-ifelse(maindata$YPC2451==1, 1,
                            ifelse(maindata$YPC2451==0, 0,
                                   NA))
table(maindata$YPC2451num)
#-9999   -10    -1     0     1 
#   19 11323   938  2594   571
#YPC2452	l1c: Respondent is currently in irregular or occasional work
table(maindata$YPC2452)
maindata$YPC2452num<-ifelse(maindata$YPC2452==1, 1,
                            ifelse(maindata$YPC2452==0, 0,
                                   NA))
table(maindata$YPC2452num)
#YPC2453	l1d: Respondent is currently doing a modern apprenticeship or other government supported training/work-experience scheme
table(maindata$YPC2453)
maindata$YPC2453num<-ifelse(maindata$YPC2453==1, 1,
                            ifelse(maindata$YPC2453==0, 0,
                                   NA))
table(maindata$YPC2453num)
#YPC2454	l1e: Respondent is currently unemployed and looking for work
table(maindata$YPC2454)
maindata$YPC2454num<-ifelse(maindata$YPC2454==1, 1,
                            ifelse(maindata$YPC2454==0, 0,
                                   NA))
table(maindata$YPC2454num)
#YPC2455	l1f: Respondent is currently unable to work through sickness/disability
table(maindata$YPC2455)
maindata$YPC2455num<-ifelse(maindata$YPC2455==1, 1,
                            ifelse(maindata$YPC2455==0, 0,
                                   NA))
table(maindata$YPC2455num)
#YPC2456	l1g: Respondent is currently in full-time education
table(maindata$YPC2456)
maindata$YPC2456num<-ifelse(maindata$YPC2456==1, 1,
                            ifelse(maindata$YPC2456==0, 0,
                                   NA))
table(maindata$YPC2456num)
#YPC2457	l1h: Respondent is currently doing voluntary work
table(maindata$YPC2457)
maindata$YPC2457num<-ifelse(maindata$YPC2457==1, 1,
                            ifelse(maindata$YPC2457==0, 0,
                                   NA))
table(maindata$YPC2457num)
#YPC2458	l1i: Respondent is currently self-employed
table(maindata$YPC2458)
maindata$YPC2458num<-ifelse(maindata$YPC2458==1, 1,
                            ifelse(maindata$YPC2458==0, 0,
                                   NA))
table(maindata$YPC2458num)
#YPC2459	l1j: Respondent is currently a full/part-time carer
table(maindata$YPC2459)
maindata$YPC2459num<-ifelse(maindata$YPC2459==1, 1,
                            ifelse(maindata$YPC2459==0, 0,
                                   NA))
table(maindata$YPC2459num)
#YPC2460	l1k: Respondent is currently other
table(maindata$YPC2460)
maindata$YPC2460num<-ifelse(maindata$YPC2460==1, 1,
                            ifelse(maindata$YPC2460==0, 0,
                                   NA))
table(maindata$YPC2460num)
#YPC2461	DV: Answers to 'other' (YPC2460) grouped into categories (e.g., part-time education, PGCE, full-time parent, etc.)
table(maindata$YPC2461)
maindata$YPC2461num<-ifelse(maindata$YPC2461==-1, NA,
                            maindata$YPC2461)
table(maindata$YPC2461num)

#Code employed at 23 variables

#If employed or in full time education = 1 (full time employment, part time employment, apprenticeship, full time education, self employed, employed but looking for other work, maternity leave, PhD, teacher training, waiting to start new job/education, student nurse)
#If unemployed or taking unpaid work = 0 (occasional work, unemployed and looking for work, sickness/disability/voluntary work/carer/internship/placement student/part time education, stay at home parent, seasonal job, currently travelling, homemaker)

#employed = full or part time paid work (incl maternity leave), self employed or in full time education (incl PhD and teacher training) and student nurse 
#unemployed
maindata$employed23<-ifelse(maindata$YPC2450==1|maindata$YPC2451==1|maindata$YPC2453==1|maindata$YPC2456==1|maindata$YPC2458==1|maindata$YPC2461==3|maindata$YPC2461==5|maindata$YPC2461==6|maindata$YPC2461==10|maindata$YPC2461==14, 1,
                            ifelse(maindata$YPC2452==1|maindata$YPC2453==1|maindata$YPC2454==1|maindata$YPC2455==1|maindata$YPC2457==1|maindata$YPC2459==1|maindata$YPC2461==4|maindata$YPC2461==7|maindata$YPC2461==8|maindata$YPC2461==9|maindata$YPC2461==11|maindata$YPC2461==12|maindata$YPC2461==13|maindata$YPC2461==15,0,
                                   NA))
table(maindata$employed23)
#0 = occasional work, apprentice (no change), unemployed, sick/disabled, voluntary work, carer, internship, placement student (no change), part time education, seasonal job (no change), currently travelling, waiting to start a new job/education, homemaker
#1 = full time paid work, part time paid work, full time education, self employed, employed but looking for other (no change), maternity leave, PhD, teacher training, student nurse (no change)
#0    1 
#333 3544 


#----------------------------------------------------
#b. Adult socio-economic predictors: income at age 23
#----------------------------------------------------

summary(maindata$YPC2510)
table(maindata$YPC2510)

#check what those in full time education are filling in
table(maindata$YPC2456)
#-9999   -10    -8    -1     0     1 
#19 11323     5   963  2654   481 
table(maindata$YPC2456num, maindata$YPC2510)
#                  -9999  -10   -1    0    1    2    3    4    5    6    7
#in full time edu:     0    0  240   15  138   48   27   10    1    0    0
#some of those in full time education still has income, some others did not fill in (but we don't know if this is because their income is 0 or they just didn't fill in)

#New variable = incomeMP (for income mid point)
#Need to recode YPC2510 from levels into values in incomeMP
#0 incomeMP = 0
#£1 - £499 incomeMP = 250
#£500 - £999 incomeMP = 749.5
#£1000 - £1499 incomeMP = 1,249.5
#£1500 - £1999 incomeMP = 1,749.5
#£2000 - £2499 incomeMP = 2,249.5
#£2500 - £2999 incomeMP = 2,749.5
#£3000 and above incomeMP = 4,500
#Layard, R., Nickell, S.J. and Mayraz, G. (2008). 'The marginal utility of income'
#" For those in the lowest income band we assumed an income of two thirds of the
#  upper limit of the band, and for respondents in the highest income band
#  we assumed an income of 1.5 of the lower income limit of the band."

maindata$incomeMP <- ifelse(maindata$YPC2510 == 0, 0, 
                            ifelse(maindata$YPC2510 == 1, 250, 
                                   ifelse(maindata$YPC2510 == 2, 749.5,
                                          ifelse(maindata$YPC2510 == 3, 1249.5,
                                                 ifelse(maindata$YPC2510 == 4, 1749.5,
                                                        ifelse(maindata$YPC2510 == 5, 2249.5,
                                                               ifelse(maindata$YPC2510 == 6, 2749.5,
                                                                      ifelse(maindata$YPC2510 == 7, 4500, 
                                                                             NA))))))))


head(maindata$incomeMP, n=20)
head(maindata$YPC2510, n=20)
table(maindata$incomeMP)
#0    250  749.5 1249.5 1749.5 2249.5 2749.5   4500 
#44    360    565   1372    714    185     46     19 
table(maindata$YPC2510)
summary(maindata$incomeMP) #11938NAs

#If no to full time or part time paid employment, then income = 0
maindata$incomeMP2<-ifelse(is.na(maindata$incomeMP)&maindata$YPC2450num==0&maindata$YPC2451num==0, 0,
                           maindata$incomeMP)
summary(maindata$incomeMP2) #11489NA's
#If all other categories are 0 and unemployed = 1, then income = 0
maindata$incomeMP3<-ifelse(is.na(maindata$incomeMP2)&is.na(maindata$YPC2450num)&is.na(maindata$YPC2451num)&is.na(maindata$YPC2452num)&is.na(maindata$YPC2453num)&maindata$YPC2454num==1&is.na(maindata$YPC2455num)&is.na(maindata$YPC2456num)&is.na(maindata$YPC2457num)&is.na(maindata$YPC2458num)&is.na(maindata$YPC2459num)&is.na(maindata$YPC2461num), 0,
                           maindata$incomeMP2)
summary(maindata$incomeMP3) #11465NA's

#Assume that those who have not put that they are in paid employment have a salary of 0
test<-maindata[,c("YPC2450num", "YPC2451num", "YPC2452num", "YPC2453num", "YPC2454num", "YPC2455num", "YPC2456num", "YPC2457num", "YPC2458num", "YPC2459num", "YPC2460num", "YPC2461num", "incomeMP", "incomeMP2", "incomeMP3")]
test2<-maindata[is.na(maindata$incomeMP3),c("YPC2450num", "YPC2451num", "YPC2452num", "YPC2453num", "YPC2454num", "YPC2455num", "YPC2456num", "YPC2457num", "YPC2458num", "YPC2459num", "YPC2460num", "YPC2461num", "incomeMP", "incomeMP2", "incomeMP3")]

#Using the natural logarithm of income  - allow a given percentage change in income to have the same effect on absolute wellbeing whether the person is rich or poor
#Previous research has shown that log income is the best predictor of absolute life satisfaction (Layard, R., Nickell, S.J. and Mayraz, G. (2008). 'The marginal utility of income')
#log incomeMP
maindata$adultloginc<-log(maindata$incomeMP3+1)
hist(maindata$incomeMP)
hist(maindata$adultloginc)
summary(maindata$adultloginc)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.000   6.621   7.131   6.034   7.468   8.412   11465 


#-----------------------------------------------------------
#c. Adult socio-economic predictors: educational achievement
#-----------------------------------------------------------

#1. identify all those with a degree level qualification at age 20, AQ = 4

summary(maindata$CCU4000) #E1a) Degree-level qualification including foundation degrees, graduate membership of a professional institute, PGCE, or higher == level 4
table(maindata$CCU4000)
sum(!is.na(maindata$CCU4000)) #3947
summary(maindata$CCU4001) #E1b) HNC/HND == level 4
summary(maindata$CCU4002) #E1c) ONC/OND  == level 3
summary(maindata$CCU4003) #E1d) BTEC/EdExcel/LQL #All NA's: E1d: BTEC/EdExcel/LQL qualification (level 3)
summary(maindata$CCU4004) #E1e) SCOTVEC, SCOTEC or SCOTBEC == level 3
summary(maindata$CCU4005) #E1f) Teaching qualification (excluding PGCE) == level 3
summary(maindata$CCU4006) #E1g) Nursing or other medical qualification not yet mentioned ==level 3
summary(maindata$CCU4007) #E1h) A-level/Vocational A-level/GCE in applied subjects or equivalents == level 3
summary(maindata$CCU4008) #E1i) New Diploma == level 3
summary(maindata$CCU4009) #E1j) Welsh Baccalaureate == level 3
summary(maindata$CCU4010) #E1k) International Baccalaureate == level 3
summary(maindata$CCU4011) #E1l) NVQ/SVQ == level 3 ***depends on which level though
summary(maindata$CCU4012) #E1m) GNVQ/GSVQ == level 3 ***depends on which level though
summary(maindata$CCU4013) #E1n) AS-level/Vocational AS-level or equivalent  == level 2
summary(maindata$CCU4014) #E1o) Access to HE  == level 3
summary(maindata$CCU4015) #E1p) Standard Grade (Scotland)  == level 2
summary(maindata$CCU4016) #E1q) GCSE/Vocational GCSE or equivalent == level 2
summary(maindata$CCU4017) #E1r) Advanced Higher/Higher (Scotland) == level 2
summary(maindata$CCU4018) #E1s) Intermediate/Access qualifications. (Scotland) == level 2
summary(maindata$CCU4019) #E1t) RSA/OCR == level 1
summary(maindata$CCU4020) #E1u) City & Guilds == level 1 ***depends on which level though
summary(maindata$CCU4021) #E1v) Key Skills/Core Skills (Scotland) == level 1
summary(maindata$CCU4022) #E1w) Basic Skills (Skills for life/literacy/numeracy/language) == level 1
summary(maindata$CCU4023) #E1x) Entry-Level Qualifications == level 1
summary(maindata$CCU4024) #E1y) Any other professional/work-related qualification/foreign qualifications ***???

#First group variable responses into 4 level variables with yes=1/no=0 response
#But leaving out the unknown *** ones for now to check which group they are most likely to fall into
table(maindata$CCU4021)
maindata$AQlevel1 <- ifelse(maindata$CCU4023 == 1|maindata$CCU4022 == 1|maindata$CCU4021 == 1|maindata$CCU4019 == 1, 1,
                            ifelse(maindata$CCU4023 == 2&maindata$CCU4022 == 2&maindata$CCU4021 == 2&maindata$CCU4019 == 2, 0, 
                                   NA))
check3<-maindata[,c("CCU4023", "CCU4022", "CCU4021", "CCU4020", "CCU4019", "AQlevel1")]
head(check3, n=50)
#It seems that when CCU4021 answers yes, CCU4020 answers yes and same for no

maindata$AQlevel2 <- ifelse(maindata$CCU4018 == 1|maindata$CCU4017 == 1|maindata$CCU4016 == 1|maindata$CCU4015 == 1|maindata$CCU4013 == 1, 1,
                            ifelse(maindata$CCU4018 == 2&maindata$CCU4017 == 2&maindata$CCU4016 == 2&maindata$CCU4015 == 2&maindata$CCU4013 == 2, 0, 
                                   NA))
check2<-maindata[,c("CCU4018", "CCU4017", "CCU4016", "CCU4015", "CCU4013", "AQlevel2", "CCU4024", "CCU4012", "CCU4011")]
head(check2, n=50)

maindata$AQlevel3 <- ifelse(maindata$CCU4014 == 1|maindata$CCU4010 == 1|maindata$CCU4009 == 1|maindata$CCU4008 == 1|maindata$CCU4007 == 1|maindata$CCU4006 == 1|
                              maindata$CCU4005 == 1|maindata$CCU4004 == 1|maindata$CCU4002 == 1, 1,
                            ifelse(maindata$CCU4014 == 2&maindata$CCU4010 == 2&maindata$CCU4009 == 2&maindata$CCU4008 == 2&maindata$CCU4007 == 2&maindata$CCU4006 == 2&
                                     maindata$CCU4005 == 2&maindata$CCU4004 == 2&maindata$CCU4002 == 2, 0, 
                                   NA))

check2<-maindata[,c("CCU4014", "CCU4010", "CCU4009", "CCU4008", "CCU4007", "CCU4006", "CCU4005", "CCU4004", "CCU4003", "CCU4002", "AQlevel3")]
head(check2, n=50)

maindata$AQlevel4 <- ifelse(maindata$CCU4000 == 1|maindata$CCU4001 == 1, 1,
                            ifelse(maindata$CCU4000 == 2 &maindata$CCU4001 == 2, 0,
                                   NA))
check2<-maindata[,c("CCU4000", "CCU4001", "AQlevel4")]
head(check2, n=50)
table(maindata$AQlevel4)


#Create a final education variable
maindata$AQ<-ifelse(maindata$AQlevel1==1, 1,
                    ifelse(maindata$AQlevel1==0,0,
                           NA))
head(maindata$AQ, n=40)
head(maindata$AQlevel1, n=40)
tail(maindata$AQ, n=40)
tail(maindata$AQlevel1, n=40)
maindata$AQ<-ifelse(maindata$AQlevel2==1, 2,
                    maindata$AQ)
maindata$AQ<-ifelse(maindata$AQlevel3==1, 3,
                    maindata$AQ)
maindata$AQ<-ifelse(maindata$AQlevel4==1, 4,
                    maindata$AQ)
table(maindata$AQ)
#0    1    2    3    4 
#61   36  487 2191 1049 

#2. For those who have AQ 0-3, did they get awarded a university place at 18?

table(maindata$cct3002)
#-9999   -10    -2    -1     1(yes)     2(no) 
#   19 12055  1422    59     1437        453

maindata$AQguess<-ifelse(maindata$AQ==0&maindata$cct3002==1,4,
                         ifelse(maindata$AQ==1&maindata$cct3002==1,4,
                                ifelse(maindata$AQ==2&maindata$cct3002==1,4,
                                       ifelse(maindata$AQ==3&maindata$cct3002==1,4,
                                              ifelse(is.na(maindata$AQ)&maindata$cct3002==1,4,
                                                     maindata$AQ)))))
table(maindata$AQguess)
#0    1    2    3    4 
#56   35  473 1586 1710
maindata$AQguess2<-ifelse(is.na(maindata$AQlevel1)&maindata$AQlevel2==1&maindata$AQlevel3==1&is.na(maindata$AQlevel4)&is.na(maindata$AQ)&maindata$cct3002==2, 3,
                          ifelse(is.na(maindata$AQ)&maindata$cct3002==1, 4,
                                 ifelse(maindata$AQlevel1==1&maindata$AQlevel2==1&is.na(maindata$AQlevel3)&is.na(maindata$AQlevel4)&is.na(maindata$AQ)&maindata$cct3002==-2, 2,
                                        ifelse(maindata$AQlevel1==1&maindata$AQlevel2==1&is.na(maindata$AQlevel3)&is.na(maindata$AQlevel4)&is.na(maindata$AQ)&maindata$cct3002==-10, 2,
                                               ifelse(maindata$AQlevel1==1&maindata$AQlevel2==1&is.na(maindata$AQlevel3)&is.na(maindata$AQlevel4)&is.na(maindata$AQ)&maindata$cct3002==-9999, 2,
                                                      ifelse(is.na(maindata$AQlevel1)&maindata$AQlevel2==1&maindata$AQlevel3==1&is.na(maindata$AQlevel4)&is.na(maindata$AQ)&maindata$cct3002==-2, 3,
                                                             ifelse(is.na(maindata$AQlevel1)&maindata$AQlevel2==1&maindata$AQlevel3==1&is.na(maindata$AQlevel4)&is.na(maindata$AQ)&maindata$cct3002==-10, 3,
                                                                    ifelse(is.na(maindata$AQlevel1)&maindata$AQlevel2==1&maindata$AQlevel3==1&is.na(maindata$AQlevel4)&is.na(maindata$AQ)&maindata$cct3002==-9999, 3,
                                                                           maindata$AQguess))))))))

test<-maindata[,c("AQlevel1", "AQlevel2", "AQlevel3", "AQlevel4", "AQ", "cct3002", "AQguess", "AQguess2", "CCU4024")]
table(maindata$AQguess2)
#0    1    2    3    4 
#56   35  501 1650 2032 
#higher number is higher level of education


#--------------------------------------------------
#d. Adult socio-economic predictors:married/partner
#--------------------------------------------------

#Marrried or civil partnership or living with partner at least 1 year
table(maindata$YPC0370)
#yes=1, no=0
maindata$adultpartner <- ifelse(maindata$YPC0370 == 1, 1, 
                                ifelse(maindata$YPC0370 == 0, 0, 
                                       NA))
table(maindata$adultpartner)
#0    1 
#2632 1049 
#1 is partnered


#---------------------------------------
#e. Adult behavioural predictors:conduct
#---------------------------------------

#Antisocial behaviour questionnaire at age 22 (antisocial behaviour over the last year)
#Total frequency score
summary(maindata$YPB4492)
table(maindata$YPB4492)
maindata$YPB4492num<-ifelse(maindata$YPB4492<0, NA, maindata$YPB4492)
table(maindata$YPB4492num)
hist(maindata$YPB4492num) #higher score means more antisocial
maindata$adultantis<-maindata$YPB4492num
table(maindata$adultantis) #higher number means more antisocial
#   0    1    2    3    4    5    6    7    8   11   12   13 
#3345  238  120   61   26    8   12    4    1    1    3    1 


#------------------------------------------------
#f. Adult health predictors:Self-perceived health
#------------------------------------------------

#General Health subscale raw
table(maindata$YPB1000) #higher score means worse health
maindata$selfhealthR<-ifelse(maindata$YPB1000<0, NA,
                             maindata$YPB1000)
table(maindata$selfhealthR)
maindata$selfhealth<-5-maindata$selfhealthR
table(maindata$selfhealth) #higher score means better health
summary(maindata$selfhealth)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.000   2.000   3.000   2.807   3.000   4.000   11496 


#-------------------------------------------
#g. Adult health predictors:Emotional health
#-------------------------------------------

#MFQ total score
summary(maindata$YPB5180) #min=0, max=26
table(maindata$YPB5180)
maindata$YPB5180num<-ifelse(maindata$YPB5180<0, NA,
                            maindata$YPB5180)
table(maindata$YPB5180num)
str(maindata$YPB5180num)
hist(maindata$YPB5180num) #higher score = more depressed
#Higher score = more depression
maindata$adultemohealth<-maindata$YPB5180num
hist(maindata$adultemohealth)
table(maindata$adultemohealth) #higher score means more depressed, more "emotional problems"

#--------------------#
#-Data distributions-#
#--------------------#

table(maindata$employed23) #a) Adult socio-economic predictors - adult employment/at university; dummy #employed = 1
hist(maindata$adultloginc) #b) Adult socio-economic predictors - income; continuous #higher score = more income
hist(maindata$AQguess2) #c) Adult socio-economic predictors - educational achievement; continuous integer #higher score = higher academic qualification
table(maindata$adultpartner) #d) Adult socio-economic predictors - adult living in marriage or with partner; dummy #partner = 1
hist(maindata$adultantis) #e) Adult behavioural predictors - antisocial behaviour; continuous #higher score = more antisocial
hist(maindata$selfhealth) #f) Adult health predictors - self-perceived health using SF General Health subscale; continuous #higher = better health
hist(maindata$adultemohealth) #g) Adult emotional health predictors - composite of MFQ ; continuous #higher score = more depressed


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~Preparing Child Predictors~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~Child socio-economic predictors~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#---------------------------------
#a. Child Intellectual Predictors
#---------------------------------

#The WISC (age 8) and WASI (age 15.5) assessed in the clinic - total IQ

describe(maindata$f8ws112)
describe(maindata$fh6280)

table(maindata$f8ws112)
maindata$f8ws112num<-ifelse(maindata$f8ws112<0, NA,
                            maindata$f8ws112)
table(maindata$f8ws112num)
summary(maindata$f8ws112num)

table(maindata$fh6280)
maindata$fh6280num<-ifelse(maindata$fh6280<0, NA,
                           maindata$fh6280)
table(maindata$fh6280num)

maindata$childIQ <- maindata$f8ws112num
head(maindata$f8ws112, n=20)
head(maindata$fh6280, n=20)
head(maindata$childIQ, n=20)
summary(maindata$childIQ)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   45.0    93.0   104.0   104.1   116.0   151.0    7990 
test<-maindata[,c("f8ws112", "fh6280", "childIQ")]
tail(test,n=50)
summary(test)
hist(maindata$childIQ)

#--------------------------------#
#--Child behavioural predictors--#
#--------------------------------#

#----------------------------
#b. Child conduct predictors
#----------------------------

#Child conduct predictors = SDQ taken from maindata
dim(maindata) #15445 x 1510
#At ages 3, 6, 9, 11, 13, 16

# Make an externalising problems score from subscales from the SDQ conduct and SDQ hyperactivity subscales 
# The externalising score ranges from 0 to 20 and is the sum of the conduct and hyperactivity scales.

#Age 3
summary(maindata$kj644) #Conduct difficulties score (RR) Age 42months #10769 NA's
sum(!is.na(maindata$kj644)) #10053 not na
describe(maindata$kj644)
table(maindata$kj644)
maindata$kj644clean <- ifelse(maindata$kj644 <0 , NA, 
                              maindata$kj644)
table(maindata$kj644)
table(maindata$kj644clean)
summary(maindata$kj645) #Hyperactivity score (RR) Age 42months #10769 NAs
sum(!is.na(maindata$kj645)) #10047 not na
table(maindata$kj645)
maindata$kj645clean <- ifelse(maindata$kj645 < 0, NA, 
                              maindata$kj645)
table(maindata$kj645)
table(maindata$kj645clean)
maindata$Age3ext<-maindata$kj644clean+maindata$kj645clean
table(maindata$Age3ext)
summary(maindata$Age3ext) #5427na's

#Age 9
summary(maindata$ku706a) #DV: SDQ - Hyperactivity score (complete cases) Age 9 years #5674 na's
table(maindata$ku706a)
maindata$ku706aclean <- ifelse(maindata$ku706a < 0 , NA, 
                               maindata$ku706a)
table(maindata$ku706aclean)
summary(maindata$ku708a) #DV: SDQ - Conduct problems score (complete cases) Age 9 years #5674 na's
table(maindata$ku708a)
maindata$ku708aclean <- ifelse(maindata$ku708a < 0, NA, 
                               maindata$ku708a)
table(maindata$ku708aclean)
maindata$Age9ext<-maindata$ku706aclean+maindata$ku708aclean
summary(maindata$Age9ext) #5674 na's
test<-maindata[, c("ku706a", "ku708a", "Age9ext")]
head(test, n=20)

#Age 13 Conduct
table(maindata$ta7004) #I5: Teenager has often had temper tantrums or hot tempers
maindata$ta7004num<-ifelse(maindata$ta7004==1, 0,
                           ifelse(maindata$ta7004==2, 1,
                                  ifelse(maindata$ta7004==3, 2,
                                         NA)))
table(maindata$ta7004num)
table(maindata$ta7006) #I7: Teenager is generally obedient, usually does what adults request
maindata$ta7006numR<-ifelse(maindata$ta7006==1, 2,
                            ifelse(maindata$ta7006==2, 1,
                                   ifelse(maindata$ta7006==3, 0,
                                          NA)))
table(maindata$ta7006numR)
table(maindata$ta7011) #I12: Teenager often fights or bullies other children/teenagers
maindata$ta7011num<-ifelse(maindata$ta7011==1, 0,
                           ifelse(maindata$ta7011==2, 1,
                                  ifelse(maindata$ta7011==3, 2,
                                         NA)))
table(maindata$ta7011num)
table(maindata$ta7017) #I18: Teenager often lies or cheats
maindata$ta7017num<-ifelse(maindata$ta7017==1, 0,
                           ifelse(maindata$ta7017==2, 1,
                                  ifelse(maindata$ta7017==3, 2,
                                         NA)))
table(maindata$ta7017num)
table(maindata$ta7021) #I22: Teenager steels from home, school, elsewhere
maindata$ta7021num<-ifelse(maindata$ta7021==1, 0,
                           ifelse(maindata$ta7021==2, 1,
                                  ifelse(maindata$ta7021==3, 2,
                                         NA)))
table(maindata$ta7021num)
#higher score means higher conduct problems
maindata$Age13conduct<-maindata$ta7004num+maindata$ta7006numR+maindata$ta7011num+maindata$ta7017num+maindata$ta7021num
table(maindata$Age13conduct)


#Age 13 Hyperactivity
table(maindata$ta7001) #I2: Teenager has been restless, overactive and can't stay still for long
maindata$ta7001num<-ifelse(maindata$ta7001==1, 0,
                           ifelse(maindata$ta7001==2, 1,
                                  ifelse(maindata$ta7001==3, 2,
                                         NA)))
table(maindata$ta7001num)
summary(maindata$ta7009) #I10: Teenager is constantly fidgeting or squirming
maindata$ta7009num<-ifelse(maindata$ta7009==1, 0,
                           ifelse(maindata$ta7009==2, 1,
                                  ifelse(maindata$ta7009==3, 2,
                                         NA)))
table(maindata$ta7009num)
summary(maindata$ta7014) #I15: Teenager is easily distracted, concentration wanders
maindata$ta7014num<-ifelse(maindata$ta7014==1, 0,
                           ifelse(maindata$ta7014==2, 1,
                                  ifelse(maindata$ta7014==3, 2,
                                         NA)))
table(maindata$ta7014num)
summary(maindata$ta7020) #I21: He thinks thjings out before acting
maindata$ta7020numR<-ifelse(maindata$ta7020==1, 2,
                            ifelse(maindata$ta7020==2, 1,
                                   ifelse(maindata$ta7020==3, 0,
                                          NA)))
table(maindata$ta7020numR)
summary(maindata$ta7024) #I25: Teenager sees tasks through to end, has good attention span
maindata$ta7024numR<-ifelse(maindata$ta7024==1, 2,
                            ifelse(maindata$ta7024==2, 1,
                                   ifelse(maindata$ta7024==3, 0,
                                          NA)))
table(maindata$ta7024numR)
#higher score means higher hyperactivity problems
maindata$Age13hyperact<-maindata$ta7001num+maindata$ta7009num+maindata$ta7014num+maindata$ta7020numR+maindata$ta7024numR
summary(maindata$Age13hyperact) #13985 Na's
table(maindata$Age13hyperact) #6831 responses

#Age 13 externalising problems
maindata$Age13ext<-maindata$Age13conduct+maindata$Age13hyperact #higher score means more externalising problems
test<-maindata[, c("Age13conduct", "Age13hyperact", "Age13ext")]
head(test, n=20)
summary(maindata$Age13ext)

#Take a composite (average) of the scaled values of externalising variables across ages 3, 6, 9, 11, 13 and 16
maindata$zAge3ext<-scale(maindata$Age3ext)
maindata$zAge9ext<-scale(maindata$Age9ext)
maindata$zAge13ext<-scale(maindata$Age13ext)

#Complete cases composite
maindata$childextR <- rowMeans(maindata[,c("zAge3ext", "zAge9ext", "zAge13ext")])
test<-maindata[,c("zAge3ext", "zAge9ext", "zAge13ext", "childextR")]
summary(maindata$childextR)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   -1.470  -0.649  -0.161  -0.055   0.376   3.971   10311 
describe(maindata$childextR)
hist(maindata$childextR) #higher score means more externalising problems
maindata$childext<-maindata$childextR #now higher score means more externalising
summary(maindata$childext)
hist(maindata$childext)
describe(maindata$childext)
#vars    n mean  sd median trimmed  mad  min  max range skew kurtosis   se
#   1 4932 -0.06 0.8  -0.16   -0.12 0.76 -1.47 3.97  5.44  0.9     1.04 0.01

#---------------------------#
#--Child health predictors--#
#---------------------------#

#-----------------------------
#C. Child emotional predictors
#-----------------------------

#SDQ emotional subscale - age 3, 9, 13 
#Age 3: no peer problem score
#Age 4-18
#The externalising score ranges from 0 to 20 and is the sum of the conduct and hyperactivity scales. 
#The internalising score ranges from 0 to 20 and is the sum of the emotional and peer problems scales. 
#Using these two amalgamated scales may be preferable to using the four separate scales in community samples, whereas using the
#four separate scales may add more value in high-risk samples (see Goodman & Goodman. 2009 Strengths and difficulties questionnaire as a dimensional measure of child mental health. JAm Acad Child Adolesc Psychiatry 48(4), 400-403).

#Age 3
summary(maindata$kj643) #Emotional difficulties
table(maindata$kj643)
maindata$Age3emodiff <- ifelse(maindata$kj643 < 0, NA, 
                               maindata$kj643)
table(maindata$Age3emodiff) #High score means more emotional difficulty
hist(maindata$Age3emodiff)

#Age 9
#The internalising score ranges from 0 to 20 and is the sum of the emotional and peer problems scales. 
summary(maindata$ku707a) #Emotional symptoms (complete cases)
table(maindata$ku707a)
maindata$Age9emodiff <- ifelse(maindata$ku707a < 0, NA, 
                               maindata$ku707a)
table(maindata$Age9emodiff) #High score means more emotional difficulty
hist(maindata$Age9emodiff)
summary(maindata$ku709a) #Peer Problems  (complete cases)
table(maindata$ku709a)
maindata$Age9peerdiff <- ifelse(maindata$ku709a < 0, NA, 
                                maindata$ku709a)
table(maindata$Age9peerdiff) #High score means more peer problems
hist(maindata$Age9peerdiff)
head(maindata[, c("ku707a", "ku709a")], n=10)
maindata$Age9int<-maindata$Age9peerdiff+maindata$Age9emodiff
head(maindata[, c("ku707a", "ku709a", "Age9int")], n=10)
hist(maindata$Age9int)

#Age 13
#The internalising score ranges from 0 to 20 and is the sum of the emotional and peer problems scales. 
#Emotional symptoms of SDQ - F10 (worries many things), F11 (solitary), F14 (miserable tearful), F20 (afraid new things), F33 (cries easily), F42 (stares blankly)
#Recode missing NA, 1=0, 2=1, 3=2
#Complains headaches ta7002
summary(maindata$ta7002)
table(maindata$ta7002)
maindata$ta7002num<-ifelse(maindata$ta7002==1, 0,
                           ifelse(maindata$ta7002==2, 1,
                                  ifelse(maindata$ta7002==3, 2,
                                         NA)))
table(maindata$ta7002num)
summary(maindata$ta7002num)
#worries (ta7007)
summary(maindata$ta7007)
table(maindata$ta7007)
maindata$ta7007num<-ifelse(maindata$ta7007==1, 0,
                           ifelse(maindata$ta7007==2, 1,
                                  ifelse(maindata$ta7007==3, 2,
                                         NA)))
table(maindata$ta7007num)
summary(maindata$ta7007num)
#often unhappy (ta7012)
summary(maindata$ta7012)
table(maindata$ta7012)
maindata$ta7012num<-ifelse(maindata$ta7012==1, 0,
                           ifelse(maindata$ta7012==2, 1,
                                  ifelse(maindata$ta7012==3, 2,
                                         NA)))
table(maindata$ta7012num)
summary(maindata$ta7012num)
#nervous or clingy (ta7015)
summary(maindata$ta7015)
table(maindata$ta7015)
maindata$ta7015num<-ifelse(maindata$ta7015==1, 0,
                           ifelse(maindata$ta7015==2, 1,
                                  ifelse(maindata$ta7015==3, 2,
                                         NA)))
table(maindata$ta7015num)
summary(maindata$ta7015num)
#many fears easily scared (ta7023)
summary(maindata$ta7023)
table(maindata$ta7023)
maindata$ta7023num<-ifelse(maindata$ta7023==1, 0,
                           ifelse(maindata$ta7023==2, 1,
                                  ifelse(maindata$ta7023==3, 2,
                                         NA)))
table(maindata$ta7023num)
summary(maindata$ta7023num)
#Emotional difficulty score age 13
maindata$Age13emodiff<-rowSums(maindata[,c("ta7002num", "ta7007num", "ta7012num", "ta7015num", "ta7023num")])
summary(maindata$Age13emodiff)
hist(maindata$Age13emodiff)
table(maindata$Age13emodiff)
head(maindata[,c("ta7002num", "ta7007num", "ta7012num", "ta7015num", "ta7023num", "Age13emodiff")], n=10)

#Peer problems
#ITEM 6: Rather solitary, tends to play alone (I am usually on my own) 0 1 2 ta7005, ALSPAC:1=change scoring to:0, 2=1, 3=2
summary(maindata$ta7005)
table(maindata$ta7005)
maindata$ta7005num<-ifelse(maindata$ta7005==1, 0,
                           ifelse(maindata$ta7005==2, 1,
                                  ifelse(maindata$ta7005==3, 2,
                                         NA)))
table(maindata$ta7005num)
summary(maindata$ta7005num)
#ITEM 11: Has at least one good friend (I have one goof friend or more) 2 1 0 ta7010 1=2, 2 = 1, 3 = 0
summary(maindata$ta7010)
table(maindata$ta7010)
maindata$ta7010num<-ifelse(maindata$ta7010==1, 2,
                           ifelse(maindata$ta7010==2, 1,
                                  ifelse(maindata$ta7010==3, 0,
                                         NA)))
table(maindata$ta7010num)
summary(maindata$ta7010num)
#ITEM 14: Generally liked by other children (Other people my age generally like me) 2 1 0 ta7013 1=2, 2=1, 3=0
summary(maindata$ta7013)
table(maindata$ta7013)
maindata$ta7013num<-ifelse(maindata$ta7013==1, 2,
                           ifelse(maindata$ta7013==2, 1,
                                  ifelse(maindata$ta7013==3, 0,
                                         NA)))
table(maindata$ta7013num)
summary(maindata$ta7013num)
#ITEM 19: Picked on or bullied by other children… (Other children or young people pick on me) 0 1 2 ta7018 1 = 0, 2 = 1, 3 = 2
summary(maindata$ta7018)
table(maindata$ta7018)
maindata$ta7018num<-ifelse(maindata$ta7018==1, 0,
                           ifelse(maindata$ta7018==2, 1,
                                  ifelse(maindata$ta7018==3, 2,
                                         NA)))
table(maindata$ta7018num)
summary(maindata$ta7018num)
#ITEM 23: Gets on better with adults than with other children (I get on better with adults than with people my age) ta7022 1 = 0, 2 = 1, 3 = 2
summary(maindata$ta7022)
table(maindata$ta7022)
maindata$ta7022num<-ifelse(maindata$ta7022==1, 0,
                           ifelse(maindata$ta7022==2, 1,
                                  ifelse(maindata$ta7022==3, 2,
                                         NA)))
table(maindata$ta7022num)
summary(maindata$ta7022num)
#Peer problems score score age 13
maindata$Age13peer<-rowSums(maindata[,c("ta7005num", "ta7010num", "ta7013num", "ta7018num", "ta7022num")])
summary(maindata$Age13peer)
hist(maindata$Age13peer)
table(maindata$Age13peer) #higher score means higher peer problems
head(maindata[,c("ta7005num", "ta7010num", "ta7013num", "ta7018num", "ta7022num", "Age13peer")], n=10)

head(maindata[, c("Age13emodiff", "Age13peer")], n=10)
maindata$Age13int<-maindata$Age13peer+maindata$Age13emodiff
head(maindata[, c("Age13emodiff", "Age13peer", "Age13int")], n=10)
hist(maindata$Age13int) #higher score means more internalising problems 

#Composite internalising problems (SDQ) in childhood, higher score = more internalising problems
maindata$zAge3emodiff<-scale(maindata$Age3emodiff)
maindata$zAge9int<-scale(maindata$Age9int)
maindata$zAge13int<-scale(maindata$Age13int)

#Complete cases emotional health: SDQ from age 3 and 9 and 13
maindata$childemohealthR<-rowMeans(maindata[,c("zAge3emodiff", "zAge9int", "zAge13int")])
test<-maindata[,c("zAge3emodiff", "zAge9int", "zAge13int", "childemohealthR")]
head(test, n=10)
hist(maindata$childemohealthR) #higher score is more internalising problems
summary(maindata$childemohealthR)
describe(maindata$childemohealthR)
maindata$childemohealth<-(maindata$childemohealthR)
summary(maindata$childemohealth)
hist(maindata$childemohealth) #higher score means better emotional health
describe(maindata$childemohealth)
# vars    n mean   sd median trimmed mad  min  max range  skew kurtosis   se
#    1 4761 -0.05 0.72  -0.18   -0.13 0.64 -1.12 4.36  5.48 1.16     1.88 0.01

#-----------------------------
#D. Child physical predictors
#-----------------------------

#Age 3
summary(maindata$kj011) #3 #child's health in past year
#very healthy, no problems 1
#healthy, but a few minor problems 2
#sometimes quite ill 3
#almost always unwell 4
table(maindata$kj011)
maindata$Age3health <- ifelse(maindata$kj011 < 0, NA, 
                              maindata$kj011)
table(maindata$Age3health) #higher score means worse health


#Age 8
table(maindata$ks1001) #8 years
maindata$ks1001num<-ifelse(maindata$ks1001<0, NA,
                           maindata$ks1001)
table(maindata$ks1001num) #higher score means worse health


#Age 13
table(maindata$ta1001) #13 years
maindata$ta1001num<-ifelse(maindata$ta1001<0, NA,
                           maindata$ta1001)
table(maindata$ta1001num) #higher score means worse health


#Composite - mean of scores of health measures from at least 2 time points
maindata$childcurrenthealthR <- rowMeans(maindata[,c("Age3health", "ks1001num", "ta1001num")])
test<-maindata[,c("Age3health", "ks1001num", "ta1001num", "childcurrenthealthR")]
head(test, n=10)
summary(maindata$childcurrenthealthR)
describe(maindata$childcurrenthealthR)
maindata$childcurrenthealth<-5-maindata$childcurrenthealthR
summary(maindata$childcurrenthealth)
describe(maindata$childcurrenthealth) #higher score is better health
# vars    n mean   sd median trimmed  mad  min max range  skew kurtosis   se
#    1 5302 3.56 0.39   3.67    3.59 0.49 1.67   4  2.33 -0.64    -0.07 0.01
hist(maindata$childcurrenthealth)

#Checking the final childhood measures
hist(maindata$childIQ) #a childhood intellectual
hist(maindata$childext) #b childhood behavioural is more externalising problems
hist(maindata$childemohealth) #c childhood emotional health higher score is more internalising problems
hist(maindata$childcurrenthealth) #d childhood physical predictor; higher score is better health

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~Preparing Family Predictors~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~Family economic predictors~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#-------------------------
#a. Parental Social Class
#-------------------------

#Complete cases: take a average social class of mother and father at 18 weeks, and 3 years 11 months

#social class at 18 weeks
table(maindata$b_sc_m) #18 weeks gest
maindata$b_sc_mnum<-ifelse(maindata$b_sc_m<0, NA, maindata$b_sc_m)
table(maindata$b_sc_mnum)
table(maindata$pb_sc_p) #partner
maindata$pb_sc_pnum<-ifelse(maindata$pb_sc_p<0, NA, maindata$pb_sc_p)
table(maindata$pb_sc_pnum)
maindata$SC18wk<-rowMeans(maindata[,c("b_sc_mnum", "pb_sc_pnum")]) #higher number is higher social class
summary(maindata$SC18wk)

#Composite - mean of scores of parental social class measures from 18 weeks gest (mother and partner) and 3 years 11 months (mother and partner)
maindata$parentSC <- maindata$SC18wk
hist(maindata$parentSC) #higher score is higher social class
summary(maindata$parentSC)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   2.500   3.000   3.079   3.500   6.000    7243 
describe(maindata$parentSC)

#-----------------
#b. Family income
#-----------------

table(maindata$h470) #Family income per week #2 years 9 months - first time asked
#On average, about how much is the take home family income each week (include social benefits etc)?
#Not stated      other       <100  100 - 199  200 - 299  300 - 399       >400         dk       NA's 
#       900          0        765       1542       2480       1851       2095          0      10155 
#Layard, R., Nickell, S.J. and Mayraz, G. (2008). 'The marginal utility of income'
#" For those in the lowest income band we assumed an income of two thirds of the
#  upper limit of the band, and for respondents in the highest income band
#  we assumed an income of 1.5 of the lower income limit of the band."
maindata$h470num <- ifelse(maindata$h470 == 1, 66.66667, 
                           ifelse(maindata$h470 == 2, 149.5, 
                                  ifelse(maindata$h470 == 3, 249.5,
                                         ifelse(maindata$h470 == 4, 349.5,
                                                ifelse(maindata$h470 == 5, 600,
                                                       NA)))))
table(maindata$h470num)
summary(maindata$h470num)

summary(maindata$n8130)	#J9a: Average family take-home income per week #8 years
levels(maindata$n8130)
maindata$n8130num <- ifelse(maindata$n8130 == 1, 66.66667, 
                            ifelse(maindata$n8130 == 2, 149.5, 
                                   ifelse(maindata$n8130 == 3, 249.5,
                                          ifelse(maindata$n8130 == 4, 349.5,
                                                 ifelse(maindata$n8130 == 5, 600,
                                                        NA)))))
table(maindata$n8130num)
summary(maindata$n8130num)

summary(maindata$r9020)	#On average, about how much is the take-home family income each week (include social benefits etc.)? #11 years 2 months
levels(maindata$r9020)
maindata$r9020num <- ifelse(maindata$r9020 == 1, 80, 
                            ifelse(maindata$r9020 == 2, 154.5, 
                                   ifelse(maindata$r9020 == 3, 214.5,
                                          ifelse(maindata$r9020 == 4, 264.5,
                                                 ifelse(maindata$r9020 == 5, 324.5,
                                                        ifelse(maindata$r9020 == 6, 394.5,
                                                               ifelse(maindata$r9020 == 7, 454.5,
                                                                      ifelse(maindata$r9020 == 8, 519.5,
                                                                             ifelse(maindata$r9020 == 9, 679.5,
                                                                                    ifelse(maindata$r9020 == 10, 1200,
                                                                                           NA))))))))))
table(maindata$r9020num)
summary(maindata$r9020num)

#Composite - complete cases 2 years 9 months, 8 years and 11 years 2 months
maindata$famincome <- rowMeans(maindata[,c("h470num", "n8130num", "r9020num")])
hist(maindata$famincome) #higher score is higher income
#Logarithm of income
maindata$familyloginc<-log(maindata$famincome)
summary(maindata$familyloginc)
hist(maindata$familyloginc)
describe(maindata$familyloginc)
# vars    n mean   sd median trimmed  mad  min  max range skew kurtosis   se
#    1 4658 6.01 0.48   6.07    6.04 0.51 4.26 6.68  2.42 -0.7      0.3 0.01

#----------------------
#c. Number of siblings
#----------------------

table(maindata$YPC1160)
maindata$siblings<-ifelse(maindata$YPC1160<0, NA,
                          maindata$YPC1160)
table(maindata$siblings)
summary(maindata$siblings)

#------------------
#d. Father in work
#------------------

#f565 partner currently employed 8 months yes = 1, no = 2
summary(maindata$f565)
table(maindata$f565)
maindata$f565num <- ifelse(maindata$f565<0, NA,
                           ifelse(maindata$f565==2, 0,
                                  maindata$f565))
table(maindata$f565num) #0 = not employed, 1 = employed

#p3080 husband/partner currently employed 9 years 2 months yes = 1, no = 2
table(maindata$p3080)
maindata$p3080num <- ifelse(maindata$p3080<0, NA,
                            ifelse(maindata$p3080==2, 0,
                                   maindata$p3080))
table(maindata$p3080num) #0 = not employed, 1 = employed
summary(maindata$p3080num) #higher score = employment

#s3080 partner is currently employed 12 years 1 month #yes = 1, no = 2
table(maindata$s3080)
maindata$s3080num <- ifelse(maindata$s3080<0, NA,
                            ifelse(maindata$s3080==2, 0,
                                   maindata$s3080))
table(maindata$s3080num) #higher score = employment #0 = not employed, 1 = employed
summary(maindata$s3080num)

#Composite: complete cases
maindata$fatherwork<-rowSums(maindata[,c("f565num", "p3080num", "s3080num")]) 
summary(maindata$fatherwork) #yes = 1, no = 0 so great number = more times employed
table(maindata$fatherwork)
#   0    1    2    3 
#   55  119  563 4329 
hist(maindata$fatherwork) #higher score means more employment

#----------------------------------------------------
#e. Mother and Father highest academic qualification
#----------------------------------------------------

#Mother's highest qualification when child is 32 weeks gestation
table(maindata$c645a)
maindata$c645anum<-ifelse(maindata$c645a<0, NA, maindata$c645a)
table(maindata$c645anum)

#Father's highest qualification when child is 18 weeks gestation
table(maindata$pb325a)
maindata$pb325anum<-ifelse(maindata$pb325a<0, NA, maindata$pb325a)
table(maindata$pb325anum)

#Average family educational qualification
maindata$famedu<-rowMeans(maindata[,c("c645anum", "pb325anum")])
test<-maindata[,c("c645anum", "pb325anum", "famedu")]
head(test,n=10)
describe(maindata$famedu)
# vars    n mean   sd median trimmed  mad min max range  skew kurtosis   se
#    1 9260 3.14 1.15      3    3.16 1.48   1   5     4 -0.12    -0.83 0.01
hist(maindata$famedu) #higher score is higher education

#------------------------------------
#Parental Socioeconomic Composite
#------------------------------------

hist(maindata$parentSC) #higher score = higher social class
hist(maindata$familyloginc) #higher score = higher income
summary(maindata$siblings)
hist(maindata$siblings)
maindata$siblings<-22-maindata$siblings
hist(maindata$siblings) #higher score = less siblings (related to better outcomes)
hist(maindata$fatherwork) #higher score = more occasions (out of the three time points) where the father has been employed
hist(maindata$famedu) #higher score = higher family average education

maindata$zparentSC<-scale(maindata$parentSC)
hist(maindata$zparentSC) #higher score means higher social class
maindata$zfamilyloginc<-scale(maindata$familyloginc)
hist(maindata$zfamilyloginc) #higher score is higher income
maindata$zsiblings<-scale(maindata$siblings)
hist(maindata$zsiblings) #higher score means lower "sibling score" - reverse coding this so that it fits into economic composite - since lower SES is linked to more children
maindata$zfatherwork<-scale(maindata$fatherwork)
hist(maindata$zfatherwork) #higher score means more family at work 
maindata$zfamedu<-scale(maindata$famedu)
hist(maindata$zfamedu) #higher score means parental education is higher
maindata$econcomp<-rowMeans(maindata[,c("zparentSC", "zfamilyloginc", "zsiblings", "zfatherwork", "zfamedu")])
summary(maindata$econcomp)
hist(maindata$econcomp)
describe(maindata$econcomp)
#  vars   n mean   sd median trimmed  mad   min  max range  skew kurtosis   se
#    1 1225 0.14 0.35    0.2    0.17 0.29 -1.59 0.92  2.51 -1.21     2.43 0.01

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~Family psych-social predictors~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#----------------------------------------------------------
#a. Mother's emotional health
#----------------------------------------------------------

#Mother EPDS Age 18 weeks gest
summary(maindata$b370)
table(maindata$b370)
maindata$b370num<-ifelse(maindata$b370<0, NA, 
                         maindata$b370)
table(maindata$b370num)

#Mother EPDS Age 8 years

#D24 n6060	Sense of humour in past WK (1 = 0) (2 = 1) (3 = 2) (4 = 3)
summary(maindata$n6060)
maindata$EPDS1<-ifelse(maindata$n6060==1,0,
                       ifelse(maindata$n6060==2, 1,
                              ifelse(maindata$n6060==3, 2,
                                     ifelse(maindata$n6060==4, 3,
                                            NA))))
table(maindata$EPDS1)
#D25 n6061	Looked forward to things in past WK (1 = 0) (2 = 1) (3 = 2) (4 = 3)
summary(maindata$n6061)
maindata$EPDS2<-ifelse(maindata$n6061==1,0,
                       ifelse(maindata$n6061==2, 1,
                              ifelse(maindata$n6061==3, 2,
                                     ifelse(maindata$n6061==4, 3,
                                            NA))))
table(maindata$EPDS2)
#D26 n6062	Unnecessary self blame in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$n6062)
maindata$EPDS3<-ifelse(maindata$n6062==1,3,
                       ifelse(maindata$n6062==2, 2,
                              ifelse(maindata$n6062==3, 1,
                                     ifelse(maindata$n6062==4, 0,
                                            NA))))
table(maindata$EPDS3)
#D27 n6063	Unnecessary anxiety or worry in past WK (1 = 0) (2 = 1) (3 = 2) (4 = 3)
summary(maindata$n6063)
maindata$EPDS4<-ifelse(maindata$n6063==1,0,
                       ifelse(maindata$n6063==2, 1,
                              ifelse(maindata$n6063==3, 2,
                                     ifelse(maindata$n6063==4, 3,
                                            NA))))
table(maindata$EPDS4)
#D28 n6064	Unnecessary panic or fear in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$n6064)
maindata$EPDS5<-ifelse(maindata$n6064==1,3,
                       ifelse(maindata$n6064==2, 2,
                              ifelse(maindata$n6064==3, 1,
                                     ifelse(maindata$n6064==4, 0,
                                            NA))))
table(maindata$EPDS5)
#D29 n6065	Things getting too much in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$n6065)
maindata$EPDS6<-ifelse(maindata$n6065==1,3,
                       ifelse(maindata$n6065==2, 2,
                              ifelse(maindata$n6065==3, 1,
                                     ifelse(maindata$n6065==4, 0,
                                            NA))))
table(maindata$EPDS6)
#D30 n6066	Sleeping PROB due to sadness in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$n6066)
maindata$EPDS7<-ifelse(maindata$n6066==1,3,
                       ifelse(maindata$n6066==2, 2,
                              ifelse(maindata$n6066==3, 1,
                                     ifelse(maindata$n6066==4, 0,
                                            NA))))
table(maindata$EPDS7)
#D31 n6067	Sad or miserable in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$n6067)
maindata$EPDS8<-ifelse(maindata$n6067==1,3,
                       ifelse(maindata$n6067==2, 2,
                              ifelse(maindata$n6067==3, 1,
                                     ifelse(maindata$n6067==4, 0,
                                            NA))))
table(maindata$EPDS8)
#D32 n6068	Crying due to unhappiness in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$n6068)
maindata$EPDS9<-ifelse(maindata$n6068==1,3,
                       ifelse(maindata$n6068==2, 2,
                              ifelse(maindata$n6068==3, 1,
                                     ifelse(maindata$n6068==4, 0,
                                            NA))))
table(maindata$EPDS9)
#D33 n6069	Considered selfharm in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$n6069)
maindata$EPDS10<-ifelse(maindata$n6069==1,3,
                        ifelse(maindata$n6069==2, 2,
                               ifelse(maindata$n6069==3, 1,
                                      ifelse(maindata$n6069==4, 0,
                                             NA))))
table(maindata$EPDS10)

maindata$nEPDS<-maindata$EPDS1+maindata$EPDS2+maindata$EPDS3+maindata$EPDS4+maindata$EPDS5+maindata$EPDS6+maindata$EPDS7+maindata$EPDS8+maindata$EPDS9+maindata$EPDS10
summary(maindata$nEPDS)
hist(maindata$nEPDS) #higher score = more depression, less emotionally health


#Age 11 years

#D24 r4010	Sense of humour in past WK (1 = 0) (2 = 1) (3 = 2) (4 = 3)
summary(maindata$r4010)
maindata$EPDS1<-ifelse(maindata$r4010==1,0,
                       ifelse(maindata$r4010==2, 1,
                              ifelse(maindata$r4010==3, 2,
                                     ifelse(maindata$r4010==4, 3,
                                            NA))))
table(maindata$EPDS1)
#D25 r4011	Looked forward to things in past WK (1 = 0) (2 = 1) (3 = 2) (4 = 3)
summary(maindata$r4011)
maindata$EPDS2<-ifelse(maindata$r4011==1,0,
                       ifelse(maindata$r4011==2, 1,
                              ifelse(maindata$r4011==3, 2,
                                     ifelse(maindata$r4011==4, 3,
                                            NA))))
table(maindata$EPDS2)
#D26 r4012	Unnecessary self blame in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$r4012)
maindata$EPDS3<-ifelse(maindata$r4012==1,3,
                       ifelse(maindata$r4012==2, 2,
                              ifelse(maindata$r4012==3, 1,
                                     ifelse(maindata$r4012==4, 0,
                                            NA))))
table(maindata$EPDS3)
#D27 r4013	Unnecessary anxiety or worry in past WK (1 = 0) (2 = 1) (3 = 2) (4 = 3)
summary(maindata$r4013)
maindata$EPDS4<-ifelse(maindata$r4013==1,0,
                       ifelse(maindata$r4013==2, 1,
                              ifelse(maindata$r4013==3, 2,
                                     ifelse(maindata$r4013==4, 3,
                                            NA))))
table(maindata$EPDS4)
#D28 r4014	Unnecessary panic or fear in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$r4014)
maindata$EPDS5<-ifelse(maindata$r4014==1,3,
                       ifelse(maindata$r4014==2, 2,
                              ifelse(maindata$r4014==3, 1,
                                     ifelse(maindata$r4014==4, 0,
                                            NA))))
table(maindata$EPDS5)
#D29 r4015	Things getting too much in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$r4015)
maindata$EPDS6<-ifelse(maindata$r4015==1,3,
                       ifelse(maindata$r4015==2, 2,
                              ifelse(maindata$r4015==3, 1,
                                     ifelse(maindata$r4015==4, 0,
                                            NA))))
table(maindata$EPDS6)
#D30 r4016	Sleeping PROB due to sadness in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$r4016)
maindata$EPDS7<-ifelse(maindata$r4016==1,3,
                       ifelse(maindata$r4016==2, 2,
                              ifelse(maindata$r4016==3, 1,
                                     ifelse(maindata$r4016==4, 0,
                                            NA))))
table(maindata$EPDS7)
#D31 r4017	Sad or miserable in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$r4017)
maindata$EPDS8<-ifelse(maindata$r4017==1,3,
                       ifelse(maindata$r4017==2, 2,
                              ifelse(maindata$r4017==3, 1,
                                     ifelse(maindata$r4017==4, 0,
                                            NA))))
table(maindata$EPDS8)
#D32 r4018	Crying due to unhappiness in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$r4018)
maindata$EPDS9<-ifelse(maindata$r4018==1,3,
                       ifelse(maindata$r4018==2, 2,
                              ifelse(maindata$r4018==3, 1,
                                     ifelse(maindata$r4018==4, 0,
                                            NA))))
table(maindata$EPDS9)
#D33 r4019	Considered selfharm in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
summary(maindata$r4019)
maindata$EPDS10<-ifelse(maindata$r4019==1,3,
                        ifelse(maindata$r4019==2, 2,
                               ifelse(maindata$r4019==3, 1,
                                      ifelse(maindata$r4019==4, 0,
                                             NA))))
table(maindata$EPDS10)

maindata$rEPDS<-maindata$EPDS1+maindata$EPDS2+maindata$EPDS3+maindata$EPDS4+maindata$EPDS5+maindata$EPDS6+maindata$EPDS7+maindata$EPDS8+maindata$EPDS9+maindata$EPDS10
summary(maindata$rEPDS)
hist(maindata$rEPDS)

maindata$zb370<-scale(maindata$b370num)
maindata$znEPDS<-scale(maindata$nEPDS)
maindata$zrEPDS<-scale(maindata$rEPDS)
maindata$motheremo<-rowMeans(maindata[,c("zb370", "znEPDS", "zrEPDS")])
summary(maindata$motheremo)
dim(maindata)
describe(maindata$motheremo)
# vars    n  mean   sd median trimmed  mad   min  max range skew kurtosis   se
#     1 5625 -0.08 0.77  -0.21   -0.14 0.76 -1.24 3.83  5.07 0.81     0.42 0.01
hist(maindata$motheremo) #higher score = mother is more emotionally unhealthy

#Father's emotional health

table(maindata$pb261) #18 weeks gest
maindata$pb261num<-ifelse(maindata$pb261<0, NA, maindata$pb261)
table(maindata$pb261num) #higher score = more depressed

#8 years

#D24 n6060	Sense of humour in past WK (1 = 0) (2 = 1) (3 = 2) (4 = 3)
table(maindata$pl6060)
maindata$EPDS1<-ifelse(maindata$pl6060==1,0,
                       ifelse(maindata$pl6060==2, 1,
                              ifelse(maindata$pl6060==3, 2,
                                     ifelse(maindata$pl6060==4, 3,
                                            NA))))
table(maindata$EPDS1)

#D25 n6061	Looked forward to things in past WK (1 = 0) (2 = 1) (3 = 2) (4 = 3)
table(maindata$pl6061)
maindata$EPDS2<-ifelse(maindata$pl6061==1,0,
                       ifelse(maindata$pl6061==2, 1,
                              ifelse(maindata$pl6061==3, 2,
                                     ifelse(maindata$pl6061==4, 3,
                                            NA))))
table(maindata$EPDS2)
#D26 n6062	Unnecessary self blame in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pl6062)
maindata$EPDS3<-ifelse(maindata$pl6062==1,3,
                       ifelse(maindata$pl6062==2, 2,
                              ifelse(maindata$pl6062==3, 1,
                                     ifelse(maindata$pl6062==4, 0,
                                            NA))))
table(maindata$EPDS3)
#D27 n6063	Unnecessary anxiety or worry in past WK (1 = 0) (2 = 1) (3 = 2) (4 = 3)
table(maindata$pl6063)
maindata$EPDS4<-ifelse(maindata$pl6063==1,0,
                       ifelse(maindata$pl6063==2, 1,
                              ifelse(maindata$pl6063==3, 2,
                                     ifelse(maindata$pl6063==4, 3,
                                            NA))))
table(maindata$EPDS4)
#D28 n6064	Unnecessary panic or fear in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pl6064)
maindata$EPDS5<-ifelse(maindata$pl6064==1,3,
                       ifelse(maindata$pl6064==2, 2,
                              ifelse(maindata$pl6064==3, 1,
                                     ifelse(maindata$pl6064==4, 0,
                                            NA))))
table(maindata$EPDS5)
#D29 n6065	Things getting too much in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pl6065)
maindata$EPDS6<-ifelse(maindata$pl6065==1,3,
                       ifelse(maindata$pl6065==2, 2,
                              ifelse(maindata$pl6065==3, 1,
                                     ifelse(maindata$pl6065==4, 0,
                                            NA))))
table(maindata$EPDS6)
#D30 n6066	Sleeping PROB due to sadness in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pl6066)
maindata$EPDS7<-ifelse(maindata$pl6066==1,3,
                       ifelse(maindata$pl6066==2, 2,
                              ifelse(maindata$pl6066==3, 1,
                                     ifelse(maindata$pl6066==4, 0,
                                            NA))))
table(maindata$EPDS7)
#D31 n6067	Sad or miserable in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pl6067)
maindata$EPDS8<-ifelse(maindata$pl6067==1,3,
                       ifelse(maindata$pl6067==2, 2,
                              ifelse(maindata$pl6067==3, 1,
                                     ifelse(maindata$pl6067==4, 0,
                                            NA))))
table(maindata$EPDS8)
#D32 n6068	Crying due to unhappiness in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pl6068)
maindata$EPDS9<-ifelse(maindata$pl6068==1,3,
                       ifelse(maindata$pl6068==2, 2,
                              ifelse(maindata$pl6068==3, 1,
                                     ifelse(maindata$pl6068==4, 0,
                                            NA))))
table(maindata$EPDS9)
#D33 n6069	Considered selfharm in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pl6069)
maindata$EPDS10<-ifelse(maindata$pl6069==1,3,
                        ifelse(maindata$pl6069==2, 2,
                               ifelse(maindata$pl6069==3, 1,
                                      ifelse(maindata$pl6069==4, 0,
                                             NA))))
table(maindata$EPDS10)

maindata$plEPDS<-maindata$EPDS1+maindata$EPDS2+maindata$EPDS3+maindata$EPDS4+maindata$EPDS5+maindata$EPDS6+maindata$EPDS7+maindata$EPDS8+maindata$EPDS9+maindata$EPDS10
summary(maindata$plEPDS)
hist(maindata$plEPDS) #higher score = more depressed

#EPDS partner 11 years

#D24 n6060	Sense of humour in past WK (1 = 0) (2 = 1) (3 = 2) (4 = 3)
table(maindata$pp4010)
maindata$EPDS1<-ifelse(maindata$pp4010==1,0,
                       ifelse(maindata$pp4010==2, 1,
                              ifelse(maindata$pp4010==3, 2,
                                     ifelse(maindata$pp4010==4, 3,
                                            NA))))
table(maindata$EPDS1)
#D25 n6061	Looked forward to things in past WK (1 = 0) (2 = 1) (3 = 2) (4 = 3)
table(maindata$pp4011)
maindata$EPDS2<-ifelse(maindata$pp4011==1,0,
                       ifelse(maindata$pp4011==2, 1,
                              ifelse(maindata$pp4011==3, 2,
                                     ifelse(maindata$pp4011==4, 3,
                                            NA))))
table(maindata$EPDS2)
#D26 n6062	Unnecessary self blame in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pp4012)
maindata$EPDS3<-ifelse(maindata$pp4012==1,3,
                       ifelse(maindata$pp4012==2, 2,
                              ifelse(maindata$pp4012==3, 1,
                                     ifelse(maindata$pp4012==4, 0,
                                            NA))))
table(maindata$EPDS3)
#D27 n6063	Unnecessary anxiety or worry in past WK (1 = 0) (2 = 1) (3 = 2) (4 = 3)
table(maindata$pp4013)
maindata$EPDS4<-ifelse(maindata$pp4013==1,0,
                       ifelse(maindata$pp4013==2, 1,
                              ifelse(maindata$pp4013==3, 2,
                                     ifelse(maindata$pp4013==4, 3,
                                            NA))))
table(maindata$EPDS4)
#D28 n6064	Unnecessary panic or fear in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pp4014)
maindata$EPDS5<-ifelse(maindata$pp4014==1,3,
                       ifelse(maindata$pp4014==2, 2,
                              ifelse(maindata$pp4014==3, 1,
                                     ifelse(maindata$pp4014==4, 0,
                                            NA))))
table(maindata$EPDS5)
#D29 n6065	Things getting too much in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pp4015)
maindata$EPDS6<-ifelse(maindata$pp4015==1,3,
                       ifelse(maindata$pp4015==2, 2,
                              ifelse(maindata$pp4015==3, 1,
                                     ifelse(maindata$pp4015==4, 0,
                                            NA))))
table(maindata$EPDS6)
#D30 n6066	Sleeping PROB due to sadness in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pp4016)
maindata$EPDS7<-ifelse(maindata$pp4016==1,3,
                       ifelse(maindata$pp4016==2, 2,
                              ifelse(maindata$pp4016==3, 1,
                                     ifelse(maindata$pp4016==4, 0,
                                            NA))))
table(maindata$EPDS7)
#D31 n6067	Sad or miserable in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pp4017)
maindata$EPDS8<-ifelse(maindata$pp4017==1,3,
                       ifelse(maindata$pp4017==2, 2,
                              ifelse(maindata$pp4017==3, 1,
                                     ifelse(maindata$pp4017==4, 0,
                                            NA))))
table(maindata$EPDS8)
#D32 n6068	Crying due to unhappiness in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pp4018)
maindata$EPDS9<-ifelse(maindata$pp4018==1,3,
                       ifelse(maindata$pp4018==2, 2,
                              ifelse(maindata$pp4018==3, 1,
                                     ifelse(maindata$pp4018==4, 0,
                                            NA))))
table(maindata$EPDS9)
#D33 n6069	Considered selfharm in past WK (1 = 3) (2 = 2) (3 = 1) (4 = 0)
table(maindata$pp4019)
maindata$EPDS10<-ifelse(maindata$pp4019==1,3,
                        ifelse(maindata$pp4019==2, 2,
                               ifelse(maindata$pp4019==3, 1,
                                      ifelse(maindata$pp4019==4, 0,
                                             NA))))
table(maindata$EPDS10)

maindata$ppEPDS<-maindata$EPDS1+maindata$EPDS2+maindata$EPDS3+maindata$EPDS4+maindata$EPDS5+maindata$EPDS6+maindata$EPDS7+maindata$EPDS8+maindata$EPDS9+maindata$EPDS10
summary(maindata$ppEPDS)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.000   0.000   3.000   3.929   6.000  29.000   11861 
hist(maindata$ppEPDS) #higher score = more depressed

maindata$zpb261num<-scale(maindata$pb261num)
maindata$zplEPDS<-scale(maindata$plEPDS)
maindata$zppEPDS<-scale(maindata$ppEPDS)
maindata$partneremo<-rowMeans(maindata[,c("zpb261num", "zplEPDS", "zppEPDS")])
hist(maindata$partneremo) #higher score still means less emotionally healthy
describe(maindata$partneremo)
# vars    n  mean   sd median trimmed mad   min  max range skew kurtosis   se
#    1 2510 -0.08 0.74  -0.26   -0.17 0.69 -0.98 3.41   4.4 1.13     1.13 0.01

#Mother's physical health
#lower score is healthier
summary(maindata$e310) #Description of health #8 weeks
table(maindata$e310)
maindata$e310num<-ifelse(maindata$e310<0, NA,
                         maindata$e310)
table(maindata$e310num) #higher score means less healthy
table(maindata$p1000) # Description of mother's current health #9 years 2 months
maindata$p1000num<-ifelse(maindata$p1000<0,NA, maindata$p1000)
table(maindata$p1000num)
table(maindata$s1000) #mother's rating of own health in the past 4 weeks 12 years 1 months
maindata$s1000num<-ifelse(maindata$s1000<0, NA, maindata$s1000)
table(maindata$s1000num)
maindata$motherphys<-rowMeans(maindata[,c("e310num", "p1000num", "s1000num")])
#maindata$motherphys<-maindata$e310num
summary(maindata$motherphys)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   1.333   1.333   1.534   2.000   3.333    9486 
hist(maindata$motherphys) #higher score = less healthy

#Partner's physical health
summary(maindata$pc051) #Description of health #8 weeks
table(maindata$pc051)
maindata$pc051num<-ifelse(maindata$pc051<0, NA,
                          maindata$pc051)
table(maindata$pc051num)
table(maindata$pm1000) # Description of partner's current health #9 years 2 months
maindata$pm1000num<-ifelse(maindata$pm1000<0,NA, maindata$pm1000)
table(maindata$pm1000num)
table(maindata$pq1000) #Partner's assessment of health 12 years 1 month
maindata$pq1000num<-ifelse(maindata$pq1000<0, NA, maindata$pq1000)
table(maindata$pq1000num)
maindata$partnerphys<-rowMeans(maindata[,c("pc051num", "pm1000num", "pq1000num")])
summary(maindata$partnerphys)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.000   1.000   1.333   1.404   1.667   3.667   12945 
hist(maindata$partnerphys) #higher score = less health

#Parental general health
describe(maindata$motherphys)
table(maindata$motherphys)
describe(maindata$partnerphys)
table(maindata$partnerphys)
maindata$pargenhealthR<-rowMeans(maindata[,c("motherphys", "partnerphys")]) #higher score means worse general health
#Reverse pargenhealthR health so higher score means better general health
describe(maindata$pargenhealthR)
maindata$pargenhealth<-3-maindata$pargenhealthR
hist(maindata$pargenhealthR)
hist(maindata$pargenhealth)
describe(maindata$pargenhealthR)
describe(maindata$pargenhealth)

#Parental emotional problems

hist(maindata$motheremo) #higher score is lower emotional health
summary(maindata$motheremo)
hist(maindata$partneremo)
summary(maindata$partneremo)

maindata$zmotheremo<-scale(maindata$motheremo)
maindata$zpartneremo<-scale(maindata$partneremo)

hist(maindata$zmotheremo)
hist(maindata$zpartneremo)

maindata$paremohealth<-rowMeans(maindata[,c("zmotheremo", "zpartneremo")])
summary(maindata$paremohealth)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -1.365  -0.650  -0.175  -0.069   0.397   3.179   13048 
hist(maindata$paremohealth) #higher score means more emotional problems

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~Family behavioural predictors~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#----------------------------------------------------------
#a Child conceived within marriage
#----------------------------------------------------------

table(maindata$a520) #Do you currently have a partner? yes,husband = 1, yes, other male partner = 2, no,not at all = 3, other = 4
table(maindata$a521) #Is your partner the father of your unborn child yes = 1, no =2, not sure = 3

maindata$conceivedmarr<-ifelse(maindata$a520==1&maindata$a521==1, 1,
                               ifelse(maindata$a520==2&maindata$a521==1,0,
                                      ifelse(maindata$a520==4&maindata$a521==1, 0,
                                             ifelse(maindata$a520==1&maindata$a521==2, 0,
                                                    ifelse(maindata$a520==2&maindata$a521==2,0,
                                                           ifelse(maindata$a520==4&maindata$a521==2,0,
                                                                  ifelse(maindata$a520 ==3, 0,
                                                                         NA)))))))
summary(maindata$conceivedmarr)
table(maindata$conceivedmarr) #higher score (1) means conceived in marriage (positive)

#----------------------------------------------------------
#b Both parents still together
#----------------------------------------------------------

#Both parents still together at age 10
#Both biological parents still live with childr
table(maindata$q3050) #biological father lives with child 1 = no, yes = 2
table(maindata$q3060) #biological maindata lives with child

maindata$partogev10<-ifelse(maindata$q3050==2&maindata$q3060==2,1,
                            ifelse(maindata$q3050==2&maindata$q3060==1, 0,
                                   ifelse(maindata$q3050==1&maindata$q3060==2, 0,
                                          ifelse(maindata$q3050==1&maindata$q3060==1, 0, 
                                                 NA))))
table(maindata$partogev10) #parents still together = 1, parents not together = 0 #higher number is better
summary(maindata$partogev10)  

#----------------------------------------------------------
#c Parent's criminal activity
#----------------------------------------------------------

#Mother has ever been in trouble with the law age 12, yes=1, no=2
table(maindata$s5000)
maindata$s5000num<-ifelse(maindata$s5000<0, NA, 
                          ifelse(maindata$s5000==1, 0,
                                 ifelse(maindata$s5000==2, 1,
                                        NA)))
table(maindata$s5000num) #yes = 0, no = 1
head(maindata[,c("s5000", "s5000num")], n=10) 

#Partner has ever been in trouble with the law child age 12, yes=1, no=2
table(maindata$pq5000)
maindata$pq5000num<-ifelse(maindata$pq5000<0, NA, 
                           ifelse(maindata$pq5000==1, 0,
                                  ifelse(maindata$pq5000==2, 1,
                                         NA))) #yes =0, no = 1
table(maindata$pq5000num)

#Criminal activity of parents
maindata$parcrime<-rowSums(maindata[,c("s5000num", "pq5000num")]) #higher score means less crime in the family 
test<-maindata[,c("s5000", "pq5000", "parcrime")]
head(test, n=10)
table(maindata$parcrime) #higher score means less encounters with the law, 0 = both parents been in trouble with the law, 1 = 1 parent was in trouble, 2 = no parents in trouble with law

maindata$zconceivedmarr<-scale(maindata$conceivedmarr)
maindata$zpartogev10<-scale(maindata$partogev10)
maindata$zparcrime<-scale(maindata$parcrime)
hist(maindata$zconceivedmarr)
hist(maindata$zpartogev10)
hist(maindata$zparcrime)
maindata$fambehav<-rowSums(maindata[,c("zconceivedmarr", "zpartogev10", "zparcrime")])
summary(maindata$fambehav)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-6.700  -0.077   1.782   0.650   1.782   1.782   12559 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~Combine with wellbeing data~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Set working directory to folder containing data
list.files()
wellbeing23<-read.csv("SectionC_composites.csv")
table(wellbeing23$qlet)
#Take away the B children
dim(wellbeing23) #4178 x  19
head(wellbeing23)
wellbeing23$ID = paste(wellbeing23$aln, wellbeing23$qlet, sep="")
head(wellbeing23)
wellbeing23$aln<-wellbeing23$alnWB
wellbeing23$qlet<-wellbeing23$qletWB
wellbeing23$aln<-NULL
wellbeing23$qlet<-NULL
dim(maindata) #15243 x 1630

# #add demographic information
# list.files()
# corrections<-read.csv("corrections.csv")
# names(corrections)
# listnames<-data.frame(names(maindata))
# corrections$ID = paste(corrections$aln, corrections$qlet, sep="")
# corrections1<-corrections[,c("ID", "a053", "a551", "c800", "c804",
#                              "f304", "f305", "f306", "f365", "f460", "f992",
#                              "f345", "f443", "f366")]
# maindata1<-merge(maindata, corrections1, by="ID", all.x=TRUE, all.y=FALSE) 
# maindata1$f443na<-ifelse(maindata1$f443<0,NA,maindata1$f443)
# maindata1$f345na<-ifelse(maindata1$f345<0,NA,maindata1$f345)
# maindata1$ppr<-maindata1$f443na/maindata1$f345na

adultchildfamilypredictors<-maindata[, c("aln", "qlet", "ID", "adultloginc", "AQguess2", "employed23", "adultpartner", "adultantis", "selfhealth", "adultemohealth", 
                                         "childext", "childemohealth", "childIQ", "childcurrenthealth",
                                         "econcomp", "pargenhealth", "paremohealth", "fambehav")]
dim(adultchildfamilypredictors) #15243  x   18
head(adultchildfamilypredictors, n=10)
summary(adultchildfamilypredictors)

#Merge adultpredictors dataset with wellbeing dataset
dim(wellbeing23) #4178 x  18
head(wellbeing23, n=2)
dim(adultchildfamilypredictors) #15243  x   18
head(adultchildfamilypredictors, n=2)
adultchildfamily<-merge(wellbeing23, adultchildfamilypredictors, by="ID", all=TRUE) 
dim(adultchildfamily) #15401 x   35
head(adultchildfamily, n=20)

#Only take indivduals who took part in 23 wellbeing questionnaire (subset based on X)
dim(wellbeing23) #4178 x  18
adultchildfamily2<-adultchildfamily[!is.na(adultchildfamily$X),]
summary(adultchildfamily2)
dim(adultchildfamily2) #4178 x  35



descriptiveadd<-maindata[, c("aln", "qlet", "ID", "incomeMP3", "AQguess2", "employed23", "adultpartner", "adultantis", "selfhealth", "adultemohealth", 
                             "childext", "childemohealth", "childIQ", "childcurrenthealth", 
                             "parentSC", "famincome", "fatherwork", "famedu",
                             "econcomp", "pargenhealth", "paremohealth", "fambehav", "parcrime")]
descriptiveadd2<-merge(wellbeing23, descriptiveadd, by="ID", all.x=TRUE) 
describe(descriptiveadd2)
