##set working directory in menu bar of R
setwd('C:/Users/peterss/Dropbox/Work/T2/R')

##Load libraryies
library (ggplot2)
library (nlme)

##Read in data, adjust datafile name. Data is now called 'dat'
dat<-read.csv2('T1T2alles_17feb2016_voorR.csv',header=T)

WM_nomissing.data<-subset(dat,dat$mentalcounters_longformat>0)
CT_nomissing.data<-subset(dat,dat$CT_splL>0)
WM_CT_nomissing.data<-subset(dat,dat$mentalcounters_longformat>0&dat$CT_splL>0)
WM_CT_testos_nomissing.data<-subset(dat,dat$mentalcounters_longformat>0&dat$CT_splL>0&dat$log_testos>0)




##_________________________________________________________
##mfgLR


##null model
Intercept<-gls(anatfsl50_mfgLR_enep_cp~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(anatfsl50_mfgLR_enep_cp~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(anatfsl50_mfgLR_enep_cp~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)


##is age wel sign?
summary(RandomInterceptAge_quad)

##random slope better?
RandomInterceptAgeSlope_quad<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2), data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_quad,RandomInterceptAgeSlope_quad)

##model: wat verklaart activity beste (age beste vorm,performance,WM,struct)
RandomInterceptAge_quadPerfWMStruc<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned+CT_mfgLR_demeaned, data=dat,random=~1|id,method="ML",na.action=na.exclude)
summary(RandomInterceptAge_quadPerfWMStruc)


## model: wat verklaart activity beste (age beste vorm,performance,WM,struct)
RandomInterceptAge_quadPerfWMStruc<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned+CT_mfgLR_demeaned, data=WM_CT_nomissing.data,random=~1|id,method="ML",na.action=na.exclude)
summary(RandomInterceptAge_quadPerfWMStruc)



#stepwise age-perf-WM-struct

RandomInterceptAge_quad<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_Perf<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_Perf)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfWM<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfWM)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfStruct<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+CT_mfgLR_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfStruct)

summary(RandomInterceptAge_quad_Perf)
intervals(RandomInterceptAge_quad_Perf)


#stepwise age-struct

RandomInterceptAge_quad<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_struct<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+CT_mfgLR_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_struct)


#stepwise age-working memory

RandomInterceptAge_quad<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_WM<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_WM)

#op beste model testos nog beter?

RandomInterceptAge_quad_Perf<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_testos_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_Perf_testos<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+log_testos_demeaned, data=WM_CT_testos_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)

anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_Perf_testos)

##_________________________________________________________
##sma


##null model
Intercept<-gls(anatfsl50_sma_enep_cp~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(anatfsl50_sma_enep_cp~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatfsl50_sma_enep_cp~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatfsl50_sma_enep_cp~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

##is age wel sign?
summary(RandomInterceptAge_lin)

##random slope better?
RandomInterceptAgeSlope_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_lin,RandomInterceptAgeSlope_lin)


#stepwise age-perf-WM-struct

RandomInterceptAge_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_Perf<-lme(anatfsl50_sma_enep_cp~age_lin+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_Perf)

summary(RandomInterceptAge_lin_Perf)
RandomInterceptAge_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_WM<-lme(anatfsl50_sma_enep_cp~age_lin+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_WM)

RandomInterceptAge_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_Struct<-lme(anatfsl50_sma_enep_cp~age_lin+CT_sma_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_Struct)

summary(RandomInterceptAge_lin_Struct)
intervals(RandomInterceptAge_lin_Struct)

#stepwise age-struct
RandomInterceptAge_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_struct<-lme(anatfsl50_sma_enep_cp~age_lin+CT_sma_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_struct)


#stepwise age-WM
RandomInterceptAge_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_WM<-lme(anatfsl50_sma_enep_cp~age_lin+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_WM)

#op beste model testos nog beter?

RandomInterceptAge_lin_struct<-lme(anatfsl50_sma_enep_cp~age_lin+CT_sma_demeaned, data=WM_CT_testos_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_struct_testos<-lme(anatfsl50_sma_enep_cp~age_lin+CT_sma_demeaned+log_testos_demeaned, data=WM_CT_testos_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin_struct,RandomInterceptAge_lin_struct_testos)



##_________________________________________________________
##acc


##null model
Intercept<-gls(anatfsl50_acc_enep_cp~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(anatfsl50_acc_enep_cp~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(anatfsl50_acc_enep_cp~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatfsl50_acc_enep_cp~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatfsl50_acc_enep_cp~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

##is age wel sign?
summary(RandomInterceptAge_lin)

##random slope better?
RandomInterceptAgeSlope_lin<-lme(anatfsl50_acc_enep_cp~age_lin, data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_lin,RandomInterceptAgeSlope_lin)


#stepwise age-perf-WM-struct

RandomInterceptAge_lin<-lme(anatfsl50_acc_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_Perf<-lme(anatfsl50_acc_enep_cp~age_lin+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_Perf)

RandomInterceptAge_lin<-lme(anatfsl50_acc_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_WM<-lme(anatfsl50_acc_enep_cp~age_lin+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_WM)

RandomInterceptAge_lin<-lme(anatfsl50_acc_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_Struct<-lme(anatfsl50_acc_enep_cp~age_lin+CT_acc_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_Struct)

summary(RandomInterceptAge_lin)
intervals(RandomInterceptAge_lin)

#stepwise age-struct
RandomInterceptAge_lin<-lme(anatfsl50_acc_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_struct<-lme(anatfsl50_acc_enep_cp~age_lin+CT_acc_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_struct)


#stepwise age-WM
RandomInterceptAge_lin<-lme(anatfsl50_acc_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_WM<-lme(anatfsl50_acc_enep_cp~age_lin+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_WM)

#op beste model testos nog beter?

RandomInterceptAge_lin<-lme(anatfsl50_acc_enep_cp~age_lin, data=WM_CT_testos_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_testos<-lme(anatfsl50_acc_enep_cp~age_lin+log_testos_demeaned, data=WM_CT_testos_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_testos)



##_________________________________________________________
##splLR


##null model
Intercept<-gls(anatfsl50_splLR_enep_cp~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(anatfsl50_splLR_enep_cp~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(anatfsl50_splLR_enep_cp~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)


##is age wel sign?
summary(RandomInterceptAge_quad)

##random slope better?
RandomInterceptAgeSlope_quad<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2), data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_quad,RandomInterceptAgeSlope_quad)

##model: wat verklaart activity beste (age beste vorm,performance,WM,struct)
RandomInterceptAge_quadPerfWMStruc<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned+CT_splLR_demeaned, data=dat,random=~1|id,method="ML",na.action=na.exclude)
summary(RandomInterceptAge_quadPerfWMStruc)


#stepwise age-perf-WM-struct

RandomInterceptAge_quad<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_Perf<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_Perf)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfWM<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfWM)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfStruct<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+CT_splLR_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfStruct)

summary(RandomInterceptAge_quad_Perf)
intervals(RandomInterceptAge_quad_Perf)

#stepwise age-struct

RandomInterceptAge_quad<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_struct<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+CT_mfgLR_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_struct)


#stepwise age-WM

RandomInterceptAge_quad<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_WM<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_WM)


#op beste model testos nog beter?

RandomInterceptAge_quad_Perf<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_testos_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_Perf_testos<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+log_testos_demeaned, data=WM_CT_testos_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)

anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_Perf_testos)


##_________________________________________________________
##mfgL


##null model
Intercept<-gls(anatfsl50_mfgL_enep_cp~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(anatfsl50_mfgL_enep_cp~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(anatfsl50_mfgL_enep_cp~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatfsl50_mfgL_enep_cp~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatfsl50_mfgL_enep_cp~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

##is age wel sign?
summary(RandomInterceptAge_lin)

##random slope better?
RandomInterceptAgeSlope_lin<-lme(anatfsl50_mfgL_enep_cp~age_lin, data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_lin,RandomInterceptAgeSlope_lin)


##model: wat verklaart activity beste (age beste vorm,performance,WM,struct)
RandomInterceptAge_linPerfWMStruc<-lme(anatfsl50_mfgL_enep_cp~age_lin+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned+CT_mfgL_demeaned, data=dat,random=~1|id,method="ML",na.action=na.exclude)
summary(RandomInterceptAge_linPerfWMStruc)

#stepwise age-perf-WM-struct
RandomInterceptAge_lin<-lme(anatfsl50_mfgL_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_Perf<-lme(anatfsl50_mfgL_enep_cp~age_lin+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_Perf)

RandomInterceptAge_lin_Perf<-lme(anatfsl50_mfgL_enep_cp~age_lin+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_PerfWM<-lme(anatfsl50_mfgL_enep_cp~age_lin+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin_Perf,RandomInterceptAge_lin_PerfWM)

RandomInterceptAge_lin_Perf<-lme(anatfsl50_mfgL_enep_cp~age_lin+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_PerfStruct<-lme(anatfsl50_mfgL_enep_cp~age_lin+learningrate_longformat_demeaned+CT_mfgL_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin_Perf,RandomInterceptAge_lin_PerfStruct)





##_________________________________________________________
##mfgR


##null model
Intercept<-gls(anatfsl50_mfgR_enep_cp~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(anatfsl50_mfgR_enep_cp~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(anatfsl50_mfgR_enep_cp~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatfsl50_mfgR_enep_cp~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatfsl50_mfgR_enep_cp~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

##is age wel sign?
summary(RandomInterceptAge_quad)

##random slope better?
RandomInterceptAgeSlope_quad<-lme(anatfsl50_mfgR_enep_cp~poly(age_lin,2), data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_quad,RandomInterceptAgeSlope_quad)

##model: wat verklaart activity beste (age beste vorm,performance,WM,struct)
RandomInterceptAge_quadPerfWMStruc<-lme(anatfsl50_mfgR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned+CT_mfgR_demeaned, data=dat,random=~1|id,method="ML",na.action=na.exclude)
summary(RandomInterceptAge_quadPerfWMStruc)

#stepwise age-perf-WM-struct

RandomInterceptAge_quad<-lme(anatfsl50_mfgR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_Perf<-lme(anatfsl50_mfgR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_Perf)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_mfgR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfWM<-lme(anatfsl50_mfgR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfWM)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_mfgR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfStruct<-lme(anatfsl50_mfgR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+CT_mfgR_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfStruct)


##_________________________________________________________
##splL


##null model
Intercept<-gls(anatfsl50_splL_enep_cp~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(anatfsl50_splL_enep_cp~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(anatfsl50_splL_enep_cp~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatfsl50_splL_enep_cp~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatfsl50_splL_enep_cp~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)


##is age wel sign?
summary(RandomInterceptAge_quad)

##random slope better?
RandomInterceptAgeSlope_lin<-lme(anatfsl50_splL_enep_cp~age_lin, data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_lin,RandomInterceptAgeSlope_lin)

##model: wat verklaart activity beste (age beste vorm,performance,WM,struct)
RandomInterceptAge_quadPerfWMStruc<-lme(anatfsl50_splL_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned+CT_splL_demeaned, data=dat,random=~1|id,method="ML",na.action=na.exclude)
summary(RandomInterceptAge_quadPerfWMStruc)


#stepwise age-perf-WM-struct

RandomInterceptAge_quad<-lme(anatfsl50_splL_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_Perf<-lme(anatfsl50_splL_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_Perf)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_splL_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfWM<-lme(anatfsl50_splL_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfWM)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_splL_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfStruct<-lme(anatfsl50_splL_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+CT_mfgR_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfStruct)


##_________________________________________________________
##splR


##null model
Intercept<-gls(anatfsl50_splR_enep_cp~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(anatfsl50_splR_enep_cp~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(anatfsl50_splR_enep_cp~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatfsl50_splR_enep_cp~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatfsl50_splR_enep_cp~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)


##is age wel sign?
summary(RandomInterceptAge_quad)

##random slope better?
RandomInterceptAgeSlope_quad<-lme(anatfsl50_splR_enep_cp~poly(age_lin,2), data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_quad,RandomInterceptAgeSlope_quad)

##model: wat verklaart activity beste (age beste vorm,performance,WM,struct)
RandomInterceptAge_quadPerfWMStruc<-lme(anatfsl50_splR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned+CT_splR_demeaned, data=dat,random=~1|id,method="ML",na.action=na.exclude)
summary(RandomInterceptAge_quadPerfWMStruc)

#stepwise age-perf-WM-struct

RandomInterceptAge_quad<-lme(anatfsl50_splR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_Perf<-lme(anatfsl50_splR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_Perf)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_splR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfWM<-lme(anatfsl50_splR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfWM)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_splR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfStruct<-lme(anatfsl50_splR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+CT_splR_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfStruct)





##agevorm van predictors toetsen
##_________________________________________________________
##learning rate


##null model
Intercept<-gls(learningrate_longformat~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(learningrate_longformat~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(learningrate_longformat~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(learningrate_longformat~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(learningrate_longformat~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

##random slope better?
RandomInterceptAgeSlope_quad<-lme(learningrate_longformat~poly(age_lin,2), data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_quad,RandomInterceptAgeSlope_quad)

summary(RandomInterceptAge_quad)

## op beste model voor testos nog beter?
RandomInterceptAge_quad<-lme(learningrate_longformat~poly(age_lin,2), data=WM_CT_testos_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_testos<-lme(learningrate_longformat~poly(age_lin,2)+log_testos_demeaned, data=WM_CT_testos_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_testos)

##_____________________________________________________________
##working memory
##null model
Intercept<-gls(mentalcounters_longformat~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(mentalcounters_longformat~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models

RandomInterceptAge_lin<-lme(mentalcounters_longformat~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(mentalcounters_longformat~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(mentalcounters_longformat~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

##random slope better?
RandomInterceptAgeSlope_quad<-lme(mentalcounters_longformat~poly(age_lin,2), data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_quad,RandomInterceptAgeSlope_quad)

summary(RandomInterceptAge_quad)

RandomInterceptAge_quad<-lme(mentalcounters_longformat~poly(age_lin,2), data=WM_CT_testos_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_testos<-lme(mentalcounters_longformat~poly(age_lin,2)+log_testos_demeaned, data=WM_CT_testos_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_testos)



###structure

#mfgLR
##null model
Intercept<-gls(CT_mfgLR~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(CT_mfgLR~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(CT_mfgLR~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(CT_mfgLR~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(CT_mfgLR~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

##random slope better?
RandomInterceptAgeSlope_lin<-lme(CT_mfgLR~age_lin, data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_lin,RandomInterceptAgeSlope_lin)



#sma
##null model
Intercept<-gls(CT_sma~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(CT_sma~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(CT_sma~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(CT_sma~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(CT_sma~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomInterceptAge_cub)

##random slope better?
RandomInterceptAgeSlope_cub<-lme(CT_sma~poly(age_lin,3), data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_cub,RandomInterceptAgeSlope_cub)


#acc
##null model
Intercept<-gls(CT_acc~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(CT_acc~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(CT_acc~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(CT_acc~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(CT_acc~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomInterceptAge_quad)

##random slope better?
RandomInterceptAgeSlope_quad<-lme(CT_acc~poly(age_lin,2), data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_quad,RandomInterceptAgeSlope_quad)






#splLR
##null model
Intercept<-gls(CT_splLR~1, data=dat, method="ML", na.action=na.exclude)
RandomIntercept<-lme(CT_splLR~1, data=dat,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(CT_splLR~age_lin, data=dat,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(CT_splLR~poly(age_lin,2), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(CT_splLR~poly(age_lin,3), data=dat,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)
summary(RandomInterceptAge_quad)

##random slope better?
RandomInterceptAgeSlope_quad<-lme(CT_splLR~poly(age_lin,2), data=dat,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_quad,RandomInterceptAgeSlope_quad)
















##________________________________________________________________________________________________________________________________________
## t/m age 17


age_tm17.data<-subset(dat,dat$ageT1<18)


##_________________________________________________________
##mfgLR


##null model
Intercept<-gls(anatfsl50_mfgLR_enep_cp~1, data=age_tm17.data, method="ML", na.action=na.exclude)
RandomIntercept<-lme(anatfsl50_mfgLR_enep_cp~1, data=age_tm17.data,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(anatfsl50_mfgLR_enep_cp~age_lin, data=age_tm17.data,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2), data=age_tm17.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,3), data=age_tm17.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)


##is age wel sign?
summary(RandomInterceptAge_quad)

##random slope better?
RandomInterceptAgeSlope_quad<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2), data=age_tm17.data,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_quad,RandomInterceptAgeSlope_quad)

##model: wat verklaart activity beste (age beste vorm,performance,WM,struct)
RandomInterceptAge_quadPerfWMStruc<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned+CT_mfgLR_demeaned, data=age_tm17.data,random=~1|id,method="ML",na.action=na.exclude)
summary(RandomInterceptAge_quadPerfWMStruc)


## model: wat verklaart activity beste (age beste vorm,performance,WM,struct)
RandomInterceptAge_quadPerfWMStruc<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned+CT_mfgLR_demeaned, data=WM_CT_nomissing.data,random=~1|id,method="ML",na.action=na.exclude)
summary(RandomInterceptAge_quadPerfWMStruc)



#stepwise age-perf-WM-struct

RandomInterceptAge_quad<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_Perf<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_Perf)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfWM<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfWM)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfStruct<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+CT_mfgLR_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfStruct)

summary(RandomInterceptAge_quad_Perf)
intervals(RandomInterceptAge_quad_Perf)


#stepwise age-struct

RandomInterceptAge_quad<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_struct<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+CT_mfgLR_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_struct)


#stepwise age-working memory

RandomInterceptAge_quad<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_WM<-lme(anatfsl50_mfgLR_enep_cp~poly(age_lin,2)+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_WM)

##_________________________________________________________
##sma


##null model
Intercept<-gls(anatfsl50_sma_enep_cp~1, data=age_tm17.data, method="ML", na.action=na.exclude)
RandomIntercept<-lme(anatfsl50_sma_enep_cp~1, data=age_tm17.data,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=age_tm17.data,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatfsl50_sma_enep_cp~poly(age_lin,2), data=age_tm17.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatfsl50_sma_enep_cp~poly(age_lin,3), data=age_tm17.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)

##is age wel sign?
summary(RandomInterceptAge_lin)

##random slope better?
RandomInterceptAgeSlope_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=age_tm17.data,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_lin,RandomInterceptAgeSlope_lin)


#stepwise age-perf-WM-struct

RandomInterceptAge_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_Perf<-lme(anatfsl50_sma_enep_cp~age_lin+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_Perf)

RandomInterceptAge_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_WM<-lme(anatfsl50_sma_enep_cp~age_lin+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_WM)

RandomInterceptAge_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_Struct<-lme(anatfsl50_sma_enep_cp~age_lin+CT_sma_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_Struct)

summary(RandomInterceptAge_lin_Struct)
intervals(RandomInterceptAge_lin_Struct)

#stepwise age-struct
RandomInterceptAge_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_struct<-lme(anatfsl50_sma_enep_cp~age_lin+CT_sma_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_struct)


#stepwise age-WM
RandomInterceptAge_lin<-lme(anatfsl50_sma_enep_cp~age_lin, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_lin_WM<-lme(anatfsl50_sma_enep_cp~age_lin+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_lin,RandomInterceptAge_lin_WM)

##_________________________________________________________
##splLR


##null model
Intercept<-gls(anatfsl50_splLR_enep_cp~1, data=age_tm17.data, method="ML", na.action=na.exclude)
RandomIntercept<-lme(anatfsl50_splLR_enep_cp~1, data=age_tm17.data,random=~1|id,method="ML",na.action=na.exclude)

##compare age models
RandomInterceptAge_lin<-lme(anatfsl50_splLR_enep_cp~age_lin, data=age_tm17.data,random=~1|id,method="ML",na.action=na.exclude)
RandomInterceptAge_quad<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2), data=age_tm17.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_cub<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,3), data=age_tm17.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomIntercept,RandomInterceptAge_lin,RandomInterceptAge_quad,RandomInterceptAge_cub)


##is age wel sign?
summary(RandomInterceptAge_quad)

##random slope better?
RandomInterceptAgeSlope_quad<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2), data=age_tm17.data,random=~age_lin|id,method="ML",na.action=na.exclude,control=list(maxIter=2000,opt="optim"))
anova(RandomInterceptAge_quad,RandomInterceptAgeSlope_quad)

##model: wat verklaart activity beste (age beste vorm,performance,WM,struct)
RandomInterceptAge_quadPerfWMStruc<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned+CT_splLR_demeaned, data=age_tm17.data,random=~1|id,method="ML",na.action=na.exclude)
summary(RandomInterceptAge_quadPerfWMStruc)


#stepwise age-perf-WM-struct

RandomInterceptAge_quad<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_Perf<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_Perf)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfWM<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfWM)

RandomInterceptAge_quad_Perf<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_PerfStruct<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+learningrate_longformat_demeaned+CT_splLR_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad_Perf,RandomInterceptAge_quad_PerfStruct)

summary(RandomInterceptAge_quad_Perf)
intervals(RandomInterceptAge_quad_Perf)

#stepwise age-struct

RandomInterceptAge_quad<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_struct<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+CT_mfgLR_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_struct)


#stepwise age-WM

RandomInterceptAge_quad<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2), data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
RandomInterceptAge_quad_WM<-lme(anatfsl50_splLR_enep_cp~poly(age_lin,2)+mentalcounters_longformat_demeaned, data=WM_CT_nomissing.data,random=~1|Subject, method="ML",na.action=na.exclude)
anova(RandomInterceptAge_quad,RandomInterceptAge_quad_WM)




