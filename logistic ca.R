#IMPORTING DATASET 
cell2cell<-read.csv(file="C:/Users/Abhishek/Desktop/BA/Proactive Attrition Management-Logistic Regression Case Study/Proactive Attrition Management-Logistic Regression Case Study.csv",header = TRUE)
#CHANGING DATATYPE
cell2cell$CSA<-as.character(cell2cell$CSA)
cell2cell$CUSTOMER<-as.character(cell2cell$CUSTOMER)

cell2cell$MARRIAGE<-factor(ifelse(cell2cell$MARRYYES==1,1,ifelse(cell2cell$MARRYNO==0,0,0)))
cell2cell$CREDIT_RATES<-ordered(ifelse(cell2cell$CREDITA==1,1,ifelse(cell2cell$CREDITAA==1,2,ifelse(
cell2cell$CREDITB==1,3,ifelse(cell2cell$CREDITC==1,4,ifelse(cell2cell$CREDITDE==1,5,ifelse(cell2cell$CREDITGY==1,6,
ifelse(cell2cell$CREDITZ==1,7,0))))))))
cell2cell$PRIZMCODE<-factor(ifelse(cell2cell$PRIZMRUR==1,1,ifelse(cell2cell$PRIZMUB==1,2,ifelse(cell2cell$PRIZMTWN==1,
3,0))))
cell2cell$OCCUPATION<-factor(ifelse(cell2cell$OCCPROF==1,1,ifelse(cell2cell$OCCCLER==1,2,ifelse(cell2cell$OCCCRFT==1,
3,ifelse(cell2cell$OCCSTUD==1,4,ifelse(cell2cell$OCCHMKR==1,5,ifelse(cell2cell$OCCRET==1,6,ifelse(
cell2cell$OCCSELF==1,7,0))))))))


cell2cell$MARRYUN<-NULL
cell2cell$MARRYYES<-NULL
cell2cell$MARRYNO<-NULL
cell2cell$CREDITA<-NULL
cell2cell$CREDITAA<-NULL
cell2cell$CREDITB<-NULL
cell2cell$CREDITC<-NULL
cell2cell$CREDITDE<-NULL
cell2cell$CREDITGY<-NULL
cell2cell$CREDITZ<-NULL
cell2cell$PRIZMRUR<-NULL
cell2cell$PRIZMUB<-NULL
cell2cell$PRIZMTWN<-NULL
cell2cell$OCCPROF<-NULL
cell2cell$OCCCLER<-NULL
cell2cell$OCCCRFT<-NULL
cell2cell$OCCSTUD<-NULL
cell2cell$OCCHMKR<-NULL
cell2cell$OCCRET<-NULL
cell2cell$OCCSELF<-NULL
cell2cell$RETCALLS<-NULL
cell2cell$RETCALL<-NULL
cell2cell$RETACCPT<-NULL
cell2cell$CHURNDEP<-NULL
cell2cell$AGE1[cell2cell$AGE1==0]<-NA
cell2cell$AGE2[cell2cell$AGE2==0]<-NA
cell2cell$INCOME[cell2cell$INCOME==0]<-NA
cell2cell$SETPRC[cell2cell$SETPRC==0]<-NA


cell2cell$CHILDREN<-factor(cell2cell$CHILDREN)
cell2cell$MAILORD<-factor(cell2cell$MAILORD)
cell2cell$MAILRES<-factor(cell2cell$MAILRES)
cell2cell$TRAVEL<-factor(cell2cell$TRAVEL)
cell2cell$CREDITCD<-factor(cell2cell$CREDITCD)
cell2cell$NEWCELLN<-factor(cell2cell$NEWCELLN)
cell2cell$MCYCLE<-factor(cell2cell$MCYCLE)
cell2cell$REFURB<-factor(cell2cell$REFURB)
cell2cell$WEBCAP<-factor(cell2cell$WEBCAP)
cell2cell$TRUCK<-factor(cell2cell$TRUCK)
cell2cell$RV<-factor(cell2cell$RV)
cell2cell$OWNRENT<-factor(cell2cell$OWNRENT)
cell2cell$MAILFLAG<-factor(cell2cell$MAILFLAG)
cell2cell$PCOWN<-factor(cell2cell$PCOWN)
cell2cell$NEWCELLY<-factor(cell2cell$NEWCELLY)
cell2cell$INCMISS<-factor(cell2cell$INCMISS)
cell2cell$SETPRCM<-factor(cell2cell$SETPRCM)

#USERDEFINED FUNCTION
myfun=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    n<-length(x)
    nmiss<-sum(is.na(x))
    return(c(n=n,nmiss=nmiss))
  }
}
num_var= sapply(cell2cell,is.numeric)
Other_var= !sapply(cell2cell,is.numeric)


my_num<-t(data.frame(apply(cell2cell[num_var], 2, myfun)))
my_cat<-data.frame(t(apply(cell2cell[Other_var], 2, myfun)))

#OUTLIER TREATMENT
cell2cell$REVENUE[cell2cell$REVENUE<10]<-10
cell2cell$CHANGEM[cell2cell$CHANGEM< -831.89]<- -831.89
cell2cell$CHANGER[cell2cell$CHANGER< -104.536]<- -104.536
cell2cell$ACTVSUBS[cell2cell$ACTVSUBS< 1]<- 1
cell2cell$EQPDAYS[cell2cell$EQPDAYS<7]<-7
cell2cell$AGE1[cell2cell$AGE1<22]<-22

cell2cell$REVENUE[cell2cell$REVENUE>225.512]<-225.512
cell2cell$MOU[cell2cell$MOU>2450.125]<-2450.125
cell2cell$RECCHRGE[cell2cell$RECCHRGE > 119.99] <- 119.99
cell2cell$DIRECTAS[cell2cell$DIRECTAS > 9.65] <- 9.65
cell2cell$OVERAGE[cell2cell$OVERAGE > 427.674999999999] <- 427.674999999999
cell2cell$ROAM[cell2cell$ROAM > 21.557] <- 21.557
cell2cell$CHANGEM[cell2cell$CHANGEM > 739.669999999998] <- 739.669999999998
cell2cell$CHANGER[cell2cell$CHANGER > 118.345599999999] <- 118.345599999999
cell2cell$DROPVCE[cell2cell$DROPVCE > 42] <- 42
cell2cell$BLCKVCE[cell2cell$BLCKVCE > 47] <-47
cell2cell$UNANSVCE[cell2cell$UNANSVCE > 179.33] <- 179.33
cell2cell$CUSTCARE[cell2cell$CUSTCARE >21 ] <- 21
cell2cell$THREEWAY[cell2cell$THREEWAY > 4] <- 4
cell2cell$MOUREC[cell2cell$MOUREC > 772.654799999999] <- 772.654799999999
cell2cell$OUTCALLS[cell2cell$OUTCALLS > 164.33] <- 164.33
cell2cell$INCALLS[cell2cell$INCALLS > 77] <- 77
cell2cell$PEAKVCE[cell2cell$PEAKVCE > 500] <- 500
cell2cell$OPEAKVCE[cell2cell$OPEAKVCE > 437] <- 437
cell2cell$DROPBLK[cell2cell$DROPBLK > 71.33] <- 71.33
cell2cell$CALLFWDV[cell2cell$CALLFWDV > 0] <- 0
cell2cell$CALLWAIT[cell2cell$CALLWAIT > 23.33] <- 23.33
cell2cell$MONTHS[cell2cell$MONTHS > 49] <- 49
cell2cell$UNIQSUBS[cell2cell$UNIQSUBS > 5] <- 5
cell2cell$ACTVSUBS[cell2cell$ACTVSUBS > 4] <- 4
cell2cell$PHONES[cell2cell$PHONES > 7] <- 7
cell2cell$MODELS[cell2cell$MODELS > 5] <- 5
cell2cell$EQPDAYS[cell2cell$EQPDAYS > 1150] <- 1150
cell2cell$AGE1[cell2cell$AGE1 > 76] <- 76
cell2cell$REFER[cell2cell$REFER > 1] <- 1
cell2cell$INCOME[cell2cell$INCOME > 9] <- 9
cell2cell$CREDITAD[cell2cell$CREDITAD > 1] <- 1

#MISSING VALUE TREATMENT

cell2cell$AGE2<-NULL
cell2cell$SETPRC<-NULL
cell2cell$CUSTOMER<-NULL
cell2cell$CSA<-NULL

num_var= sapply(cell2cell,is.numeric)
Other_var= !sapply(cell2cell,is.numeric)

cell2cell[,num_var] <- apply(data.frame(cell2cell[,num_var]),2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
my_num<-t(data.frame(apply(cell2cell[num_var], 2, myfun)))

options(scipen = 999)
#MULTICOLLINEARITY CHECK
#ANOVA
summary(aov(CHURN~REVENUE,data=cell2cell))
summary(aov(CHURN~MOU,data=cell2cell))
summary(aov(CHURN~RECCHRGE,data=cell2cell))
summary(aov(CHURN~DIRECTAS,data=cell2cell))
summary(aov(CHURN~OVERAGE,data=cell2cell))
summary(aov(CHURN~ROAM,data=cell2cell))
summary(aov(CHURN~CHANGEM,data=cell2cell))
summary(aov(CHURN~CHANGER,data=cell2cell))
summary(aov(CHURN~DROPVCE,data=cell2cell))
summary(aov(CHURN~BLCKVCE,data=cell2cell))
summary(aov(CHURN~UNANSVCE,data=cell2cell))
summary(aov(CHURN~CUSTCARE,data=cell2cell))
summary(aov(CHURN~THREEWAY,data=cell2cell))
summary(aov(CHURN~MOUREC,data=cell2cell))
summary(aov(CHURN~OUTCALLS,data=cell2cell))
summary(aov(CHURN~INCALLS,data=cell2cell))
summary(aov(CHURN~PEAKVCE,data=cell2cell))
summary(aov(CHURN~OPEAKVCE,data=cell2cell))
summary(aov(CHURN~DROPBLK,data=cell2cell))
summary(aov(CHURN~CALLFWDV,data=cell2cell))
summary(aov(CHURN~CALLWAIT,data=cell2cell))
summary(aov(CHURN~MONTHS,data=cell2cell))
summary(aov(CHURN~UNIQSUBS,data=cell2cell))
summary(aov(CHURN~ACTVSUBS,data=cell2cell))
summary(aov(CHURN~PHONES,data=cell2cell))
summary(aov(CHURN~MODELS,data=cell2cell))
summary(aov(CHURN~EQPDAYS,data=cell2cell))
summary(aov(CHURN~AGE1,data=cell2cell))
summary(aov(CHURN~REFER,data=cell2cell))
summary(aov(CHURN~INCOME,data=cell2cell))
summary(aov(CHURN~CREDITAD,data=cell2cell))

#CHI-SQUARE
require(MASS)
chisq.test(table(cell2cell$CHURN,cell2cell$CHILDREN))
chisq.test(table(cell2cell$CHURN,cell2cell$REFURB))
chisq.test(table(cell2cell$CHURN,cell2cell$WEBCAP))
chisq.test(table(cell2cell$CHURN,cell2cell$TRUCK))
chisq.test(table(cell2cell$CHURN,cell2cell$RV))
chisq.test(table(cell2cell$CHURN,cell2cell$OWNRENT))
chisq.test(table(cell2cell$CHURN,cell2cell$MAILORD))
chisq.test(table(cell2cell$CHURN,cell2cell$MAILRES))
chisq.test(table(cell2cell$CHURN,cell2cell$MAILFLAG))
chisq.test(table(cell2cell$CHURN,cell2cell$TRAVEL))
chisq.test(table(cell2cell$CHURN,cell2cell$PCOWN))
chisq.test(table(cell2cell$CHURN,cell2cell$CREDITCD))
chisq.test(table(cell2cell$CHURN,cell2cell$NEWCELLY))
chisq.test(table(cell2cell$CHURN,cell2cell$NEWCELLN))
chisq.test(table(cell2cell$CHURN,cell2cell$INCMISS))
chisq.test(table(cell2cell$CHURN,cell2cell$MCYLE))
chisq.test(table(cell2cell$CHURN,cell2cell$SETPRCM))
chisq.test(table(cell2cell$CHURN,cell2cell$MARRIAGE))
chisq.test(table(cell2cell$CHURN,cell2cell$PRIZMCODE))
chisq.test(table(cell2cell$CHURN,cell2cell$OCCUPATION))
chisq.test(table(cell2cell$CHURN,cell2cell$CREDIT_RATES))

#SPLITTING DATA INTO TRAINING AND TESTING
training<-cell2cell[cell2cell$CALIBRAT==1,]
testing<-cell2cell[cell2cell$CALIBRAT==0,]

#MODEL
fit<-glm(CHURN~REVENUE+MOU+RECCHRGE+DIRECTAS+OVERAGE+ROAM+CHANGEM+CHANGER+DROPVCE+BLCKVCE+UNANSVCE+CUSTCARE+THREEWAY+MOUREC+OUTCALLS
 +INCALLS+PEAKVCE+OPEAKVCE+DROPBLK+CALLFWDV+CALLWAIT+MONTHS+UNIQSUBS+ACTVSUBS+PHONES+MODELS+EQPDAYS+AGE1+CHILDREN+CREDIT_RATES
 +REFURB+WEBCAP+TRUCK+RV+OWNRENT+MAILORD+MAILRES+MAILFLAG+TRAVEL+PCOWN+CREDITCD
 +NEWCELLY+NEWCELLN+REFER+INCMISS+INCOME+MCYCLE+CREDITAD+SETPRCM+OCCUPATION+MARRIAGE+PRIZMCODE,data = training,family = binomial(logit))
summary(fit)
ls(fit)
fit$model

#CONCORDANCE
source("C:/Users/Abhishek/Desktop/BA/Concordance.R")
Concordance(fit)

#STEPWISE REGRESSION
step1=step(fit,direction = "both")


fit2<-glm(CHURN ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + CHANGEM + 
            CHANGER + BLCKVCE + UNANSVCE + CUSTCARE + THREEWAY + INCALLS + 
            PEAKVCE + DROPBLK + MONTHS + UNIQSUBS + ACTVSUBS + PHONES + 
            EQPDAYS + AGE1 + CHILDREN + CREDIT_RATES + REFURB + WEBCAP + 
            MAILRES + NEWCELLY + INCMISS + INCOME + CREDITAD + SETPRCM + 
            PRIZMCODE,data=training,family = binomial(logit))
summary(fit2)
Concordance(fit2)
fit3<-glm(CHURN ~ REVENUE + MOU + RECCHRGE + OVERAGE + ROAM + CHANGEM + 
      UNANSVCE + CUSTCARE + THREEWAY + INCALLS + 
      PEAKVCE + DROPBLK + MONTHS + UNIQSUBS + ACTVSUBS + PHONES + 
      EQPDAYS + AGE1 +CREDIT_RATES + REFURB + WEBCAP + 
      MAILRES +INCOME+ INCMISS + CREDITAD + SETPRCM + 
      PRIZMCODE,data=training,family = binomial(logit))
Concordance(fit3)
#VALIDATION
#TRAINING
train1<-cbind(training,prob=predict(fit3,training,type = "response"))
declocation<-quantile(train1$prob,probs = seq(0.1,0.9,by=0.1))
train1$decile<-findInterval(train1$prob,c(-Inf,declocation,Inf))

require(dplyr)
train1$decile<-factor(train1$decile)
decile_grp<-group_by(train1,decile)
decile_summ_train<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=prob), max_prob=max(prob), churn_cnt=sum(CHURN), 
                             non_churn_cnt=total_cnt - churn_cnt )
decile_summ_train<-arrange(decile_summ_train, desc(decile))
write.csv(decile_summ_train,"C:\\Users\\Abhishek\\Desktop\\results/training.csv")

#TESTING
test1<-cbind(testing,prob=predict(fit3,testing,type = "response"))
declocation<-quantile(test1$prob,probs = seq(0.1,0.9,by=0.1))
test1$decile<-findInterval(test1$prob,c(-Inf,declocation,Inf))

test1$decile<-factor(test1$decile)
decile_grp<-group_by(test1,decile)
decile_summ_test<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=prob), max_prob=max(prob), churn_cnt=sum(CHURN), 
                             non_churn_cnt=total_cnt - churn_cnt )
decile_summ_test<-arrange(decile_summ_test, desc(decile))

write.csv(decile_summ_test,"C:\\Users\\Abhishek\\Desktop\\results/testing.csv")
#CONFUSION MATRIX
table(train1$CHURN,train1$prob>0.53)
table(train1$CHURN,train1$prob>0.50)

install.packages("ROCR")
require(ROCR)
pred_train_fit3 <- prediction(train1$prob, train1$CHURN)
perf_fit3 <- performance(pred_train_fit3, "tpr", "fpr")
plot(perf_fit3)
abline(0, 1)
performance(pred_train_fit3, "auc")@y.values





