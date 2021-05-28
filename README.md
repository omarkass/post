# post
---
title: "Session 12: NY Cabs Data - Generalized Linear Model"
author: "Lidia Montero"
date: "May 26th, 2021"
output: 
  html_document: 
    toc: true
    toc_depth: 3
    number_sections: true
editor_options: 
  chunk_output_type: console
---

# Introduction

## Data Description

Description http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml
Data Dictionary - SHL Trip Records -This data dictionary describes SHL trip data in visit http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml:

  - VendorID	A code indicating the LPEP provider that provided the record.      1= Creative Mobile Technologies, LLC; 2= VeriFone Inc.   
  - lpep_pickup_datetime	The date and time when the meter was engaged.    
  - lpep_dropoff_datetime	The date and time when the meter was disengaged.     
  - Passenger_count	The number of passengers in the vehicle. This is a driver-entered value.    -  Trip_distance 	The elapsed trip distance in miles reported by the taximeter.   
  - Pickup_longitude	 Longitude where the meter was engaged.   
  - Pickup_latitude	Latitude where the meter was engaged.   
  - RateCodeID	The final rate code in effect at the end of the trip.  1= Standard rate  2=JFK 3=Newark 4=Nassau or Westchester 5=Negotiated fare 6=Group ride   
  - Store_and_fwd_flag	This flag indicates whether the trip record was held in vehicle memory before sending to the vendor, aka "store and forward," because the vehicle did not have a connection to the server: Y= store and forward trip  N= not a store and forward trip   
  - Dropoff_longitude	Longitude where the meter was timed off.   
  - Dropoff_ latitude	Latitude where the meter was timed off.   
  - Payment_type	A numeric code signifying how the passenger paid for the trip.  1= Credit card 2= Cash 3= No charge 4= Dispute 5= Unknown 6= Voided trip   
  - Fare_amount	The time-and-distance fare calculated by the meter.   
  - Extra	 Miscellaneous extras and surcharges.  Currently, this only includes the $0.50 and $1 rush hour and overnight charges. 
  - MTA_tax	 $0.50 MTA tax that is automatically triggered dfd on the metered rate in use.    - Improvement_surcharge	$0.30 improvement surcharge assessed on hailed trips at the flag   drop. The improvement surcharge began being levied in 2015.   
  - Tip_amount	 This field is automatically populated for credit card tips. Cash tips are not included.   
  - Tolls_amount	Total amount of all tolls paid in trip.    
  - Total_amount	The total amount charged to passengers. Does not include cash tips.   
  - Trip_type	A code indicating whether the trip was a street-hail or a dispatch that is automatically assigned dfd on the metered rate in use but can be altered by the driver. 
1= Street-hail 2= Dispatch  

## Load Required Packages

```{r}
# Load Required Packages: to be increased over the course
options(contrasts=c("contr.treatment","contr.treatment"))

requiredPackages <- c("effects","FactoMineR","car","missMDA","mvoutlier","chemometrics", "factoextra","RColorBrewer","ggplot2","dplyr","ggmap","ggthemes","knitr")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(requiredPackages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
#verify they are loaded
search()
```

# Statistical Modelling

## Load Data after Cleaning and EDA
```{r}

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clean workspace
rm(list=ls())

setwd("C:/Users/lmontero/Dropbox/DOCENCIA/FIB-ADEI/PRACTICA/NYCABS/LABS")
filepath<-"C:/Users/lmontero/Dropbox/DOCENCIA/FIB-ADEI/PRACTICA/NYCABS/LABS/"
# green_tripdata_2016-01

### Load  workspace already clean and prepared

load(paste0(filepath,"Ex-MyTaxi250Clean.RData"))
summary(df)

vars_con<-names(df)[c(7:14,17:21)]
vars_dis<-names(df)[c(1,2,22,24:36)]
vars_res<-names(df)[c(15,23)]
vars_cexp<-vars_con[c(1:2,4:13)]
vars_con;vars_dis;vars_dis;vars_cexp

options(contrasts=c("contr.treatment","contr.treatment"))
```

# Binary Target

```{r}
# Feature Selection 
table(df$AnyTip,df$Payment_type)

# Or using condes()
res.cat <- catdes(df,num.var=which(names(df)=="AnyTip"))
res.cat$quanti.var
res.cat$quanti
res.cat$test.chi2
res.cat$category

df$f.extra<-0
df$f.extra[df$Extra>0]<-1
df$f.extra[df$Extra>0.5]<-2
df$f.extra<-factor(df$f.extra,labels=c("Extra-No","Extra-0.5","Extra-1"))
df$f.tax<-ifelse(df$MTA_tax>0,1,0)
df$f.tax<-factor(df$f.tax,labels=c("TAX-No","TAX-Yes"))

```

```{r}
# Split your sample: work and test
ll<-which(df$Payment_type=="Cash");length(ll)
dff<-df[-ll,]
set.seed(12345)
llwork<-sample(1:nrow(dff),0.70*nrow(dff),replace=FALSE)
llwork<-sort(llwork);length(llwork)
dffwork<-dff[llwork,]
dfftest<-dff[-llwork,]
```

# Use explanatory numeric variables

```{r}

vars_cexp <- vars_cexp[c(1,3,4,6,9:12)];vars_cexp
m50<-glm(AnyTip~.,family="binomial",data=dffwork[,c("AnyTip",vars_cexp)])
summary(m50)

m51<-glm(AnyTip~Passenger_count+tlenkm+Tolls_amount+espeed+hour+Extra,family="binomial",data=dffwork)
summary(m51)

m51aux<-glm(AnyTip~tlenkm+Tolls_amount+espeed+hour+Extra,family="binomial",data=dffwork)
anova( m51aux, m51,test="Chisq") # Deviance Test for Nested Models

Anova(m51,test="LR")
vif(m51)

m52<-step(m51) # Full sample ~3500 obs ,k=log(nrow(dffwork)) BIC monitorized
summary(m52)

Anova(m52,test="LR")
vif(m52)
plot(allEffects(m52))
marginalModelPlots(m52)
residualPlots(m52)
summary(m52)
BIC(m50,m51,m52)

# Convenient any transformation to explanatory variables

m53 <-glm(AnyTip~poly(tlenkm,3)+poly(espeed,3),family="binomial",data=dffwork)
Anova( m53, test="LR" )
summary( m53 )

m54 <-glm(AnyTip ~ tlenkm+poly(espeed,2),family="binomial",data=dffwork)
anova( m54, m53, test="Chisq")

marginalModelPlots(m54)
```

#Diagnostics

```{r}
residualPlots(m54)

# NEVER plot( m54 ): no sense
```


# Consider factors and interactions as explanatory variables

```{r}

m60<-glm(AnyTip~ tlenkm + f.speed, family="binomial", data=dffwork)
m600<-glm(AnyTip~ f.tlenkm+ poly(espeed,2) , family="binomial", data=dffwork)
summary(m60)
# anova(m54, m60) # NOT NESTED: not possible
BIC(m60,m54,m600)

m61<-glm(AnyTip~tlenkm + poly(espeed,2) + f.tax, family="binomial", data=dffwork)  # f.tax has 1 level - error
summary(m61)
summary(dffwork)
BIC(m60,m61)
vif(m61)

vars_dis

m62<-glm(AnyTip~tlenkm + poly(espeed,2) + VendorID +period +f.tolls +f.extra, family="binomial", data=dffwork)
summary(m62)
vif(m62)

m63<-step(m62) # ,k=log(nrow(dffwork))
summary(m63)
Anova( m63, test="LR")
BIC(m61,m62,m63)
vif(m63)

# Interactions: Firstly Numeric * Factors   Secondly Factor*Factor
# 
m72<-glm(AnyTip~(tlenkm + poly(espeed,2))*( VendorID +period +f.tolls +f.extra), family="binomial", data=dffwork) # Does not work

m72<-glm(AnyTip~(tlenkm + poly(espeed,2))*( period +f.tolls ) +VendorID+f.extra , family="binomial", data=dffwork) # Does not work

m73<-step(m72)
summary(m73)
BIC(m61,m62,m63,m72,m73)
vif(m73)

Anova(m73,test="LR")
vif(m73)

plot(allEffects(m73))
```

# Final Diagnostics

```{r}
# Boxplot dels residus
Boxplot(rstudent(m73),id.n=15)
sout<-which(abs(rstudent(m73))>2);length(sout)
llout<-which(row.names(dffwork) %in% names(rstudent(m73)[sout]));llout
dffwork[llout,] 
# Observacions potencialment influents
quantile(hatvalues(m73),seq(0,1,0.1))
mean(hatvalues(m73))
hh<-5*mean(hatvalues(m73));hh
shat<-which(hatvalues(m73)>hh);length(shat);shat
llhat<-which(row.names(dffwork) %in% names(rstudent(m73)[shat]));llhat
dffwork[llhat,]
# Influent data

Boxplot(cooks.distance(m73))

scoo<-which(cooks.distance(m73)>0.014);length(scoo);scoo
llcoo<-which(row.names(dffwork) %in% names(cooks.distance(m73)[scoo]));llcoo

llista<-influencePlot(m73,id=c(list="noteworthy",n=10))
attributes(llista)
dffwork[llcoo,]

# Confussion Table
predict(m73,type="response")
fit.AnyTip<-factor(ifelse(predict(m73,type="response")<0.5,0,1),labels=c("fit.No","fit.Yes"))
tt<-table(fit.AnyTip,dffwork$AnyTip);tt
sum(tt)
100*sum(diag(tt))/sum(tt)

m0<-glm(AnyTip~1, family="binomial", data=dffwork)
fit0<-predict(m0,type="response")
fit.AnyTip0<-factor(ifelse(fit0<0.5,0,1),labels=c("fit.Yes"))
tt0<-table(fit.AnyTip0,dffwork$AnyTip);tt0;sum(tt0)
100*sum(tt0[1,2])/sum(tt0)

library("ROCR")
dadesroc<-prediction(predict(m73,type="response"),dffwork$AnyTip)
par(mfrow=c(1,2))
plot(performance(dadesroc,"err"))
plot(performance(dadesroc,"tpr","fpr"))
abline(0,1,lty=2)

# Interpretation
summary(m0) # log(odd) = 1.4553
exp(1.4553)/(1+exp(1.4553)) # Probability

summary(m73)  # Interpret tlenkm
# Period - factor
coef(m73)[5] # logodd increment in Morning vs Ref - all else being equal (ceteris paribus)
exp(coef(m73)[5]) # odd  in Morning 36599562* odd Ref - all else being equal (ceteris paribus)

```
