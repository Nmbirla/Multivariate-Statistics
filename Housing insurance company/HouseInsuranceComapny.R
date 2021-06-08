############################################################################################
##Author:Neha Birla
##Date:10/8/2019
############################################################################################


##Set the working directory
setwd('N:\\umich Google Drive\\Education\\UMD\\Fall 2019\\IMSE 514\\Homeworks\\HW3\\code')

## Get the working directory
getwd()

##Read a CSV file
givendata=read.table(file="HW3Data.txt",header=T)

names(givendata)

##Attach data to drop givendata$...

#attach(givendata)


## Model 1: Linear Model
givendata$Flood<-as.factor(givendata$Flood)
help("as.matrix")
aa=as.matrix(givendata$Flood)
lm_1<-lm(givendata$Declination~givendata$Flood+givendata$MinorityPop+givendata$FireReport+givendata$CrimeRate+givendata$HouseAge+givendata$Income,data=givendata)
summary(lm_1)

##Model 1: Checking assumptions of regrssion model
##
Fit_val=fitted.values(lm_1)
Res_Val=residuals(lm_1)


plot(Fit_val,Res_Val)

plot(Res_Val,givendata$Declination) #Checkin if error and respose are independent

#Checkin if regressors and error are independent

plot(Res_Val,givendata$Flood) 
plot(Res_Val,givendata$MinorityPop)
plot(Res_Val,givendata$FireReport)
plot(Res_Val,givendata$CrimeRate)
plot(Res_Val,givendata$HouseAge)
plot(Res_Val,givendata$Income)

##Coreelation 
ac_lm_1 <- acf(resid(lm_1))


###############################################################################################
#Variable selection for improving model performance

library(leaps)
Null<-lm(givendata$Declination~1,data = givendata)

Full<-lm(givendata$Declination~.,data=givendata)
lm_Back=step(Full, scope = list(lower = Null, upper=Full), direction="backward")

summary(lm_Back)

lm_Forwd=step(Null,scope = list(lower = Null,upper=Full),direction="forward")

summary(lm_Forwd)

lm_Stepwise=step(Full,scope = list(lower = Null,upper=Full),direction="both",steps=1000)
summary(lm_Stepwise)

######################################################################################################
#Improving model using WLS

#lm_wls=lm(lm_1<-lm(givendata$Declination~givendata$Flood+givendata$MinorityPop+givendata$FireReport+givendata$CrimeRate+givendata$HouseAge+givendata$Income,data=givendata,weights = NULL)

#w <- c(0.2, 0.5, 0.05, 0.3 ,0.2)
# weights = NULL
#lm_wls=lm(lm_Stepwise,)


summary(lm_wls)

######################################################################################################
library('nlme')
##lm_gen <- gls(Declination ~ Flood + MinorityPop + FireReport + CrimeRate + HouseAge + Income, correlation = corAR1(value=0.2878,fixed=T),data=givendata)
##summary(lm_gen)
