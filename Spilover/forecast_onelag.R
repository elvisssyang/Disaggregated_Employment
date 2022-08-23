# Elvis Yang
# June 2022 
# This is the program computes the multipliers and contrafactural or scenario forecasting 



library(matrixStats)
library(ggplot2)
library(zoo)
library(tidyverse)
library(dplyr)
library(stats)


library(readxl)
library(pracma)
library(tidyverse)
library(lubridate)
library(fpp3)


# INPUTS 

# y  Time series 
#p lags 
# varagin There are the(optional) bootstrapping options 
# 'parametric'   Draw residuals parametrically from the covariance matrix 
# 'bootstrap_with _replacement'  Draw residuals randomly from the empirical residuals 
# 'double_bootstrap'   Bias correction boostrap-after -bootstrap
# 'wild_bootstrap' Allws for hetroskedasticity by multiplying by a random variable 


alldata <- readr::read_csv("ABSemp.csv")  |> 
  mutate(Quarter = yearquarter(my(Date))) |> 
  select(-Date, -`96 Total`) |> 
  filter(Quarter <= yearquarter("2019 Q4")) |> 
  select(-Quarter) 


# generate the total number 
alldata <- alldata |> 
  mutate(`96 Total` = rowSums(alldata[,1:ncol(alldata)]))


# generate the total amount of the data 
rawdata <- as.matrix(alldata) # Remove the NA values due to the loading of the data.
logdata <- log(rawdata)
d4logdata <- 100*(logdata[5:nrow(logdata),]-logdata[1:(nrow(logdata)-4),]) %>% as.matrix()
summary(d4logdata)  # check summary to see if data are read correctly 

N=dim(d4logdata)[2]
p=4
lambda=0.2 # shrinkage 
maxhor=2 # maximum forecast horizon where we are imposing conditions 





# generate the phi of your estimated model



phi <- read.csv("one_lagphi.csv",header=FALSE) %>% as.matrix #estimated parameters produced by MATLAB code

n = nrow(d4logdata)
k = ncol(d4logdata)


# For users, you have to replace the numbers phi if you really want to build up another type of model
# The phi is produced using MATLAB MAIN.m file, please run that file to generate the estimated coefficients. 
# Here the phi are divided to give four lags and one constant. 
# phi[1,j] for constant, the rest are multiplied by the four lags and do the estimation 

rawhat = matrix(0,n,k)
yhat = matrix(0,n,k)  
sdiff = matrix(0,n,k)  


for (i in 5:n){
  for (j in 1:(k-1)){
    yhat[i,j]= phi[1,j] + t(d4logdata[(i-1),]) %*% phi[2:86,j]
  }
  rawhatv = rawdata[(i-4),1:(k-1)] * exp(yhat[i,1:(k-1)]/100) # back transform 
  yhat[i,k]=100*log(sum(rawhatv)/rawdata[(i-4),k])
  rawhat[i,] = rawdata[(i-4),1:k] * exp(yhat[i,1:k]/100)
  sdiff[i,] = exp(yhat[i,1:k]/100)
}  






# Use the updated data between COVID-19
# 
# alldata <- readr::read_csv("ABSemp.csv")  |>
#   mutate(Quarter = yearquarter(my(Date))) |>
#   select(-Date)
# 
# 
# alldata <- alldata |>
#   select(-Quarter)
# 
# 
# 
# rawdata <- as.matrix(alldata)
# 
# k = 85 

# Conduct our multiplier analysis 
# The row we set 44 is 4(4 last observations to start forecasting) + 40(10 years horizon of forecasting)



multiraw = matrix(0,44,k)
multiraw[1:4,]=rawdata[(nrow(rawdata)-3):nrow(rawdata),]
multig = matrix(0,44,k)
multilevels = matrix(0,44,k-1) # matrix that saves the evolution of total employment in reaction to 1 percent point shock in each sector
multigrowth = matrix(0,44,k-1) # matrix that saves the evolution of employment growth in reaction to 1 percentage point shock in each sector


for (sector in 1:(k-1)){
  for (i in 5:44){
    for (j in 1:(k-1)){
      multig[i,j]= multig[(i-1),]%*%phi[(2:86),j]
      if (i==5 & j==sector) {multig[i,j]=multig[i,j]+1}
    }
    multiraw[i,1:(k-1)] = multiraw[(i-4),1:(k-1)]*exp(multig[i,1:(k-1)]/100) # The first value is negative means related to this 
    
    multiraw[i,k] = sum(multiraw[i,1:(k-1)])
    
    multig[i,k]=100 * log(multiraw[i,k]/multiraw[(i-4),k])# Fourth difference and convert into percentage change  # PROBLEM: WHY LOG(1) \ne 0
  }
  multilevels[,sector]=multiraw[,k]
  multigrowth[,sector]=multig[,k]   # suspect here will need to select k instead sector 
}


multipliers = colSums(multigrowth)/4 # division by 4 is necessary because of seasonal differences
mpers = colCumsums(multigrowth[5:44,])/4 #colCumsums() is a function in matrixStats package
shares=colSums(rawdata[(nrow(rawdata)-3):nrow(rawdata),1:84])/sum(rawdata[(nrow(rawdata)-3):nrow(rawdata),85]) #sector shares estimated like this to eliminate the effect of seasonality
mtplers <- rbind(shares,mpers)
write.csv(file="multipliers_1.csv",rbind(shares,mpers))









## ----- Workbook till 17/07----Below codes hasn't changed yet --EY 


## forecasts
# baseline: no covid


fraw0 = matrix(0,24,k)
fraw0[1:4,]= rawdata[(nrow(rawdata)-3):nrow(rawdata),]
fgr0 = matrix(0,24,k)
fgr0[1:4,] = d4logdata[(nrow(d4logdata)-3):nrow(d4logdata),]
for (i in 5:24){
  for (j in 1:(k-1)){
    fgr0[i,j]= phi[1,j] + fgr0[(i-1),]%*%phi[(2:21),j] + fgr0[(i-2),]%*%phi[22:41,j] + fgr0[(i-3),]%*%phi[42:61,j] + fgr0[(i-4),]%*%phi[62:81,j]
  }
  fraw0[i,1:(k-1)] = fraw0[(i-4),1:(k-1)]*exp(fgr0[i,1:(k-1)]/100)
  fraw0[i,k] = sum(fraw0[i,1:(k-1)])
  fgr0[i,k]=100*log(fraw0[i,k]/fraw0[(i-4),k])
}
dd = as.yearqtr(2020+seq(1,20)/4)
nn = c("Date", "YoY", "Employment")
allrawfcasts = data.frame(cbind(dd,as.numeric(fgr0[5:24,20]),as.numeric(fraw0[5:24,20])))
colnames(allrawfcasts) = nn
#ggplot(allrawfcasts,aes(x=Date, y=YoY)) + geom_line() + xlab("Year-Quarter") + ylab("YoY Growth in Total Employment") + scale_x_yearqtr(format = "%YQ%q", breaks = seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.5), minor_breaks= seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.25))


# Scenario O (The forecast routine in the app is much nicer)
fraw1 = matrix(0,24,k)
fraw1[1:4,]= rawdata[(nrow(rawdata)-3):nrow(rawdata),]
fgr1 = matrix(0,24,k)
fgr1[1:4,] = d4logdata[(nrow(d4logdata)-3):nrow(d4logdata),]
flag = matrix(10000,4,19)
flag[1,]=c(-9.476325055,-2.915775394,-4.083103671,-0.16813662,-6.409944992,-4.394476644,-6.823570644,-33.39984639,-2.974739421,-6.489477032,-1.037667682,-11.0133505,-5.642445473,-10.04476017,-5.143099447,-1.961685419,-2.860893739,-26.95945301,-12.04715082)
# use this if input is YoY change 
#flag[1,]=c(-9.1,	-3.7,	-4.2,	-16.2,	-6.5,	-9.2,	-9.1,	-39.7,	-0.6,	-2.2,	6.8,	-11.7,	-2.9,	-13.9,	-5.6,	0.4,	4.1,	-33.2,	-18.7)
flag[2,]=c(-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999)
for (i in 5:24){
  for (j in 1:(k-1)){
    if (i==5 | i==6 | i==7 | i==8){
      if (flag[(i-4),j] == -9999) {
        fraw1[i,j] = fraw1[i-1,j]
        fgr1[i,j] = 100*log(fraw1[i,j]/fraw1[i-4,j])
      }
      else if (flag[(i-4),j] == 10000) {
        fgr1[i,j]= phi[1,j] + fgr1[(i-1),]%*%phi[(2:21),j] + fgr1[(i-2),]%*%phi[22:41,j] + fgr1[(i-3),]%*%phi[42:61,j] + fgr1[(i-4),]%*%phi[62:81,j]
        fraw1[i,j]=fraw1[i-4,j]*exp(fgr1[i,j]/100)
      }
      else {
        #        fgr1[i,j] = flag[(i-4),j] #use this and the line below it if user supplied figures are on YoY growth rates
        #        fraw1[i,j]=fraw1[i-4,j]*exp(fgr1[i,j]/100)
        fraw1[i,j] = fraw1[i-1,j]*(1+flag[(i-4),j]/100) #use this and the line below it if user inputs QoQ growth rates
        fgr1[i,j] = 100*log(fraw1[i,j]/fraw1[i-4,j])
      }
    }  
    else 
      fgr1[i,j]= phi[1,j] + fgr1[(i-1),]%*%phi[(2:21),j] + fgr1[(i-2),]%*%phi[22:41,j] + fgr1[(i-3),]%*%phi[42:61,j] + fgr1[(i-4),]%*%phi[62:81,j]
    fraw1[i,j]=fraw1[i-4,j]*exp(fgr1[i,j]/100)
  }
  fraw1[i,k] = sum(fraw1[i,1:(k-1)])
  fgr1[i,k]=100*log(fraw1[i,k]/fraw1[(i-4),k])
}
f1 = data.frame(cbind(as.numeric(fgr1[5:24,20]),as.numeric(fraw1[5:24,20])))
colnames(f1)=c("YoY1","Employment1")
allrawfcasts = cbind(allrawfcasts,f1)
ggplot(allrawfcasts) + geom_line(aes(x=Date,y=YoY),color="blue") + geom_line(aes(x=Date, y=YoY1),color="red") + xlab("Year-Quarter") + ylab("YoY Growth in Total Employment") + scale_x_yearqtr(format = "%YQ%q", breaks = seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.5), minor_breaks= seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.25))







