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



# This function will estimate a VAR with a constant using least squares with options of 
#various methods of bootstraping 




BVAR <- function(y,p,lambda){
  
  T=dim(y)[1]
  N=dim(y)[2]
  
  
  #backcast data with the mean 
  
  y <- rbind(repmat(colMeans(y),p,1),y)
  K=p*N+1
  
  #create data matrics 
  Y<-y[-1:-(p),]
  X=c()
  
  
  for(i in 1:p){
    rnmb<-p+1-i
    cnmb<-nrow(y)-i
    Z<-y[rnmb:cnmb,]
    X<-cbind(X,Z)
  }
  
  
  # constant to stack at last 
  X<-cbind(X,ones(T,1))
  
  # This is to set prior 
  
  
  small_sig2=zeros(N,1)
  Stack_AR_coeff=zeros(p,N)
  
  
  for(i in c(1:N)){
    #calculate variance for each equation to set pror 
    #by fitting an AR(4) per equation 
    
    c(~,small_sig2[i,1])=olsvar(y[,i],4)
    ARcoeff = olsvar(y[,i],1)
    
    if (ARcoeff >= 0.8){
      Stack_AR_coeff(1,i) = 1}
    
    # run an AR(1) to set to random walk if time series is persistant 
    # (>0.8)
  }
  
  
  Y_d = rbind(matrix(0,N*p,N), diag(sqrt(small_sig2/lambda)))
  
  for (i in 1:p){
    Y_d[(i-1)*N+1:i*N, ] <- diag(Stack_AR_coeff[i,]*repmat(1,N,1)*sqrt(small_sig))/lambda
  }
  
  X_d = rbind(kron(diag(1:P) , diag(sqrt(small_sig2) / lambda)) , zeros(N,K-1))
  
  X_d[size(X_d,1)+1,K] = 1e-10
  Y_d <- rbind(Y_d,zeros(1,N))     
  
  
  # Do least square to get posterior 
  
  Y_star = rbind(Y,Y_d)
  X_star = rbind(X,X_d)
  
  # Get posterior mode/mean of VAR coefficients 
  
  phi = mldivide(t(X_star) %*% X_star , t(X_star) %*% Y_star)
  
  
  e = Y - X %*% phi
  
  e_star = Y_star - X_star %*% phi
  
  
  
  # Posterior mode/mean of covariance matrix 
  
  
  SIGMA = mrdivide((t(e_star) %*% e_star), (size(Y_star,1) - size(phi,1)))
  
  
  # rearrange phi and X to just make constant in first row 
  
  phi = rbind(phi[nrow(phi),],phi[1:nrow(phi)-1,])
  
  X = cbind(X[,ncol(X)],X[,1:ncol(X)-1])
  
  print(phi)
}









# INPUTS 

# y  Time series 
#p lags 
# varagin There are the(optional) bootstrapping options 
# 'parametric'   Draw residuals parametrically from the covariance matrix 
# 'bootstrap_with _replacement'  Draw residuals randomly from the empirical residuals 
# 'double_bootstrap'   Bias correction boostrap-after -bootstrap
# 'wild_bootstrap' Allws for hetroskedasticity by multiplying by a random variable 


alldata <- readr::read_csv("ABSemp.csv")



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


#phi <- BVAR(d4logdata,p,lambda)
      

phi <- read.csv("phi.csv",header=FALSE) %>% as.matrix #estimated parameters produced by MATLAB code

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
    yhat[i,j]= phi[1,j] + t(d4logdata[(i-1),]) %*% phi[2:86,j] + t(d4logdata[(i-2),]) %*% phi[87:171,j] + t(d4logdata[(i-3),]) %*% phi[172:256,j] + t(d4logdata[(i-4),]) %*% 
      phi[257:341,j] # y_t = y_{t-1}+y_{t-1}+y{t-3}+y_{t-4}
  }
  rawhatv = rawdata[(i-4),1:(k-1)] * exp(yhat[i,1:(k-1)]/100) # back transform 
  yhat[i,k]=100*log(sum(rawhatv)/rawdata[(i-4),k])
  rawhat[i,] = rawdata[(i-4),1:k] * exp(yhat[i,1:k]/100)
  sdiff[i,] = exp(yhat[i,1:k]/100)
}  


# Perform out for sample forecast use the rawhat(estimation) to do
# Steps are similar use the last four observation to fit the phi above to generate t+1 forecast 




# Testing model using error measurements MAPE and Scaled errors

newraw <- rawdata[9:nrow(rawdata),] # Define the actual values of forecast 
rawhat1 <- rawhat[5:nrow(rawhat),]
error <- (newraw - rawhat1)
train_err <- matrix(0,n,k)
  
# Scale-dependent errors 
MAE = mean(abs(error))
RMSE = sqrt(mean(error))

# Percentage error 


for(i in 1:(n-4)){
  for (j in 1:k){
    train_err[i,j] <- 100 * as.vector(error[i,j]) / as.vector(newraw[i,j]) # Percentage error 
  }
}

# To calculate the MAPE we apply the formula as   sum(abs(y_t - \hat{y_t}/y_t))/(n)    

MAPE = sum(abs(train_err))/(n-4 * k ) 


# Scaled error -- Hyndman & Koehler(2006) see [https://otexts.com/fpp3/accuracy.html]


# First: Define the scaled error yt - y_{t-4} as sdiff and calculated in previous steps 

# sdiff = yt - y_{t-4} 

# Second, calculate the denominator as for seasonal time series m=4 

denom = sum(sdiff)/(n-4)



# Third calculate the q_j 

qj = matrix(0,n,k)

for(a in 1:i){
  for (b in 1:k){
    qj[a,b] = error[a,b]/denom
  }
}

MASE = mean(abs(qj))
RMSSE = sqrt(mean(qj^2))


# Conduct our multiplier analysis 

multiraw = matrix(0,44,k) # ELVIS -- why here we set the number equals to 44  
multiraw[1:4,] = rawdata[(nrow(rawdata)-3):nrow(rawdata),]
multig = matrix(0,44,k)
multilevels = matrix(0,44,k-1) # matrix that saves the evolution of total employment in reaction to 1 percent point shock in each sector
multigrowth = matrix(0,44,k-1) # matrix that saves the evolution of employment growth in reaction to 1 percentage point shock in each sector 
for (sector in 1:(k-1)){
  for (i in 5:44){
    for (j in 1:(k-1)){
      multig[i,j]= multig[(i-1),]%*%phi[(2:86),j] + multig[(i-2),]%*%phi[87:171,j] + multig[(i-3),] %*% phi[172:256,j] + multig[(i-4),] %*% phi[257:341,j]
      if (i==5 & j==sector) {multig[i,j]=multig[i,j]+1}
    }
    multiraw[i,1:(k-1)] = multiraw[(i-4),1:(k-1)]*exp(multig[i,1:(k-1)]/100)
    multiraw[i,k] = sum(multiraw[i,1:(k-1)])
    multig[i,k]=100*log(multiraw[i,k]/multiraw[(i-4),k])
  }
  multilevels[,sector]=multiraw[,k]
  multigrowth[,sector]=multig[,k]
}  

multipliers = colSums(multigrowth)/4 # division by 4 is necessary because of seasonal differences
mpers = colCumsums(multigrowth[5:44,])/4 #colCumsums() is a function in matrixStats package
shares=colSums(rawdata[(nrow(rawdata)-3):nrow(rawdata),1:84])/sum(rawdata[(nrow(rawdata)-3):nrow(rawdata),85]) #sector shares estimated like this to eliminate the effect of seasonality
write.csv(file="multipliers.csv",rbind(shares,mpers))




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







