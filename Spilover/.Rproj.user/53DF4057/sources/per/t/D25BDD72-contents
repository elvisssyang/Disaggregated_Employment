# Elvis Yang
# June 2022 
# This is the program computes the multipliers and contrafactural or scenario forecasting 


library(matrixStats)
library(ggploot2)
library(zoo)
library(tidyverse)
library(dplyr)
library(stats)


alldata <- readr::read_csv("ABSemp.csv")
rawdata <- as.matrix(alldata[-152,-c(85:89)]) # Remove the NA values due to the loading of the data. 
logdata <- log(rawdata)
d4logdata <- 100*(logdata[5:nrow(logdata),]-logdata[1:(nrow(logdata)-4),]) %>% as.matrix()
summary(d4logdata)  # check summary to see if data are read correctly 


phi <- read.csv("phi.csv",header=FALSE) %>% as.matrix #estimated parameters produced by MATLAB code
n = nrow(d4logdata)
k = ncol(d4logdata)
yhat = matrix(0,n,k)


# For users, you have to replace the numbers phi if you really want to build up another type of model
# Here the phi are divided to give four lags and one constant. 
# phi[1,j] for constant, the rest are multiplied by the four lags and do the estimation 


for (i in 5:n){
  for (j in 1:(k-1)){
    yhat[i,j]= phi[1,j] + t(d4logdata[(i-1),]) %*% phi[2:85,j] + t(d4logdata[(i-2),]) %*% phi[86:169,j] + t(d4logdata[(i-3),]) %*% phi[170:253,j] + t(d4logdata[(i-4),]) %*% 
      phi[254:337,j]
  }
  rawhat = rawdata[(i-4),1:(k-1)] * exp(yhat[i,1:(k-1)]/100)
  yhat[i,k]=100*log(sum(rawhat)/rawdata[(i-4),k]) 
}  



# Testing model using error measurements MAPE and Scaled errors 

error <- (d4logdata - yhat)[-c(1:4), ]
train_err <- matrix(0,n,k)
  
for(i in 1:(n-4)){
  for (j in 1:k){
    train_err[i,j] <- 100 * as.vector(error[i,j]) / as.vector((d4logdata[-c(1:4),])[i,j]) # MAPE 
  }
}




# Conduct our multiplier analysis 

multiraw = matrix(0,44,k) # ELVIS -- why here we set the number equals to 44  
multiraw[1:4,] = rawdata[(nrow(rawdata)-3):nrow(rawdata),]
multig = matrix(0,44,k)
multilevel = matrix(0,44,k-1) # matrix that saves the evolution of total employment in reaction to 1 percent point shock in each sector
multigrowth = matrix(0,44,k-1) # matrix that saves the evolution of employment growth in reaction to 1 percentage point shock in each sector 
for (sector in 1:(k-1)){
  for (i in 5:44){
    for (j in 1:(k-1)){
      multig[i,j]= multig[(i-1),]%*%phi[(2:21),j] + multig[(i-2),]%*%phi[22:41,j] + multig[(i-3),]%*%phi[42:61,j] + multig[(i-4),]%*%phi[62:81,j]
      if (i==5 & j==sector) {multig[i,j]=multig[i,j]+1}
    }
    multiraw[i,1:(k-1)] = multiraw[(i-4),1:(k-1)]*exp(multig[i,1:(k-1)]/100)
    multiraw[i,k] = sum(multiraw[i,1:(k-1)])
    multig[i,k]=100*log(multiraw[i,k]/multiraw[(i-4),k])
  }
  multilevels[,sector]=multiraw[,k]
  multigrowth[,sector]=multig[,k]
}  








