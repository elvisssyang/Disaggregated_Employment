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




alldata_init <- readr::read_csv("ABSemp.csv")  |> 
  mutate(Quarter = yearquarter(my(Date))) |> 
  select(-Date, -`96 Total`) 


alldata <- alldata_init|> 
  filter(Quarter <= yearquarter("2020 Q1")) |> 
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
maxhor=2 # maximum forecast horizon where we are imposing conditions 





# generate the phi of your estimated model



phi <- read.csv("phi_all_4lags.csv",header=FALSE) %>% as.matrix #estimated parameters produced by MATLAB code

n = nrow(d4logdata)
k = ncol(d4logdata)


# For users, you have to replace the numbers phi if you really want to build up another type of model
# The phi is produced using MATLAB MAIN.m file, please run that file to generate the estimated coefficients. 


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
 
rawhat <-  rbind(matrix(0,4,85),rawhat)


dd_origin = as.yearqtr(1987 + seq(2,135)/4)
origin_fore <- data.frame(dd_origin, "Estimated" = rawhat[-c(1:8),85] , `Actual`= as_tibble(alldata[-c(1:8),85]))

 origin_fore |> 
  ggplot(aes(x = dd_origin, y = Estimated))+ geom_line(colour = "Blue") + geom_line(aes(y =`X96.Total`))





# Conduct our multiplier analysis
# The row we set 44 is 4(4 last observations to start forecasting) + 40(10 years horizon of forecasting)
# 



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
mtplers = rbind(shares,mpers)
#write.csv(file="multipliers.csv",rbind(shares,mpers))




# Calculate the spillover measures 

m5m0_div <- mtplers[41,] / shares

m5m0_min <- (mtplers[41,] - shares) 

spilloverm5_timesm0 <- m5m0_div |> 
  sort(decreasing = TRUE) |> 
  enframe()


spilloverm5_m0 <- m5m0_min |> 
  sort(decreasing = TRUE) |> 
  enframe()


Manuf <- sum(spilloverm5_m0[grep("Manufacturing", spilloverm5_m0$name),]$value)

Const <- sum(spilloverm5_m0[grep("Construction", spilloverm5_m0$name),]$value)




## forecasts
# baseline: no covid
fraw0 = matrix(0,24,k)
fraw0[1:4,]= rawdata[(nrow(rawdata)-3):nrow(rawdata),]
fgr0 = matrix(0,24,k)
fgr0[1:4,] = d4logdata[(nrow(d4logdata)-3):nrow(d4logdata),]
for (i in 5:24){
  for (j in 1:(k-1)){
    fgr0[i,j]= phi[1,j] + fgr0[(i-1),]%*%phi[(2:86),j] + fgr0[(i-2),]%*%phi[87:171,j] + fgr0[(i-3),]%*%phi[172:256,j] + fgr0[(i-4),]%*%phi[257:341,j]
  }
  fraw0[i,1:(k-1)] = fraw0[(i-4),1:(k-1)]*exp(fgr0[i,1:(k-1)]/100)
  fraw0[i,k] = sum(fraw0[i,1:(k-1)])
  fgr0[i,k]=100*log(fraw0[i,k]/fraw0[(i-4),k])
}
dd = as.yearqtr(2020+seq(1,20)/4)
nn = c("Date", "YoY", "Employment")
allrawfcasts = data.frame(cbind(dd,as.numeric(fgr0[5:24,85]),as.numeric(fraw0[5:24,85])))
colnames(allrawfcasts) = nn
ggplot(allrawfcasts,aes(x=Date, y=YoY)) + geom_line() + xlab("Year-Quarter") + ylab("YoY Growth in Total Employment") + scale_x_yearqtr(format = "%YQ%q", breaks = seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.5), minor_breaks= seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.25))




# Compare the difference between the actual data and forecasts 

actual_covid <- alldata_init |> 
  filter(Quarter >= yearquarter("2020 Q2")) |> 
  select(-Quarter)  

actual_covid <- actual_covid |> 
  mutate(`96 Total` = rowSums(actual_covid[,1:ncol(actual_covid)-1]))


comp_fore <- cbind(allrawfcasts[1:9,],  actual_covid[,ncol(actual_covid)]) 



ggplot(comp_fore,aes(x=Date, y=Employment)) + geom_line(aes(x = Date, y = Employment), colour = "Red") + geom_line(aes(x = Date, y = `96 Total`)) + ylab("Total Employment") + scale_x_yearqtr(format = "%YQ%q", breaks = seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.5), minor_breaks= seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.25))


comp_fore_val <- comp_fore |> 
  mutate(dd_origin = as.yearqtr(Date), Estimated = Employment, `X96.Total` = `96 Total`) |> 
  select(-`Date`, -`YoY`, - `Employment`, -`96 Total`)


dd_origin = as.yearqtr(1987 + seq(2,135)/4)


 origin_fore <- data.frame(dd_origin, "Estimated" = rawhat[-c(1:8),85] , `Actual`= as_tibble(alldata[-c(1:8),85]))

estim_fore <- rbind(origin_fore,comp_fore_val)

estim_fore |> 
  ggplot(aes(x = dd_origin, y = Estimated))+ geom_line(colour = "Blue") + geom_line(aes(y =`X96.Total`))




