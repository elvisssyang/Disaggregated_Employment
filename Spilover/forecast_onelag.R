# Elvis Yang
# June 2022 
# This is the program computes the multipliers and contrafactural or scenario forecasting 



library(matrixStats)
library(ggplot2)
library(zoo)
library(tidyverse)
library(dplyr)
library(stats)


library(readr)
library(pracma)
library(lubridate)
library(fpp3)
library(ggpubr)

# generate the circular plots 
library(viridis)



# Donut plot 

library(lessR)


# clean the data and reset the total employment by removing the NFD data  

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





# generate the phi of your estimated model



phi <- read.csv("phi_lambda_00808.csv",header=FALSE) %>% as.matrix #estimated parameters produced by MATLAB code

n = nrow(d4logdata)
k = ncol(d4logdata)


# For users, you have to replace the numbers phi if you really want to build up another type of model
# The phi is produced using MATLAB MAIN.m file, please run that file to generate the estimated coefficients. 

rawhat = matrix(0,n+4,k)
yhat = matrix(0,n,k)  
sdiff = matrix(0,n+4,k)  


for (i in 2:n){
  for (j in 1:(k-1)){
    yhat[i,j]= phi[1,j] + t(d4logdata[(i-1),]) %*% phi[2:86,j]
  }
  rawhatv = rawdata[i,1:(k-1)] * exp(yhat[i,1:(k-1)]/100) # back transform 
  yhat[i,k]=100*log(sum(rawhatv)/rawdata[i,k])
  rawhat[i+4,] = rawdata[i,1:k] * exp(yhat[i,1:k]/100)
  sdiff[i+4,] = exp(yhat[i,1:k]/100)
}  

test <- rawhat[-c(1:5),] - rawdata[-c(1:5),]



dd_origin = as.yearqtr(1986 + seq(0,136)/4)


origin_fore <- data.frame(dd_origin, "Estimated" = rawhat[-c(1:5),85] , `Actual`= as_tibble(alldata[-c(1:5),85]))


# Compute the original data 


origin_fore |> 
  ggplot(aes(x = dd_origin, y = Estimated))+ geom_line(colour = "Blue") + geom_line(aes(y =`X96.Total`))





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
    multiraw[i,1:(k-1)] = multiraw[(i-4),1:(k-1)] * exp(multig[i,1:(k-1)]/100) # The first value is negative means related to this

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
#write.csv(file="multipliers.csv",rbind(shares,mpers))




# Calculate the spxillover measures 

m5m0_div <- mtplers[40,] / shares

m5m0_min <- (mtplers[40,] - shares) 

# write.csv(file = "spillover_effect.csv",cbind(m5m0_div,m5m0_min))


spilloverm5_timesm0 <- m5m0_div |> 
  sort(decreasing = TRUE) |> 
  enframe()



spilloverm5_m0 <- m5m0_min |> 
  sort(decreasing = TRUE) |> 
  enframe()

write.csv(file = "long_effect_min.csv",spilloverm5_m0)
 
 

# Forecasts based on the scenario that no covid happened 

fraw0 = matrix(0,26,k)
fraw0[1:5,]= rawdata[(nrow(rawdata)-4):nrow(rawdata),]
fgr0 = matrix(0,26,k)
fgr0[1,] = d4logdata[nrow(d4logdata),]



for (i in 2:21){
  for (j in 1:(k-1)){
    fgr0[i,j]= phi[1,j] + fgr0[(i-1),]%*%phi[(2:86),j] 
  }
  fraw0[i+4,1:(k-1)] = fraw0[i,1:(k-1)]*exp(fgr0[i,1:(k-1)]/100)
  fraw0[i+4,k] = sum(fraw0[i+4,1:(k-1)])
  fgr0[i,k]=100*log(fraw0[i+4,k]/fraw0[i,k])
}


dd = as.yearqtr(2020+seq(1,20)/4)
nn = c("Date", "YoY", "Employment")
allrawfcasts = data.frame(cbind(dd,as.numeric(fgr0[2:21,85]),as.numeric(fraw0[6:25,85])))
colnames(allrawfcasts) = nn


# actual YoY 

actual_covid <- alldata_init |> 
  filter(Quarter >= yearquarter("2020 Q2")) |> 
  select(-Quarter)  

actual_covid <- actual_covid |> 
  mutate(`96 Total` = rowSums(actual_covid[,1:ncol(actual_covid)-1]))



yoy_act <- rbind(as.matrix(alldata[139:142,85]),as.matrix(actual_covid[1:9,85]) )

as.matrix(allrawfcasts$Employment)[1:9]

fgr1= matrix(0,9,1)

for (jj in 5:13){
  
  fgr1[jj-4,]= 100*log(as.matrix(yoy_act[jj,]) / as.matrix(yoy_act[jj-4,]))
  
}

duringcov <- cbind(allrawfcasts[1:9,],fgr1)

grow1 <- ggplot(duringcov,aes(x = Date, y = YoY)) + 
  geom_line(aes(x = Date, y = YoY), colour = "Red") + 
  geom_line(aes(x = Date, y = fgr1), colour = "Blue") +
  ylab("YoY Growth in Total Employment in %") + 
  scale_x_yearqtr(format = "%YQ%q", breaks = seq(as.yearqtr(2020.25),as.yearqtr(2022.25), by = 0.5), minor_breaks= seq(as.yearqtr(2020.25),as.yearqtr(2022.25), by = 0.25))+ 
  labs(title = "Counterfactual Analysis of Year on Year Growth Rate", subtitle = "Red: Optimistic Scenario; Blue: Actual ")


grow1
#write.csv(file = "growth.csv",allrawfcasts)
 
grow2 <- ggplot(allrawfcasts,aes(x=Date, y=YoY)) +
  geom_line(aes(x = Date, y = YoY), colour = "Red") +
  ylab("YoY Growth in Total Employment in %") + 
  scale_x_yearqtr(format = "%YQ%q", breaks = seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.5), minor_breaks= seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.25))+ 
  labs(title = "Forecast Values of Year-on-Year Growth Rate", subtitle = "Total Employment Based on the Optimistic Scenario")


grow1

grow2

# Compare the difference between the actual data and forecasts 



comp_fore <- cbind(allrawfcasts[1:9,],  actual_covid[,ncol(actual_covid)]) 
  

## BOOTSTRAP 

# Confidence Interval via Bootstrap based on empirical residuals 

nboot = 1000

bootstraped_list = matrix(0,20,1000)

confidence_interval = matrix(0,20,2)

resid = d4logdata[-1,] - yhat[-1,]

fraw_boot = matrix(0,26,k)
fraw_boot[1:5,]= rawdata[(nrow(rawdata)-4):nrow(rawdata),]
fgr_boot = matrix(0,26,k)
fgr_boot[1,] = d4logdata[nrow(d4logdata),]

# Conduct the bootstrap via empirical residual 

for (bb in 1:20){
  for (b in 1:nboot){
    for (i in 2:21){
      for (j in 1:(k-1)){
        fgr_boot[i,j]= phi[1,j] + fgr_boot[(i-1),]%*%phi[(2:86),j] + sample(resid, size = 1)
      }
      fraw_boot[i+4,1:(k-1)] = fraw_boot[i,1:(k-1)]*exp(fgr_boot[i,1:(k-1)]/100)
      fraw_boot[i+4,k] = sum(fraw_boot[i+4,1:(k-1)])
      fgr_boot[i,k]=100*log(fraw_boot[i+4,k]/fraw_boot[i,k])
    }
    bootstraped_list[,b] = as.numeric(fraw_boot[6:25,85])
  }

 confidence_interval[bb,] = as.numeric(quantile(bootstraped_list[bb,], prob = c(0.2,0.8)))

}


# Then we can plot the confidence interval respect to the actual forecasts 


CI_80 <- data.frame("lower_bound" = confidence_interval[,1],"upper_bound" = confidence_interval[,2])


comp_fore <- cbind(CI_80[1:9,], comp_fore)



# The forecast number of total employment via Actual data, with 95 CI generated via bootstrap $
#write.csv(file ="oneemp.csv", comp_fore)

con <- ggplot(comp_fore,aes(x=Date, y=Employment)) + 
  geom_line(aes(x = Date, y = Employment), colour = "Blue") + 
  geom_line(aes(x = Date, y = `96 Total`)) + 
  geom_line(aes(x = Date, y = `lower_bound`),colour = "Red", linetype = "dashed") +
  geom_line(aes(x = Date, y = `upper_bound`),colour = "Red", linetype = "dashed") +
  labs(title = "Counterfactual Analysis of Total Employment", subtitle = "80% CI via Bootstrap with Empirical Residuals") + 
  ylab("Total Employment") + scale_x_yearqtr(format = "%YQ%q", breaks = seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.5), minor_breaks= seq(as.yearqtr(2020.25),as.yearqtr(2025.0), by = 0.25))




comp_fore_val <- comp_fore |> 
  mutate(dd_origin = as.yearqtr(Date), Estimated = Employment, `X96.Total` = `96 Total`) |> 
  select(-`Date`, -`YoY`, - `Employment`, -`96 Total`,-`lower_bound`, -`upper_bound`)

estim_fore <- rbind(origin_fore,comp_fore_val)

fits <- estim_fore |> 
  ggplot(aes(x = dd_origin, y = Estimated))+ geom_line(colour = "Blue") + geom_line(aes(y =`X96.Total`),colour = "Red",linetype = "dashed") +
  labs(x = "Quarter", y = "Employment in ('000)", title = "Fitness of the proposed VAR Modelling", subtitle = "Red line is Actual Data;Blue line is the Estimated Data ")

ggarrange(fits,con,nrow = 1, ncol = 2)


# Extract the shares of sectors 

fore_shares <- as_tibble(t(mtplers[1,]))

actual = matrix(0,9,k)



covidloss <- colSums(as.matrix(comp_fore$Employment) - as.matrix(comp_fore$`96 Total`))

# lower bound 
covidloss_lower <- colSums(as.matrix(comp_fore$lower_bound) - as.matrix(comp_fore$`96 Total`))

# upper bound 
covidloss_upper <- colSums(as.matrix(comp_fore$upper_bound) - as.matrix(comp_fore$`96 Total`))


covid_losses<- as.matrix(fraw0[6:14,]) - as.matrix(actual_covid[1:9,])

average_losses <- colSums(covid_losses)/nrow(covid_losses)

# Average losses 

rowSums(average_losses)










