library(matrixStats)
library(ggplot2)
library(zoo)
library(tidyverse)
library(dplyr)
library(stats)
library(ggpubr)


library(readxl)
library(pracma)
library(lubridate)
library(fpp3)


labour <- read_csv("total_labour_force_monthly.csv")

employment <- read_csv("ABSemp.csv")

oneemp <- read_csv("oneemp.csv")

NFD_data<- read_csv("NFD_data.csv")

NFD <- NFD_data$Total

empfore<- oneemp$`96 Total`

cont_emp <- oneemp$Employment


tot_postcov <- employment |> 
  select(Date,`96 Total`) |> 
  mutate(Quarter = yearquarter(my(Date))) |> 
  filter(Quarter > yearquarter("2020 Q1"))


ts_labour <- labour |>
  filter(month(my(Date)) %in% c(2,5,8,11)) |> 
  mutate(Quarter = yearquarter(my(Date))) |>
  select(-Date) |>
  as_tsibble(index = Quarter)


ts_labour <- ts_labour |> 
  filter(Quarter >= yearquarter("1984 Q4") & Quarter <= yearquarter("2022 Q2")  )

# ts_labour$labour_force = ts_labour$labour_force - NFD

pre_2020 <- ts_labour |> 
  filter(Quarter <= yearquarter("2020 Q1"))


post_2020 <- ts_labour|> 
  filter(Quarter > yearquarter("2020 Q1"))



fc_model <- pre_2020 |> 
  model(stepwise = ARIMA(labour_force))




fc_emp<- fc_model |> 
  forecast(h=9)

# 
# fc_emp_aftercovid <- fc_emp |> 
#   filter(month(Month) %in% c(2,5,8,11))


plabour1<- fc_emp|> 
  autoplot(ts_labour) + 
  labs(x = "Quarter", y = "Total Labour Force", 
       title = "Forecasts of total labour force in Australia", 
       subtitle = "Black: Actual Data")

# zoomed ones 


plabour2<- fc_emp|> 
  filter(Quarter > yearquarter("2020 Q1") & Quarter <= yearquarter("2022 Q2"))|> 
  autoplot(post_2020) + 
  labs(x = "Quarter", y = "Total Labour Force", 
       title = "Forecasts of total labour force in Australia",
       subtitle = "Data Range from 2020 Q2 to 2022 Q1",
       caption = "Black: Actual Datal; Data Range till 2022 Q1")



ggarrange(plabour1,plabour2)




# accumulative loss 


acc_LOSS <- colSums(as.matrix(fc_emp$.mean - post_2020$labour_force))





fc_emp$.mean / post_2020$labour_force


# real unemployment rate 
(post_2020$labour_force - tot_postcov$`96 Total`)/post_2020$labour_force

# real unemployment rate using forecast values 

(post_2020$labour_force[1:9] - empfore)/post_2020$labour_force[1:9]


# contrafactual unemployment rate 


contfac_unemp<- (fc_emp$.mean - cont_emp)/fc_emp$.mean

upper_cont <- (fc_emp$.mean - oneemp$upper_bound)/fc_emp$.mean
  
lower_cont <- (fc_emp$.mean - oneemp$lower_bound)/fc_emp$.mean

cont_data<- data.frame(Quarter= yearquarter(post_2020$Quarter[1:9]),
                       Unemployment_Rate = as.matrix(contfac_unemp) )




## Make the unemployment rate plot for comparison 


tsemp <- employment |> 
  select(Date,`96 Total`) |> 
  mutate(Quarter = yearquarter(my(Date)))

fitsemp <- ts_labour |> 
  filter(Quarter >= yearquarter("1984 Q4") & Quarter <= yearquarter("2022 Q2"))




unemployment <- (as.matrix(fitsemp$labour_force) - as.matrix(tsemp$`96 Total`))/as.matrix(fitsemp$labour_force)
  

unemp_plot<- data.frame( `Unemploment Rate`= as.matrix(unemployment),Quarter = yearquarter(tsemp$Quarter))



data1 <- dplyr::bind_rows(unemp_plot, cont_data)
  

colors <- c("Actual" = "blue", "Forecasts" = "red")


 ggplot()+
  geom_line(data = unemp_plot, aes(x = Quarter,y = Unemploment.Rate), colour = "blue",linetype = "dashed")+ 
  geom_line(data = cont_data[-9,], aes(x= Quarter,y=Unemployment_Rate),colour="red")+
   labs(title = "Counterfactual Analysis of the Unemployment Rate in Australia", 
        subtitle = "From 1984 Q1 to 2022 Q2", 
        y = "Unemployment Rate", 
        caption = "Note: Red line is Forecast values; Blue line is actual values") 

 

# ANSWER: We will not provide the confidence Interval here due to the reason that 
 # we didn't set the constraint that employment cannot larger than the total labour foce
 # and we use the two forecasting models, if summing them up, the error will be doubled. 
 
 

## Distribute the NFD data 

# 
# NFD <- read_csv("NFD_data.csv")
# 
# employment_shares <- read_csv("ABSemp_shares.csv")[1,]
# 
# employment <- read_csv("ABSemp.csv") |> 
#   select(-Date, - `96 Total`)
# 
# employment_shares <- as.matrix(employment_shares[,-c(1,ncol(employment_shares))])
# 
# 
# # convert to matrix 
# 
# empdat <-  as.matrix(employment)
# 
# NFD <- as.matrix(NFD)
# 
# # create a new matrix to save the employment data 
# 
# new_empdat = matrix(0,nrow = nrow(empdat), ncol = ncol(empdat))
# 
# NFD_per_share = matrix(0,nrow = nrow(empdat), ncol = ncol(empdat))
# 
# 
# 
# # 
# # # add back to the original series 
# # 
# # for (i in 1:ncol(employment_shares)){
# #   
# #   for(j in 1:nrow(NFD)){
# #     
# #   NFD_per_share[,i] = NFD[,ncol(NFD)-1] * employment_shares[,i]
# #   
# #   new_empdat[,i] <-  empdat[,i] + NFD_per_share[,i]
# #   
# #   }
# #   
# # }
# 
# 
# # add back to the original series with shares by time 
# 
# 
# 
# for (i in 1:ncol(empdat)){
#   for (j in 1:nrow(NFD)){
#     
#     NFD_per_share[j,i] = NFD[j,ncol(NFD)-1] * empdat[j,i]/sum(empdat[j,])
#     
#     new_empdat[j,i] = empdat[j,i] + NFD_per_share[j,i]
#     
#   }
# }
# 
# 
# new_empdat <- cbind(new_empdat,rowSums(new_empdat))
# 
# 
# write.csv(file="full_employment_2.csv",new_empdat)
# 
# 
#  
