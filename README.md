# Disaggregated_Employment

This is the Honours thesis_ Disaggregated Employment Dynamics in Australia



*Purpose: Build up the Bayesian VAR using Minnesota prior with a nature conjugate setting mentioned in banbura paper *



# Data Preprocessing : 

Due to the reason that in the second-digit level, data is not too clean as the first digit level (In other words, it contains too many zeros). I have to modify as follows: 

- Merge the 57 Internet Publishing and Broadcasting and into 54 Publishing(except internet and music publishing). Then I will get the new number 54 item. The new name for item 54 will be -> 54 Publishing and Broadcasting 


- Merge the  96 Private Households Employing Staff and Undifferentiated Goods- and Service- Producing Activities of Households for Own Use  to  95 Personal and Other Services to avoid two zero values appeared in 96 then the new 95 will be -> 95  Personal and other services (include activities for own use) 



# Operating steps : 


## Matlab code is used to 

+ calculate the estimated phi, please **RUN the MAIN.m** to draw the estimated parameters of **BVAR** 

+ Estimated coefficients of BVAR is in **phi.csv **



## R code is used to : 

+ Train the best estimated hyper parameter  $\lambda$

+ Conduct spillover analysis 

+ Do the scenario forecasting 



 

