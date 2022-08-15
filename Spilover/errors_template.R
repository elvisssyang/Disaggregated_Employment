
# Perform out for sample forecast use the rawhat(estimation) to do
# Steps are similar use the last four observation to fit the phi above to generate t+1 forecast 
# 
# 
# 
# 
# # Testing model using error measurements MAPE and Scaled errors
# 
# newraw <- rawdata[9:nrow(rawdata),] # Define the actual values of forecast 
# rawhat1 <- rawhat[5:nrow(rawhat),]
# error <- (newraw - rawhat1)
# train_err <- matrix(0,n,k)
#   
# # Scale-dependent errors 
# MAE = mean(abs(error))
# RMSE = sqrt(mean(error))
# 
# # Percentage error 
# 
# 
# for(i in 1:(n-4)){
#   for (j in 1:k){
#     train_err[i,j] <- 100 * as.vector(error[i,j]) / as.vector(newraw[i,j]) # Percentage error 
#   }
# }
# 
# # To calculate the MAPE we apply the formula as   sum(abs(y_t - \hat{y_t}/y_t))/(n)    
# 
# MAPE = sum(abs(train_err))/(n-4 * k ) 
# 
# 
# # Scaled error -- Hyndman & Koehler(2006) see [https://otexts.com/fpp3/accuracy.html]
# 
# 
# # First: Define the scaled error yt - y_{t-4} as sdiff and calculated in previous steps 
# 
# # sdiff = yt - y_{t-4} 
# 
# # Second, calculate the denominator as for seasonal time series m=4 
# 
# denom = sum(sdiff)/(n-4)
# 
# 
# 
# # Third calculate the q_j 
# 
# qj = matrix(0,n,k)
# 
# for(a in 1:i){
#   for (b in 1:k){
#     qj[a,b] = error[a,b]/denom
#   }
# }
# 
# MASE = mean(abs(qj))
# RMSSE = sqrt(mean(qj^2))
