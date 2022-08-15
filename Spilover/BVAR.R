
library(pracma)
library(tidyverse)




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





# This function will estimate a VAR with a constant using least squares with options of 
#various methods of bootstraping 


# 
# 
# BVAR <- function(y,p,lambda){
#   
#   T=dim(y)[1]
#   N=dim(y)[2]
#   
#   
#   #backcast data with the mean 
#   
#   y <- rbind(repmat(colMeans(y),p,1),y)
#   K=p*N+1
#   
#   #create data matrics 
#   Y<-y[-1:-(p),]
#   X=c()
#   
#   
#   for(i in 1:p){
#     rnmb<-p+1-i
#     cnmb<-nrow(y)-i
#     Z<-y[rnmb:cnmb,]
#     X<-cbind(X,Z)
#   }
#   
#   
#   # constant to stack at last 
#   X<-cbind(X,ones(T,1))
#   
#   # This is to set prior 
#   
#   
#   small_sig2=zeros(N,1)
#   Stack_AR_coeff=zeros(p,N)
#   
#   
#   for(i in c(1:N)){
#     #calculate variance for each equation to set pror 
#     #by fitting an AR(4) per equation 
#     
#     c(~,small_sig2[i,1])=olsvar(y[,i],4)
#     ARcoeff = olsvar(y[,i],1)
#     
#     if (ARcoeff >= 0.8){
#       Stack_AR_coeff(1,i) = 1}
#     
#     # run an AR(1) to set to random walk if time series is persistant 
#     # (>0.8)
#   }
#   
#   
#   Y_d = rbind(matrix(0,N*p,N), diag(sqrt(small_sig2/lambda)))
#   
#   for (i in 1:p){
#     Y_d[(i-1)*N+1:i*N, ] <- diag(Stack_AR_coeff[i,]*repmat(1,N,1)*sqrt(small_sig))/lambda
#   }
#   
#   X_d = rbind(kron(diag(1:P) , diag(sqrt(small_sig2) / lambda)) , zeros(N,K-1))
#   
#   X_d[size(X_d,1)+1,K] = 1e-10
#   Y_d <- rbind(Y_d,zeros(1,N))     
#   
#   
#   # Do least square to get posterior 
#   
#   Y_star = rbind(Y,Y_d)
#   X_star = rbind(X,X_d)
#   
#   # Get posterior mode/mean of VAR coefficients 
#   
#   phi = mldivide(t(X_star) %*% X_star , t(X_star) %*% Y_star)
#   
#   
#   e = Y - X %*% phi
#   
#   e_star = Y_star - X_star %*% phi
#   
#   
#   
#   # Posterior mode/mean of covariance matrix 
#   
#   
#   SIGMA = mrdivide((t(e_star) %*% e_star), (size(Y_star,1) - size(phi,1)))
#   
#   
#   # rearrange phi and X to just make constant in first row 
#   
#   phi = rbind(phi[nrow(phi),],phi[1:nrow(phi)-1,])
#   
#   X = cbind(X[,ncol(X)],X[,1:ncol(X)-1])
#   
#   print(phi)
# }
# 
# 
# 
# 
# 
# 



