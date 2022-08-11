%% ELvis Yang 
% Monash University
% July 2022

% This is method 4:  Mean forecast based on the Original data 



clear all
clc

addpath('_funcs')



% Test script to estimate Australia BVAR of employment growth
% Read data, 19 sectors in the first 19 columns - total in the 20th column
alldata =  xlsread('ABSemp.xlsx', "B2:CH142");



N = size(alldata,2);
p = 4;
  
hor = 21; % forecast horizon for iteration (horizon for CV=1)
   

n = size(alldata,1);
k = size(alldata,2);


i= 120;
    
    
training = alldata(1:i,:);
test = alldata(i+1:141,:);


% At least fit the model with a data t = 4 
% At most conduct one step forecast 





mean_fore = [repmat([mean(alldata)],21,1)];


train_err = zeros(hor,k);
 

error = mean_fore - test;


MAE = mean(sumabs(error));
RMSE = sqrt(mean(sumabs(error)));




 % percentage error 

   for e = 1:hor 

       for j = 1:k

           train_err_mea(e,j) = 100 .* abs(error(e,j) / test(e,j));

       end 


   end 




   MAPE = sumabs(train_err_mea)/(hor .* k);



   % Scaled error 
% 
%    
% 
%    q_j = zeros(hor,k); % set up the $q_j$ see https://otexts.com/fpp3/accuracy.html
% 
% 
%    
% 
%    for a= 1:hor
% 
%        for b = 1:k 
%            q_j(a,b) = error_mean(a,b)/denom_mean;
%        end
% 
% 
%    end 
% 
%    
%    MASE_mean = meanabs(q_j);
% 
%    RMSSE_mean = sqrt(meanabs(q_j)^2);
% 
%    weighted_error_mean = (MAPE_mean + MASE_mean + RMSSE_mean) / 3;
% 




 




