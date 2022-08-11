%% ELvis Yang 
% Monash University
% July 2022
% This is method 3:  Mean forecast based on the log-differenced data 


clear all
clc

addpath('_funcs')



% Test script to estimate Australia BVAR of employment growth
% Read data, 19 sectors in the first 19 columns - total in the 20th column
alldata =  xlsread('ABSemp.xlsx', "B2:CH142");


% At least fit the model with a data t = 4 
% At most conduct one step forecast 






   % split training and test, starting with minimum training is 20q=5yrs

    i= 120;
    
    
    training = alldata(1:i,:);
    test = alldata(i+1:141,:);



    % Transform data to year on year growth rate 100*ln(y_t/y_{t-4})
    logdat = log(training);


    % Change later if we are doing BVAR on all sectors
    
    y = 100*(logdat(5:end,1:85)-logdat(1:(end-4),1:85));% take fourth difference--elv seasonal difference 
    
    N = size(y,2);
    p = 4;
  
    hor = 21; % forecast horizon for iteration (horizon for CV=1)
   

    n = size(y,1);
    k = size(y,2);



%% FORECAST Comparison with MEAN forecast 

   



 mean_fore = [zeros(n,k) ; repmat([mean(y)],21,1)];

 mean_fore_raw = zeros(n+hor,k); 


% recover the mean forecasts


 mean_sdiff_fore = zeros(n+hor, k);



 for c = n+1:n+hor 


 mean_fore_raw(c,:) = alldata((c-4),1:k) .* exp(mean_fore(c,1:k)/100);

 mean_sdiff_fore(c,:) = exp(mean_fore(c,1:k)/100);


 end 






  error_mean = mean_fore_raw(n+1:n+hor,:) - test; % calculate forecast difference between rawhat(estimated ones) and real data 
  train_err_mea = zeros(hor,k);



 

  % Scale dependent error 

   MAE_mean = mean(sumabs(error_mean));
   RMSE_mean = sqrt(mean(sumabs(error_mean)));
   




   % percentage error 

   for e = 1:hor 

       for j = 1:k

           train_err_mea(e,j) = 100 .* abs(error_mean(e,j) / test(e,j));

       end 


   end 




   MAPE_mean = sumabs(train_err_mea)/(hor .* k);



   %%

    %%

   % scaled error Hyndman & Koehler (2006) see [https://otexts.com/fpp3/accuracy.html]
% 
%    denom_mean = sumabs(mean_sdiff_fore)/hor; % set up the denominator 
%  
%    q_j = zeros(hor,k); % set up the $q_j$ see https://otexts.com/fpp3/accuracy.html
% 
%    
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



 %%
