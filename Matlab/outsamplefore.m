%% ELvis Yang 
% Monash University
% July 2022

% This is method 1: Out of sample forecast based on the original data 

clear all
clc

addpath('_funcs')



% Test script to estimate Australia BVAR of employment growth
% Read data, 19 sectors in the first 19 columns - total in the 20th column
alldata =  xlsread('ABSemp.xlsx', "B2:CH142");


% At least fit the model with a data t = 4 
% At most conduct one step forecast 





% generate a list of lambda 


 lambda_lst = [0.01]';





%% 
for l = 1:numel(lambda_lst)


    % split training and test

    i= 120;
    
    
    training = alldata(1:i,:);
    test = alldata(i+1:141,:);



    % Transform data to year on year growth rate 100*ln(y_t/y_{t-4})
    logdat = log(training);


    % Change later if we are doing BVAR on all sectors
    
    y = 100*(logdat(5:end,1:85)-logdat(1:(end-4),1:85));% take fourth difference--elv seasonal difference 
    
    N = size(y,2);
    p = 4;
    lambda = lambda_lst(l,1); %shrinakge lambda ---- YOU CAN REPLACE WITH DIFFERENT LAMBDA 
    hor = 21; % forecast horizon 

    
    [phi,SIGMA,X,e] = BVAR(y,p,lambda);

    n = size(y,1);
    k = size(y,2);
    
    % prepare to conduct our forecasts 


    rawhat = zeros(n,k);
    yhat = zeros(n+hor,k);

    


    % CONDUCT THE FORECAST 

    % First: Draw estimates 

    for a = n-4:n  

        for j = 1:k
         
        yhat(a,j) = y(a,j);
    
        end 


    end


    % Second: Backtransform the estimate and conduct forecast 



   rawhat_fore = zeros(n+hor,k); % The plus number is the forecast horizon 
   sdiff_fore = zeros(n+hor,k);



   for b = n+1:n+hor % Set the forecast horizon 


       for j = 1:k-1 % conduct the forecast 

       yhat(b,j) = phi(1,j) + yhat(b-1,:) * phi(2:86,j) + yhat(b-2,:) * phi(87:171,j) + yhat(b-3,:) * phi(172:256,j) + yhat(b-4,:) * phi(257:341,j); % do the forecast

       end


        rawhatv = alldata(b-4,1:(k-1)) .* exp(yhat(b,1:(k-1))/100);
        yhat(b,k) = 100 .* log(sum(rawhatv)/alldata(b-4,k));
        rawhat_fore(b,:)= alldata((b-4),1:k) .* exp(yhat(b,1:k)/100);
        sdiff_fore(b,:) = exp(yhat(b,1:k)/100); % calculate the yt-y(t-4)


   end 

   newraw = test;
   error = rawhat_fore(n+1:n+hor,:) - newraw; % calculate forecast difference between rawhat(estimated ones) and real data 
   train_err = zeros(hor,k);
 


%% ERROR MEASUREMENTS 


   % Scale dependent error 

   MAE = mean(sumabs(error));
   RMSE = sqrt(mean(sumabs(error)));
   

   % percentage error 

   for e = 1:hor 

       for j = 1:k

           train_err(e,j) = 100 .* abs(error(e,j) / newraw(e,j));

       end 


   end 


   
   tnerror = mean(train_err);


   MAPE = sumabs(train_err)/(hor .* k);



   % scaled error Hyndman & Koehler (2006) see [https://otexts.com/fpp3/accuracy.html]
% 
%    denom = sumabs(sdiff_fore)/hor; % set up the denominator 
%  
%    q_j = zeros(hor,k); % set up the $q_j$ see https://otexts.com/fpp3/accuracy.html
% 
% 
%    
% 
%    for a= 1:hor
% 
%        for b = 1:k 
%            q_j(a,b) = error(a,b)/denom;
%        end
% 
% 
%    end 
% 
%    
%    MASE = meanabs(q_j);
% 
%    RMSSE = sqrt(meanabs(q_j)^2);
% 
% 
% 
%    weighted_error(l,1)= (MAPE + MASE + RMSSE) / 3; 
% 



 end 

 % min(weighted_error) % Get the minimum of the weighted error 







 