%% ELvis Yang 
% Monash University
% July 2022

clear all
clc

addpath('_funcs')



% Test script to estimate Australia BVAR of employment growth
% Read data, 19 sectors in the first 19 columns - total in the 20th column
alldata =  xlsread('ABSemp.xlsx', "B2:CH142");


% At least fit the model with a data t = 4 
% At most conduct one step forecast 






% set up the accumulated error measures 
   MAE_acc  = [];
   RMSE_acc = [];
   MAPE_acc = [];
   MASE_acc = [];
   RMSSE_acc = [];


% generate a list of lambda 

 lambda_lst = [0.001:0.001:0.1]';

% generate the weighted error for each iteration 

%%%%% weighted_error = zeros(numel(lambda_lst),1);



for l = 1:numel(lambda_lst)


% ABANDON THE CROSS VALIDATION -- 4 AUG 2022 

% 20 is the minimum required number to fit the model

%for i= 20:140 
       
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
    hor = 21; % forecast horizon for iteration (horizon for CV=1)
   
    
    [phi,SIGMA,X,e] = BVAR(y,p,lambda);

    n = size(y,1);
    k = size(y,2);
    
    % prepare to conduct our forecasts 
    rawhat = zeros(n,k);
    yhat = zeros(n+hor,k);

    


    % CONDUCT THE FORECAST 

    % First: Draw estimates 

    for a = 5:n  

        for j = 1:k-1
         
        yhat(a,j) = phi(1,j) + y(a-1,:) * phi(2:86,j) + y(a-2,:) * phi(87:171,j) + y(a-3,:)* phi(172:256,j) + y(a-4,:) * phi(257:341,j); % do the estimate 
    
        end 

        rawhatv = alldata(a-4,1:(k-1)) .* exp(yhat(a,1:(k-1))/100);
        yhat(a,k) = 100 .* log(sum(rawhatv)/alldata(a-4,k));
        rawhat(a,:)= alldata(a-4,1:k) .* exp(yhat(a,1:k)/100);
      

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
        sdiff_fore(b,:) = exp(yhat(b,1:k)/100);


   end 

   newraw = test;
   error = rawhat_fore(n+1:n+hor,:) - newraw; % calculate forecast difference between rawhat(estimated ones) and real data 
   train_err = zeros(hor,k);

   % Scale dependent error 


   MAE = mean(sumabs(error));
   RMSE = sqrt(mean(sumabs(error)));
   

   % percentage error 

   for e = 1:hor 

       for j = 1:k

           train_err(e,j) = 100 .* error(e,j) / newraw(e,j);

       end 


   end 

   MAPE = sumabs(train_err)/(hor .* k);



   % scaled error Hyndman & Koehler (2006) see [https://otexts.com/fpp3/accuracy.html]

   denom = sumabs(sdiff_fore)/hor; % set up the denominator 
 
   q_j = zeros(hor,k); % set up the $q_j$ see https://otexts.com/fpp3/accuracy.html




   for a= 1:hor

       for b = 1:k 
           q_j(a,b) = error(a,b)/denom;
       end


   end 
   
   MASE = meanabs(q_j);

   RMSSE = sqrt(meanabs(q_j));



    
   % Accumulative errors

   MAE_acc  = [MAE_acc;MAE];
   RMSE_acc = [RMSE_acc;RMSE];
   MAPE_acc = [MAPE_acc;MAPE];
   MASE_acc = [MASE_acc;MASE];
   RMSSE_acc = [RMSSE_acc;RMSSE];



%Fit an BVAR to get the parameters for the BVAR 


% Train use the time series cross validation with various lambda avaliable 
%end



% Total Trained errors 

%Overall_MAE = sumabs(MAE_acc) / (size(MAE_acc,1)); % have to average all the tests (START:20, END:141 in this case) START can be changed but min threashold is 20.
%Overall_RMSE = sumabs(RMSE_acc) / (size(RMSE_acc,1));
%Overall_MAPE = sumabs(MAPE_acc) / (size(MAPE_acc,1));
%Overall_MASE = sumabs(MASE_acc) / (size(MASE_acc,1));
%Overall_RMSSE = sumabs(RMSSE_acc) / (size(RMSSE_acc,1));


% weighted trained errors 
% here only weighted scaled error due to the reason of scale independent

%weighted_error(l,1)= (sum(Overall_RMSE)+ sum(Overall_MASE) + sum(Overall_RMSSE)) / 3; 


weighted_error =(RMSE+ MASE + RMSSE) / 3;

 end 

 min(weighted_error) 



