%% ELvis Yang 
% Monash University
% July 2022
% This is the forecast directly based on the original data 



clear all
clc

addpath('_funcs')

addpath('bvartools')


addpath('cmintools')



% Test script to estimate Australia BVAR of employment growth
% Read data, 19 sectors in the first 19 columns - total in the 20th column
alldata =  xlsread('ABSemp.xlsx', "B2:CH143");

MAPE_acc = [];
MASE_acc = [];
RMSSE_acc = [];



lambda_lst = [0.0001: 0.0001: 0.1]'; %--- we found the minima between 0.05 & 0.06

% lambda_lst = [0.05:0.001:0.6]'; --- we found the minima between 0.058 & 0.059

% lambda_lst = [0.058:0.0001:0.059]' -- we found the minima between 0.0585
% and 0.0583 -- or we finally select 0.0586

for l = 1:numel(lambda_lst)

    i= 120; 
    
    
    training = alldata(1:i,:);
    test = alldata(i+1:141,:);



    % Transform data to year on year growth rate 100*ln(y_t/y_{t-4})

    logdat = log(training);


    % Change later if we are doing BVAR on all sectors
    
    y = 100*(logdat(5:end,1:85)-logdat(1:(end-4),1:85));% take fourth difference--elv seasonal difference 
    
    N = size(y,2);
    p = 4;
    lambda = lambda_lst(l,:); %shrinakge lambda ---- YOU CAN REPLACE WITH DIFFERENT LAMBDA 
   % lambda = 0.0586;
    hor = 21; % forecast horizon for iteration (horizon for CV=1)
   
    
    [phi,SIGMA,X,e] = BVAR(y,p,lambda);

    n = size(y,1);
    k = size(y,2);
    
    % prepare to conduct our forecasts 
    yhat = [y;zeros(hor,k)];

    






    









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





%    % Scaled error 
%     
% 
   % Denominator 

   seasonal_diff = training(5:end,1:85) - training(1:(end-4),1:85);
   m=4; % set the seasonality as quarter

% 
   denom = sumabs(seasonal_diff)/ size(training,1)- m;
   denom2 = sumabs(seasonal_diff.^2) / size(training,1)- m;
% 
% 
% 
% 
%     scaled error Hyndman & Koehler (2006) see [https://otexts.com/fpp3/accuracy.html]
% 
   q_j = zeros(hor,k); % set up the $q_j$ see https://otexts.com/fpp3/accuracy.html
% 
   q_j2 = zeros(hor,k);
%     
   for a= 1:hor

       for b = 1:k 
           q_j(a,b) = (error(a,b))/denom;
           q_j2(a,b) = (error(a,b).^2)/denom2;
       end


   end 
   

   

   
   MASE = meanabs(q_j);

   RMSSE = sqrt(meanabs(q_j2));


   % Accumulative errors

   MAPE_acc = [MAPE_acc;MAPE];

   MASE_acc = [MASE_acc;MASE];

   RMSSE_acc = [RMSSE_acc;RMSSE];


end 

% Use combined errors. Otherwise, use the MAPE only 
% combinederror = (MAPE_acc + MASE_acc + RMSSE_acc)/3;
% 
% for f = 1: size(combinederror,1)
% 
%     if combinederror(f,1) == min(combinederror) 
%        
%        min_lambda = lambda_lst(f,1);
% 
%     end 
% 
% end 


% Use MAPE only 
% 
for f = 1: size(MAPE_acc,1)

    if MAPE_acc(f,1) == min(MAPE_acc) 
       
       min_lambda = lambda_lst(f,1);

    end 

end 
% 




logall = log(alldata);

all_y = 100*(logall(5:end,1:85)-logall(1:(end-4),1:85));

% ### Package Use 
% options.prior.name = "Minnesota";
% BVAR = bvar_(all_y,p,options);
% csvwrite( "canova_phi.csv", BVAR.Phi_ols)
% ### 


p=4;
min_lambda = 0.1;
[phi_2,SIGMA,X,e] = BVAR(all_y,p,min_lambda);



csvwrite( "phi_all_4lags.csv", phi_2)


