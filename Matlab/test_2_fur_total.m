%% ELvis Yang 
% Monash University
% July 2022
% This is the forecast directly based on the original data 



clear all
clc

addpath('_funcs')

MAPE_acc = [];
MASE_acc = [];
RMSE_acc = [];



% Test script to estimate Australia BVAR of employment growth
% Read data, 19 sectors in the first 19 columns - total in the 20th column
alldata =  xlsread('ABSemp.xlsx', "B2:CH143");

%fulldata = csvread('full_employment_2.csv');
%alldata = fulldata(1:142,:);

% alldata = fulldata;
%lambda_lst =  [0.1:0.0001:0.3]'; %--- we found the minima between 0.05 & 0.06


lambda_lst =  [0.0001:0.0001:0.3]';

%for i = 60:1:120

for l = 1:numel(lambda_lst)


    i= 120;
    
    
    
    training = alldata(1:i,:);
    testing = alldata(i+1:end,:);



    % Transform data to year on year growth rate 100*ln(y_t/y_{t-4})

    logdat = log(training);


    % Change later if we are doing BVAR on all sectors
    
    y = 100*(logdat(5:end,1:85)-logdat(1:(end-4),1:85));% take fourth difference--elv seasonal difference 
    
    N = size(y,2);
    p = 1;
    lambda = lambda_lst(l,:);

    hor = 142-i; % forecast horizon for iteration (horizon for CV=1)
   
    
    [phi,SIGMA,X,e] = BVAR(y,p,lambda);

    n = size(y,1);
    k = size(y,2);
    
    % prepare to conduct our forecasts 
    yhat = [y;zeros(hor,k)];

    




   rawhat_fore = zeros(n+hor,k); % The plus number is the forecast horizon 

   sdiff_fore = zeros(n+hor,k);

   train_error = zeros(hor,1);

   error = zeros(hor,1);
   


   newraw = testing;


   for b = n+1:n+hor % Set the forecast horizon 


       for j = 1:k-1 % conduct the forecast 

       yhat(b,j) = phi(1,j) + yhat(b-1,:) * phi(2:86,j); % do the forecast


       end


       rawhatv = alldata(b,1:(k-1)) .* exp(yhat(b,1:(k-1))/100);

       yhat(b,k) = 100 .* log(sum(rawhatv)/alldata(b,k));

       rawhat_fore(b+4,:)= alldata(b,1:k) .* exp(yhat(b,1:k)/100);


       error(b-n,1) = rawhat_fore(b+4,k) - newraw(b-n,k);
        
       yhat(b,:) = 100 .* (log(alldata(b+4,1:k))-log(alldata(b,1:k)));
       
       
       % percentage error 

   
  

      train_err(b-n,1) = 100 * error(b-n,1) / newraw(b-n,1);


% 
      MAPE = abs(train_err(b-n,1));

      MAPE_withinloop(b-n,:) = MAPE;


   end 


   RMSE = sqrt(mean(error.^2));

   % Accumulative errors

   MAPE_total = mean(MAPE_withinloop);

    
   MAPE_acc(l,:) = MAPE_total;

   RMSE_acc(l,1) = RMSE;

   
end 


%test_acc(i,1) = min(RMSE_acc);

%end 



% 
% weighted_error = (MAPE_acc+RMSE_acc)/ 2;
% 
% 
% 
% 
% for f = 1: size(weighted_error,1)
% 
%     if weighted_error(f,1) == min(weighted_error) 
%        
%        min_lambda = lambda_lst(f,1);
% 
%     end 
% 
% end 



   
% Use MAPE only 

% for f = 1: size(MAPE_acc,1)
% 
%     if MAPE_acc(f,1) == min(MAPE_acc) 
%        
%        min_lambda = lambda_lst(f,1);
% 
%     end 
% 
% end 



for f = 1: size(RMSE_acc,1)

    if RMSE_acc(f,1) == min(RMSE_acc) 
       
       min_lambda = lambda_lst(f,1);

    end 

end 





logall = log(alldata);

all_y = 100*(logall(5:end,1:85)-logall(1:(end-4),1:85));

p=1;





[phi_2,SIGMA,X,e] = BVAR(all_y,p,min_lambda);




csvwrite( "phi_lambda_min.csv", phi_2)



