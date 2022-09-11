%% ELvis Yang 
% Monash University
% July 2022
% This is the forecast directly based on the original data 



clear all
clc

addpath('_funcs')

MAPE_acc = [];
MASE_acc = [];
RMSSE_acc = [];



% Test script to estimate Australia BVAR of employment growth
% Read data, 19 sectors in the first 19 columns - total in the 20th column
alldata =  xlsread('ABSemp.xlsx', "B2:CH143");


lambda_lst =  [0.001:0.001:0.2]'; %--- we found the minima between 0.05 & 0.06




for l = 1:numel(lambda_lst)

    i= 120; 
    
    
    training = alldata(1:i,:);
    testing = alldata(i+1:141,:);



    % Transform data to year on year growth rate 100*ln(y_t/y_{t-4})

    logdat = log(training);


    % Change later if we are doing BVAR on all sectors
    
    y = 100*(logdat(5:end,1:85)-logdat(1:(end-4),1:85));% take fourth difference--elv seasonal difference 
    
    N = size(y,2);
    p = 1;
    lambda = 0.1;

    hor = 21; % forecast horizon for iteration (horizon for CV=1)
   
    
    [phi,SIGMA,X,e] = BVAR(y,p,lambda);

    n = size(y,1);
    k = size(y,2);
    
    % prepare to conduct our forecasts 
    yhat = [y;zeros(hor,k)];

    

   rawhat_fore = zeros(n+hor,k); % The plus number is the forecast horizon 
   sdiff_fore = zeros(n+hor,k);

   train_error = zeros(hor,k);

   error = zeros(hor,k);
  

   newraw = testing;


   for b = n+1:n+hor % Set the forecast horizon 


       for j = 1:k-1 % conduct the forecast 

       yhat(b,j) = phi(1,j) + yhat(b-1,:) * phi(2:86,j); % do the forecast

       end


       rawhatv = alldata(b,1:(k-1)) .* exp(yhat(b,1:(k-1))/100);

       yhat(b,k) = 100 .* log(sum(rawhatv)/alldata(b,k));

       rawhat_fore(b+4,:)= alldata(b,1:k) .* exp(yhat(b,1:k)/100);
        
       yhat(b,:) = 100*(log(alldata(b+4,1:k))-log(alldata(b,1:k)));
        
       % percentage error 

       error(b-n,:) = rawhat_fore(b+4,:) - newraw(b-n,:);
      

       for j = 1:k

           train_err(b-n,j) = 100 .* error(b-n,j) / newraw(b-n,j);

       end 

       

       % Scale dependent error 


        MAPE = sumabs(train_err(b-n,:))/(1.* k);

        MAPE_acc(b-n,:) = MAPE;


   end 

end 


   % Accumulative errors

   MAPE_total = sum(MAPE_acc)/hor;

