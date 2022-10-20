%% ELvis Yang 
% Monash University
% July 2022
% This is the out-of-sample forecast experiment algorithm 


clear all
clc

addpath('_funcs')


RMSFE_acc = [];



% Test script to estimate Australia BVAR of employment growth
% Read data, 84 sectors in the first 19 columns - total in the 85th column
alldata =  xlsread('ABSemp.xlsx', "B2:CH143");



lambda_lst =  [0.0001:0.0001:0.3]';


for l = 1:numel(lambda_lst)


    i= 120;
    
    
    
    training = alldata(1:i,:);
    testing = alldata(i+1:end,:);



    % Transform data to year on year growth rate 100*ln(y_t/y_{t-4})

    logdat = log(training);


    % Change later if we are doing BVAR on all sectors
    
    y = 100*(logdat(5:end,1:85)-logdat(1:(end-4),1:85));% take fourth difference (seasonal difference)
    
    N = size(y,2);
    p = 1;
    lambda = lambda_lst(l,:);

    hor = 142-i; % forecast horizon for iteration (horizon for CV=1)
   
    
    [phi,SIGMA,X,e] = BVAR(y,p,lambda);

    n = size(y,1);
    k = size(y,2);
    
    % prepare to conduct our forecasts 
    yhat = [y;zeros(hor,k)];

    


   % The hor is the forecast horizon 


   rawhat_fore = zeros(n+hor,k); 
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
       
       
  

   end 


   RMSFE = sqrt(mean(error.^2));


   % Accumulative errors



   RMSFE_acc(l,1) = RMSFE;

   
end 



% Only use the minimum RMSFE error measurement 

for f = 1: size(RMSFE_acc,1)

    if RMSFE_acc(f,1) == min(RMSFE_acc) 
       
       min_lambda = lambda_lst(f,1);

    end 

end 





logall = log(alldata);

all_y = 100*(logall(5:end,1:85)-logall(1:(end-4),1:85));

p=1;





[phi_2,SIGMA,X,e] = BVAR(all_y,p,min_lambda);




%csvwrite( "phi_lambda_min.csv", phi_2)



