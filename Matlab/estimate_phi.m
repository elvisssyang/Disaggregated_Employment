%% ELvis Yang 
% Monash University
% Jun 2022

clear all
clc

addpath('_funcs')

% Test script to estimate Australia BVAR of employment growth
% Read data, 19 sectors in the first 19 columns - total in the 20th column
alldata =  xlsread('ABSemp.xlsx', "B2:CH142");


% Transform data to year on year growth rate 100*ln(y_t/y_{t-4})
logdat = log(alldata);
% Change later if we are doing BVAR on all sectors

y = 100*(logdat(5:end,1:85)-logdat(1:(end-4),1:85));% take fourth difference--elv seasonal difference 

N = size(y,2);
p = 1;
lambda = 0.0391; %shrinakge
maxhor = 2; %maximum forecast horizon where we are imposing conditions


k = 85;

%Fit an BVAR to get the parameters for the BVAR 



[phi,SIGMA,X,e] = BVAR(y,p,lambda);


% write out the phi to csv file

% csvwrite('one_lagphi.csv',phi);





%% Spillover Analysis 

multiraw = zeros(44,k);
multiraw(1:4,:) = alldata(end-3:end,:);
multig = zeros(44,k);
multilevels = zeros(44,k-1);
multigrowth = zeros(44,k-1);


for sector = 1:(k-1)

    for i = 5:44


        for j = 1:k-1
            multig(i,j) = multig(i-1,:) * phi(2:86, j);
            
            if i == 5 && j == sector
                multig(i,j) = multig(i,j)+1;
            end 

        end

            multiraw(i,1:k-1) = multiraw(i-4,1:k-1) .* exp(multig(i,1:k-1)/ 100);
            

            multiraw(i,k) = sum(multiraw(i,1:k-1));
            multig(i,k) = 100 .* log(multiraw(i,k) / multiraw(i-4,k));

         

    end 


     multilevels(:,sector) = multiraw(:,k);
     multigrowth(:,sector) = multig(:,k);

end 

multipliers = sum(multigrowth, 1) / 4;

mpers = cumsum(multigrowth(5:44,:),1) / 4; 


shares = sum(alldata(end-3:end,1:84),1) / sum(alldata(end-3:end,85));

mtplers = [shares;mpers];
