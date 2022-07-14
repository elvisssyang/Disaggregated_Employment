%% ELvis Yang 
% Monash University
% Jun 2022

clear all
clc

addpath('_funcs')

% Test script to estimate Australia BVAR of employment growth
% Read data, 19 sectors in the first 19 columns - total in the 20th column
alldata =  xlsread('ABSemp.xlsx', "A2:CF152");


% Transform data to year on year growth rate 100*ln(y_t/y_{t-4})
logdat = log(alldata);
% Change later if we are doing BVAR on all sectors

y = 100*(logdat(5:end,1:84)-logdat(1:(end-4),1:84));% take fourth difference--elv seasonal difference 

N = size(y,2);
p = 4;
lambda = 0.2; %shrinakge
maxhor = 2; %maximum forecast horizon where we are imposing conditions


%Fit an BVAR to get the parameters for the BVAR 
[phi,SIGMA,X,e] = BVAR(y,p,lambda);


% write out the phi to csv file 
% csvwrite('phi.csv',phi)  

A0 = chol(SIGMA,'lower');%prepare A0 for the transformation used in conditional forecasting 


[unconditional_forecasts] = unconditional_forecast(phi,y(end:-1:end-p+1,:),84);

