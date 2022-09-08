%% ELvis Yang 
% Monash University
% July 2022
% This is the forecast directly based on the original data 




clear all
clc

addpath('_funcs')



% Test script to estimate Australia BVAR of employment growth
% Read data, 19 sectors in the first 19 columns - total in the 20th column
alldata =  xlsread('ABSemp.xlsx', "B2:CH143");



logall = log(alldata);

all_y = 100*(logall(5:end,1:85)-logall(1:(end-4),1:85));

p=1;

min_lambda = 0.1;


% ### Package Use 
% options.prior.name = "Minnesota";
% BVAR = bvar_(all_y,p,options);
% csvwrite( "canova_phi.csv", BVAR.Phi_ols)
% ### 



[phi_2,SIGMA,X,e] = BVAR(all_y,p,min_lambda);




csvwrite( "phi_lambda_01.csv", phi_2)






