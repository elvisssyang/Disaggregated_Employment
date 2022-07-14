function [phi,SIGMA,X,e,phi_boot,SIGMA_boot] = olsvar(y,p,varagin)
% Elvis Yang
% Monash University
% Edited June 2022
% Estimates a VAR with a constant using least squares with options of
% various methods of bootstrapping
%
%% INPUTS
%
%y                        Time series
%p                        lags
%varagin                  These are the (optional) bootstrapping options
% 'parametric'    Draw residuals parametrically from the covariance matrix
% 'bootstrap_with_replacement'    Draws residuals randomly from the empirical residuals
% 'double_bootstrap'              Bias Correction Bootstrap-after-bootstrap (see Kilian, REStat1998)
% 'wild_bootstrap'                Allows for heteroskedasticity by multiplying by a random normal variable
%                                 (see Kilian and Gonclaves, Joe, 2004)
%
%% OUTPUTS
%phi                          VAR Coefficients
%SIGMA                      Estimated Covariance Matrix
%X                          Dependent Variable Matrix (all the lags)
%e                          reduced form residuals
%phi_boot                   The bootstrap samples of the VAR coefficients
%                           and constant
%SIGMA_boot                 The bootstrapped samples of the covariance
%                           matrix
%% Preliminaries

[T N] = size(y);

% This is used for the bootstrap options
phi_boot = [];
SIGMA_boot = [];

%backcast data with the mean
y = [repmat(mean(y),p,1);y];
K = p*N + 1;        %number of parameters per equation (plus one for constant)

%Create Data Matrices
Y = y(p+1:end,:);     %Cut Away first p lags (the backcasted stuff)
X = ones(T,1);        %Start with a matrix of constants  

for i = 1:p
    Z = y(p+1-i:end-i,:);
    X = [X Z];
end


%% Estimate VAR

phi = (X'*X)\(X'*Y);      % VAR Coefficients
e = Y - X*phi;            % Reduced form residuals
SIGMA = (e'*e)/(T-K);     % Covariance Matrix





%% Bootstrap Options Here
if nargin > 2
    phi_boot = NaN(K,N,1000);
    SIGMA_boot = NaN(N,N,1000);
    if sum(strcmp(varagin,'parametric')) == 1
           
        parametric_bootstrap
      
    elseif sum(strcmp(varagin,'bootstrap_with_replacement')) == 1 
        
        bootstrap_with_replacement
       
    elseif sum(strcmp(varagin,'double_bootstrap')) == 1 
         
        bootstrap_after_bootstrap
        
   elseif sum(strcmp(varagin,'wild_bootstrap')) == 1 
         
        wild_bootstrap
        
    end

end


end