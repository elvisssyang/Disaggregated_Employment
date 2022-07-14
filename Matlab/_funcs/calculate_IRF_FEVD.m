function [IRF,FEVD] = calculate_IRF_FEVD(phi,A0,hor,varagin)
%Elvis Yang
% Monash University
% Edited June 2022
% Calculates Impulse Response Function and Forecast variance decomposition
% from a VAR
% This function should be used in conjuction with olsvar or the BVAR
% function
%% Inputs
%phi                The VAR Coefficients
%A0                 The impact matrix for identification
%hor                The maximum horizon of the IRF and FEVD
%varagin            Enter 'FEVD' if want to compute variance decomposition
%
%% OUTPUTS
% IRF               The estimated impulse response function
% FEVD              The calculated variance decomposition
%% Preliminaries

N = size(phi,2);
p = (size(phi,1)-1)/N;        % minus one because of constant
IRF = NaN(N^2,hor+1);
FEVD = NaN(N,N,hor+1);

%Construct companion matrix
F = [phi(2:end,:)';eye(N*(p-1)) zeros(N*(p-1),N)];
bigeye = [eye(N) zeros(N,N*(p-1))];

for jj = 1:hor+1
    IRF(:,jj) = reshape(bigeye*(F^(jj-1))*bigeye'*A0,[],1);
end


%% Calculate Variance Decomposition
if nargin > 3
    
    if sum(strcmp(varagin,'FEVD')) == 1
        IRF_cumulative_variance = cumsum(IRF.^2,2);
        for ii = 1:N
            % Calculate total variance across the same variable
            total_variance(ii,:) = sum(IRF_cumulative_variance(ii:N:end,:));
        end
        %Create a matrix with the totoal so we can devide
        total_variance = repmat(total_variance,N,1);
        % FEVD here is in Vectorized form
        FEVD_vectorized = 100*IRF_cumulative_variance./total_variance;
        % Rearrange FEVD
        for jj = 1:hor+1
            FEVD(:,:,jj) = reshape(FEVD_vectorized(:,jj),N,N);
        end
    end
end
end

