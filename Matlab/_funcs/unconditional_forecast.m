function [forecasts] = unconditional_forecast(phi,init,hor)
% Calculates unconditional of VAR by iterating
% phi       VAR coefficients (with constant)
% X         Initial conditions
% hor       The number of step ahead forecast
% stack X where the column is each variable, the first lag in first row, second
% lag second row etc
if (size(phi,1)-1)/size(phi,2) ~= size(init,1)
    error('check dimension of initial conditions or VAR')
end

M = size(phi,2);
p = (size(phi,1)-1)/M;

forecasts = NaN(hor,M);

F = [phi(2:end,:)';eye(M*(p-1)) zeros(M*(p-1),M)];

X = reshape(init',[],1);


for jj = 1:hor
    temp_forecast = F*X; 
    forecasts(jj,:) = temp_forecast(1:M,:) + phi(1,:)'; %adding constant
    
    X = [forecasts(jj,:)';X(1:end-M)];
end






end

