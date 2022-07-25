function [phi,SIGMA,X,e] = BVAR(y,p,lambda)

% Elvis Yang
% Monash University
% Edited June 2022
% Estimates a BVAR with a constant using dummy observations ala Banbura, Giannone, Reichlin JAE 2010
% This uses an analytical solution with a Normal-Wishart prior which is
% conjugate
%% INPUTS
%
%y                        Time series
%p                        lags
%lambda                   shrinkage parameter
%
%% OUTPUTS
%phi                        VAR Coefficients from posterior mode/mean
%SIGMA                      Estimated Covariance Matrix
%X                          Dependent Variable Matrix (all the lags)
%e                          reduced form residuals
%phi_draws                  This are just draws from the analytical
%                           posterior distribution
%SIGMA_draws                These are just draws of the covariance matrix
%                           from the analytical posterior distribution
%% Preliminaries


[T N] = size(y);

%backcast data with the mean
y = [repmat(mean(y),p,1);y];
K = p*N + 1;        %number of parameters per equation (plus one for constant)

%Create Data Matrices
Y = y(p+1:end,:);     %Cut Away first p lags (the backcasted stuff)
X = [];

% Create the X_t matrix in banbura paper 

for i = 1:p    
    Z = y(p+1-i:end-i,:);
    X = [X Z];
end

X = [X ones(T,1)];        %constants stacked last to make it easy


% This is to set the prior
small_sig2 =  zeros(N,1);
Stack_AR_coeff = zeros(p,N);

for i = 1:N
    %     calculate variance for each equation to set prior
    %     by fitting an AR(4) per equation

    [~,small_sig2(i,1)] = olsvar(y(:,i),4); % should be big Y or y ,it's depends on whether this to used to calculate coefficients or not. 


    %run an AR(1) to set to random walk if time series is persistent
    %(>0.8)
    % Can set this to zero
    [AR_coeff] = olsvar(y(:,i),1);  %should this be y or Y? Must same as above
    if AR_coeff(2) >= 0.8
        Stack_AR_coeff(1,i) = 1; % in minnesota prior either set 1 or 0
    end                       %% Elvis-- T his can be set to zero as we impose the prior belief of white noise ?? 
end

% Above steps are setting the E[(A_k)ij] = 1 if j=i and k=1, otherwise 0. 







%% Set up dummy observations

Y_d = [zeros(N*p,N);
    diag(sqrt(small_sig2))];   % Elvis-- a little bit question related to Y_d I reckon should be np instead of np+p
                                % does np+p because the original lags in
                                %  the data ? they use y instead of Y with
                                %  lags beej cut
for i = 1:p      
    Y_d((i-1)*N+1:i*N,:) = diag(Stack_AR_coeff(i,:)'.*repmat(i,N,1).*sqrt(small_sig2))/lambda;
end   %% here Elvis don't know why it only have 1 value changed, try to figure it out whether is error or not.

X_d = [kron(diag(1:p),diag(sqrt(small_sig2)/lambda));
    zeros(N,K-1)]; %X_d construct well 

%prior on the constant (note: constant is the last column)

X_d(size(X_d,1)+1,K) = 1e-10;
Y_d = [Y_d;zeros(1,N)];






%% Do Least Squares to Get Posterior
Y_star = [Y;Y_d]; X_star = [X; X_d];

%Get posterior mode/mean of VAR coefficients
phi = (X_star'*X_star)\(X_star'*Y_star);

%Get residuals
e = Y-X*phi;

e_star = Y_star-X_star*phi;

%Posterior mode/mean of covariance matrix
SIGMA = (e_star'*e_star)/(size(Y_star,1)-size(phi,1));



%% Get draw from the posterior
% 
% phi_draws = NaN(K,N,1000);
% SIGMA_draws = NaN(N,N,1000);
% 
% 
% phi_covar = kron(SIGMA,inv(X_star'*X_star));
% 
% for jj = 1:1000 % 1000 bootstraps
%     EIG = 1;
%     
%     
%     while EIG > 0
%         %Implement rejection sampling to ensure stationarity    
%     phi_temp_draw = phi(:) + chol(phi_covar)*randn(K*N,1);
%     phi_temp_draw = reshape(phi_temp_draw,K,N);
%     
%     F_draw = [phi_temp_draw(1:end-1,:)';eye(N*(p-1)) zeros(N*(p-1),N)];
%     if max(abs(eig(F_draw))) < 1
%         EIG = 0;
%     end
%     end 
%     %shift last row to first for constant
%     phi_draws(:,:,jj) = [phi_temp_draw(end,:);phi_temp_draw(1:end-1,:)];
%     
%     SIGMA_draws(:,:,jj) = iwishrnd(e_star'*e_star,size(Y_star,1)-size(phi,1));
% end



%%
%rearrange phi and X to just make constant in first row
phi = [phi(end,:);phi(1:end-1,:)];
X = [X(:,end) X(:,1:end-1)];


end


