data{
  int run_estimation;
  int T;
  int T_forward;
  int T_backward;
  int P;
  int n; 
  vector[n] N_obs;
  int pop_obs[n];
  int year_obs[n];
  vector<lower=0>[P] N_0_med_prior;
  vector[P] x;//easting
  vector[P] y;//northing
}
transformed data{
  vector[P] Zero; //vector used for process error correlation matrix
  row_vector[2] XY[P];
	Zero = rep_vector(0,P);
  for (p in 1:P) {
        XY[p, 1] = x[p];
        XY[p, 2] = y[p];
  }
}
parameters{
  matrix[T-1,P] eps;
  vector[P] eps_slope;
  real slope_mu;
  real<lower=0> sigma_slope;
  vector<lower=0>[P] N_0;
  real<lower=0> sigma_rn_mu;
  real<lower=0> sigma_wn_mu;
  real<lower=0> sigma_rn_sigma;
  real<lower=0> sigma_wn_sigma;
  vector<lower=0>[P] eps_sigma_rn; 
  vector<lower=0>[P] eps_sigma_wn; 
  cholesky_factor_corr[P] L;
  real<lower=0> rho;//length-scale
  real<lower=0> alpha;//spatial variance
}
transformed parameters{
  matrix<lower=0>[T,P] N;
  vector<lower=0>[P] sigma_rn = sigma_rn_mu + eps_sigma_rn * sigma_rn_sigma; 
  vector<lower=0>[P] sigma_wn = sigma_wn_mu + eps_sigma_wn * sigma_wn_sigma; 
  matrix[P,P] L_K;
  matrix[P,P] K = cov_exp_quad(XY,alpha, rho);//alpha is same for all pops....hmmm
  for (p in 1:P){
    K[p,p] = K[p,p] + square(sigma_rn[p]); //sigma_rn is nonspatial process error here
  }
  L_K = cholesky_decompose(K);
  N[1,1:P] = to_row_vector(N_0[1:P]);
  for(t in 2:T){
    N[t,1:P] = to_row_vector(exp(to_vector(log(N[t-1,1:P])) + slope_mu + eps_slope[1:P] * sigma_slope + L_K * to_vector(eps[t-1,1:P])));
  }
}
model{
  vector[n] local_N;
  vector[n] local_sigma_wn;
  for(i in 1:n){
    local_N[i] = N[year_obs[i],pop_obs[i]];
    local_sigma_wn[i] = sigma_wn[pop_obs[i]];
  }
  //=========Priors================
  //slope
  slope_mu ~ normal(0,0.25); 
  sigma_slope ~ cauchy(0,0.1);
  eps_slope[1:P] ~ std_normal();//right now trend is non-spatial; could make spatial
  //observation  & process error sds
  sigma_rn_mu ~ inv_gamma(1,0.125); 
  sigma_wn_mu ~ inv_gamma(1,0.125);
  sigma_rn_sigma ~ cauchy(0,0.1);
  sigma_wn_sigma ~ cauchy(0,0.1);
  eps_sigma_rn ~ cauchy(0,1);
  eps_sigma_wn ~ cauchy(0,1);
  //correlation matrix
  L ~ lkj_corr_cholesky(1);
  //process errors
  to_vector(eps) ~ std_normal();
  //initial states
  N_0 ~ lognormal(log(N_0_med_prior),2);
  //length-scale
  rho ~ gamma(1, 0.01);
  //marginal (spatial) variance
  alpha ~ std_normal();
  //=========likelihood=============
  if(run_estimation==1){
    N_obs ~ lognormal(log(local_N), local_sigma_wn);
  }
}
generated quantities{
  vector[P] slope;
  vector[n] N_sim;
  matrix[T + T_forward + T_backward,P] N_all;
  vector[P] eps_pred;
  N_all[T_backward + 1:T_backward + T,1:P] = N;
  if(run_estimation==1){
    for(i in 1:n){
      N_sim[i] = 0;
    }
  }
  if(run_estimation==0){
    for(i in 1:n){
      N_sim[i] = lognormal_rng(log(N[year_obs[i],pop_obs[i]]), sigma_wn[pop_obs[i]]);
    }
  }
  for(p in 1:P){
    slope[p] = slope_mu + eps_slope[p] * sigma_slope;//right now trend is non-spatial; could make spatial
    eps_pred[p] = normal_rng(0,1);
  }
  for(t in (T_backward + T + 1):(T_backward + T + T_forward)){
    N_all[t,1:P] = to_row_vector(exp(to_vector(log(N_all[t-1,1:P])) + slope[1:P] + L_K * eps_pred));
  }
  for(t in 1 : T_backward){
    N_all[T_backward - t + 1,1:P] = to_row_vector(exp(to_vector(log(N_all[T_backward - t + 2,1:P])) - slope[1:P] - L_K * eps_pred));
  }
}