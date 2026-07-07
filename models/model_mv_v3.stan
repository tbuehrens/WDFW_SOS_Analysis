data{
  int run_estimation;
  int T;
  int T_forward;
  int T_backward;
  int P;
  real<lower=2> nu_slope;
  int n; 
  vector[n] N_obs;
  int pop_obs[n];
  int year_obs[n];
  real<lower=0>N_0_med_prior[P];
}
transformed data{
  vector[P] Zero; //vector used for process error correlation matrix
	Zero = rep_vector(0,P);
}
parameters{
  matrix[T-1,P] eps2;
  vector[P] eps_slope;
  real slope_mu;
  real<lower=0> sigma_slope;
  vector<lower=0>[P] N_0;
  
  real mu_log_sigma_total;
  real<lower=0> sd_log_sigma_total;
  vector[P] z_log_sigma_total;

  real mu_logit_prop_proc;
  real<lower=0> sd_logit_prop_proc;
  vector[P] z_logit_prop_proc;
  
  cholesky_factor_corr[P] L;
}
transformed parameters{
  matrix[T-1,P] eps;
  vector[P] mean_eps;
  matrix<lower=0>[T,P] N;
  vector<lower=0>[P] sigma_total;
  vector<lower=0, upper=1>[P] prop_proc;
  vector<lower=0>[P] sigma_rn;
  vector<lower=0>[P] sigma_wn;

  sigma_total = exp(mu_log_sigma_total + sd_log_sigma_total * z_log_sigma_total);
  prop_proc = inv_logit(mu_logit_prop_proc + 
                      sd_logit_prop_proc * z_logit_prop_proc);
  sigma_rn = sqrt(prop_proc) .* sigma_total;
  sigma_wn = sqrt(1 - prop_proc) .* sigma_total;
  
  for (p in 1:P){
    mean_eps[p] = mean(eps2[, p]);
  }

  // Center process errors within each population
  eps = eps2 - rep_matrix(mean_eps', T - 1);
  
  N[1,1:P] = to_row_vector(N_0[1:P]);
  for(t in 2:T){
    N[t,1:P] = to_row_vector(exp(to_vector(log(N[t-1,1:P])) + slope_mu + eps_slope[1:P] * sigma_slope + diag_pre_multiply(sigma_rn,L) * to_vector(eps[t-1,1:P])));
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
  eps_slope[1:P] ~ student_t(nu_slope,0,1);
  //observation  & process error sds
  mu_log_sigma_total ~ normal(log(0.25), 1);
  sd_log_sigma_total ~ normal(0, 0.5);
  z_log_sigma_total ~ std_normal();

  mu_logit_prop_proc ~ normal(0, 1.5);
  sd_logit_prop_proc ~ normal(0, 1);
  z_logit_prop_proc ~ std_normal();
  //correlation matrix
  L ~ lkj_corr_cholesky(1);
  //process errors
  to_vector(eps2) ~ std_normal();
  //initial states
  N_0 ~ lognormal(log(N_0_med_prior),2);
  //=========likelihood=============
  if(run_estimation==1){
    N_obs ~ lognormal(log(local_N), local_sigma_wn);
  }
}
generated quantities{
  vector[P] slope;
  matrix[P,P] Omega = multiply_lower_tri_self_transpose(L);
  matrix[P,P] Sigma = quad_form_diag(Omega, sigma_rn);
  vector[n] N_sim;
  matrix[T + T_forward + T_backward,P] N_all;
  matrix[T_backward + T + T_forward,P] eps_all;
  N_all[T_backward + 1:T_backward + T,1:P] = N;
  eps_all[T_backward + 1,1:P] = rep_row_vector(0,P);
  eps_all[T_backward + 2:T_backward + T,1:P] = eps;
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
    slope[p] = slope_mu + eps_slope[p] * sigma_slope;
  }
  for(t in (T_backward + T + 1):(T_backward + T + T_forward)){
    for(p in 1:P){
      eps_all[t,p] = normal_rng(0,1);
    }
    N_all[t,1:P] = to_row_vector(exp(to_vector(log(N_all[t-1,1:P])) + slope[1:P] + diag_pre_multiply(sigma_rn, L) * to_vector(eps_all[t,1:P])));
  }
  for(t in 1 : T_backward){
    for(p in 1:P){
      eps_all[t,p] = normal_rng(0,1);
    }
    N_all[T_backward - t + 1,1:P] = to_row_vector(exp(to_vector(log(N_all[T_backward - t + 2,1:P])) - slope[1:P] - diag_pre_multiply(sigma_rn, L) * to_vector(eps_all[t,1:P])));
  }
}
