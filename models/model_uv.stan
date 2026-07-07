data{
  int T;
  int T_forward;
  int T_backward;
  int n;
  vector[n] N_obs;
  int year_obs[n];
  real<lower=0> N_0_med_prior;
}
parameters{
  vector[T-1] eps2;
  real slope;
  real<lower=0> N_0;
  real mu_log_sigma_total;
  real mu_logit_prop_proc;
}
transformed parameters{
  vector[T-1] eps;
  real mean_eps;
  real<lower=0> sigma_total;
  real<lower=0, upper=1> prop_proc;
  real<lower=0> sigma_rn;
  real<lower=0> sigma_wn;
  vector<lower=0>[T] N;

  mean_eps = mean(eps2);
  eps = eps2 - mean_eps;

  sigma_total = exp(mu_log_sigma_total);
  prop_proc = inv_logit(mu_logit_prop_proc);

  sigma_rn = sqrt(prop_proc) * sigma_total;
  sigma_wn = sqrt(1 - prop_proc) * sigma_total;

  N[1] = N_0;
  for(t in 2:T){
    N[t] = exp(log(N[t-1]) + slope + eps[t-1] * sigma_rn);
  }
}
model{
    vector[n] local_N;
    for(i in 1:n){
      local_N[i] = N[year_obs[i]];
    }
    //Priors
    slope ~ normal(0,0.25);
    mu_log_sigma_total ~ normal(log(0.25), 1);
    mu_logit_prop_proc ~ normal(0, 1.5);
    eps2 ~ std_normal();
    N_0 ~ lognormal(log(N_0_med_prior),2);
    //likelihood
    N_obs ~ lognormal(log(local_N), sigma_wn);
}
generated quantities{
  vector[T + T_forward + T_backward] N_all;
  N_all[T_backward + 1:T_backward + T] = N;
  vector[T_backward + T + T_forward] eps_all;
  eps_all[T_backward + 1] = 0;
  eps_all[T_backward + 2:T_backward + T] = eps;

  for(t in (T_backward + T + 1):(T_backward + T + T_forward)){
    eps_all[t] = normal_rng(0,1);
    N_all[t] = exp(log(N_all[t-1]) + slope + eps_all[t] * sigma_rn);
  }
  for(t in 1 : T_backward){
    eps_all[t] = normal_rng(0,1);
    N_all[T_backward - t + 1] = exp(log(N_all[T_backward - t + 2]) - slope - eps_all[t] * sigma_rn);
  }
}

