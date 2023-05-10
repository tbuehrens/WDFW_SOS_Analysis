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
  vector[T-1] eps;
  real slope;
  real<lower=0> N_0;
  real<lower=0> sigma_rn;
  real<lower=0> sigma_wn; 
  //real<lower=0> sigma_tn;
  //real<lower=0> p_wn;  
}
transformed parameters{
    //real<lower=0> sigma_rn = sqrt(square(sigma_tn) * (1-p_wn));
    //real<lower=0> sigma_wn = sqrt(square(sigma_tn) * p_wn);  
    vector<lower=0>[T] N;
    N[1] = N_0;
    for(t in 2:T){
      N[t] = exp(log(N[t-1]) + slope + eps[t-1] * sigma_rn );
    }
}
model{
    vector[n] local_N;
    for(i in 1:n){
      local_N[i] = N[year_obs[i]];
    }
    //Priors
    slope ~ normal(0,0.25);
    sigma_rn ~ cauchy(0,0.25); 
    sigma_wn ~ cauchy(0,0.25); 
    //sigma_tn ~ cauchy(0,0.5);
    //p_wn ~ beta(2,2);
    eps ~ std_normal();
    N_0 ~ lognormal(log(N_0_med_prior),2);
    //likelihood
    N_obs ~ lognormal(log(local_N), sigma_wn);
}
// generated quantities{
//   vector[T + T_forward + T_backward] N_all;
//   N_all[T_backward + 1:T_backward + T] = N;
//   vector[T_backward + T + T_forward] eps_all;
//   eps_all[T_backward + 1] = 0;
//   eps_all[T_backward + 2:T_backward + T] = eps;
//   
//   for(t in (T_backward + T + 1):(T_backward + T + T_forward)){
//     eps_all[t] = normal_rng(0,1);
//     N_all[t] = exp(log(N_all[t-1]) + slope + eps_all[t] * sigma_rn);
//   }
//   for(t in 1 : T_backward){
//     eps_all[t] = normal_rng(0,1);
//     N_all[T_backward - t + 1] = exp(log(N_all[T_backward - t + 2]) - slope - eps_all[t] * sigma_rn);
//   }
// }

