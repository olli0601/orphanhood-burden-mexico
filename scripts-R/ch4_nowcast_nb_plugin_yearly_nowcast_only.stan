
data {
  int<lower=1> N;
  array[N] int<lower=0> y;       // R^(today)
  vector[N] log_pop;             // population offset
  vector[N] log_std;             // national std fertility rate offset
  vector[N] log_c;               // delay offset

  int<lower=1> K_age;
  matrix[N, K_age] B_age;

  int<lower=1> K_year;
  matrix[N, K_year] B_year;

  int<lower=1> M;
  array[N] int<lower=1, upper=M> mun;
  array[N] int<lower=0, upper=1> sex01;
}
parameters {
  real alpha;
  vector[K_age]  beta_age;
  vector[K_year] beta_year;
  real beta_sex;

  vector[M] z_m;                 // non-centered municipal RE
  real<lower=0> sigma_m;

  real log_phi;                  // robust overdispersion parameterization
}
transformed parameters {
  vector[M] b_m = sigma_m * z_m;
  real<lower=1e-8> phi = exp(log_phi);

  vector[N] eta = alpha
                + B_age  * beta_age
                + B_year * beta_year
                + beta_sex * to_vector(sex01)
                + b_m[mun];
}
model {
  // Priors
  alpha      ~ normal(0, 1.5);
  beta_age   ~ normal(0, 1);
  beta_year  ~ normal(0, 1);
  beta_sex   ~ normal(0, 1);
  z_m        ~ normal(0, 1);
  sigma_m    ~ exponential(1);
  log_phi    ~ normal(log(10), 1);  // median phi ~ 10 (tune if needed)

  // Likelihood: NB with THREE offsets (pop, std_rate, delay)
  y ~ neg_binomial_2_log(log_pop + log_std + log_c + eta, phi);
}
generated quantities {
  vector[N] mu_lambda = exp(log_pop + log_std + eta);         // totals Λ (c_t=1)
  vector[N] mu_today  = exp(log_pop + log_std + log_c + eta); // as-of-today mean
  array[N] int yrep;
  for (n in 1:N) yrep[n] = neg_binomial_2_rng(mu_today[n], phi);
}

