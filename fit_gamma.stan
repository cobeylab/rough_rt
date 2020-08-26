data {
  int<lower=0> N;
  vector[N] x;
}
parameters {
  real<lower=0> shape;
  real<lower=0> rate;
}
model {
  x ~ gamma(shape, rate);
}