data{
  int<lower=0> N;
  vector[2] y[N];
}
parameters{
  row_vector[2] theta;
  // real rho=0.8;
}
transformed parameters{
  cov_matrix[2] Sigma=[[1,0.8],[0.8,1]];
}
model{
  theta ~ multi_normal([0,0],[[1,0],[0,1]]);
  y ~ multi_normal(theta,Sigma);
}