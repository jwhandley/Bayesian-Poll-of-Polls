data {
  int<lower=0> T; // # of Time steps
  int<lower=0> J; // # of Parties
  int<lower=0> N; // # of Observations
  int<lower=1> P; // # of Pollsters
  array[N] int<lower=1, upper=T> t; // Time step
  array[N,J] int<lower=0> vi; // Vote intention observations 
  array[N] int<lower=1, upper=P> pollster; // Pollster for each observation
  simplex[J] vote0; // Initial vote states; take from election results
}

parameters {
  array[T] simplex[J] vote; // True states/polling average
  real<lower=0> alpha_state; // State noise
  matrix[P, J] house_effects; // House effects for each pollster and party
}

model {
  // Priors
  alpha_state ~ gamma(1, 1.0/5000);
  for (p in 1:P) {
    house_effects[p] ~ normal(0, 0.05);
  }

  // True polling average follows a random walk for each party
  vote[1] ~ dirichlet(to_vector(vote0 * alpha_state));
  for (n in 2:T) {
    vote[n] ~ dirichlet(to_vector(vote[n-1] * alpha_state));
  }

  // Observations are noisy measurements of state with pollster bias added
  for (n in 1:N) {
    int time = t[n];
    int pollster_n = pollster[n];
    vector[J] log_vote_with_house_effect = log(vote[time]) + to_vector(house_effects[pollster_n,:]);
    vector[J] vote_with_house_effect = softmax(log_vote_with_house_effect);

    vi[n] ~ multinomial(vote_with_house_effect);
  }
}
