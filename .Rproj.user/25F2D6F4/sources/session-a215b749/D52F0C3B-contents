data {
  // Metadata
  int N;      // no. of participants
  int T;      // no. trials
  int n_options; // no. of options
  
  // Indices
  array[T] int p_ix; // Pariticpant number for each datapoint
 
  // Data
  array[T] int actions;          // Dependent variable: action taken (0 = option 1; 1 = option 2)
  array[T] int outcomes;         // Outcome (0 = no reward; 1 = reward)
  array[T] int time_since_switch; // time since last reversal at trial t
}

transformed data{
  // Task information
  real pr_win_given_correct   = .70;
  real pr_win_given_incorrect = 1 - pr_win_given_correct;
  real pr_switch              = .10;
  
  real reward                 = 1;

}

parameters{
  vector<lower=0>[N] beta;
  vector[N] eta_pr;
}

transformed parameters{
  vector[N] eta = Phi_approx(eta_pr);
}

model{
  // participant-level priors
  beta ~ normal(0, 1);
  eta_pr  ~ normal(0, 1);

 // containers
 vector[2]  Q;     // Q-values of each option
 vector[T]  Q_diff; // difference in Q values
 vector[T]  alpha;  // parameter for bernoulli logit
 
 // fill utilities with calculated options
 for (trial_ix in 1:T){
   
   // intialise Q-values for a new participant
   if (trial_ix == 1){
     Q = rep_vector(reward/n_options, 2);
   }
   
   // Calculate difference in Q-values
   Q_diff[trial_ix] = Q[2] - Q[1];
   
   // Calculate parmater for bernoulli logit
   alpha[trial_ix] = beta[p_ix[trial_ix]] * Q_diff[trial_ix];
   
   // Update Q values
   Q[actions[trial_ix] + 1] +=  eta[p_ix[trial_ix]] * (outcomes[trial_ix] - Q[actions[trial_ix] + 1]);
   
   
   // Specify probability
   actions[trial_ix] ~ bernoulli_logit(alpha[trial_ix]);
 }
}

generated quantities {
 // containers
 vector[2]  Q;     // Q-values of each option
 vector[T]  Q_diff; // difference in Q values
 vector[T]  alpha;  // parameter for bernoulli logit
 
 vector[T] choice_log_lik; // container for choice log likelihoods
 vector[T] choice_pred;    // container for choice predictions
 
 // fill utilities with calculated options
  for (trial_ix in 1:T){
   
   // intialise Q-values for a new participant
   if (trial_ix == 1){
     Q = rep_vector(reward/n_options, 2);
   }
   
   // Calculate difference in Q-values
   Q_diff[trial_ix] = Q[2] - Q[1];
   
   // Calculate parmater for bernoulli logit
   alpha[trial_ix] = beta[p_ix[trial_ix]] * Q_diff[trial_ix];
   
   // Update Q values
   Q[actions[trial_ix] + 1] +=  eta[p_ix[trial_ix]] * (outcomes[trial_ix] - Q[actions[trial_ix] + 1]);
   
  // Choice log likelihood
  // lpmf = log prob mass function
  choice_log_lik[trial_ix] = bernoulli_logit_lpmf(actions[trial_ix] | alpha[trial_ix]);
  // Choice predictions
  choice_pred[trial_ix] = bernoulli_logit_rng(alpha[trial_ix]); 
 }
}
