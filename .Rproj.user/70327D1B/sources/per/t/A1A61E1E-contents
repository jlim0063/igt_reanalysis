data {
  // Metadata
  int N;          // no. of participants
  int T;          // no. trials
  int n_options;  // no. of options
  
  // Indices
  array[4] int all_options; // indices of decks 1, 2, 3, 4: for looping through weight assoc. with each deck
  array[T] int p_ix; // Participant number for each datapoint
 
  // Data
  array[T] real gain;      // amount gained on trial t
  array[T] real loss;      // amount lost on trial t 
  array[T] int trial_no; // Trial number
  array[T] int actions;  // Choice of deck 1, 2, 3 or 4
}

transformed data{
     
}

parameters{
   // Participant level priors
   vector[N] rho_pr;    // risk-seeking (shape parameter)
   vector[N] lambda_pr; // loss aversion
   vector[N] eta_pr;    // learning rate
   vector[N] beta_pr;   // inverse temperature
}

transformed parameters{
   vector[N] rho    = Phi_approx(rho_pr);        // risk-seeking (shape parameter)
   vector[N] lambda = Phi_approx(lambda_pr) * 5; // loss aversion
   vector[N] eta    = Phi_approx(eta_pr);        // learning rate
   vector[N] beta   = Phi_approx(beta_pr) * 5;   // inverse temperature
   vector[N] C      = pow(3, beta) - 1;          // consistency (transformed from beta)
}

model{
   
   // Specify priors ------------------------- //
   
   // Participant level priors
   rho_pr     ~ normal(0, 2);
   lambda_pr  ~ normal(0, 2);
   eta_pr     ~ normal(0, 2);
   beta_pr    ~ normal(0, 2);

   // containers
   real xt;                         // Net gain/loss from chosen deck on trial t
   real ut;                         // Utility of deck on trial t
   vector[n_options] EV;            // Expected utility of each deck
   vector[n_options] choice_weight; // softmax choice weight of each deck
   
   // fill utilities with calculated options
   for (trial_ix in 1:T){
   
      // intialise exploit explore for a new participant
      if (trial_no[trial_ix] == 1){
        EV = rep_vector(0, n_options);
      }
      
      // Calculate utility of chosen deck
      xt = gain[trial_ix] - loss[trial_ix];
      if (xt >= 0){
         ut = pow(xt, rho[p_ix[trial_ix]]);
      } else{
         ut = (-1 * lambda[p_ix[trial_ix]]) * pow(abs(xt), rho[p_ix[trial_ix]]);
      }
      
      // Update expected utility
      EV[actions[trial_ix]] += eta[p_ix[trial_ix]] * (ut - EV[actions[trial_ix]]); 
         
      // calculate softmax choice weight for each deck
      for (i in all_options){
         choice_weight[i] = EV[i];
      }
      
      // Specify probability
      actions[trial_ix] ~ categorical_logit(C[p_ix[trial_ix]] * choice_weight);
   }
}

generated quantities {
   // containers
   real xt;                         // Net gain/loss from chosen deck on trial t
   real ut;                         // Utility of deck on trial t
   vector[n_options] EV;            // Expected utility of each deck
   vector[n_options] choice_weight; // softmax choice weight of each deck
   
   vector[T] choice_log_lik; // container for choice log likelihoods
   vector[T] choice_pred;    // container for choice predictions
   
   // fill utilities with calculated options
   for (trial_ix in 1:T){
   
     // intialise exploit explore for a new participant
      if (trial_no[trial_ix] == 1){
        EV = rep_vector(0, n_options);
      }
      
      // Calculate utility of chosen deck
      xt = gain[trial_ix] - loss[trial_ix];
      if (xt >= 0){
         ut = pow(xt, rho[p_ix[trial_ix]]);
      } else{
         ut = (-1 * lambda[p_ix[trial_ix]]) * pow(abs(xt), rho[p_ix[trial_ix]]);
      }
      
      // Update expected utility
      EV[actions[trial_ix]] += eta[p_ix[trial_ix]] * (ut - EV[actions[trial_ix]]); 
         
      // calculate softmax choice weight for each deck
      for (i in all_options){
         choice_weight[i] = EV[i];
      }
      
      // Choice log likelihood
      // lpmf = log prob mass function
      choice_log_lik[trial_ix] = categorical_logit_lpmf(actions[trial_ix] | C[p_ix[trial_ix]] * choice_weight);
      // Choice predictions
      choice_pred[trial_ix] = categorical_logit_rng(C[p_ix[trial_ix]] * choice_weight); 
   }
}
