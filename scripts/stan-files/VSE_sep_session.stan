data {
  // Metadata
  int N;          // no. of participants
  int T;          // no. trials
  int n_options;  // no. of options
  
  // Indices
  array[4] int all_options; // indices of decks 1, 2, 3, 4: for looping through weight assoc. with each deck
  array[T] int p_ix;        // Participant number for each datapoint
 
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
   vector<lower = 0, upper = 1>[N] theta;  // value sensitivity
   vector<lower = 0, upper = 1>[N] gamma;  // decay rate
   vector[N] phi;                          // exploration bonus
   vector<lower = 0, upper = 1>[N] eta;    // learning rate (exploration update)
   vector<lower = 0>[N] C;                 // Consistency
}

transformed parameters{
}

model{
   
   // Specify priors ------------------------- //
   
   // Participant level priors
   theta  ~ beta(1, 1);
   gamma  ~ beta(1, 1);
   phi    ~ normal(0, 1);
   eta    ~ beta(1, 1);
   C      ~ normal(0, 1);

   // containers
   vector[n_options] exploit;       // exploitation weight of each deck
   vector[n_options] explore;       // exploration weight of each deck
   vector[n_options] choice_weight; // softmax choice weight of each deck
   
   real vt;                         // Value of chosen deck at trial t

   // fill utilities with calculated options
   for (trial_ix in 1:T){
     
      // intialise exploit explore for a new session
      if (trial_no[trial_ix] == 1){
        exploit = rep_vector(0, n_options);
        explore = rep_vector(phi[p_ix[trial_ix]], n_options);
      }

      for (i in all_options){
         // calculate softmax choice weight for each deck
         choice_weight[i] = explore[i] + exploit[i];
      }

      // Specify probability
      actions[trial_ix] ~ categorical_logit(C[p_ix[trial_ix]] * choice_weight);
      
      // Update exploitation & exploration weights
      vt = pow(gain[trial_ix], theta[p_ix[trial_ix]]) - pow(loss[trial_ix], theta[p_ix[trial_ix]]);
      
      for (i in all_options){
         if (i == actions[trial_ix]){
            exploit[i] = (exploit[i] * gamma[p_ix[trial_ix]]) + vt;
            explore[i] = 0;
         } else {
            exploit[i] = exploit[i] * gamma[p_ix[trial_ix]];
            explore[i] += eta[p_ix[trial_ix]] * (phi[p_ix[trial_ix]] - explore[i]);
         }
      }
      
   }
}

generated quantities {
   // containers
   vector[n_options] exploit;       // exploitation weight of each deck
   vector[n_options] explore;       // exploration weight of each deck
   vector[n_options] choice_weight; // softmax choice weight of each deck
   
   real vt;                         // Value of chosen deck at trial t
   vector[T] choice_log_lik;        // container for choice log likelihoods
   vector[T] choice_pred;           // container for choice predictions

   // fill utilities with calculated options
   for (trial_ix in 1:T){
     
      // intialise exploit explore for a new session
      if (trial_no[trial_ix] == 1){
        exploit = rep_vector(0, n_options);
        explore = rep_vector(phi[p_ix[trial_ix]], n_options);
      }
      
      for (i in all_options){
         // calculate softmax choice weight for each deck
         choice_weight[i] = explore[i] + exploit[i];
      }

      // Choice log likelihood (lpmf = log probability mass function)
      choice_log_lik[trial_ix] = categorical_logit_lpmf(actions[trial_ix] | choice_weight*C[p_ix[trial_ix]]);
      // Choice predictions
      choice_pred[trial_ix] = categorical_logit_rng(choice_weight * C[p_ix[trial_ix]]); 
      
      // Update exploitation & exploration weights
      vt = pow(gain[trial_ix], theta[p_ix[trial_ix]]) - pow(loss[trial_ix], theta[p_ix[trial_ix]]);
      
      for (i in all_options){
         if (i == actions[trial_ix]){
            exploit[i] = (exploit[i] * gamma[p_ix[trial_ix]]) + vt;
            explore[i] = 0;
         } else {
            exploit[i] = exploit[i] * gamma[p_ix[trial_ix]];
            explore[i] += eta[p_ix[trial_ix]] * (phi[p_ix[trial_ix]] - explore[i]);
         }
      }
      
   }
}
