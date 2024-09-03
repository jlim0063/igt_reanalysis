data {
  // Metadata
  int N;         // no. of participants
  int T;         // no. trials
  int n_options; // no. of options
  int condition  // condition, 1 = control and 2 = experimental
  
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
}

parameters{
  // Group-level means
  real beta_mu_pr;        // Inverse temperature
  real w_prior_mu_pr;     // Subjective weight on prior
  real delta_w_prior_mu_pr; // Change in weight on prior
  
  // Group-level SDs
  real<lower=0> beta_sigma_pr;        // Inverse temperature
  real<lower=0> w_prior_sigma_pr;     // Subjective weight on prior
  real<lower=0> delta_w_prior_sigma_pr; // Change in weight on prior
  
  // Participant-level parameters
  vector[N] beta_pr;          // Inverse temperature
  vector[N] w_prior_pr;       // Subjective weight on prior
  vector[N] delta_w_prior_pr; // Change in weight on prior
}

transformed parameters{
  vector[N] beta;          // Inverse temperature
  vector[N] w_prior;       // Subjective weight on prior
  vector[N] w_evidence;    // Subjective weight on evidence (1 - w_prior)
  vector[N] delta_w_prior; // w_prior change
  
  vector[N] w_prior_sr;    // Weight on prior during SR
  vector[N] w_evidence_sr; // Weight on evidence during SR
  
  for (loop_p_ix in 1:N){
    // Baseline parameters
    beta[loop_p_ix]        = exp(beta_mu_pr + beta_sigma_pr * beta_pr[loop_p_ix]);
    w_prior[loop_p_ix]     = Phi_approx(w_prior_mu_pr + w_prior_sigma_pr * w_prior_pr[loop_p_ix]);
    
    // Deviation parameter
    delta_w_prior[loop_p_ix] = delta_w_prior_mu_pr + delta_w_prior_sigma_pr * delta_w_prior_pr[loop_p_ix];
    
    // Parameters during SR
    w_prior_sr[loop_p_ix] = Phi_approx(w_prior_mu_pr + w_prior_sigma_pr * w_prior_pr[loop_p_ix] + delta_w_prior[loop_p_ix]);
  }
  
  // Weight on evidence, which is inversely proportional to weight on prior
  w_evidence    = 1 - w_prior;
  w_evidence_sr = 1 - w_prior_sr;
}

model{
  
  // Specify priors ------------------------- //
  // group-level priors for means
  beta_mu_pr          ~ normal(0,1);
  w_prior_mu_pr       ~ normal(0,1);
  delta_w_prior_mu_pr ~ normal(0,1);
  
  // group-level priors for SDs
  beta_sigma_pr          ~ exponential(0.1);
  w_prior_sigma_pr       ~ exponential(0.1);
  delta_w_prior_sigma_pr ~ exponential(1);
  
  // participant-level priors
  beta_pr          ~ normal(0,1);
  w_prior_pr       ~ normal(0,1);
  delta_w_prior_pr ~ normal(0,1);
  
  // Create containers---------------------- //
  
  vector[n_options] beliefs = rep_vector(1.0/n_options, n_options); // Beliefs; intialise as maximal uncertainty
  vector[n_options] likelihood;
  vector[n_options] log_beliefs;
  vector[n_options] log_likelihood;
  vector[T]         p;
  
  // Model -------------------------------- //
  
  // Update beliefs based on switch probability
  // ***Need supplementary explanation of the math for this part
  // beliefs = beliefs.*(1-pr_switch) + (1-beliefs).*pr_switch.*(1.0/(n_options-1)); // Dan's original code
  beliefs = beliefs.*(1-pr_switch) + (1-beliefs).*pr_switch;
  
  for (trial_ix in 1:T){
    // Compute probability of choosing option 2 based on beliefs
    // NOTE: The use of the inverse temperature parameter here is different from that in the QLearning model
          // You can only do this if two choices. For three or more choice, need to apply different formula
    p[trial_ix] = beta[p_ix[trial_ix]] * (beliefs[2] - beliefs[1]);
    
    
    // Compute likelihood of observed outcome
    if(outcomes[trial_ix] == 1){
      likelihood = rep_vector(pr_win_given_incorrect, n_options);
      likelihood[actions[trial_ix] + 1] = pr_win_given_correct;
    } else if (outcomes[trial_ix] == 0){
      likelihood = rep_vector(1 - pr_win_given_incorrect, n_options);
      likelihood[actions[trial_ix] + 1] = 1 - pr_win_given_correct;
    }

    // Bayesian updating with weighting parameters: w_prior and w_evidence
    log_beliefs    = log(beliefs ./ (1-beliefs));                                   // transform beliefs to log odds scale
    log_likelihood = log(likelihood ./(1-likelihood));                              // transform likelihood to log odds scale
  
    if (condition[trial_ix] == 1){
      // weight log-odds beliefs and likelihood by control condition parameters and sum
      log_beliefs = w_prior[p_ix[trial_ix]].*log_beliefs + w_evidence[p_ix[trial_ix]].*log_likelihood;   
    } else if (condition[trial_ix == 2){
      // weight log-odds beliefs and likelihood by experimental condition parameters and sum
      log_beliefs = w_prior_sr[p_ix[trial_ix]].*log_beliefs + w_evidence_sr[p_ix[trial_ix]].*log_likelihood;
    }
    
    beliefs = exp(log_beliefs)./(1+exp(log_beliefs)); // exponentiate to reverse the log
    
  }
  

  // Choice likelihood
  // equivalent of this function in R is Rlab::rbern()
  actions ~  bernoulli_logit( p ); 

}

generated quantities {
  
  // container for transformed means of beta and w_prior
  real beta_mu    = exp(beta_mu_pr);
  real w_prior_mu = Phi_approx(w_prior_mu_pr);
  
  // create containers
  vector[n_options] beliefs = rep_vector(1.0/n_options, n_options); // Beliefs; intialise as maximal uncertainty
  vector[n_options] likelihood;
  vector[n_options] log_beliefs;
  vector[n_options] log_likelihood;
  vector[T]         p;
  
  vector[T] choice_log_lik; // Container for choice log likelihoods
  vector[T] choice_pred;    // Container for choice predictions
  
  // Update beliefs based on switch probability
  // ***Need supplementary explanation of the math for the part
  // beliefs = beliefs.*(1-pr_switch) + (1-beliefs).*pr_switch.*(1.0/(n_options-1)); // Dan's original code
  beliefs = beliefs.*(1-pr_switch) + (1-beliefs).*pr_switch;
  
  for (trial_ix in 1:T){
    // Compute probability of choosing option 2 based on beliefs
    p[trial_ix] = beta[p_ix[trial_ix]] * (beliefs[2] - beliefs[1]);
    
    
    // Compute likelihood of observed outcome
    if(outcomes[trial_ix] == 1){
      likelihood = rep_vector(pr_win_given_incorrect, n_options);
      likelihood[actions[trial_ix] + 1] = pr_win_given_correct;
    } else if (outcomes[trial_ix] == 0){
      likelihood = rep_vector(1 - pr_win_given_incorrect, n_options);
      likelihood[actions[trial_ix] + 1] = 1 - pr_win_given_correct;
    }
    
    // update beliefs based on chosen action and observed outcome
    // beliefs = (beliefs .* likelihood) / sum(beliefs .* likelihood);
    
    // version with weighting parameters: w_prior and w_evidence
    log_beliefs    = log(beliefs ./ (1-beliefs));                                   // transform beliefs to log odds scale
    log_likelihood = log(likelihood ./(1-likelihood));                              // transform likelihood to log odds scale
    
    // Condition check
    if (condition[trial_ix] == 1){
      // weight log-odds beliefs and likelihood by control condition parameters and sum
      log_beliefs = w_prior[p_ix[trial_ix]].*log_beliefs + w_evidence[p_ix[trial_ix]].*log_likelihood;   
    } else {
      // weight log-odds beliefs and likelihood by experimental condition parameters and sum
      log_beliefs = w_prior_sr[p_ix[trial_ix]].*log_beliefs + w_evidence_sr[p_ix[trial_ix]].*log_likelihood;
    }
    
    beliefs = exp(log_beliefs)./(1+exp(log_beliefs)); // exponentiate to reverse the log
   
    // Choice log likelihood
    // lpmf = log prob mass function
    choice_log_lik[trial_ix] = bernoulli_logit_lpmf(actions[trial_ix] | p[trial_ix]);
    // Choice predictions
    choice_pred[trial_ix] = bernoulli_logit_rng(p[trial_ix]); 
  }
}