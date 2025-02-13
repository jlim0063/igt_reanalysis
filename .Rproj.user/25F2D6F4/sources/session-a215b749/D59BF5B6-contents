# Load required packages
require(data.table)
require(dplyr)
require(tibble)

options(scipen = 10)
set.seed(123)

# Load helper functions ---------------------------------------------------
## List of helper functions:
## calc_choice_weight() - Calculates individual deck weight for softmax choice policy
## return_deck_index()  - Returns 1:4 given decks A:D
source(here::here("scripts", "simulation-helper-functions.R"))


## Set n participants and trials
n_pt     <- 40
n_trials <- 100

# Load previously generated data?
load_previous_data <- T

if (load_previous_data){
  load(here::here("data", "simulated", "simulated_data_PVLD_123.Rdata"))
}

# Simulation script -------------------------------------------------------

if (load_previous_data==F){
  source(here::here("scripts", "generate-trials-Bechara1994.R"))
  
  # Sample participant parameters -------------------------------------------
  
  rho_list         <- pnorm(rnorm(n_pt, mean = 0, sd = 1))     # SHAPE parameter
  lambda_list      <- pnorm(rnorm(n_pt, mean = 0, sd = 1)) * 5 # LOSS AVERSION parameter
  eta_pre_list.raw <- rnorm(n_pt, mean = 0, sd = 1)            # LEARNING RATE (updating) parameter at baseline (UNTRANSFORMED)
  eta_pre_list     <- pnorm(eta_pre_list.raw)                  # LEARNING RATE (updating) parameter at baseline, bounded 0-1 (TRANSFORMED)
  
  beta_list <- pnorm(rnorm(n_pt, mean = -1.5, sd = .5)) * 5      # INVERSE TEMPERATURE parameter
  C_list    <- 3^beta_list - 1                                 # CONSISTENCY parameter, TRANSFORMED from INVERSE TEMPERATURE
  
  delta_eta_list   <- rnorm(n_pt, mean = -.2, sd = .4)         # Offset in LEARNING RATE during POST condition
  eta_post_list    <- pnorm(eta_pre_list.raw + delta_eta_list) # LEARNING RATE (updating) parameter during POST condition, bounded 0-1
  
  ## Record true param values to trial_data
  for (x in 1:n_pt){
    trial_data[subject == x , true_rho := rho_list[x]]
    trial_data[subject == x , true_lambda := lambda_list[x]]
    trial_data[subject == x , true_eta_pre := eta_pre_list[x]]
    trial_data[subject == x , true_beta := beta_list[x]]
    trial_data[subject == x , true_C := C_list[x]]
    trial_data[subject == x , true_delta_eta := delta_eta_list[x]]
    trial_data[subject == x , true_eta_post := eta_post_list[x]]
  }
  
  # Initialize relevant containers in data.table ---------------------------
  trial_data[,trial_EV:=list()]            # Expected utility of all 4 decks
  trial_data[,pr_choices:=list()]            # Expected utility of all 4 decks
  trial_data[,ptresp:=as.integer()]        # Participant choice 
  
  ## Loop to sample choices for each trial
  counter <- 0
  
  for (pt_ix in 1:n_pt){
    message(paste("Simulating choices for Subject", pt_ix))
    
    for (cond_ix in unique(trial_data[subject==pt_ix]$condition)){
      message(paste("Condition:", cond_ix))
      ## Record current participant's param values
      rho    <- rho_list[pt_ix]                                                   # SHAPE/RISK-SEEKING
      lambda <- lambda_list[pt_ix]                                                # LOSS AVERSION
      beta_x <- beta_list[pt_ix]                                                  # INVERSE TEMPERATURE
      C      <- C_list[pt_ix]                                                     # CONSISTENCY
      eta    <- ifelse(cond_ix=="pre", eta_pre_list[pt_ix], eta_post_list[pt_ix]) # LEARNING RATE
      
      ## Initialise containers
      all_EVs     <- rep(0, 4) ## EVs set to 0 for all decks
      all_choices <- c()       ## Container to hold all choices.
      
      for (trial_x in 1:n_trials){
        
        ## Softmax function to compute probability of picking each deck, then sample current choice
        pr_choices <- calc_choice_weight(alpha = all_EVs, beta = beta_x, beta_trf = T)/sum(calc_choice_weight(alpha = all_EVs, beta = beta_x, beta_trf = T))
        current_choice <- sample(1:4, size = 1, prob = pr_choices)
        
        ## Write pr_choices and EVs to trial_data
        counter <- counter + 1
        trial_data[counter, "trial_EV"]   <- all_EVs
        trial_data[counter, "pr_choices"] <- pr_choices
  
        ## Record choice into choices container
        all_choices <- c(all_choices, current_choice)
        
        ## Record outcome of that choice
        all_deck_wins   <- trial_data[subject==pt_ix & condition ==cond_ix & trial_no==trial_x, c("A_win", "B_win", "C_win", "D_win")]
        all_deck_losses <- trial_data[subject==pt_ix & condition ==cond_ix & trial_no==trial_x, c("A_loss", "B_loss", "C_loss", "D_loss")]
        switch(current_choice,
               {win_amt <- all_deck_wins[, 1] %>% as.numeric() ; loss_amt <- all_deck_losses[, 1] %>% as.numeric()},
               {win_amt <- all_deck_wins[, 2] %>% as.numeric() ; loss_amt <- all_deck_losses[, 2] %>% as.numeric()},
               {win_amt <- all_deck_wins[, 3] %>% as.numeric() ; loss_amt <- all_deck_losses[, 3] %>% as.numeric()},
               {win_amt <- all_deck_wins[, 4] %>% as.numeric() ; loss_amt <- all_deck_losses[, 4] %>% as.numeric()}
        )
        
        ## Common practice to divide feedback by 100.
        win_amt  <- win_amt /100
        loss_amt <- loss_amt/100
        
        # Calculate experienced utility of current selection on current trial
        Xt <- win_amt - loss_amt  # net gain
        Ut <- ifelse(Xt >= 0, Xt^rho, -lambda*(abs(Xt)^rho))
        
        # Update EV of current deck
        all_EVs[current_choice] <- all_EVs[current_choice] + eta*(Ut - all_EVs[current_choice])
      }
      ## Write all choices of current trial set to data table
      trial_data[subject==pt_ix & condition==cond_ix]$ptresp <- all_choices
    }
    
    # Clear global env of looping variables
    rm(rho); rm(lambda); rm(beta_x); rm(C); rm(eta); rm(win_amt); rm(loss_amt); rm(Ut); rm(Xt)
    rm(all_deck_wins); rm(all_deck_losses)
  }
  
  # Save data
  save(trial_data, file = here::here('data', 'simulated', "simulated_data_PVLD_123.Rdata"))
  
}
