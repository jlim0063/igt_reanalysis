# Load required packages
require(data.table)
require(dplyr)
require(tibble)

options(scipen = 10)
set.seed(123)

# Load helper functions ---------------------------------------------------

## List of helper functions:
## calc_choice_weight_VSE() - Calculates individual deck weight for softmax choice policy (VSE model-specific)
## return_deck_index()  - Returns 1:4 given decks A:D
source(here::here("scripts", "simulation-helper-functions.R"))

# Set n participants and trials
n_pt     <- 40
n_trials <- 100

# Load previously generated data?
load_previous_data <- T

if (load_previous_data){
  load(here::here("data", "simulated", "simulated_data_VSE_123.Rdata"))
}

# Simulation script -------------------------------------------------------

if (load_previous_data == F){
    
  # Generate trial sets
  source(here::here("scripts", "generate-trials-Bechara1994.R"))
  
  
  # Sample participant parameters -------------------------------------------
  
  theta_list       <- rnorm(n_pt, mean = -.5, sd = .3) %>% pnorm() # VALUE SENSITIVITY parameter, bounded 0-1
  gamma_list       <- rnorm(n_pt, mean = 0, sd = .5) %>% pnorm()   # DECAY parameter, bounded 0-1
  eta_pre_list.raw <- rnorm(n_pt, mean = 0, sd = .5)               # LEARNING RATE parameter at baseline (UNTRANSFORMED)
  eta_pre_list     <- eta_pre_list.raw %>% pnorm()                 # LEARNING RATE parameter at baseline, bounded 0-1 (TRANSFORMED)
  phi_list         <- rnorm(n_pt, mean = 1.5, sd = sqrt(3))        # EXPLORATION BONUS parameter, unbounded
  beta_list        <- pnorm(rnorm(n_pt, mean = -1.5, sd = .5)) * 5 # INVERSE TEMPERATURE parameter, bounded 0-5 (range is around .7)
  C_list           <- 3^beta_list - 1                              # CONSISTENCY parameter, transformed from beta 
  
  delta_eta_list <- rnorm(n_pt, mean = -.2, sd = .5)          # Deviation in LEARNING RATE after experimental manipulation
  eta_post_list  <- pnorm(eta_pre_list.raw + delta_eta_list)  # LEARNING RATE parameter after experimental manipulation
  
  ## Record true param values to trial_data
  for (x in 1:n_pt){
    trial_data[subject == x ,true_theta:=theta_list[x]]
    trial_data[subject == x ,true_gamma:=gamma_list[x]]
    trial_data[subject == x ,true_eta_pre:=eta_pre_list[x]]
    trial_data[subject == x ,true_phi:=phi_list[x]]
    trial_data[subject == x ,true_beta:=beta_list[x]]
    trial_data[subject == x ,true_C:=C_list[x]]
    trial_data[subject == x ,true_delta_eta:=delta_eta_list[x]]
    trial_data[subject == x ,true_eta_post:=eta_post_list[x]]
  }
  
  # Initialise relevant containers in data.table ---------------------------
  trial_data[,exploit_weights:=list()] # Exploit weights
  trial_data[,explore_weights:=list()] # Exploit weights
  trial_data[,ptresp:=as.integer()]    # Participant choice 
  
  # Sample participant choices ----------------------------------------------
  
  ## Sampling loop (3 levels: participant, condition, trial)
  counter <- 0
  for (pt_ix in 1:n_pt){
    
    message(paste("Simulating choices for Subject", pt_ix))
  
    ## Loop through both conditions
    for (cond_ix in unique(trial_data[subject==pt_ix]$condition)){
      message(paste("Condition:", cond_ix))
      ## Record current participant's param values
      theta  <- theta_list[pt_ix]                                                 # VALUE SENSITIVITY 
      gamma  <- gamma_list[pt_ix]                                                 # DECAY RATE
      phi    <- phi_list[pt_ix]                                                   # EXPLORATION BONUS
      beta_x <- beta_list[pt_ix]                                                  # INVERSE TEMPERATURE
      C      <- C_list[pt_ix]                                                     # CONSISTENCY
      eta    <- ifelse(cond_ix=="pre", eta_pre_list[pt_ix], eta_post_list[pt_ix]) # LEARNING RATE (in current condition)
      
      ## Loop to sample choices for each trial
      for (trial_ix in 1:n_trials){
        if (trial_ix == 1){
          ## on first trial, exploit and explore weights for all 4 decks are all = 0
          exploit <- rep(0, 4)
          explore <- rep(phi, 4)
          
          ## Initialize vector to hold all participant choices
          all_choices <- c()
        }
        
        ## Add exploit/explore weights into trial_data
        counter <- counter + 1
        trial_data[counter, "exploit_weights"] <- exploit
        trial_data[counter, "explore_weights"] <- explore
        
        ## Softmax function to compute probability of picking each deck, then sample current choice
        pr_choices     <- calc_choice_weight_VSE(exploit, explore, beta_x)/sum(calc_choice_weight_VSE(exploit, explore, beta_x))
        current_choice <- sample(1:4, size = 1, prob = pr_choices)
        
        ## Record choice into choices container
        all_choices <- c(all_choices, current_choice)
        
        ## Record outcome of that choice
        all_deck_wins   <- trial_data[subject==pt_ix & condition ==cond_ix & trial_no==trial_ix, c("A_win", "B_win", "C_win", "D_win")]
        all_deck_losses <- trial_data[subject==pt_ix & condition ==cond_ix & trial_no==trial_ix, c("A_loss", "B_loss", "C_loss", "D_loss")]
        switch(current_choice,
               {win_amt <- all_deck_wins[, 1] %>% as.numeric() ; loss_amt <- all_deck_losses[, 1] %>% as.numeric()},
               {win_amt <- all_deck_wins[, 2] %>% as.numeric() ; loss_amt <- all_deck_losses[, 2] %>% as.numeric()},
               {win_amt <- all_deck_wins[, 3] %>% as.numeric() ; loss_amt <- all_deck_losses[, 3] %>% as.numeric()},
               {win_amt <- all_deck_wins[, 4] %>% as.numeric() ; loss_amt <- all_deck_losses[, 4] %>% as.numeric()}
        )
        
        ## Common practice to divide feedback by 100.
        win_amt  <- win_amt/100
        loss_amt <- loss_amt/100
        
        ## Calculate value of chosen deck and update respective exploit weight
        vt <- abs(win_amt)^theta - abs(loss_amt)^theta
        
        exploit[current_choice] <- exploit[current_choice]*gamma + vt      # For chosen deck
        exploit[1:4!=current_choice] <- exploit[1:4!=current_choice]*gamma # For other decks
        
        ## Update explore weights
        ## Explore weight for a deck drops to 0 once it's chosen
        explore[current_choice]      <- 0       
        explore[1:4!=current_choice] <- explore[1:4!=current_choice] + eta*(phi - explore[1:4!=current_choice])
        
      }
      ## Write all choices of current trial set to data table
      trial_data[subject==pt_ix & condition==cond_ix]$ptresp <- all_choices
    }
    
    # Clear global env of looping variables
    rm(theta); rm(phi); rm(gamma); rm(beta_x); rm(C); rm(eta); rm(win_amt); rm(loss_amt); rm(vt)
    rm(all_deck_wins); rm(all_deck_losses)
  }
  
  # Calculate net winnings/score of each participant ---------------------------------
  
  # Create win feedback and loss feedback columns
  trial_data[,win_feedback:=ifelse(
    ptresp == 1, A_win, ifelse(
      ptresp == 2, B_win, ifelse(
        ptresp == 3, C_win, D_win
      )
    )
  )]
  trial_data[,loss_feedback:=ifelse(
    ptresp == 1, A_loss, ifelse(
      ptresp == 2, B_loss, ifelse(
        ptresp == 3, C_loss, D_loss
      )
    )
  )]
  
  # Tally net winnings and conventional IGT scores for each participant/condition
  for (x in 1:n_pt){
    for (cond_x in unique(trial_data[subject==x]$condition)){
    
      # Tally net wins
      total_winnings <- sum(trial_data[subject==x & condition==cond_x, "win_feedback"])
      total_losses   <- sum(trial_data[subject==x & condition==cond_x, "loss_feedback"])
      trial_data[subject==x & condition==cond_x, "net_win"] <- total_winnings - total_losses
      
      
      # Tally IGT task score
      n_safe_choices  <- nrow(trial_data[subject==x & condition==cond_x & ptresp %in% c(3, 4)])
      n_risky_choices <- nrow(trial_data[subject==x & condition==cond_x & ptresp %in% c(1, 2)])
      trial_data[subject==x & condition==cond_x, "score"] <- n_safe_choices - n_risky_choices
      
      # Remove looping variables
      rm(total_winnings); rm(total_losses); rm(n_safe_choices); rm(n_risky_choices)
    }
  }
  
  # Ring when finished
  # beepr::beep("mario")
  
  # Save data
  save(trial_data, file = here::here('data', 'simulated', "simulated_data_VSE_123.Rdata"))
  
}

# Paired samples t test of net score and winnings -------------------------
test.data <- distinct(trial_data, subject, net_win, score, condition, .keep_all = T)
t.test(score ~ condition, data = test.data, paired = T)
t.test(net_win ~ condition, data = test.data, paired = T)
t.test(unique(trial_data$true_eta_pre), unique(trial_data$true_eta_post), paired = T)
