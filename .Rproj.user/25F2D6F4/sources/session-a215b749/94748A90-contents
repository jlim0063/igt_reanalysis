# Load required packages
require(data.table)
require(dplyr)

# Set win values for each deck
A_win_amt <- 100
B_win_amt <- 100
C_win_amt <- 50
D_win_amt <- 50

# Record possible loss values for each deck
A_loss_amt <- rep(c(0, 0, 0, 0, 0, 150, 200, 250, 300, 350), 10)
B_loss_amt <- rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1250), 10)
C_loss_amt <- rep(c(0, 0, 0, 0, 0, 50, 50, 50, 50, 50), 10)
D_loss_amt <- rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 250), 10)

# Shuffle decks for each participant, in each condition
trial_data <- data.table()

for (pt_ix in 1:n_pt){
  for (set_x in 1:2){
    ## Shuffle decks
    A_loss <- sample(A_loss_amt)
    B_loss <- sample(B_loss_amt)
    C_loss <- sample(C_loss_amt)
    D_loss <- sample(D_loss_amt)
    
    subject <- rep(pt_ix, n_trials)
    if (set_x == 1){
      condition <- rep("pre", n_trials)
    } else{
      condition <- rep("post", n_trials)
    }
    trial_no <- 1:n_trials
    
    current_set <- data.table(subject, condition, trial_no, A_loss, B_loss, C_loss, D_loss)
    trial_data  <- rbind(trial_data, current_set)
  }
}

# Add win amounts to trial_data
trial_data[, A_win:=A_win_amt]
trial_data[, B_win:=B_win_amt]
trial_data[, C_win:=C_win_amt]
trial_data[, D_win:=D_win_amt]

# Print visual confirmation
message(paste("Trial set generated for", n_pt, "participants and 2 conditions (pre and post).", n_trials, "trials per set.", sep =" "))
message("Trial sets saved as `trial_data` in global environment.")

