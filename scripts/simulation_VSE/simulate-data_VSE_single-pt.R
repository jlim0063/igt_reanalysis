# Clear global environment
# rm(list=ls())

# Load required packages
require(data.table)
require(dplyr)
require(ggplot2)

# Set n = 1
n_pt <- 1
n_trials <- 100

# Load helper functions and generate trial data
source(here::here('scripts', 'simulation-helper-functions.R'))
source(here::here('scripts', 'generate-trials-Bechara1994.R'))

# Isolate just the first trial set
trial_data <- trial_data[condition == "pre"]

# Test cases
## 0 = random sampling of parameter values
## 1 = high exploitative
## 2 = high explorative
## 3 = accumulating exploit weight demonstration without dividing feedback
case <- 3

if (case == 1){
  theta <- .6
  gamma <- .8  
  eta   <- .5
  phi   <- -2
  beta  <- 2
} else if (case == 2){
   theta  <- 0 
   gamma  <- .9 
   eta    <- .5 
   phi    <- 3 
   beta   <- 4.5  
} else if (case == 3){ 
   theta  <- .8 
   gamma  <- .9 
   eta    <- .5 
   phi    <- 3 
   beta   <- 4  
} else {
  set.seed(sample(1:999, size = 1))
  theta <- pnorm(rnorm(1, mean = 0, sd = 1))
  gamma <- pnorm(rnorm(1, mean = 0, sd = 1))
  eta   <- pnorm(rnorm(1, mean = 0, sd = 1))
  phi   <- rnorm(1, mean = 0, sd = 1)
  beta  <- pnorm(rnorm(1, mean = 0, sd = 1)) * 5
}

C      <- 3^beta - 1 

# Simulate x number of trials --------------------------------------------------------
sim_trials <- ifelse(case == 0, 10, 100)

set.seed(123)
for (x in 0:sim_trials){
  if (x == 0){
    # Initialise container to hold choice pattern
    choice_pattern <- c()
    
    # Initialise exploit/explore weights
    exploit <- rep(0, 4)
    explore <- rep(phi, 4)
    
    # Initialise cumulative gain/loss as 0
    total_gain <- 0
    total_loss <- 0
    
    # Initialise no. of risky vs advantageous choices as 0
    n_risky <- 0
    n_adv   <- 0
    
    # Initialise container for exploit weights
    exploit1 <- c()
    exploit2 <- c()
    exploit3 <- c()
    exploit4 <- c()
    
    message(paste0(
      "==================================", "\n",
      "Participant parameters \n",
      "==================================", "\n",
      "Value sensitivity: ", theta, "\n",
      "Decay rate: ", gamma, "\n",
      "Learning rate: ", eta,"\n",
      "Exploration bonus: ", phi, "\n",
      "Inverse temperature: ", beta, "\n", 
      "Consistency: ", C, "\n",
      "----------------------------------", "\n"
    ))
  } else {
    ## Record all possible gains and losses from the 4 decks
    possible_gains  <- trial_data[trial_no == x, c("A_win", "B_win", "C_win", "D_win")] %>% as.numeric()
    possible_losses <- trial_data[trial_no == x, c("A_loss", "B_loss", "C_loss", "D_loss")] %>% as.numeric()
    
    ## Softmax and sample choice
    pr_choices <- calc_choice_weight_VSE(exploit, explore, beta)/sum(calc_choice_weight_VSE(exploit, explore, beta))
    choice     <- sample(1:4, size = 1, prob = pr_choices)
    
    ## Record feedback
    gain_amt <- possible_gains[choice]
    loss_amt <- possible_losses[choice]
    
    ## Record value of deck on current trial, i.e., v(t)
    if (case == 3){
      ## only for demonstration of exploitation weight accumulation
      vt <- abs(gain_amt)^theta - abs(loss_amt)^theta
    } else {
      vt <- abs(gain_amt/100)^theta - abs(loss_amt/100)^theta
    }
    
    ## Update exploit weights
    exploit[choice]        <- exploit[choice]*gamma + vt
    exploit[1:4 != choice] <- exploit[1:4 != choice] * gamma
    
    ## Update explore weights
    explore[choice]        <- 0
    explore[1:4 != choice] <- explore[1:4 != choice] + eta*(phi - explore[1:4 != choice])
    
    # Record choice and cumulative gain/losses
    choice_pattern <- c(choice_pattern, choice)
    total_gain <- total_gain + gain_amt
    total_loss <- total_loss + loss_amt
    
    # Update n of risky or advantageous choices
    switch(choice,
           {n_risky <- n_risky + 1},
           {n_risky <- n_risky + 1},
           {n_adv   <- n_adv + 1},
           {n_adv   <- n_adv + 1},
    )
    
    # Print current trial information
    message(paste(
      "Trial", x,  "\n",
      "----------------------------------", "\n",
      "Agent selected: DECK", choice, "\n",
      "received", gain_amt, "and penalty of", loss_amt , ".\n",
      "Updated Exploitataion weights: ", "\n", 
      exploit[1], exploit[2], exploit[3], exploit[4], "\n",
      "Updated Exploration weights:", "\n",
      explore[1], explore[2], explore[3], explore[4], "\n",
      "----------------------------------", "\n"
    ))
    
    # Print choice pattern, net gain, and score at the end
    if (x == sim_trials){
      message(paste("Overall agent performance over", sim_trials, "trials:"))
      message(paste(choice_pattern, collapse = "-"))
      message(paste("IGT score:", n_adv-n_risky))
      message(paste0("Total amount earned: $", total_gain - total_loss))
    }
    
    if (case ==0){
      Sys.sleep(.75)
    }
    
    exploit1 <- c(exploit1, exploit[1])
    exploit2 <- c(exploit2, exploit[2])
    exploit3 <- c(exploit3, exploit[3])
    exploit4 <- c(exploit4, exploit[4])
    
  }
}


# Plot exploit weights ----------------------------------------------------

if (case == 3){
  trial         <- rep(1:100, 4)
  exploit       <- c(exploit1, exploit2, exploit3, exploit4)
  deck          <- rep(1:4, each = 100)
  case3_exploit <- data.table(trial, deck, exploit)
  
  ggplot(data= case3_exploit, aes(x = trial, y = exploit, color = factor(deck))) +
    geom_line(linewidth = .8) +
    # ylim(c(-4, 4)) +
    scale_x_continuous(breaks = seq(1, 100, by = 10))+
    labs(title = expression(paste("Case of runaway ", Exploit^d, " values")), color = "Deck") + 
    hrbrthemes::theme_ipsum() +
    theme(plot.title   = element_text(size = 23, family = "Times New Roman"), 
          legend.title = element_text(size = 18, family = "Times New Roman"),
          axis.title.x = element_text(size = 18, family = "Times New Roman"),
          axis.title.y = element_text(size = 18, family = "Times New Roman")) +
    xlab("Trial") + ylab(expression(Exploit^d))
    
}
  
