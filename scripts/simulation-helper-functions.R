# Default method of calculating choice weights for softmax policy (single deck)
calc_choice_weight <- function(alpha, beta, beta_trf = T){
  if(!beta_trf  %in% c(T, F)){
    stop("'consistency' argument needs to be a boolean value")
  }
  if (beta_trf == T){
    C <- 3^beta - 1
    return(exp(alpha*C))
  } else {return(exp(alpha*beta))}
}

# VSE model's version of calculating choice weights for softmax policy (single deck)
calc_choice_weight_VSE <- function(exploit, explore, beta){
  # message("Note: VSE model utilises a Consistency parameter = 3^beta - 1")
  C <- 3^beta - 1
  return(exp(exploit+explore)*C)
}

# Function to return deck numerical index (1-4 given A-D)
return_deck_index <- function(choices = c()){
  indices <- c()
  for(x in choices){
    if (x %in% c("A", "K", "Q")){
      indices <- c(indices, 1)
    } else if (x %in% c("B", "L", "R")){
      indices <- c(indices, 2)
    } else if (x %in% c("C", "M", "S")) {
      indices <- c(indices, 3)
    } else if (x %in% c("D", "N", "T")){
      indices <- c(indices, 4)
    }
  }
  return(indices)
}


