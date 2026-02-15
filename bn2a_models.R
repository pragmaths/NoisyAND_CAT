################################################################################
# BN2A Models: 
# Implements both leak noisy-and and simple noisy-and models
# Notation: Y = observed variables (items), X = hidden variables (skills)
################################################################################

################################################################################
# LEAK NOISY-AND MODEL (BN2A)
################################################################################
#' Calculate P(Y_l = 1 | x) for leak noisy-and model
#'
#' @param x Vector of length K (skill mastery profile)
#' @param item_id Item index (1 to J)
#' @param Q Q-matrix (K x J matrix)
#' @param params_leak List with q0 (base probs) and penalties (K x J matrix)
#' @return Probability of correct response
prob_leak_noisy_and <- function(x, item_id, Q, params_leak) {
  K <- length(x)
  
  # Base probability
  q0 <- params_leak$q0[item_id]
  
  # Product term: multiply penalties for non-mastered required skills
  product_term <- 1.0
  
  for (k in 1:K) {
    # Check if skill k is required by this item
    if (Q[k, item_id] == 1) {
      # Get penalty parameter
      q_k <- params_leak$penalties[k, item_id]
      # Apply penalty factor: q_k^(1 - x_k)
      # If x_k = 0: multiply by q_k (penalty applied)
      # If x_k = 1: multiply by q_k^0 = 1 (no penalty)
      product_term <- product_term * (q_k ^ (1 - x[k]))
    }
  }
  
  # Final probability
  prob <- q0 * product_term
  
  return(prob)
}

################################################################################
# SIMPLE NOISY-AND MODEL (DINA-like)
################################################################################
#' Calculate P(Y_l = 1 | x) for simple noisy-and model
#'
#' @param x Vector of length K (skill mastery profile)
#' @param item_id Item index (1 to J)
#' @param Q Q-matrix (K x J matrix)
#' @param params_simple List with slip and guess vectors (length J)
#' @return Probability of correct response
prob_simple_noisy_and <- function(x, item_id, Q, params_simple) {
  K <- length(x)
  
  # Compute ideal response: eta = 1 if ALL required skills mastered
  eta <- 1  # Start assuming ideal response
  
  for (k in 1:K) {
    if (Q[k, item_id] == 1) {  # If skill k is required
      if (x[k] == 0) {          # If NOT mastered
        eta <- 0                # Not an ideal response
        break
      }
    }
  }
  
  # Get slip and guess parameters
  slip <- params_simple$slip[item_id]
  guess <- params_simple$guess[item_id]
  
  # Probability formula
  if (eta == 1) {
    prob <- 1 - slip
  } else {
    prob <- guess
  }
  
  return(prob)
}