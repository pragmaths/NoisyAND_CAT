################################################################################
# Item Selection Algorithms for CD-CAT
# Implements: PWKL, SHE, and MI
# Notation: Y = observed variables (items), X = hidden variables (skills)
################################################################################

# Source the model functions
source("bn2a_models.R")

################################################################################
# HELPER FUNCTIONS
################################################################################

#' Generate all possible skill patterns
#'
#' @param K Number of skills
#' @return Matrix with 2^K rows and K columns
generate_all_patterns <- function(K) {
  # Generate all binary combinations
  patterns <- expand.grid(rep(list(0:1), K))
  patterns <- as.matrix(patterns)
  colnames(patterns) <- paste0("X", 1:K)
  return(patterns)
}

#' Compute posterior distribution given responses
#'
#' @param responses Vector of responses (0/1) to items administered so far
#' @param item_ids Vector of item IDs that were administered
#' @param Q Q-matrix
#' @param params Parameters (either params_leak or params_simple)
#' @param model_type Either "leak" or "simple"
#' @param prior_probs Prior probabilities for each pattern (default: uniform)
#' @return Vector of length 2^K with posterior probabilities
compute_posterior <- function(responses, item_ids, Q, params, model_type, 
                              prior_probs = NULL) {
  K <- nrow(Q)
  
  # Generate all possible patterns
  all_patterns <- generate_all_patterns(K)
  num_patterns <- nrow(all_patterns)
  
  # Initialize with prior (uniform if not specified)
  if (is.null(prior_probs)) {
    posterior <- rep(1/num_patterns, num_patterns)
  } else {
    posterior <- prior_probs
  }
  
  # If no items administered yet, return prior
  if (length(responses) == 0) {
    return(posterior)
  }
  
  # Update posterior for each response
  likelihood <- rep(1, num_patterns)
  
  for (i in 1:length(responses)) {
    item_id <- item_ids[i]
    response <- responses[i]
    
    # Compute P(Y = response | x) for each pattern
    for (p in 1:num_patterns) {
      x <- all_patterns[p, ]
      
      if (model_type == "leak") {
        prob_correct <- prob_leak_noisy_and(x, item_id, Q, params)
      } else {
        prob_correct <- prob_simple_noisy_and(x, item_id, Q, params)
      }
      
      if (response == 1) {
        likelihood[p] <- likelihood[p] * prob_correct
      } else {
        likelihood[p] <- likelihood[p] * (1 - prob_correct)
      }
    }
  }
  
  # Multiply prior by likelihood
  posterior <- posterior * likelihood
  
  # Normalize
  posterior <- posterior / sum(posterior)
  
  return(posterior)
}

################################################################################
# POSTERIOR-WEIGHTED KULLBACK-LEIBLER (PWKL)
################################################################################

#' Compute PWKL index for a candidate item
#'
#' @param item_id Candidate item ID
#' @param current_estimate Current MAP estimate (vector of length K)
#' @param posterior Current posterior distribution (vector of length 2^K)
#' @param Q Q-matrix
#' @param params Model parameters
#' @param model_type "leak" or "simple"
#' @return PWKL index value
compute_pwkl <- function(item_id, current_estimate, posterior, Q, params, model_type) {
  K <- nrow(Q)
  all_patterns <- generate_all_patterns(K)
  num_patterns <- nrow(all_patterns)
  
  # Compute P(Y = y | current_estimate) for y in {0, 1}
  if (model_type == "leak") {
    p_y1_current <- prob_leak_noisy_and(current_estimate, item_id, Q, params)
  } else {
    p_y1_current <- prob_simple_noisy_and(current_estimate, item_id, Q, params)
  }
  p_y0_current <- 1 - p_y1_current
  
  # Compute PWKL
  pwkl <- 0
  
  for (c in 1:num_patterns) {
    x_c <- all_patterns[c, ]
    
    # Compute P(Y = y | x_c) for y in {0, 1}
    if (model_type == "leak") {
      p_y1_c <- prob_leak_noisy_and(x_c, item_id, Q, params)
    } else {
      p_y1_c <- prob_simple_noisy_and(x_c, item_id, Q, params)
    }
    p_y0_c <- 1 - p_y1_c
    
    # KL divergence for this pattern
    kl_c <- 0
    
    # Add KL for Y = 1
    if (p_y1_current > 0 && p_y1_c > 0) {
      kl_c <- kl_c + p_y1_current * log(p_y1_current / p_y1_c)
    }
    
    # Add KL for Y = 0
    if (p_y0_current > 0 && p_y0_c > 0) {
      kl_c <- kl_c + p_y0_current * log(p_y0_current / p_y0_c)
    }
    
    # Weight by posterior probability
    pwkl <- pwkl + posterior[c] * kl_c
  }
  
  return(pwkl)
}

################################################################################
# SHANNON ENTROPY (SHE)
################################################################################

#' Compute expected Shannon entropy after administering an item
#'
#' @param item_id Candidate item ID
#' @param posterior Current posterior distribution
#' @param Q Q-matrix
#' @param params Model parameters
#' @param model_type "leak" or "simple"
#' @return Expected Shannon entropy
compute_she <- function(item_id, posterior, Q, params, model_type) {
  K <- nrow(Q)
  all_patterns <- generate_all_patterns(K)
  num_patterns <- nrow(all_patterns)
  
  # Compute P(Y = y) marginalized over patterns
  p_y1 <- 0
  for (c in 1:num_patterns) {
    x_c <- all_patterns[c, ]
    if (model_type == "leak") {
      prob_c <- prob_leak_noisy_and(x_c, item_id, Q, params)
    } else {
      prob_c <- prob_simple_noisy_and(x_c, item_id, Q, params)
    }
    p_y1 <- p_y1 + posterior[c] * prob_c
  }
  p_y0 <- 1 - p_y1
  
  # Expected entropy over both possible responses
  expected_entropy <- 0
  
  for (y in c(0, 1)) {
    # Compute posterior after observing Y = y
    posterior_after <- numeric(num_patterns)
    
    for (c in 1:num_patterns) {
      x_c <- all_patterns[c, ]
      if (model_type == "leak") {
        prob_c <- prob_leak_noisy_and(x_c, item_id, Q, params)
      } else {
        prob_c <- prob_simple_noisy_and(x_c, item_id, Q, params)
      }
      
      if (y == 1) {
        posterior_after[c] <- posterior[c] * prob_c
      } else {
        posterior_after[c] <- posterior[c] * (1 - prob_c)
      }
    }
    
    # Normalize
    if (sum(posterior_after) > 0) {
      posterior_after <- posterior_after / sum(posterior_after)
    }
    
    # Compute Shannon entropy of this posterior
    entropy_y <- 0
    for (c in 1:num_patterns) {
      if (posterior_after[c] > 0) {
        entropy_y <- entropy_y - posterior_after[c] * log(posterior_after[c])
      }
    }
    
    # Weight by probability of observing Y = y
    p_y <- ifelse(y == 1, p_y1, p_y0)
    expected_entropy <- expected_entropy + p_y * entropy_y
  }
  
  return(expected_entropy)
}

################################################################################
# MUTUAL INFORMATION (MI)
################################################################################

#' Compute mutual information for a candidate item
#'
#' @param item_id Candidate item ID
#' @param posterior Current posterior distribution
#' @param Q Q-matrix
#' @param params Model parameters
#' @param model_type "leak" or "simple"
#' @return Mutual information value
compute_mi <- function(item_id, posterior, Q, params, model_type) {
  K <- nrow(Q)
  all_patterns <- generate_all_patterns(K)
  num_patterns <- nrow(all_patterns)
  
  mi <- 0
  
  # Compute P(Y = y) for y in {0, 1}
  p_y1 <- 0
  for (c in 1:num_patterns) {
    x_c <- all_patterns[c, ]
    if (model_type == "leak") {
      prob_c <- prob_leak_noisy_and(x_c, item_id, Q, params)
    } else {
      prob_c <- prob_simple_noisy_and(x_c, item_id, Q, params)
    }
    p_y1 <- p_y1 + posterior[c] * prob_c
  }
  p_y0 <- 1 - p_y1
  
  # Compute MI = sum over (x, y) of p(x, y) * log(p(x, y) / (p(x) * p(y)))
  for (c in 1:num_patterns) {
    x_c <- all_patterns[c, ]
    
    if (model_type == "leak") {
      p_y1_given_c <- prob_leak_noisy_and(x_c, item_id, Q, params)
    } else {
      p_y1_given_c <- prob_simple_noisy_and(x_c, item_id, Q, params)
    }
    p_y0_given_c <- 1 - p_y1_given_c
    
    # Joint probabilities
    p_x_y1 <- posterior[c] * p_y1_given_c
    p_x_y0 <- posterior[c] * p_y0_given_c
    
    # Add to MI for y = 1
    if (p_x_y1 > 0 && p_y1 > 0 && posterior[c] > 0) {
      mi <- mi + p_x_y1 * log(p_x_y1 / (posterior[c] * p_y1))
    }
    
    # Add to MI for y = 0
    if (p_x_y0 > 0 && p_y0 > 0 && posterior[c] > 0) {
      mi <- mi + p_x_y0 * log(p_x_y0 / (posterior[c] * p_y0))
    }
  }
  
  return(mi)
}

################################################################################
# ITEM SELECTION WRAPPER
################################################################################

#' Select next item using specified algorithm
#'
#' @param algorithm One of "PWKL", "SHE", "MI"
#' @param available_items Vector of item IDs still available
#' @param responses Responses so far
#' @param item_ids Items administered so far
#' @param Q Q-matrix
#' @param params Model parameters
#' @param model_type "leak" or "simple"
#' @return Selected item ID
select_next_item <- function(algorithm, available_items, responses, item_ids, 
                             Q, params, model_type) {
  K <- nrow(Q)
  
  # Compute current posterior
  posterior <- compute_posterior(responses, item_ids, Q, params, model_type)
  
  # Get MAP estimate
  all_patterns <- generate_all_patterns(K)
  map_idx <- which.max(posterior)
  current_estimate <- all_patterns[map_idx, ]
  
  # Compute index for each available item
  indices <- numeric(length(available_items))
  
  for (i in 1:length(available_items)) {
    item_id <- available_items[i]
    
    if (algorithm == "PWKL") {
      indices[i] <- compute_pwkl(item_id, current_estimate, posterior, Q, params, model_type)
    } else if (algorithm == "SHE") {
      indices[i] <- compute_she(item_id, posterior, Q, params, model_type)
    } else if (algorithm == "MI") {
      indices[i] <- compute_mi(item_id, posterior, Q, params, model_type)
    }
  }
  
  # Select item
  if (algorithm == "SHE") {
    # SHE: minimize expected entropy
    selected_idx <- which.min(indices)
  } else {
    # PWKL and MI: maximize index
    selected_idx <- which.max(indices)
  }
  
  return(available_items[selected_idx])
}
