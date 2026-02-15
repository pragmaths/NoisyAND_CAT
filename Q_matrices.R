################################################################################
# Q-Matrix Generation for Noisy-AND CAT Simulation Study
# Generates Q-matrices with different complexity levels
# K = 5 skills, J = 60 items
################################################################################

set.seed(2026)  # For reproducibility

K <- 5  # Number of skills
J <- 60 # Number of items

################################################################################
# CONFIGURATION: Choose Q-matrix structure
################################################################################

# Set to "simple" or "complex"
STRUCTURE <- "simple"  # Change this to switch between structures

################################################################################
# FUNCTION: Generate Q-matrix
################################################################################

#' Generate Q-matrix with specified structure
#'
#' @param K Number of skills
#' @param J Number of items (must be 60)
#' @param structure Either "simple" or "complex"
#' @return Q-matrix (K x J)
generate_qmatrix <- function(K, J, structure) {
  
  Q <- matrix(0, nrow = K, ncol = J)
  current_item <- 1
  
  if (structure == "simple") {
    # SIMPLE STRUCTURE (mean 2.0 skills per item)
    # - 15 items measure 1 skill (3 per skill)
    # - 30 items measure 2 skills
    # - 15 items measure 3 skills
    
    # 1-skill items
    for (k in 1:K) {
      for (rep in 1:3) {
        Q[k, current_item] <- 1
        current_item <- current_item + 1
      }
    }
    
    # 2-skill items
    pairs <- combn(K, 2)
    num_pairs <- ncol(pairs)
    reps_per_pair <- ceiling(30 / num_pairs)
    pair_indices <- rep(1:num_pairs, times = reps_per_pair)[1:30]
    
    for (idx in pair_indices) {
      attrs <- pairs[, idx]
      Q[attrs, current_item] <- 1
      current_item <- current_item + 1
    }
    
    # 3-skill items
    triplets <- combn(K, 3)
    num_triplets <- ncol(triplets)
    triplet_indices <- sample(1:num_triplets, size = 15, replace = TRUE)
    
    for (idx in triplet_indices) {
      attrs <- triplets[, idx]
      Q[attrs, current_item] <- 1
      current_item <- current_item + 1
    }
    
  } else {
    # COMPLEX STRUCTURE (mean 2.5 skills per item)
    # - 10 items measure 1 skill (2 per skill)
    # - 20 items measure 2 skills
    # - 20 items measure 3 skills
    # - 10 items measure 4 skills
    
    # 1-skill items
    for (k in 1:K) {
      for (rep in 1:2) {
        Q[k, current_item] <- 1
        current_item <- current_item + 1
      }
    }
    
    # 2-skill items
    pairs <- combn(K, 2)
    num_pairs <- ncol(pairs)
    reps_per_pair <- ceiling(20 / num_pairs)
    pair_indices <- rep(1:num_pairs, times = reps_per_pair)[1:20]
    
    for (idx in pair_indices) {
      attrs <- pairs[, idx]
      Q[attrs, current_item] <- 1
      current_item <- current_item + 1
    }
    
    # 3-skill items
    triplets <- combn(K, 3)
    num_triplets <- ncol(triplets)
    triplet_indices <- sample(1:num_triplets, size = 20, replace = TRUE)
    
    for (idx in triplet_indices) {
      attrs <- triplets[, idx]
      Q[attrs, current_item] <- 1
      current_item <- current_item + 1
    }
    
    # 4-skill items
    quadruplets <- combn(K, 4)
    num_quads <- ncol(quadruplets)
    quad_indices <- sample(1:num_quads, size = 10, replace = TRUE)
    
    for (idx in quad_indices) {
      attrs <- quadruplets[, idx]
      Q[attrs, current_item] <- 1
      current_item <- current_item + 1
    }
    
  }
  
  return(Q)
}

################################################################################
# GENERATE Q-MATRIX
################################################################################

Q <- generate_qmatrix(K, J, STRUCTURE)


################################################################################
# SAVE DATA
################################################################################

# Convert to data frame
Q_df <- as.data.frame(t(Q))  # Transpose: items as rows, skills as columns
colnames(Q_df) <- paste0("X", 1:K)
Q_df$item_id <- 1:J
Q_df <- Q_df[, c("item_id", paste0("X", 1:K))]

write.csv(Q_df, "Q_matrix.csv", row.names = FALSE)
save(Q, file = "Q_matrix.RData")
