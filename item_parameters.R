################################################################################
# Item Parameters Generation for for NoisyAND-CAT Simulation Study
# Generates parameters for both models:
# - Leak for NoisyAND (RRUM) : q_{l,0} and q_{l,k}
# - Simple for NoisyAND (DINA): s_l and g_l
# RELATION: s_l = 1 - q_{l,0} to ensure fair comparison
################################################################################

set.seed(2026)

# Load Q-matrix
if (!file.exists("Q_matrix.RData")) {
  stop("Q_matrix.RData not found. Run Q_matrices.R first.")
}
load("Q_matrix.RData")

K <- nrow(Q)
J <- ncol(Q)

################################################################################
# PARAMETER RANGES
################################################################################

# Leak NoisyAND
base_prob_values <- c(0.75, 0.80, 0.85, 0.90)
base_prob_probs  <- c(0.20, 0.30, 0.30, 0.20)  # Favor 0.80-0.85

penalty_values <- c(0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40)
penalty_probs  <- c(0.05, 0.15, 0.25, 0.25, 0.20, 0.07, 0.03)  # Favor 0.15-0.30

# Simple NoisyAND
# NOTE: s_l will be computed as 1 - q_{l,0} to ensure fair comparison
# Only g_l is sampled independently
guess_values <- c(0.10, 0.15, 0.20, 0.25)
guess_probs  <- c(0.30, 0.40, 0.20, 0.10)  # Favor lower values

################################################################################
# FUNCTION: Generate Leak NoisyAND Parameters
################################################################################

generate_leak_params <- function(Q) {
  J <- ncol(Q)
  K <- nrow(Q)
  
  params <- list()
  
  # Base probabilities q_{l,0} for all items
  params$q0 <- sample(base_prob_values, size = J, replace = TRUE, 
                      prob = base_prob_probs)
  
  # Penalty parameters q_{l,k} - only for attributes measured by each item
  params$penalties <- matrix(NA, nrow = K, ncol = J)
  
  for (l in 1:J) {
    for (k in 1:K) {
      if (Q[k, l] == 1) {
        params$penalties[k, l] <- sample(penalty_values, size = 1, 
                                         prob = penalty_probs)
      }
    }
  }
  
  return(params)
}

################################################################################
# FUNCTION: Generate Simple NoisyAND Parameters
################################################################################

generate_simple_params <- function(q0_values) {
  J <- length(q0_values)
  params <- list()
  
  # CRITICAL: Set slip = 1 - q_{l,0} for direct comparability
  params$slip <- 1 - q0_values
  
  # Guess is sampled independently
  params$guess <- sample(guess_values, size = J, replace = TRUE, 
                         prob = guess_probs)
  
  return(params)
}

################################################################################
# GENERATE PARAMETERS FOR BOTH MODELS
################################################################################

params_leak   <- generate_leak_params(Q)
params_simple <- generate_simple_params(params_leak$q0)  # Pass q0 values


################################################################################
# EXPORT TO CSV FILES
################################################################################

# Leak noisy-and parameters
leak_df <- data.frame(item_id = 1:J, q0 = params_leak$q0)

# Add penalty columns (only non-NA values)
for (k in 1:K) {
  col_name <- paste0("q", k)
  leak_df[[col_name]] <- params_leak$penalties[k, ]
}

# Add Q-matrix info for reference
for (k in 1:K) {
  col_name <- paste0("Q_A", k)
  leak_df[[col_name]] <- Q[k, ]
}

# Simple NoisyAND parameters
simple_df <- data.frame(
  item_id = 1:J,
  slip = params_simple$slip,
  guess = params_simple$guess,
  q0_reference = params_leak$q0  # Include for verification
)

# Save CSV files
write.csv(leak_df, "params_leak.csv", row.names = FALSE)
write.csv(simple_df, "params_simple.csv", row.names = FALSE)

# Save in R format
save(params_leak, params_simple, file = "item_parameters.RData")

