################################################################################
# Q-Matrix Generation for NoisyAND-CAT Simulation Study
# Generates Simple structure Q-matrix (Q1)
# K = 5 attributes, J = 60 items
################################################################################

set.seed(2026)  # For reproducibility

K <- 5  # Number of attributes
J <- 60 # Number of items

################################################################################
# Q1: SIMPLE STRUCTURE
# - 15 items measure 1 attribute (3 per attribute)
# - 30 items measure 2 attributes
# - 15 items measure 3 attributes
################################################################################

 Q <- matrix(0, nrow = K, ncol = J)
 current_item <- 1
 
 # 15 items measuring exactly 1 attribute (3 items per attribute)
 for (k in 1:K) {
   for (rep in 1:3) {
     Q[k, current_item] <- 1
     current_item <- current_item + 1
   }
 }
 
 # 30 items measuring exactly 2 attributes
 # Generate all possible pairs of attributes
 pairs <- combn(K, 2)
 num_pairs <- ncol(pairs)
 
 # Replicate pairs to get 30 items 
 reps_per_pair <- ceiling(30 / num_pairs)
 pair_indices <- rep(1:num_pairs, times = reps_per_pair)[1:30]
 
 for (idx in pair_indices) {
   attrs <- pairs[, idx]
   Q[attrs, current_item] <- 1
   current_item <- current_item + 1
 }
 
 # 15 items measuring exactly 3 attributes
 triplets <- combn(K, 3)
 num_triplets <- ncol(triplets)
 
 # Sample 15 triplets 
 triplet_indices <- sample(1:num_triplets, size = 15, replace = TRUE)
 
 for (idx in triplet_indices) {
   attrs <- triplets[, idx]
   Q[attrs, current_item] <- 1
   current_item <- current_item + 1
 }

################################################################################
# SAVE DATA
################################################################################

# Convert to data frame with row and column names
Q_df <- as.data.frame(t(Q))  # Transpose
colnames(Q_df) <- paste0("A", 1:K)
Q_df$item_id <- 1:J
Q_df <- Q_df[, c("item_id", paste0("A", 1:K))]

# Save
write.csv(Q_df, "Q_matrix.csv", row.names = FALSE)
save(Q, file = "Q_matrix.RData")

