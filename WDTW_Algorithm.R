#WDTW algorithm

if (!requireNamespace("dtw", quietly = TRUE)) install.packages("dtw")
library(dtw)

# Step 1: WDTW Distance Matrix Computation
wdtw_distance_alignment <- function(x, y, alpha1) {
  alignment <- dtw(x, y, keep = TRUE)
  index_p <- alignment$index1
  index_q <- alignment$index2
  
  L_counts <- table(index_p)
  
  total_cost <- 0
  for (i in seq_along(index_p)) {
    p_i <- index_p[i]
    q_j <- index_q[i]
    L_pi <- as.numeric(L_counts[as.character(p_i)])
    weight <- exp(-alpha1 * L_pi)
    total_cost <- total_cost + weight * (x[p_i] - y[q_j])^2
  }
  sqrt(total_cost)
}

compute_wdtw_distance_matrix_alignment <- function(trajectories, alpha1) {
  n <- length(trajectories)
  distance_matrix <- matrix(0, nrow = n, ncol = n)
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      d <- wdtw_distance_alignment(trajectories[[i]], trajectories[[j]], alpha1)
      distance_matrix[i,j] <- d
      distance_matrix[j,i] <- d
    }
  }
  
  return(distance_matrix)
}

# Step 2: Intermediate Reference Trajectory Generation
find_reference_trajectory <- function(distance_matrix, trajectories) {
  n <- nrow(distance_matrix)
  
  total_distances <- apply(distance_matrix, 1, sum)
  min_value <- min(total_distances)
  candidate_indices <- which(total_distances == min_value)
  
  if (length(candidate_indices) == 1) {
    reference_index <- candidate_indices
  } else {
    lengths <- sapply(trajectories[candidate_indices], length)
    reference_index <- candidate_indices[which.min(lengths)]
  }
  
  return(reference_index)
}

# Step 3: Alignment of Trajectories to Intermediate Reference
align_trajectories_to_reference <- function(trajectories, reference_trajectory) {
  n <- length(trajectories)
  aligned_trajectories <- vector("list", n)
  
  for (i in 1:n) {
    current_trajectory <- trajectories[[i]]
    alignment <- dtw(current_trajectory, reference_trajectory, keep = TRUE)
    index_p <- alignment$index1
    index_q <- alignment$index2
    
    aligned_length <- length(reference_trajectory)
    T_prime_i <- numeric(aligned_length)
    
    for (v in 1:aligned_length) {
      matched_indices <- which(index_q == v)
      
      if (length(matched_indices) > 0) {
        matched_points <- current_trajectory[index_p[matched_indices]]
        T_prime_i[v] <- mean(matched_points)
      } else {
        T_prime_i[v] <- NA
      }
    }
    aligned_trajectories[[i]] <- T_prime_i
  }
  return(aligned_trajectories)
}

# Step 4: Weighted Average of the Aligned Trajectories
compute_weighted_average_trajectory <- function(aligned_trajectories, weights) {
  n <- length(aligned_trajectories)
  trajectory_length <- length(aligned_trajectories[[1]])
  
  updated_reference <- numeric(trajectory_length)
  
  for (t in 1:trajectory_length) {
    numerator <- 0
    denominator <- 0
    
    for (i in 1:n) {
      w_it <- weights[[i]][t]
      T_prime_it <- aligned_trajectories[[i]][t]
      
      if (!is.na(T_prime_it)) {
        numerator <- numerator + w_it * T_prime_it
        denominator <- denominator + w_it
      }
    }
    if (denominator > 0) {
      updated_reference[t] <- numerator / denominator
    } else {
      updated_reference[t] <- NA
    }
  }
  return(updated_reference)
}
