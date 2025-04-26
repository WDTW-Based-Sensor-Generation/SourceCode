
# Build empirical CDF functions
empirical_cdf <- function(data) {
  sorted_data <- sort(data)
  probs <- seq(0, 1, length.out = length(sorted_data))
  return(approxfun(probs, sorted_data, rule = 2)) # rule=2 means extrapolate using endpoints
}

# Assume you have a real dataset called 'real_data' with columns:
# lat, long, alt, vx, vy, vz

empirical_cdf <- function(data) {
  sorted_data <- sort(data)
  probs <- seq(0, 1, length.out = length(sorted_data))
  return(approxfun(probs, sorted_data, rule = 2)) # rule=2 means extrapolate using endpoints
}

# Create inverse CDFs from real_data
inv_cdf_lat <- empirical_cdf(real_data$lat)
inv_cdf_long <- empirical_cdf(real_data$long)
inv_cdf_alt <- empirical_cdf(real_data$alt)
inv_cdf_vx <- empirical_cdf(real_data$vx)
inv_cdf_vy <- empirical_cdf(real_data$vy)
inv_cdf_vz <- empirical_cdf(real_data$vz)

# --- Probability-Based Trajectory Generation Algorithm ---

simulate_trajectory <- function(trajectory_length,inv_cdf_lat, inv_cdf_long, inv_cdf_alt, inv_cdf_vx, inv_cdf_vy, inv_cdf_vz, V_min, V_max, delta_t = 1) {
  
  # Initialize storage lists
  lat <- numeric()
  long <- numeric()
  alt <- numeric()
  vx <- numeric()
  vy <- numeric()
  vz <- numeric()
  
  # Initial position
  lat[1] <- inv_cdf_lat(runif(1))
  long[1] <- inv_cdf_long(runif(1))
  alt[1] <- inv_cdf_alt(runif(1))
  
  total_distance <- 0  # Initialize total traveled distance
  t <- 1
  
  while (total_distance < trajectory_length) {
    # Generate velocity components
    vx_tmp <- inv_cdf_vx(runif(1))
    vy_tmp <- inv_cdf_vy(runif(1))
    vz_tmp <- inv_cdf_vz(runif(1))
    
    # Check speed
    V <- sqrt(vx_tmp^2 + vy_tmp^2 + vz_tmp^2)
    
    if (!(V_min <= V && V <= V_max)) {
      # Rescale velocity
      V_star <- runif(1, V_min, V_max)
      scaling_factor <- V_star / V
      vx_tmp <- vx_tmp * scaling_factor
      vy_tmp <- vy_tmp * scaling_factor
      vz_tmp <- vz_tmp * scaling_factor
      V <- V_star
    }
    
    # Save velocity
    vx[t] <- vx_tmp
    vy[t] <- vy_tmp
    vz[t] <- vz_tmp
    
    # Update position
    new_lat <- lat[t] + (vy[t] * delta_t) / 111000
    new_long <- long[t] + (vx[t] * delta_t) / (111000 * cos(pi * lat[t] / 180))
    new_alt <- alt[t] + vz[t] * delta_t
  
    # Save new position
    lat[t+1] <- new_lat
    long[t+1] <- new_long
    alt[t+1] <- new_alt
    
    t <- t + 1
  }
  
  # Final velocity
  vx[t] <- vx[t-1]
  vy[t] <- vy[t-1]
  vz[t] <- vz[t-1]
  
  # Return the trajectory as a data frame
  return(data.frame(lat = lat,
                    long = long,
                    alt = alt,
                    vx = vx,
                    vy = vy,
                    vz = vz))
}
