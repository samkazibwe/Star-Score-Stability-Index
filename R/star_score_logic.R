#' Calculate Star-Score Stability Index
#' @param env_values Numeric vector of environmental indicators (0-10)
#' @param ik_weights Numeric vector of indigenous multipliers
#' @param econ_values Numeric vector of household productivity (0-10)
calculate_star_score <- function(env_values, ik_weights, econ_values) {
  
  # 1. Apply Indigenous Sovereignty Weighting
  weighted_env <- mean(env_values * ik_weights)
  
  # 2. Calculate Economic Mean
  weighted_econ <- mean(econ_values)
  
  # 3. Geometric Mean (prevents ecological masking)
  # Stability = sqrt(E * P) * 10 (Scaled to 0-100)
  star_score <- sqrt(weighted_env * weighted_econ) * 10
  
  return(round(star_score, 2))
}

# Example Test Run
env <- c(7.5, 4.2, 8.0) # Soil, Water, Bio
weights <- c(1.2, 1.5, 1.1) # IK Multipliers
econ <- c(6.5, 7.0) # Trade, Labor

message("District Stability Score: ", calculate_star_score(env, weights, econ))
