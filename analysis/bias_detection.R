library(tidyverse)

# Load the Community Ledger
ledger <- read.csv("data/stability_ledger.csv")

# Perform Bias Check
# If Economic productivity / Environmental health > 1.5, flag as Urban Bias
check_policy_bias <- function(data) {
  env_total <- mean(data$VALUE[data$SECTION == "ENVIRONMENTAL"])
  econ_total <- mean(data$VALUE[data$SECTION == "ECONOMIC"])
  
  ratio <- econ_total / env_total
  
  status <- if_else(ratio > 1.5, "URBAN_BIAS_DANGER", "ECOLOGICAL_HARMONY")
  
  return(list(Ratio = ratio, Status = status))
}

print(check_policy_bias(ledger))
