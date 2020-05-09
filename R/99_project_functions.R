# Get lower matrix triangle -------------------------------------------------

get_lower_tri <- function(cormat) {
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Deceased_model ------------------------------------------------------------

deceased_model <- function(df) {
  #lm(height ~ weight, data = df)
  lm(deceased_time_age ~ confirmed_time_age, data = df)
}