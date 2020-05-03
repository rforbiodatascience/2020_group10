# Get lower tri ------------------------------------------------------------

# Gets lower triangle of correlation matrix
get_lower_tri <- function(cormat) {
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}