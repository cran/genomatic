genobins <- function(samples)
  {
  # Function to remove allele calls (in base pairs)
  # and leave bin calls.  This prepares a file for
  # analysis.

  bins <- sort(c(seq(1, ncol(samples), by=4), seq(2, ncol(samples), by=4)))
  samples <- samples[,bins]

  return(samples)
  }