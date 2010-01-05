pop2indiv <- function(sites)
  {
  ##### #####
  # Function to expand a vector of site names
  # to site names and individual number
  # for n individuals per site.

  sites[,1] <- as.character(sites[,1])
  samp <- c()
  for (i in 1:nrow(sites))
    {
    samp <- c(samp, paste(sites[i,1], 1:sites[i, 2], sep="_"))
    }
  return(samp)
  }