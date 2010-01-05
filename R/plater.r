plater <- function(samps){
  ##### #####
  # Function that takes a vector of samples
  # and organizes them in 96 well plates.

  n_plates <- ceiling(length(samps)/96)
  plates <- list()

  for (i in 1:n_plates) # plate counter
    {
    plates[[i]] <- matrix(samps[((i-1)*96)+(1:96)], nrow=8, ncol=12)
    dimnames(plates[[i]]) <- list(c("A","B","C","D","E","F","G","H"), 1:12)
    }

  return(plates)
  }
