dup_replace <- function(all_samps, replace_samps)
  {
  # Function 'dup_replace'
  # Accepts a data.frame of samples
  # which contains multiple instances of some samples
  # and a data.frame containing a single copy of
  # each duplicated sample.  The duplicated samples
  # in the first data.frame are removed and replaced
  # with single instance of the sample from the single
  # copy data.frame.

  # All sorting is performed on the sample names
  # which are assumed to be in the first column.

  # Verify we are sent the proper data structures:
  if (is.data.frame(all_samps) != "TRUE")
    {cat("error: first object is not a data.frame!\n")}

  if (is.data.frame(replace_samps) != "TRUE")
    {cat("error: second object is not a data.frame!\n")}

  # Make sure replace_samps has replacements.
  if (nrow(replace_samps) < 1)
    {cat("gmtc error: replace samps has less than one row.\n")
     break}
  
  # Remove population.
  all_samps <- all_samps[,-1]
  replace_samps <- replace_samps[,-1]
  
  # Recast the sample names as characters.
  all_samps[,1] <- as.character(all_samps[,1])
  replace_samps[,1] <- as.character(replace_samps[,1])

  # First remove the samples in the complete dataset that
  # occur in the replace dataset.

  for (i in 1:nrow(replace_samps))
    {
    all_samps <- all_samps[all_samps[,1]!=replace_samps[i,1],]
#    dups <- grep(replace_samps[i,1], all_samps[,1])
#      if (is.na(dups[1]) == TRUE) next
#    all_samps <- all_samps[-dups,]
    }

  # Add the dups to the end.
  all_samps <- rbind(all_samps, replace_samps)

  all_samps <- cbind(pop_get(all_samps[,1]), all_samps)
  names(all_samps)[1] <- "population"
    
  return(all_samps)
  }
