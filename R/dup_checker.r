dup_checker <- function(samples)
  {
  # Function dup_checker.
  # Takes a data.frame of samples and searches for 
  # duplicates (e.g., reruns).  If duplicates are
  # found a dataframe of them is returned.

  # Assumes sample name is in the second column.
  samples <- samples[,-1]

  dup_df <- data.frame(matrix(nrow=0, ncol=ncol(samples)))
  names(dup_df) <- names(samples)
  samps <- levels(samples[,1])
  
  for (i in 1:length(samps)) # sample counter
      {
#      temp <- samples[samples[,1]==samps[i],]
      temp <- subset(samples, samples[,1] == samps[i])

      if (nrow(temp)>1)
        {
        dup_df <- rbind(dup_df, temp)          
        }
      }

  if (nrow(dup_df) == 0){
      cat("\nno duplicates found\n")
      return()}  else {
        dup_df<- cbind(pop_get(dup_df[,1]), dup_df)
        names(dup_df)[1] <- "population"
        cat("\nFound duplicates\n")
        cat("Check returned data.frame!\n")
        return(dup_df)}
  }


