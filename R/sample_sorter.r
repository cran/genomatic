sample_sorter <- function (samp_names, locus_l)
  {
  # Sample Sorter
  # Takes a vector of names and a list where each element
  # contains binned alleles.  These list elements are joined
  # into one data.frame using the sample names as an index.

  sorted_samps <- c()

  for (i in 1:length(samp_names)) # i = sample counter
    {
    samp_v <- samp_names[i]

    for (j in 1:length(locus_l)) # j = locus (list element) counter
      {
      samp <- subset(locus_l[[j]], locus_l[[j]][,1] == samp_names[i])

      # Fail if a sample occurs more than once.
      if (length(samp[,1]) > 1)
        {
        cat("error: a sample occurs multiple times\n")
        cat("i equals ", i, ", j equals ", j, "\n")
        cat(as.character(samp[1,1]), "\n")
        }

      if (is.na(samp[1,2])) 
        {
        sample_names <- names(samp)
        samp <- data.frame(matrix(rep(NA, length(samp)), nrow=1, ncol=length(samp)))
        names(samp) <- sample_names
        }
      samp_v <- cbind(samp_v, samp[2:length(samp)])
      }
    sorted_samps <- rbind(sorted_samps, samp_v)
    }
  return(sorted_samps)
  }
