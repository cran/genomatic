allele_char <- function(allele.v, allele.df)
  {
  # allele.v is a vector of alleles for one locus.
  #
  # allele.df is a data.frame with bin min and max
  # in columns 8 and 9.
  #

  allele.v <- as.vector(na.omit(allele.v))
  
  for (i in 1:nrow(allele.df))
    {
    allele <- allele.v[allele.v >= allele.df[i,8] & allele.v <= allele.df[i,9]]
    allele <- as.vector(na.omit(allele))
    #
    if (length(allele) < 1)
      {
      allele.df[i,2] <- 0
      allele.df[i,3] <- 0
      allele.df[i,4] <- allele.df[i,8]
      allele.df[i,5] <- allele.df[i,9]
      allele.df[i,6] <- 0
      allele.df[i,7] <- 0

      } else {
      allele.df[i,2] <- mean(allele)
      allele.df[i,3] <- sd(allele)
      allele.df[i,4] <- min(allele)
      allele.df[i,5] <- max(allele)
      allele.df[i,6] <- length(allele)
      allele.df[i,7] <- max(allele) - min(allele)
      }
    #
    }
  #
  return(allele.df)
  }
