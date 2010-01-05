bin_caller <- function(locus, locus_char, ploid=2)
  {
  # Ploid describes how many columns are in the data.frame
  # 'locus' (ploid + 1). Only defined for haploid and diploid.
  # Remove NAs.
  locus <- subset(locus, locus[,2] != "NA")
  # Remove bins with less than 1 call.
#  locus_char <- subset(locus_char, locus_char[,6] > 1)
  #
  if (ploid == 1)
    {
    allele_a <- rep(NA, nrow(locus))
    #
    for(j in 1:nrow(locus_char))
      {
      allele_a[
        locus[,2] >= locus_char[j,8] &
        locus[,2] <= locus_char[j,9]
               ] <- locus_char[,1][j]
      }
    #
    locus <- cbind(locus, allele_a)
    names(locus)[3] <- paste(names(locus)[2], c("bin"), sep="_")
    #
    return(locus)
    } else {
    #
    if (ploid == 2)
      {
      allele_a <- rep(NA, nrow(locus))
      allele_b <- rep(NA, nrow(locus))
      #
      for(j in 1:nrow(locus_char))
        {
        allele_a[
          locus[,2] >= locus_char[j,8] &
          locus[,2] <= locus_char[j,9]
                 ] <- locus_char[,1][j]
        #
        allele_b[
          locus[,3] >= locus_char[j,8] &
          locus[,3] <= locus_char[j,9]
                 ] <- locus_char$allele_name[j]
        }
      #
      locus <- cbind(locus, allele_a, allele_b)
      names(locus)[4:5] <- paste(names(locus)[2:3], c("bin"), sep="_")
      #
      return(locus)
      } else {print('bin_caller error: undefined ploidy')}
    }
  }
