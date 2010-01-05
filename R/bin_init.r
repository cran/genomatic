bin_init <- function(samples, gap=0.3)
  {
  # 'samples' is a genomatic ('_gmtc.csv') file
  # where the first column contains populations,
  # the second contains individuals within populations,
  # and all subsequent columns contain allelic data
  # where each diploid locus occupies two columns (a and b). 

  # 'gap' delimits bins and is the minimum gap
  # between bins.
    
  # Grab locus names.
  loci <- names(samples)[seq(3,ncol(samples), by=2)]
  loci <- substr(loci, 1, nchar(loci)-1)

  # Flag bin boundaries.
  locus_l <- list()
  #
  for (i in seq(3, ncol(samples), by=2))
    {
    alleles <- c(samples[i], samples[i+1])
    alleles <- as.vector(unlist(alleles))
    alleles <- sort(na.omit(alleles))
    #
    bin.m <- matrix(c(alleles[1],0), nrow=1, ncol=2)
    #
    k <- 1 # bin counter.
    for(j in 2:length(alleles))
      {
      if(alleles[j]-alleles[j-1] >= gap)
        {
        bin.m[k,2] <- alleles[j-1]
        k <- k+1
        bin.m <- rbind(bin.m, c(alleles[j],0))
        }
      }
    bin.m[k,2] <- alleles[j]

    na.v <- rep('NA', length.out=nrow(bin.m))
    #
    bin.df <- as.data.frame(cbind(round(bin.m[,1]), na.v, na.v, na.v,
                                  na.v, na.v, na.v, bin.m))
    names(bin.df) <- c('allele_name', 'allele_mu', 'allele_sd',
                       'allele_min', 'allele_max', 'allele_n',
                       'allele_rge', 'bin_min', 'bin_max')
    #
    locus_l[[(i-1)/2]] <- bin.df
    }
    #

  ##### ##### ##### ##### #####
  # Write alleles to file, one file per locus.
  
  for (i in 1:length(loci))
    {
    write.table(locus_l[[i]], 
    file = paste(loci[i], "_alleles.csv", sep=""),
    sep=",", row.names = FALSE)
    }
  }
