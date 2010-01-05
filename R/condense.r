condense <- function(gmtc)
  {
  # Takes a data.frame which may have
  # a single sample spread over several
  # rows and some of these rows may
  # contain redundant or different data.
  # Samples containing no redundant data
  # are condensed into one row.
  # samples containing redundant data
  # are left alone.

  # Make sure we're dealing with numbers.
  for(i in 3:ncol(gmtc))
    {
    gmtc[,i] <- as.numeric(gmtc[,i])
    }

  # Initialize a new data.frame.
  gmtc2 <- data.frame(matrix(nrow=0, ncol=ncol(gmtc)))
  names(gmtc2) <- names(gmtc)

  # Capture sample names.
  samps <- levels(as.factor(gmtc[,2]))

  condenser <- function(temp)
    {
    consensus <- data.frame(matrix(nrow=0, ncol=ncol(temp)))
    names(consensus) <- names(temp)
    cons <- temp[1,]
    temp <- temp[-1,]
    #
    while(nrow(temp) > 0) # Consensus counter.
      {
      cond <- c()
      #
      for (j in 1:nrow(temp)) # Temp. counter.
        {
        test2 <- colSums(abs(is.na(rbind(cons[,-c(1:2)],
                                         temp[j,-c(1:2)]))-1))
        test2[test2 == 1] <- 0
        #
        if (sum(test2) == 0)
          {
          # There is only one value per allele.
          cons <- colSums(rbind(cons[,-c(1:2)],
                                temp[j,-c(1:2)]), na.rm=TRUE)
          cons <- as.data.frame(c(temp[1,1:2], cons))
          cons[cons == 0] <- NA
          cond <- c(cond, j)
          }
        }
#      gmtc2 <- rbind(gmtc2, consensus)
      #
      consensus <- rbind(consensus, cons)
      #
      # Remove condensed records.
      if (is.null(cond) == FALSE) {temp <- temp[-cond,]}
      #
      #if (nrow(temp) == 0) {break}
      if (nrow(temp) == 1)
        {
        consensus <- rbind(consensus, temp)
        temp <- temp[-1,]
        } else
        {
        cons <- temp[1,]
        temp <- temp[-1,]
        }
      } # End while statement.
    return(consensus)
    }

  # Condense.
  for(i in 1:length(samps)) # Sample counter.
    {
    # Isolate all records for one individual.
    temp <- subset(gmtc, gmtc[,2] == samps[i])
    #
    # Test for redundant data and condense.
    if (nrow(temp) == 1) # If there is only one occurrence.
      {
      gmtc2 <- rbind(gmtc2, temp)
      } else
      {
      test <- abs(is.na(temp[,-c(1:2)])-1) # Count non-NAs.
      test <- colSums(test)
      test[test == 1] <- 0
#      test <- colSums(is.na(temp[,-c(1:2)]))
      # NAs equal 'TRUE' equals 1.
      if (sum(test) == 0)
        {
        # There is only one value per allele.
        consensus <- colSums(temp[,-c(1:2)], na.rm=TRUE)
        consensus <- as.data.frame(c(temp[1,1:2], consensus))
        gmtc2 <- rbind(gmtc2, consensus)
        } else
        {
        # We have multiple occurrences of alleles.
#        if (sum(is.na(dist(temp[,-c(1:2)]))) == 0)
#          {
          # No further condensation is possible.
#          gmtc2 <- rbind(gmtc2, temp)
#          } else
#          {
          # Some samples can still be condensed.
          consensus <- condenser(temp)
          gmtc2 <- rbind(gmtc2, consensus)
#          }
        }
      }
    }
  #
  # Manage zeros.
  gmtc2[gmtc2 == 0] <- NA
  #  is.na(gmtc2[gmtc2==0]) <- TRUE
  #
  return(gmtc2)
  }
