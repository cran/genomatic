cat_sorter <- function(gdata)
  {
  # Function cat_sorter
  # Takes a file where a sample is repeated over 
  # several rows and combines the data into one row.
  #
  # gdata is expected to be a data.frame where:
  # Column #1 contains sample names.
  # Column #2 contains categories to sort upon.

  if (class(gdata[,2]) != "factor")
    {
    print("cat_sorter_error: column 2 is not a factor")
    break()
    }
  
  samps <- levels(gdata[,1])
  ncat <- length(levels(gdata[,2]))

  if (length(gdata[,1])/ncat > 96)
       {cat("cat_sorter error: too many alleles, ", length(gdata[,1]),
            "/", ncat, " > 96\n\n")
       return()
       }

    # Create a list where each element is a 
    # data.frame containing one category
    cats <- list()
    for (i in 1:ncat)
        {
        cats[[i]] <- subset(gdata, gdata[,2] == levels(gdata[,2])[i])
        #
        names(cats[[i]])[3] <- paste(levels(gdata[,2])[i], "a", sep="")
        names(cats[[i]])[4] <- paste(levels(gdata[,2])[i], "b", sep="")
        #
        # Duplicate homozygotes (if necessary).
        homo <- subset(cats[[i]], is.na(cats[[i]][,4]) == TRUE)
        homo <- subset(homo, is.na(homo[,3]) == FALSE)
        homo[,4] <- homo[,3]
        #
        # Remove allele NAs.
        cats[[i]] <- subset(cats[[i]], is.na(cats[[i]][,4])==FALSE)
        cats[[i]] <- rbind(cats[[i]], homo)
        }

    # Concatenate the list into one dataframe
    # with one sample per row
    #
    scored <- cats[[1]][1,1:4]
    #
    if(length(cats) > 1)
      {
      for (i in 2:length(cats))
        {
        scored <- cbind(scored, cats[[i]][1,3:4])
        }
      }
    scored <- scored[-1,]
    scored <- scored[,-2]

    for(i in 1:length(samps))
      {
      ssamp <- samps[i]
      #
      for (j in 1:length(cats))
        {
        temp <- subset(cats[[j]], cats[[j]][,1]==samps[i])
        #
        if(nrow(temp)==1)
          {ssamp <- c(ssamp, as.vector(temp[,3:4]))} else
        if(nrow(temp)>1)
          {
          cat("cat_sorter error: a sample appears twice.\n")
          print(as.character(temp[,1]))
          cat("\n\n")
          }else
        if(nrow(temp)==0)
          {ssamp <- c(ssamp, NA, NA)}
        }
      scored[i,] <- ssamp
      }
    #
    return(scored)
    }
