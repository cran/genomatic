genotyper2genomatic <- function(gmtc, plate.l)
  {
  ##### ##### ##### ##### #####
  # Add new plates (plate.l)
  # to the master plate(gmtc).
  #
  # plate.l is a list where each element is
  # a plate of samples in genotyper format.  

  # If we're handed a data.frame instead of a list let's
  # recast and handle it as a list.
  if (class(plate.l) == "data.frame")
    {
    plate.l <- list(plate.l)
    }

  # Take a file with one sample and one category per row
  # and return a file with one sample per row with several
  # categories.
  for(i in 1:length(plate.l))
    {
    plate.l[[i]] <- cat_sorter(plate.l[[i]])
    }

  # Reformat each plate to match the master plate columns.
  #
  gmtc.l <- list()  
  for(i in 1:length(plate.l))
    {
    gmtc.l[[i]] <- data.frame(matrix(nrow=nrow(plate.l[[i]]),
                                       ncol=ncol(gmtc)))
    gmtc.l[[i]][,2] <- plate.l[[i]][,1]
    names(gmtc.l[[i]]) <- names(gmtc)
    #
    index <- match(names(plate.l[[i]]), names(gmtc.l[[i]]))
    index <- na.omit(index)
    gmtc.l[[i]][,index] <- plate.l[[i]][,-as.integer(attr(index, "na.action"))]
    #
    fail <- names(plate.l[[i]])[as.integer(attr(index, "na.action"))]
    fail <- fail[-1]
    if(length(fail)>0)
      {
      cat("genotyper2genomatic error: unmatched loci:\n ", fail, " \n\n")
      }
    }

  # Bind all the plates together.
  gmtc2 <- gmtc.l[[1]]
  if(length(gmtc.l)>1)
    {
    for(i in 2:length(gmtc.l))
      {
      gmtc2 <- rbind(gmtc2, gmtc.l[[i]])
      }
    }

  ##### ##### ##### ##### #####
  # Extract sample name from File.Name field.
  # The File.Name should be similar to: A10_PUTR_068_01_A10_02.fsa
  # Where the individual is identified as PUTR_068_01, 
  # and the population is identified as PUTR_068.
  sample <- filename2sample(gmtc2[,2])
  pop <- pop_get(sample)
  #
  gmtc2$sample <- sample
  gmtc2$population <- pop

  # Bind the new to the master.
  gmtc <- rbind(gmtc, gmtc2)

  #
  gmtc <- cbind(gmtc$sample, gmtc)
  gmtc <- order_sample(gmtc)
  #
  gmtc <- gmtc[,-1]
  attr(gmtc, which="row.names") <- 1:nrow(gmtc)
  #
  return(gmtc)
  }
