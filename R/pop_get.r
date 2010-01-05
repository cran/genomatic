pop_get <- function(data)
  {
  # Function pop_get.
  # Extracts the population name from
  # a vector of sample names.
  #
  # Input should look like:
  #
  # PUTR_068_01
  # 
  # Where 'PUTR_068' is the population name.
  # Note that the character '_' is used to delimit
  # the text.
  #
  pop_name <- function(samp)
    {
    samp <- strsplit(as.character(samp), "_")
    pop  <- paste(samp[[1]][1], samp[[1]][2], sep="_")
    return(pop)
    }
  #
  pop_v <- sapply(data, pop_name)
  pop_v <- as.vector(pop_v)
  #
  return(pop_v)
  }
