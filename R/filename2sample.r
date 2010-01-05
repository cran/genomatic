filename2sample <- function(file.name)
  {
  # Function filename2sample.r
  # Extracts the sample name from the Genotyper 'File.Name'
  # field.
  # 
  # Assumes that the data is a vector of factors
  # similar to:
  #
  # A10_PUTR_068_01_A10_02.fsa
  #
  # where PUTR_068_01 is the sample name.
  # Note that the characer "_" is used to
  # delimit the text.
      
  samp_name <- function(samp)
    {
    samp <- strsplit(as.character(samp), "_")
    samp <- paste(samp[[1]][2], samp[[1]][3], samp[[1]][4], sep="_")
    return(samp)
    }
  samp_v <- sapply(file.name, samp_name)

  return(samp_v)
  }

