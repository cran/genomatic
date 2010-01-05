bin_score <- function(plate, loci, outfile="_scored_gmtc.csv")
  {
  # Load allele characteristics.
  # Assumes file naming used in the function allele_process.
  # 'plate' should have sample names as the first column
  # and subsequent columns should be allele sizes.

  # Remove population.
  plate <- plate[,-1]

  # Read in allele files.
  locus_char_l <- list()
  #
  for (i in 1:length(loci))
    {
    locus <- paste(loci[i], "alleles.csv", sep="_")
    locus_char_l[[i]] <- read.table(locus, header=TRUE, sep=",")
    }

  # Call the bins.

  locus_l <- list()

  for (i in 1:length(loci))
    {
    locus_l[[i]] <- bin_caller(plate[,c(1, (i*2), (i*2+1))], locus_char_l[[i]], ploid=2)
    }

  # Sort the samples.
  # This next one takes a bit to execute.
  # Note: sample_sorter fails if a sample appears more than once.

  purshia_ssr <- sample_sorter(plate[,1], locus_l)

  # Add a population code to the second column
  # of the data.frame.

  pop <- pop_get(purshia_ssr[,1])
  purshia_ssr <- cbind(pop, purshia_ssr)

  names(purshia_ssr)[2] <- c("sample")

  # Write the data.frame as a comma delimited text file.
  write.table(purshia_ssr, file=outfile, sep=",", row.names=FALSE)
  }
