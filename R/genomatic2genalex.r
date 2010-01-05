genomatic2genalex <- function(data, outfile)
  {
  ##### ##### ##### ##### #####
  # Format genomatic data for GenAlEx
  # Assumes the sample name is in the first column
  # and the population name is in the second column.
  # Assumes that the file is sorted by population
  # and individual.

  # Calcualte each population's size.

  popsize <- c()

  for (i in 1:length(levels(data[,2])))
    {
    popsize <- c(popsize, length(subset(data[,1], 
                 data[,2] == levels(data[,2])[i])))
    }

  # Create headers
  head1 <- c((length(data[1,])-2)/2, length(data[,1]),
             length(levels(data[,2])),
             popsize, 1, length(data[,2]))
  head2 <- c(outfile, "", "", levels(data[,2]))

  head1 <- matrix(head1, nrow=1)
  head2 <- matrix(head2, nrow=1)

  # Replace 'NA' with '0'
  for (i in 3:length(data[1,]))
    {
    data[is.na(data[,i]),i] <- 0
    }

  outfile <- paste(outfile, "_genalex.csv", sep="")

  write.table(head1, file=outfile, append=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
  write.table(head2, file=outfile, append=TRUE, sep=",", row.names=FALSE, col.names=FALSE)
  write.table(matrix(names(data), nrow=1), file=outfile, append=TRUE, sep=",", row.names=FALSE, col.names=FALSE)
  write.table(data, file=outfile, append=TRUE, sep=",", row.names=FALSE, col.names=FALSE)
  }
