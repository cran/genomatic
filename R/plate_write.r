plate_write <- function(plates, prefix)
  {
  ##### ##### ##### ##### #####
  # Function that takes a list, where each element is
  # a 96 well plate, and writes comma delimited files
  # that are named with the prefix and plate number. 

  for (i in 1:length(plates))
    {
    write.table(matrix(0:12, nrow=1), file=paste(prefix, "_", i, ".csv", sep=""), 
      append=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
    write.table(plates[[i]], file=paste(prefix, "_", i, ".csv", sep=""),
      append=TRUE, sep=",", row.names=TRUE, col.names=FALSE)
    }
  return()
  }