

abi_sub <- function(plate, fset, outfile)
  {
  ##### ##### ##### ##### #####
  # abi_sub.r
  # Function that take a 96-well plate and reformats it
  # into a submission form.
  # Color sets 4 and 5 are supported.

  # 'plate' is a 96-well plate map such what is created by
  # the function 'plater.'
  # 'fset' describes the filterset.
  # 'outfile' is the prefix for the outfile.

#    library(xlsReadWrite)

  n <- length(na.omit(unlist(plate)))
  well <- paste(rep(rep(c("A", "B", "C", "D", "E", "F", "G", "H"), each = 5), 12), rep(1:12, each = 40), sep="")
  well <- well[1:(5*n)]
  color.number <- rep(1:5, n)
  stnd.dye <- rep(fset[,1], n)
  dye.set <-  rep(as.character(fset[,2]), n)
  color.info <- rep(as.character(fset[,3]), n)

  if (is.na(fset[1,4]) == FALSE)
    {loci <- rep(as.character(fset[,4]), n)} else 
    {loci <- c()}

  # Convert plate matrix to a vector of sample names.
  plate_v <- c()
  for (i in 1:length(plate[1,])) 
      {plate_v <- c(plate_v, as.character(plate[,i]))}
  plate_v <- plate_v[1:n]

  # Create sample names.
  sample.names <- paste(well, rep(plate_v, each=5), sep="_")

  # Create color comment
  color.cmt <- paste(sample.names, color.info, loci, sep="_")


  ##### ##### ##### ##### #####
  # Assemble info into data.frames.
  
  sub_df <- data.frame(well, sample.names, color.number, stnd.dye,
                       dye.set, color.info, color.cmt)
  names(sub_df) <- c("Well", "Sample Names", "Color Number", "Standard Dye",
                     "Dye Set", "Color Info", "Color Comment")

  # Create headers.
  header1 <- c(1)
  header2 <- c("full_plate_96 GS 96-Well")
  header3 <- matrix(names(sub_df), nrow=1)

  # Create outfile name.
  outfile <- paste(outfile, fset$Dye.Set[1], "abi_sub.csv", sep="_")

  # Output to comma delimited file.
  write.table(header1, file=outfile, row.names=FALSE, col.names=FALSE)
  write.table(header2, file=outfile, append=TRUE, sep=",", row.names=FALSE, col.names=FALSE)
  write.table(header3, file=outfile, append=TRUE, sep=",", row.names=FALSE, col.names=FALSE)
  write.table(sub_df, file=outfile, append=TRUE, sep=",", row.names=FALSE, col.names=FALSE)

  return()
  }
