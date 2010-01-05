genomatic2ntsys <- function(samples, outfile)
  {
  otu <- as.character(samples[,1])
  samples <- samples[,3:ncol(samples)]
  loci <- names(samples)

  ntm <- matrix(nrow= length(samples[1,]), ncol= length(samples[,1]), dimnames=list(loci, NULL))

  for (i in 1:length(samples[,1]))
    {
    ntm[,i] <- as.numeric(samples[i,])
    }

  otu <- data.frame(rbind(as.character(otu)))
  otu <- cbind("", otu)

  hdr <- data.frame(1, length(ntm[,1]), length(ntm[1,]), 0)

  outfile <- paste(outfile, "_ntsys.csv", sep="")

  write.table(hdr, file=outfile, sep=",", row.names=FALSE, col.names=FALSE)
  write.table(otu, file=outfile, append=TRUE, quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)
  write.table(ntm, file=outfile, append=TRUE, quote=FALSE, sep=",", row.names=TRUE, col.names=FALSE)
  }