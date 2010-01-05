init_gmtc <- function(loci)
  {
  loci <- paste(rep(levels(loci[,1]), each=2), c('a','b'), sep="")
  gmtc <- data.frame(matrix(nrow=0, ncol=length(loci)+2))
  #
  names(gmtc) <- c("population", "sample", loci)
  #
  return(gmtc)
  }
