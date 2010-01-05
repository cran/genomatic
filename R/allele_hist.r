allele_hist <- function(gmtc.a, allele.df, xlim=NULL)
  {
  # gmtc.a is a locus (two columns) of a genomatic file.
  #
  # allele.df is a data.frame cotaining bin information.

  locus <- names(gmtc.a)[1]
  locus <- substr(locus, 1, nchar(locus)-1)
  #
  allele.df[,8] <- (floor(10*allele.df[,8]))/10
  allele.df[,9] <- (ceiling(10*allele.df[,9]))/10
  #
  # Widen bins of zero width.
  allele.df[allele.df[,9]-allele.df[,8] == 0,8] <- allele.df[allele.df[,9]-allele.df[,8] == 0,8] - 0.1
  #
  alleles <- as.vector(na.omit(unlist(gmtc.a[,1:2])))
  a.max <- ceiling(max(alleles, na.rm=TRUE))
  a.min <- floor(min(alleles, na.rm=TRUE))
  #
  #
  bincol <- seq(a.min, a.max, by=0.1)
  bincol <- cbind(bincol, bincol+0.05)
  #
  ahist <- hist(alleles, breaks=bincol[,1], plot=FALSE)
  bincol <- cbind(bincol, c(ahist$counts, 0))

  bincol <- cbind(bincol, rep(NA, length.out=nrow(bincol)))

  # Bin the bars.
  for (i in 1:nrow(allele.df))
    {
    bincol[,4][bincol[,2] >= allele.df[i,8] & bincol[,2] <= allele.df[i,9]] <- allele.df[i,1]
    }

  # Include left bar..
  for (i in 1:(nrow(bincol)-1))
    {
    if (is.na(bincol[i,4]) & is.na(bincol[(i+1),4])==FALSE)
      {bincol[i,4] <- bincol[(i+1),4]}
    }
  
  bincol <- cbind(bincol, rep(NA, length.out=nrow(bincol)))
  binlev <- levels(as.factor(bincol[,4]))
  if (binlev[1] == "0")
    {binlev <- binlev[-1]}
  
  # Apply alternate colors to bars.
  binc <- rep(c(2,3,4), length.out=length(binlev))

  for (i in 1:length(binlev))
    {
    bincol[bincol[,4]==binlev[i],4] <- binc[i]
    }
  
  bincol[,4][is.na(bincol[,4])] <- 1

  if (is.null(xlim))
    {
    xlim <- range(bincol[,1])
    } else {xlim <- xlim}

  hist(alleles, breaks=bincol[,1], col=bincol[,4],
       border=bincol[,4], axes=FALSE,
       main='', xlab='Mobility-Based Size (bp)', xlim=xlim)
  axis(2)
  Axis(at=seq(a.min,a.max, by=1),
       side=1, cex.axis=0.6, las=2)
  legend('topright', locus)
  }
