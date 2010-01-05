allele_process <- function(gmtc, allele.l)
  {
  # gmtc is a data.frame containing samples.
  #
  # allele.l is a list which contains bin
  # min and max in columns 8 and 9.
  #
  # Requires functions: bin_by_num.

  ##### ##### ##### ##### #####
  # Get loci names.
  loci <- names(gmtc)[seq(3,ncol(gmtc), by=2)]
  loci <- substr(loci, 1, nchar(loci)-1)

  ##### ##### ##### ##### #####
  # Characterize bins.

  for (i in 1:length(loci))
    {
    allele.v <- c(gmtc[,(i*2)+1], gmtc[,(i*2)+2])
    allele.l[[i]] <- allele_char(allele.v, allele.l[[i]])
    #
    write.table(allele.l[[i]], paste(loci[i], '_alleles.csv', sep=''),
                sep=',', row.names=FALSE)
    }

  ##### ##### ##### ##### #####
  # Allele histograms.
  
  pdf('allele_histograms.pdf', width=7.5, height=10)

  par(mar=c(3,3,1,1), mfrow=c(4,1), mgp=c(1.5,0.5,0))
#  palette(c('blue', 'red', 'forestgreen'))

  for(i in seq(3,ncol(gmtc), by=2))
    {
    allele_hist(gmtc[,i:(i+1)], allele.l[[(i-1)/2]])
    }

  dev.off()

#  for(i in seq(3,ncol(gmtc), by=2))
#    {
#    alleles <- c(gmtc[,i], gmtc[,i+1])
#    a.max <- ceiling(max(alleles, na.rm=T))
#    a.min <- floor(min(alleles, na.rm=T))
#    bincol <- seq(a.min, a.max, by=0.1)
#    #
#    bincol <- bin_caller(as.data.frame(cbind(bincol, bincol)),
#                         allele.l[[(i-1)/2]], ploid=1)
#    #
#    hist(alleles, breaks=bincol[,1],
#         col=bincol[,3], border=bincol[,3], axes=F, main='',
#         xlab='Mobility-Based Size (bp)')
#    axis(2)
#    Axis(at=seq(a.min,a.max, by=1),
#         side=1)
#    legend('topright', loci[(i-1)/2])
#    }



#  palette('default')

  ##### ##### ##### ##### #####
  # Plot locus diagnostics.

  pdf(file= "locus_characterizations.pdf")

  for (i in 1:length(loci))
    {
    bin_by_num(allele.l[[i]], loci[i], 0)
    }

  dev.off()
  }
