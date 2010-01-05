bin_by_num <- function(locus, loci, ident = 0)
  {
  # Plots bin width as a function of the number of alleles it contains.

  plot(locus[,6], locus[,5]-locus[,4], 
#       xlim=c(0, 400), ylim=c(0, 1.5),
       main=paste(loci, "characteristics", sep=" "), 
       xlab="Count of alleles scored", 
       ylab="Range (in base pairs) of each allele",
#       pch=24, bg="dodgerblue1")
       type='n',
      )

  lines(c(0,max(locus[,6])), c(1,1), col="firebrick3", lwd=2)

  for (i in 1:4)
    {
    lines(c(0,max(locus[,6])), c(1+i/10,1+i/10), col="firebrick3", lwd=1)
    }

  locus_lm <- lm(locus[,5]-locus[,4] ~ locus[,6])
  abline(locus_lm, lwd=2, col="royalblue1")

  text(locus[,6], locus[,5]-locus[,4], locus[,1])

  legend('bottomright', legend=paste(nrow(locus), 'bins'), bty='o')
  
  if (ident == 1)
    {
    identify(locus[,6], locus[,5]-locus[,4], 
      labels = locus[,2], atpen=TRUE)
    }
  }
