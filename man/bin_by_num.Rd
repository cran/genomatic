\name{bin_by_num}
\alias{bin_by_num}
\docType{data}
\title{ Graphical output of bin quality }
\description{
  Graphical output of bin quality.  Creates plots of bin width as a function number of peaks in the bin.
}
\usage{bin_by_num(locus, loci, ident = 0)}
\arguments{
  \item{locus}{allele characteristics produced \sQuote{allele\_char}}
  \item{loci}{a data.frame of locus names}
  \item{ident}{flag to enable interactive identification of points when set to 1. Default is no. }
}
\details{
  Graphical output of bin quality.  Creates plots of bin width as a function number of peaks in the bin.  A heavy line is plotted at one base pair and thinner lines are plotted at 1.1, 1.2, 1.3, and 1.4 base pairs.  It seems intuitive that a bin should not range more than a base pair, however experience suggests that bins that range more than a single base pair are frequently acceptable.  This could be due to error in the size calling (how close the peak is to a size standard), among capillary error (capillary quality may induce error), or other unforeseen issues.
}
\value{
  Returns a NULL.
}
\references{ 
Knaus, B.J. In prep. Genomatic: an R package for DNA fragment analysis project management.
}
\author{ 
Brian J. Knaus
  \email{bknaus@fs.fed.us}
  \url{http://brianknaus.com}
}
\keyword{manip}
