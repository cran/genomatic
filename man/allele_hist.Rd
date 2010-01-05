\name{allele_hist}
\alias{allele_hist}
\title{ Creates histograms of alleles }
\description{
  Creates hisograms of alleles with bins in alternating colors and
  non-bins in black.
}
\usage{
allele_hist(gmtc.a, allele.df, xlim=NULL)
}
\arguments{
  \item{gmtc.a}{ Allelic data in the form of two columns from a
    genomatic file. }
  \item{allele.df}{ An allele file containing bin information for the locus}
  \item{xlim}{ A vector containing the minimum and maximum values for the x axis}
}
\details{
 Creates hisograms of alleles with bins in alternating colors and
  non-bins in black.
}
\value{
  Prints histogram to graphical device.
}
\references{ Knaus, B.J. In prep.  Genomatic: an R package for DNA
  fragment analysis project management }
\author{ 
Brian J. Knaus 
  \email{bknaus@fs.fed.us}
  \url{http://brianknaus.com}
}
\keyword{ manip }
