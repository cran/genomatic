\name{allele_process}
\alias{allele_process}
\docType{data}
\title{ Processes alleles into bins }
\description{
  Processes alleles into bins for the entire project.  Descriptive plots and text output are created.
}
\usage{allele_process(gmtc, allele.l)}
\arguments{
  \item{gmtc}{a genomatic data.frame.}
  \item{allele.l}{a list where each element contains a genomatic allele file.}
}
\details{
  Processes alleles into bins for characterization.  Alleles are binned
  by the min and max provided in columns 8 and 9 of each allele file.
  Histograms are created for each locus with alternating color to
  indicate bins.  Range by count plots are provided to find bins that
  are unusually wide.
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
