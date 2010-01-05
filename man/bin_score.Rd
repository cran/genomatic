\name{bin_score}
\alias{bin_score}
\docType{data}
\title{ Automates bin scoring }
\description{
  Automates the bin calling process
}
\usage{bin_score(plate, loci, outfile="_scored_gmtc.csv")}
\arguments{
  \item{plate}{ a 96-well plate of samples }
  \item{loci}{ a table of allele characteristics }
  \item{outfile}{ a name for an outfile }
}
\details{
  Automates bin calling, extracting population info from the names, sorting the samples and writes a text output file.
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
