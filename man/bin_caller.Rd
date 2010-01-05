\name{bin_caller}
\alias{bin_caller}
\docType{data}
\title{ Bins peaks }
\description{
  Uses allele characteristics to call bins.
}
\usage{bin_caller(locus, locus_char, ploid=2)}
\arguments{
  \item{locus}{a data.frame containing all the peaks to be binned}
  \item{locus\_char}{a table output from \sQuote{allele\_char} containing characteristics of the alleles (mean, sd, etc.)}
  \item{ploid}{describes whether allelic data is spread over one column
    (haploid) or two (diploid).}
}
\details{
  Uses allele characteristics to call bins.  If a particular peak does not fall within any of the bins it is flagged with an 'NA.'
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
