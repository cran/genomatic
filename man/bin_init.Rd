\name{bin_init}
\alias{bin_init}
\docType{data}
\title{ Initializes allele files }
\description{
  Initializes allele files
}
\usage{bin_init(samples, gap=0.3)}
\arguments{
  \item{samples}{ a genomatic file where column one contains
    populations, column 2 contains individuals and subsequent columns
    contain allelic data, two columns per locus }
  \item{gap}{ defines the gap size required to start a new bin }
}
\details{
  Initializes allele files.  Writes a comma delimited allele file for
  each locus. 
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
