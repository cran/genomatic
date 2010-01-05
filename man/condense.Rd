\name{condense}
\alias{condense}
\title{ Merges samples with multiple non-identical records }
\description{
  Merges samples with multiple non-identical records, only removes NAs.
  }
\usage{
condense(gmtc)
}
\arguments{
  \item{gmtc}{ A genomatic data.frame.  }
}
\details{
When a sample has been run more than once but for different loci (multiplexes) these multiple records can be combined without losing data, only NAs.  
}
\value{
  Returns a genomatic data.frame.
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
