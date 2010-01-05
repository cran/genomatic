\name{plate_write}
\alias{plate_write}
\title{ Write 96-well maps }
\description{
  A function that takes a list of 96-well plates and writes each to an individual comma delimited file.
}
\usage{
plate_write(plates, prefix)
}
\arguments{
  \item{plates}{ A list where each element is a matrix representation of a 96-well plate with samples.  }
  \item{prefix}{ A prefix for you outfile names. }
}
\details{
  This function takes a list of 96-well plates that has been created by the function 'plater' (or is in the same format) and saves each 96-well plate as a comma delimited file.  Comma delimited files can easily be imported into spreadsheets for further editing.
}
\value{
  Ruturns a NULL.
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
