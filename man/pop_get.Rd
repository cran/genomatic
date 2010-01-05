\name{ pop_get }
\alias{ pop_get }
\title{ Get populations from sample names }
\description{
  Extracts the population name from a sample's name
}
\usage{
pop_get(data)
}
\arguments{
  \item{data}{ A data.frame of sample names where the sample names are in the first column.  All subsequent columns are ignored. }
}
\details{
  This function takes a data.frame of sample names which follows the format specified in the function \sQuote{filename2sample}.  For example, the sample name \sQuote{PUTR\_068\_01} is from the population \sQuote{PUTR\_068.}  A data.frame consisting of one column which contains the population names is returned.
}
\value{
  A data.frame consisting of one column which contains the population names.
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
