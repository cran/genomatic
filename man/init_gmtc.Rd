\name{init_gmtc}
\alias{init_gmtc}
\title{ Initializes a genomatic data.frame }
\description{
  Initializes a genomatic data.frame.
}
\usage{
init_gmtc(loci)
}
\arguments{
  \item{loci}{ A data.frame of loci used in the project }
}
\details{
  This function initializes a genomatic data.frame.  This format is used throughout the genomatic process.  A genomatic data.frame is specific to diploid organisms.  The first coloumn is a population code.  The second column is an individual code.  All following columns are diploid loci in pairs.  For example, columns three and four would contain locus one alleles a and b.
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
