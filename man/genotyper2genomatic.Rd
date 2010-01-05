\name{genotyper2genomatic}
\alias{genotyper2genomatic}
\title{ Import genotyper tables }
\description{
  Reformats data from the Genotyper table export format to the genomatic format.
}
\usage{
genotyper2genomatic(gmtc, plate.l)
}
\arguments{
  \item{gmtc}{ A genomatic data.frame. }
  \item{plate.l}{ A list where each element is a plate of samples. }
}
\details{
  This function reformats data from the Genotyper table export format to the genomatic format.  It calls the functions \sQuote{cat\_sorter}, \sQuote{filename2sample}, \sQuote{pop\_get}, and \sQuote{order\_sample} to accomplish this.
}
\value{
  Returns a formatted data.frame.
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
