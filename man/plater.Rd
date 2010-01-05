\name{plater}
\alias{plater}
\title{ Organizes individual names into 96-well plates }
\description{
  Organizes individual names into 96-well plates
}
\usage{
plater(samps)
}
\arguments{
  \item{samps}{A vector of individual names}
}
\details{
  Takes a vector of individual names and organizes them into a matrix in the format of 96-well plates.
}
\value{
  Returns a list where each element is a 96-well plate of samples.  Each element is a matrix of samples.
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
