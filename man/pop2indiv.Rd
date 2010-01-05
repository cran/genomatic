\name{pop2indiv}
\alias{pop2indiv}
\title{ Population Name to Individual Name Conversion }
\description{
  Takes population names and the respective sample size per population (n) and expands the population names to sample names of population\_name\_1, . . ., population\_name\_n.
}
\usage{
pop2indiv(sites)
}
\arguments{
  \item{sites}{ data.grame containing population names and sample sizes. }
}
\details{
  The object \sQuote{samps} should be a vector of factors containing the population names. The object \sQuote{n} should be a vector of integers containing population sample sizes in the same population order as the object \sQuote{samps.}
}
\value{
  returns a vector of character strings where each element is a population name and sample number (e.g., population\_name\_1, . . ., population\_name\_n).
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
