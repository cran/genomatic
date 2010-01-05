\name{dup_checker}
\alias{dup_checker}
\title{ Checks for duplicate samples }
\description{
  Checks a file for duplicates.  Returns a sorted file containing duplicates.
}
\usage{
dup_checker(samples)
}
\arguments{
  \item{samples}{ A data.frame containing your samples. }
}
\details{
  Checks a file for duplicates.  Returns a sorted file containing duplicates.  Duplicates are sorted so they appear next to each other.  This file can then be manually sorted to that there is only one instance of each sample.  This file is subsequently used by the function \sQuote{dup\_replace} to remove duplicates and replace them with a single instance.
}
\value{
  Retruns a data.frame of samples that appear more than once.
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
