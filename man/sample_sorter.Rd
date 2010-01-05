\name{sample_sorter}
\alias{sample_sorter}
\docType{data}
\title{ Sorts data from separate lists into a data.frame }
\description{
  Sorts data from separate lists into a data.frame
}
\usage{sample_sorter(samp_names, locus_l)}

\arguments{
  \item{samp_names}{ a vector of sample names to sort all samples with }
  \item{locus_l}{ a list where each element contains data for a different locus }
}
\details{
  Takes a vector of sample names and a list where each element in the list is a data.frame containinig a different locus and organizes this into a single data.frame.
}
\value{
  Returns a data.frame of sorted data.
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
