\name{allele_char}
\alias{allele_char}
\title{ Characterizes bins }
\description{
  Records the mean, standard deviation, minimum, maximum, and count for each allele in a locus.
}
\usage{
allele_char(allele.v, allele.df)
}
\arguments{
  \item{allele.v}{ All the peaks for a locus. }
  \item{allele.df}{an allele file containing bin mixima and minima in
    columns 8 and 9.}
}
\details{
  Characterizes bins based on allele data.  Alleles in allele.v are
  sorted from smallest to biggest and then sorted into bins based on the
  min and max included in allele.df.  A mean is recorded and rounded to
  produce an allele name.  Standard deviation, min, max, count and range
  are also recorded.  Complete allele files are printed to file, one for
  each locus.
}
\value{
returns a data.frame.
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
