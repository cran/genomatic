\name{cat_sorter}
\alias{cat_sorter}
\title{ Sorts categories to their sample }
\description{
  This function takes the output from Genotyper export tables and sorts it into elements of a list where each element includes only one category (locus).
}
\usage{
cat_sorter(gdata)
}
\arguments{
  \item{gdata}{ A data.frame which is input from a Genotyper table export.  }
}
\details{
The output from the Genotyper table export will include one category (locus) per row and will therefore include one sample on many rows.  This function reformats this file so that each sample occupies only a single row and has many categores (loci).  
}
\value{
  Returns a data.frame of scored data.
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
