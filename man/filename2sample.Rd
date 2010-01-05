\name{filename2sample}
\alias{filename2sample}
\title{ Extracts sample names from file names }
\description{
  Takes the field 'File.Name' and extracts the sample name from it.
}
\usage{
filename2sample(file.name)
}
\arguments{
  \item{file.name}{ A vector of File.Names. }
}
\details{
  The file name is expected to follow the format specified in the function \sQuote{plate\_write.}  An example is \sQuote{A10\_PUTR\_068\_01\_A10\_02.fsa} where the sample name is \sQuote{PUTR\_068\_01.}  This function extracts this sample name assuming the character \sQuote{\_} delimits fields within \sQuote{File.Name} and returns a data.frame containing just the sample name.
}
\value{
  A data.frame containing sample names.
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
