\name{abi_sub}
\alias{abi_sub}
\title{ Creates ABI submission forms }
\description{
  Takes a 96-well plate and converts it into a submission form for ABI genotyping.
}
\usage{
abi_sub(plate, fset, outfile)
}
\arguments{
  \item{plate}{ A 96-well plate of samples }
  \item{fset}{ filter set to be used }
  \item{outfile}{ Name of the outfile (ABI submission form) to be created }
}
\details{
  This function takes a 96-well plate and converts it into an ABI format submission file.  The well location is indicated.  A sample name is created by concatenating the well and the sample name.  The color number is indicated.  The standard dye, dye set, and color info is set based on the color info input by the user.  A color comment is created by concatenating the sample name (with well number), the color info, and the information provided in \sQuote{loci\_cats.}  This is output to a comma delimited file which can be directly used by the ABI software or it can be opened with a spreadsheet and saved in a proprietary format.
}
\value{
  Returns a NULL.
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
