\name{dup_replace}
\alias{dup_replace}
\title{ Replaces duplicate samples with a single sample }
\description{
  Uses the manually edited file created in \sQuote{dup\_checker} to remove duplicate samples in a data.frame.
}
\usage{
dup_replace(all_samps, replace_samps)
}
\arguments{
  \item{all_samps}{ A data.frame of your entire dataset (including duplicates). }
  \item{replace_samps}{ A data.frame of samples created by \sQuote{dup\_checker} and manually edited so only a single instance of each sample appears. }
}
\details{
  Uses the manually edited file created in \sQuote{dup\_checker} to remove duplicate samples in a data.frame.  The samples names in \sQuote{replace\_samps} are used to remove all instances of these names from \sQuote{all\_samps.}  \sQuote{replace\_samps} is then added to the dataset.  Finally \sQuote{order\_sample} is used to order the samples alpha-numericallly. 
}
\value{
  Returns a data.frame that is sorted and includes no duplicates.
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
