\name{exampleIonLib}
\alias{exampleIonLib}
\docType{data}
\title{
An example of the ionLib required by Metab.
}
\description{
The function MetReport requires a spectral library for identifying metabolites in GC-MS samples.
}
\usage{data(exampleIonLib)}
\format{
  A data frame containing a spectral library.
  \describe{
    \item{\code{Name}}{Example of spectral library}
    \item{\code{RT}}{Example of spectral library}
    \item{\code{ion2to1}}{Example of spectral library}
    \item{\code{ion3to1}}{Example of spectral library}
    \item{\code{ion4to1}}{Example of spectral library}
    \item{\code{ref_ion1}}{Example of spectral library}
    \item{\code{ref_ion2}}{Example of spectral library}
    \item{\code{ref_ion3}}{Example of spectral library}
    \item{\code{ref_ion4}}{Example of spectral library}
  }
}
\examples{
data(exampleIonLib)
print(exampleIonLib)
}
\keyword{datasets}