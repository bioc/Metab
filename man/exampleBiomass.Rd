\name{exampleBiomass}
\alias{exampleBiomass}
\docType{data}
\title{
An example of the biomass data frame to be used in conjunction with the function normalizeByBiomass.
}
\description{
The function normalizeByBiomass requires a data frame containing the name of the samples under analysis and their respective biomasses. 
}
\usage{
data(exampleBiomass)
}
\format{
  A data frame containing the name of the samples in the first column and their respective amount of biomass (e.g. grams, O.D., cells, etc...) in the second column. The name of each column will be Sample and Biomass, respectively.
  \describe{
    \item{\code{Sample}}{column containing the name of samples under analysis}
    \item{\code{Biomass}}{column containing the amount of biomass (e.g. grams, O.D., cells, etc...) relative to each sample}
  }
}
\examples{
data(exampleBiomass)
}
\keyword{
biomass
}