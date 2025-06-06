% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_phosphor.R
\name{osi_c_phosphor_be}
\alias{osi_c_phosphor_be}
\title{Calculate the phosphorus availability index in Belgium}
\usage{
osi_c_phosphor_be(B_LU, A_P_AL)
}
\arguments{
\item{B_LU}{(character) The crop code}

\item{A_P_AL}{(numeric) The exchangeable P-content of the soil measured via ammonium lactate extraction}
}
\value{
The phosphorus availability index in Belgium estimated from extractable phosphorus. A numeric value.
}
\description{
This function calculates the phosphorus availability.
}
\examples{
osi_c_phosphor_be(B_LU = 'SOJ', A_P_AL = 45)

}
