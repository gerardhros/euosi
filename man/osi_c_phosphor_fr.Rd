% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_phosphor.R
\name{osi_c_phosphor_fr}
\alias{osi_c_phosphor_fr}
\title{Calculate the phosphate availability index in France}
\usage{
osi_c_phosphor_fr(
  B_LU,
  A_P_OL,
  B_SOILTYPE_AGR = NA_character_,
  B_AER_FR = NA_character_,
  A_PH_WA = NA_real_
)
}
\arguments{
\item{B_LU}{(character) The crop code}

\item{A_P_OL}{(numeric) The P-content of the soil extracted with Olsen}

\item{B_SOILTYPE_AGR}{(character) The soil type in a particular region. Optional.}

\item{B_AER_FR}{(character) An agroeconomic region in France. Optional.}

\item{A_PH_WA}{(numeric) The pH measured in water.}
}
\value{
The phosphate availability index in France estimated from extractable soil P Olsen (a numeric value).
}
\description{
This function calculates the phosphate availability.
}
\details{
The function has two optional arguments soil type (B_SOILTYPE_AGR) and agricultural region (B_AER_FR). When these are unknown, then the soil type is estimated based on the pH value. Threshold values are then generalized for calcareous and non-calcareous soils.
}
\examples{
osi_c_phosphor_fr(B_LU = 'SOJ', A_P_OL = 45, 
B_SOILTYPE_AGR = 'limons battants', B_AER_FR = 'nord-picardie')

}
