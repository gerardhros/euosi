% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_phosphor.R
\name{osi_c_phosphor_es}
\alias{osi_c_phosphor_es}
\title{Calculate the phosphate availability index in Spain}
\usage{
osi_c_phosphor_es(B_LU, A_CLAY_MI, A_SAND_MI, A_P_OL)
}
\arguments{
\item{B_LU}{(numeric) The crop code}

\item{A_CLAY_MI}{(numeric) The clay content of the soil (\%)}

\item{A_SAND_MI}{(numeric) The sand content of the soil (\%)}

\item{A_P_OL}{(numeric) The P-content of the soil extracted with Olsen (mg/kg)}
}
\value{
The phosphate availability index in Spain derived from extractable soil P fractions. A numeric value.
}
\description{
This function calculates the phosphate availability.
}
\examples{
osi_c_phosphor_es(B_LU = '265',A_CLAY_MI = 5,A_SAND_MI = 25,A_P_OL = 5)
osi_c_phosphor_es(B_LU = c('265','1019'),A_CLAY_MI = c(5,10),
A_SAND_MI = c(50,50),A_P_OL = c(3.5,5.5))

}
