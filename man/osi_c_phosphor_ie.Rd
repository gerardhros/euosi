% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_phosphor.R
\name{osi_c_phosphor_ie}
\alias{osi_c_phosphor_ie}
\title{Calculate the phosphate availability index in Ireland}
\usage{
osi_c_phosphor_ie(B_LU, A_P_OL)
}
\arguments{
\item{B_LU}{(numeric) The crop code}

\item{A_P_OL}{(numeric) The P-content of the soil extracted with Olsen (mg/kg)}
}
\value{
The phosphate availability index in Ireland derived from extractable soil P fractions. A numeric value.
}
\description{
This function calculates the phosphate availability.
}
\examples{
osi_c_phosphor_ie(B_LU = '265',A_P_OL = 5)
osi_c_phosphor_ie(B_LU = c('265','1019'),A_P_OL = c(3.5,5.5))

}
