% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_boron.R
\name{osi_c_boron_ie}
\alias{osi_c_boron_ie}
\title{Calculate the boron availability index in Ireland}
\usage{
osi_c_boron_ie(B_LU, A_B_HW)
}
\arguments{
\item{B_LU}{(numeric) The crop code}

\item{A_B_HW}{(numeric) The plant available content of B in the soil (mg B per kg) extracted by hot water}
}
\value{
The boron availability index in Ireland derived from extractable soil B fractions. A numeric value.
}
\description{
This function calculates the boron availability.
}
\examples{
osi_c_boron_ie(B_LU = 265,A_B_HW = 50)
osi_c_boron_ie(B_LU = c(265,1019),A_B_HW = c(35,55))

}
