% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_nut_potassium.R
\name{osi_nut_k_hu}
\alias{osi_nut_k_hu}
\title{Calculate the potassium excess index in Hungary}
\usage{
osi_nut_k_hu(A_SOM_LOI, A_CLAY_MI, A_CACO3_IF, A_K_AL, B_LU = NA_character_)
}
\arguments{
\item{A_SOM_LOI}{(numeric) The percentage organic matter in the soil (\%)}

\item{A_CLAY_MI}{(numeric) The clay content of the soil (\%)}

\item{A_CACO3_IF}{(numeric) the percentage of CaCO3 (\%)}

\item{A_K_AL}{(numeric) The exchangeable K-content of the soil measured via Ammonium Lactate extracton (mg K/ kg)}

\item{B_LU}{(character) The crop code}
}
\value{
The potassium excess index in Hungary estimated from extractable potassium. A numeric value.
}
\description{
This function calculates the potassium excess.
}
\examples{
osi_nut_k_hu(A_K_AL = 45,A_CACO3_IF = 5,A_CLAY_MI = 5,A_SOM_LOI = 5)

}
