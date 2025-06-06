% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_nut_phosphorus.R
\name{osi_nut_p_cz}
\alias{osi_nut_p_cz}
\title{Calculate the phosphorus excess index in Czech Republic}
\usage{
osi_nut_p_cz(A_P_M3, B_LU = NA_character_)
}
\arguments{
\item{A_P_M3}{(numeric) The exchangeable P-content of the soil measured via Mehlich 3 extracton (mg P/ kg)}

\item{B_LU}{(character) The crop code}
}
\value{
The phosphorus excess index in Czech Republic estimated from extractable phosphorus. A numeric value.
}
\description{
This function calculates the phosphorus excess index
}
\examples{
osi_nut_p_cz(A_P_M3 = 81)

}
