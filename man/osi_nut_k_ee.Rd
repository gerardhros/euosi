% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_nut_potassium.R
\name{osi_nut_k_ee}
\alias{osi_nut_k_ee}
\title{Calculate the potassium excess index in Estonia}
\usage{
osi_nut_k_ee(A_K_M3, B_TEXTURE_USDA, B_LU = NA_character_)
}
\arguments{
\item{A_K_M3}{(numeric) The exchangeable K-content of the soil measured via Mehlich 3 extracton (mg P/ kg)}

\item{B_TEXTURE_USDA}{(character) The soil texture according to USDA classification system}

\item{B_LU}{(character) The crop code}
}
\value{
The potassium excess index in Estonia estimated from extractable potassium. A numeric value.
}
\description{
This function calculates the potassium excess.
}
\examples{
osi_nut_k_ee(A_K_M3 = 45,B_TEXTURE_USDA = 'clay')

}
