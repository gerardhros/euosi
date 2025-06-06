% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_nut_potassium.R
\name{osi_nut_k_it}
\alias{osi_nut_k_it}
\title{Calculate the potassium excess index in Italy}
\usage{
osi_nut_k_it(B_LU, B_TEXTURE_HYPRES, A_K_AAA)
}
\arguments{
\item{B_LU}{(numeric) The crop code}

\item{B_TEXTURE_HYPRES}{(character) The soil texture according to HYPRES classification system}

\item{A_K_AAA}{(numeric) The K-content of the soil extracted with ammoninium acetate (mg K/kg)}
}
\value{
The potassium excess index in Italy derived from extractable soil K fractions. A numeric value.
}
\description{
This function calculates the potassium excess.
}
\examples{
osi_nut_k_it(B_LU = 265,A_K_AAA = 5,B_TEXTURE_HYPRES='C')
osi_nut_k_it(B_LU = c(265,1019),A_K_AAA = c(3.5,5.5),B_TEXTURE_HYPRES=c('C','C'))

}
