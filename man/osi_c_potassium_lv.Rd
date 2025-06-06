% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_potassium.R
\name{osi_c_potassium_lv}
\alias{osi_c_potassium_lv}
\title{Calculate the potassium availability index in Latvia}
\usage{
osi_c_potassium_lv(A_K_DL, B_TEXTURE_USDA, B_LU = NA_character_)
}
\arguments{
\item{A_K_DL}{(numeric) The exchangeable K-content of the soil measured via Double Lactate extraction (mg K/ kg)}

\item{B_TEXTURE_USDA}{(character) The soil texture according to USDA classification system}

\item{B_LU}{(character) The crop code}
}
\value{
The potassium availability index in Latvia estimated from extractable potassium A numeric value.
}
\description{
This function calculates the potassium availability.
}
\examples{
osi_c_potassium_lv(A_K_DL = 45,B_TEXTURE_USDA='S')

}
