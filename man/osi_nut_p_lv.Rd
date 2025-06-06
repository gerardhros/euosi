% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_nut_phosphorus.R
\name{osi_nut_p_lv}
\alias{osi_nut_p_lv}
\title{Calculate the phosphorus excess index in Latvia}
\usage{
osi_nut_p_lv(A_P_DL, B_TEXTURE_USDA, B_LU = NA_character_)
}
\arguments{
\item{A_P_DL}{(numeric) The exchangeable P-content of the soil measured via Double Lactate extraction (mg P/ kg)}

\item{B_TEXTURE_USDA}{(character) The soil texture according to USDA classification system}

\item{B_LU}{(character) The crop code}
}
\value{
The phosphorus excess index in Latvia estimated from extractable phosphorus. A numeric value.
}
\description{
This function calculates the phosphorus excess.
}
\examples{
osi_nut_p_lv(A_P_DL = 45,B_TEXTURE_USDA = 'S')

}
