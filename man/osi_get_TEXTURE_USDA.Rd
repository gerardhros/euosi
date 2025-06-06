% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_helpers.R
\name{osi_get_TEXTURE_USDA}
\alias{osi_get_TEXTURE_USDA}
\title{Estimate soil texture according to USDA classification}
\usage{
osi_get_TEXTURE_USDA(A_CLAY_MI, A_SILT_MI, A_SAND_MI, type = "code")
}
\arguments{
\item{A_CLAY_MI}{(numeric) Clay content (\%)}

\item{A_SILT_MI}{(numeric) Silt content (\%)}

\item{A_SAND_MI}{(numeric) Silt content (\%)}

\item{type}{(character) return HYPRES classification names or codes (options: 'name' or 'code')}
}
\value{
Texture classifcation according to the USDA classification system
}
\description{
Estimate soil texture according to USDA classification
}
