% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_boron.R
\name{osi_c_boron_fr}
\alias{osi_c_boron_fr}
\title{#' Calculate the B availability index for France}
\usage{
osi_c_boron_fr(A_CLAY_MI, A_BO_HW, B_LU)
}
\arguments{
\item{A_CLAY_MI}{(numeric) The clay content %}

\item{A_BO_HW}{(numeric) The plant available content of B in the soil (mg B per kg) extracted by hot water}

\item{B_LU}{(character) The crop type}
}
\value{
The boron availability index in France estimated from extractable boron, clay, A numeric value.
}
\description{
This function calculates the B availability of a soil, using the agronomic index used in France
}