% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/osi_boron.R
\name{osi_c_boron_nl}
\alias{osi_c_boron_nl}
\title{Calculate the B availability index for agricultural soils in the Netherlands}
\usage{
osi_c_boron_nl(B_LU, A_CLAY_MI, A_SOM_LOI, A_B_HW)
}
\arguments{
\item{B_LU}{(character) The crop type}

\item{A_CLAY_MI}{(numeric) The clay content (\%)}

\item{A_SOM_LOI}{(numeric) The percentage organic matter in the soil}

\item{A_B_HW}{(numeric) The plant available content of B in the soil (mg B per kg) extracted by hot water}
}
\value{
The boron availability index in the Netherlands estimated from hot water extractable boron, and clay, A numeric value.
}
\description{
This function calculates the B availability of a soil, using the agronomic index used in France
}
