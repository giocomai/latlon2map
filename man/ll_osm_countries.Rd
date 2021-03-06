% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ll_osm_countries}
\alias{ll_osm_countries}
\title{Countries and geographic entities for which shapefiles are made availabile by Geofabrik}
\format{
A tibble
\describe{
\item{continent}{Name of the continent}
\item{country}{Name of the country}
\item{link}{Link to shapefiles in a tibble}
}

A tibble
\describe{
\item{continent}{Name of the continent}
\item{country}{Name of the country}
\item{link}{Link to shapefiles in a tibble}
}
}
\source{
\url{http://download.geofabrik.de/}

\url{http://download.geofabrik.de/}
}
\usage{
ll_osm_countries

ll_osm_countries
}
\description{
A dataset with all names of countries, continents, as included in the Geofabrik database.
They are used to download files with \code{ll_osm_download()}

A dataset with all names of geographic entities available for direct download as geopackage files
They are used to download files with \code{ll_osm_download_it()}
}
\details{
Links to shapefiles are stored as tibbles. Unnest to see them, e.g.
\code{ll_osm_countries \%>\% tidyr::unnest(link)}
or for a single country:
\code{ll_osm_countries \%>\% dplyr::filter(country == "italy") \%>\% tidyr::unnest(link)}

Links to shapefiles are stored as tibbles. Unnest to see them, e.g.
\code{ll_osm_countries \%>\% tidyr::unnest(link)}
or for a single country:
\code{ll_osm_countries \%>\% dplyr::filter(country == "italy") \%>\% tidyr::unnest(link)}
}
\keyword{datasets}
