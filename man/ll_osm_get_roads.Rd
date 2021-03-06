% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ll_osm.R
\name{ll_osm_get_roads}
\alias{ll_osm_get_roads}
\title{Extract shape files of roads from previously downloaded}
\usage{
ll_osm_get_roads(country)
}
\arguments{
\item{country}{The name of one or more geographic entities from files typically previously downloaded with \code{sn_download_osm()}}
}
\value{
Nothing, used for its side effects (extracts shapefiles from country-level zip files)
}
\description{
Extract shape files of roads from previously downloaded
}
\examples{
\dontrun{
ll_osm_get_roads(country = "Romania")
}

}
