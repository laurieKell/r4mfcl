\name{gcdist}
\alias{gcdist}
\title{ Great Circle Distances }
\description{
  Calculates great circle distances
}
\usage{
gcdist(lat, lon, lat0, lon0)
}
\arguments{
  \item{lat}{ -- destination latitude in degrees}
  \item{lon}{ -- destination longitude in degrees}
  \item{lat0}{ -- origin latitude in degrees}
  \item{lon0}{ -- origin longitude in degrees}
}
\details{
  Great circle distance between origin and destination
  is calculated with simple spherical geometry.  Any of the arguments
  can be vectors, but all vector arguments must be of same length.  If
  any arguments are vectors, a vector of great circle distances is
  returned. 
}
\value{
  Great circle distance(s) in nautical miles.
}

\author{ Pierre Kleiber}
