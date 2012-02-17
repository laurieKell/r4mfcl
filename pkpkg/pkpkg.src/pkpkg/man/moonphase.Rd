\name{moonphase}
\alias{moonphase}
\alias{moon.illumination}
\title{ phase of moon on any date }
\description{
  Calculates phase of moon or proportion of moon illuminated
  given the date
}
\usage{
moonphase(ptime)
moon.illumination(ptime)
}
\arguments{
  \item{ptime}{ -- vector of dates in POSIXct form}
}
\value{
  \code{moonphase} returns a vector of moonphase values in radians.
  \code{moon.illumination} returns a vector giving proportion of moon illuminated.
}
\author{ P Kleiber }
