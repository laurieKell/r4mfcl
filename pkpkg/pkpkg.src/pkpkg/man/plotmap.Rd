\name{plotmap}
\alias{plotmap}

\title{ plot maps }
\description{
  Plots maps using using mapping data obtained from calls to GMT.
}
\usage{
plotmap(lon1, lon2, lat1, lat2, resolution = 3, grid = FALSE,
        add = FALSE, save = FALSE, landcolor = "grey", seacolor = "white")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon1}{ -- longitude at SW corner of map }
  \item{lon2}{ -- longitude at NE corner of map }
  \item{lat1}{ -- longitude at SW corner of map }
  \item{lat2}{ -- longitude at NE corner of map }
  \item{resolution}{ -- integer, 1:5, 1 highest, 5 lowest }
  \item{grid}{ -- logical: draw grid? }
  \item{add}{ -- logical: add to existing plot? }
  \item{save}{ -- logical: save plot data? }
  \item{landcolor}{ color of land areas }
  \item{seacolor}{ color of sea areas }
}
\value{
  If \code{save} is TRUE, the plotting data are returned in a 2 column matrix.
}
\author{ Pierre Kleiber }
\examples{
plotmap(130,250,-5,65,5,landcolor="grey")
}

\keyword{ package }
