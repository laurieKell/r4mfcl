\name{prepplot}
\alias{prepplot}

\title{ function to set up a plot }
\description{
  Sets up a plotting surface with suitable defaults for x and y ranges,
  and axes drawn if desired. 
}
\usage{
prepplot(x, y, xlim = range(x, na.rm = TRUE), 
         ylim = range(if (zero) 0 else NA, y, na.rm = TRUE), 
         zero = TRUE, xaxis = TRUE, xpos=par("usr")[3], yaxis = TRUE, 
         xtix = xaxis, ytix = yaxis, cex = 1.3, lwd=1, ...)
}
%
\arguments{
  \item{x}{ -- numerical object containing x values }
  \item{y}{ -- numerical object containing y values. Needn't be same size
    as \emph{x}. }
  \item{xlim}{ -- range of \code{x} values. Overrides range of \code{x} values. }
  \item{ylim}{ -- range of \code{y} values. Overrides range of \code{y} values. }
  \item{zero}{ -- logical: y-axis to include value zero? }
  \item{xaxis}{ -- logical: draw x-axis? }
  \item{xpos}{ -- vertical position of x-axis }
  \item{yaxis}{ -- logical: draw y-axis? }
  \item{xtix}{ -- logical: draw x-tics? }
  \item{ytix}{ -- logical: draw y-tics? }
  \item{cex}{ -- character expansion for axis annotation }
  \item{lwd}{ -- axis line width }
  \item{\dots}{ -- other arguments to plot() function }
}
\details{  
  Range of x-axis in determined by numerical range of the \code{x}
  object. Range of y-axis in determined by numerical range of the
  \code{y} object as well as value zero unless argument \code{zero} =
  \option{FALSE}. x and y ranges can be overridden by \code{xlim} or
  \code{ylim} respectively.
}
\value{
  No value. A plot surface is drawn.
}
\author{ Pierre Kleiber }
\examples{
require(pkpkg)
prepplot(1:10,runif(1000,80,100))
prepplot(1:10,runif(1000,80,100),zero=FALSE)
DF <- data.frame(x=1:20,y1=runif(20,3,5),y2=runif(20,8,15))
prepplot(DF$x,DF[,-1])
with(DF,lines(x,y1))
with(DF,lines(x,y2))
}

\keyword{ package }
