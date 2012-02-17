\name{beta function stuff}
\alias{beta.values}
\alias{inv.beta.values}
\alias{plotbeta}
%
\title{ beta-function functions }
\description{\preformatted{
 \code{beta.values} -- mean, mode, sigma, of beta density function
                       given beta parameters
 \code{inv.beta.values} -- parameter of beta density function given
                           mode and sigma
 \code{plotbeta} -- plot beta density function
                       
}}
\usage{
beta.values(a, b, x1 = 0, x2 = 1)
inv.beta.values(mode, sigma, x1 = 0, x2 = 1, range = c(1, 100))
plotbeta(a, b, x1, x2, add = FALSE, plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{ -- a-parameter of beta density function }
  \item{b}{ -- b-parameter of beta density function }
  \item{x1}{ -- minimum x-value for non-standard beta }
  \item{x2}{ -- maximum x-value for non-standard beta }
  \item{mode}{ -- mode of the beta density function }
  \item{sigma}{ -- sigma of the beta density function }
  \item{range}{ -- range of x values to search }
  \item{add}{ -- logical: add to existing plot? }
  \item{plot}{ -- logical: draw plot? }
}
\details{
  \code{range} gives the range of x values within which
  \code{inv.beta.values} must search for a root solution. Normally the
  default range is sufficient. If the search fails, try a wider range.  
}
\value{

  \item{\code{beta.values()}:}{
    returns a named vector with the mode, mean and standard
    deviation of the beta density function.
  }
  
  \item{\code{inv.beta.values()}:}{
    returns a named vector with a and b
    parameters of the beta density function.
  }  

  \item{\code{plotbeta()}:}{
    plots the beta density function unless \code{plot} is
  \code{FALSE}, in which case \code{plotbeta()} returns a 2 column matrix with 100 x
  values over range from \code{x1} to \code{x2} and associated values of
  the beta density function.
  }

}
\author{ Pierre Kleiber }
\keyword{ package }

