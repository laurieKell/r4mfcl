\name{unfactor}
\alias{unfactor}
\alias{defactor}
\title{ functions to change factor variables to original stuff }
\description{
  Returns list or data.frame with all factor elements unfactored
}
\usage{
defactor(list)
unfactor(x)
}
\arguments{
  \item{x}{ -- factor type vector}
  \item{list}{ -- list or data frame with some elements (columns) as factors}
}
\value{
  \code{unfactor} returns a vector corresponding to input vector but with
  actual data rather than factor levels.  \code{defactor} returns a list
  or data frame corresponding to the input but with all columns treated
  by \code{unfactor}.
}
\author{ P Kleiber }
