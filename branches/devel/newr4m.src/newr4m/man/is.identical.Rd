\name{is.identical}
\alias{is.identical}
\title{ tests equality between two lists}
\description{ Tests equality between components of two lists allowing
  proportional differences of less than 1e-10. Identifies components
  that fail the test. }
\usage{
is.identical(tt, xx, haltpoint = 0)
}
\arguments{
  \item{tt}{ first list}
  \item{xx}{ second list}
  \item{haltpoint}{ component at which to halt with browser enabled}
}
\details{ Tests that number of list components is the same. Identifies
  components that differ. Two numbers, n1 and n2, are considered equal
  if (n1-n2)/n1 < 1e-10.
  For haltpoint>0: if haltpoint points to a component that fails
  equality test, function will halt with browser enabled, allowing the
  each version the component to be examined.
}

\value{ Returns TRUE if equality is determined for all list components,
  otherwise return FALSE.  }

\author{
Pierre Kleiber
}

\seealso{}

\examples {}

\keyword{ equality }
