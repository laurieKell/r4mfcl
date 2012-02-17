\name{scanText}
\alias{scanText}
\title{scanText}
\usage{scanText(string, what=character(0), ...)}
\description{read data from a vector of character strings}
\details{Sets up a connection to a vector of character
strings which is read by scan().}
\value{A vector or list.}
\seealso{scan()}
\author{P. Kleiber}
\arguments{\item{string}{A vector of character strings.}
\item{what}{As for scan. The type of what gives the type of data
to be read. If what is a list, it is assumed that the lines of the
data file are records each containing length(what) items
(``fields'').  The supported types are logical, integer, numeric,
complex, character and list: list values should have elements
which are one of the first five types listed or NULL.}
\item{...}{Other arguments passed to scan().}}
\examples{scanText("A few short words")
as.numeric(scanText("1 2 3\n89 90"))
scanText("A B C 4 5 6", what = list("", "", "", 0, 0, 0))}
