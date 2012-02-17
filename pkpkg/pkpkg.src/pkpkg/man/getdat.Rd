\name{getdat}
\alias{getdat}
\title{ reads tagged records in a file }
\description{
 Returns data from records in filen which are tagged at the beginning
 of each record with regular expression tag.
}
\usage{
getdat(filen,tag,what=double(0))
}
\arguments{
  \item{filen}{ -- data file name}
  
  \item{tag}{ -- regular expression}
  
  \item{what}{ -- the type of \code{what} gives the type of data to be read. If
    what is a list, it is assumed that the lines of the data file are
    records each containing \code{length(what)} items "fields". The
    supported types are logical, integer, numeric, complex, character
    and list: list values should have elements which are one of the
    first five types listed or NULL.}
}

\value{
  vector of data from records tagged with tag.  Structure of returned
  data (vector or list) depends on the \code{what} argument.  The tag
  portion of each record is not returned.}
  
\author{ P Kleiber }
