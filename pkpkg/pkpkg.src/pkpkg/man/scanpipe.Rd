\name{scanpipe}
\alias{scanpipe}
\alias{read.selection}
\title{ read data through a pipe }
\description{
  scanpipe() sets up a pipe to a shell command and sends output from that command
  to scan(). read.selection() sends sends mouse-selected text to read.table().
}
\usage{
scanpipe(cmd, ...)
read.selection(...)
}
\arguments{
  \item{cmd}{ -- A shell command that returns data readable by function scan() }
  \item{\dots}{ -- arguments passed to scan() or read.table }
}
\details{
  The command must be enclosed in quotes.
}
\value{
  A vector, list (scanpipe), or a data frame (read.selection).
}
\author{ P Kleiber }
\seealso{\link{scan}, \link{read.table}}
\examples{
 x <- scanpipe("awk 'NF>=6{print $6}' myfile.dat",skip=1)
}
