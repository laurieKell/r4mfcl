\name{jpegfile}
\alias{jpegfile}
\title{jpegfile}
\usage{jpegfile(fname="R-plot", quality=100, epsdir=FALSE)}
\description{Saves R graphic as a jpeg file.}
\details{Sequesters corresponding eps file in subdirectory "epsfiles"
which is created if necessary. Requires Imagemagick program "convert".}
\value{No return value}
\author{Pierre Kleiber}
\arguments{\item{fname}{Output file name root (or path)}
\item{quality}{jpeg quality parameter (0 - 100)}
\item{epsdir}{If not false, the directory for eps output file.}}
