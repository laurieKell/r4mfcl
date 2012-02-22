\name{read.frq}
\alias{read.frq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Read MFCL frq file}
\description{ Reads an mfcl frq file into an R object}
\usage{
read.frq(frq.file, frq.title = "", ntop = 0, fishdefs = NA, sf = 7)
}
\arguments{
  \item{frq.file}{ path to mfcl frq file to be read}
  \item{frq.title}{ optional title for the frq object}
  \item{ntop}{ number of lines of commentary following title}
  \item{fishdefs}{ optional data frame containing fishery definitions}
  \item{sf}{ significant figures in numerical data}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ A list with following structure:

 $ frq.title: chr: 
 $ top      : chr vector:
 $ fish     :'data.frame': 
  ..$ fishreg: num: 
  ..$ ctype  : num: 
  ..$ fishery: num: 
  ..$ gear   : num: 
  ..$ nations: num: 
  ..$ areas  : num: 
 $ reg      :List of 3
  ..$ relreg        : num vector: 
  ..$ incidence     : num vector: 
  ..$ seas_reg_flags: num matix: 
 $ struct   :List of 11
  ..$ nreg    : num: 
  ..$ nf      : int: 
  ..$ gendiff : num: 
  ..$ ntg     : num: 
  ..$ yr1     : num: 
  ..$ ta      : num: 
  ..$ tb      : num: 
  ..$ tc      : num: 
  ..$ td      : num: 
  ..$ te      : num: 
  ..$ age_inds: num matrix:
 $ dflags   : num matrix:
 $ mpy      : num: 
 $ mweeks   : num vector: 
 $ dl       :List of 9
  ..$ dsets   : num:
  ..$ lfint   : num: 
  ..$ lffirst : num: 
  ..$ lfwidth : num: 
  ..$ lffactor: num: 
  ..$ wfint   : num: 
  ..$ wffirst : num: 
  ..$ wfwidth : num: 
  ..$ wffactor: num: 
  $ mat      : num matrix:
}
\author{
Simon Hoyle, Pierre Kleiber
}
\note{
%%  ~~further notes~~
}

\seealso{
write.frq
}

\examples{  }

\keyword{ frq file }
