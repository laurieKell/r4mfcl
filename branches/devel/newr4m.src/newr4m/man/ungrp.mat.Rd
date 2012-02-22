\name{ungrp.mat}
\alias{ungrp.mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ungrp.mat(frq.obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frq.obj}{
%%     ~~Describe \code{frq.obj} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (frq.obj) 
{
    dsets <- frq.obj$dl$dsets
    if (dsets != nrow(frq.obj$mat)) 
        stop("discrepancy in matrix size")
    lfint <- frq.obj$dl$lfint
    wfint <- frq.obj$dl$wfint
    hh <- ifelse(frq.obj$struct$te >= 6, 7, 6)
    mat.ce <- frq.obj$mat[, 1:hh]
    if (hh == 7) 
        colnames(mat.ce) <- c("year", "qtr", "week", "fishery", 
            "catch", "effort", "cv")
    else colnames(mat.ce) <- c("year", "qtr", "week", "fishery", 
        "catch", "effort")
    mat.l <- matrix(0, dsets, lfint)
    mat.w <- matrix(0, dsets, wfint)
    mat <- frq.obj$mat[, -(1:hh)]
    for (row in 1:dsets) {
        if (mat[row, 1] < 0) {
            mat.l[row, 1] <- mat[row, 1]
            if (wfint > 0) {
                if (mat[row, 2] < 0) 
                  mat.w[row, 1] <- mat[row, 2]
                else mat.w[row, ] <- mat[row, 2:(1 + wfint)]
            }
        }
        else {
            mat.l[row, ] <- mat[row, 1:lfint]
            if (wfint > 0) {
                if (mat[row, 1 + lfint] < 0) 
                  mat.w[row, 1] <- mat[row, 1 + lfint]
                else mat.w[row, ] <- mat[row, (1 + lfint):(lfint + 
                  wfint)]
            }
        }
    }
    list(mat.ce = mat.ce, mat.l = mat.l, mat.w = mat.w)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
