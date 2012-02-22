\name{grp.mat}
\alias{grp.mat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
grp.mat(mat.ce, mat.l, mat.w)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat.ce}{
%%     ~~Describe \code{mat.ce} here~~
}
  \item{mat.l}{
%%     ~~Describe \code{mat.l} here~~
}
  \item{mat.w}{
%%     ~~Describe \code{mat.w} here~~
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
function (mat.ce, mat.l, mat.w) 
{
    dsets <- nrow(mat.ce)
    if (exists("mat.l") & (nrow(mat.l) != dsets)) 
        stop("matrices don't conform")
    if (exists("mat.w") & (nrow(mat.w) != dsets)) 
        stop("matrices don't conform")
    lfint <- ifelse(exists("mat.l"), ncol(mat.l), 0)
    wfint <- ifelse(exists("mat.w"), ncol(mat.w), 0)
    mattt <- matrix(0, nrow = dsets, ncol = lfint + wfint)
    if ((lfint != 0 && wfint == 0) || (lfint == 0 && wfint != 
        0)) {
        if (lfint == 0) {
            mat.l <- mat.w
            lfint <- wfint
        }
        for (i in 1:dsets) {
            if (mat.l[i, 1] == -1) {
                mattt[i, 1] <- -1
            }
            else {
                mattt[i, 1:lfint] <- mat.l[i, ]
            }
        }
    }
    else if (lfint != 0 && wfint != 0) {
        for (i in 1:dsets) {
            if (mat.l[i, 1] == -1) {
                mattt[i, 1] <- -1
                hh <- 2
            }
            else {
                mattt[i, 1:lfint] <- mat.l[i, ]
                hh <- lfint + 1
            }
            if (mat.w[i, 1] == -1) 
                mattt[i, hh] <- -1
            else mattt[i, hh:(hh + wfint - 1)] <- mat.w[i, ]
        }
    }
    cbind(mat.ce, mattt)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
