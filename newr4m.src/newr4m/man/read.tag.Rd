\name{read.tag}
\alias{read.tag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.tag(tagfile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tagfile}{
%%     ~~Describe \code{tagfile} here~~
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
function (tagfile) 
{
    op <- options(warn = -1)
    ff <- readLines(tagfile)
    options(op)
    ff <- gsub("\032", "", ff)
    writeLines(ff, "erasethisfile")
    a <- scan("erasethisfile", nlines = 0, comment.char = "#")
    b <- readLines("erasethisfile")
    file.remove("erasethisfile")
    loc <- grep("Tag_program", b)
    d <- b[loc]
    hd <- data.frame(nrel = a[1], l1 = a[2], nint = a[3], iwd = a[4])
    if (length(d) > 0) 
        tagprog <- substring(d, first = nchar(d) - 3)
    else tagprog <- rep("    ", hd$nrel)
    st <- 5
    nd <- st + hd$nrel - 1
    nrecov.grp <- a[st:nd]
    prog <- rep("", hd$nrel)
    rel <- data.frame(reg = rep(0, hd$nrel), y = rep(0, hd$nrel), 
        m = rep(0, hd$nrel))
    rel.lens <- matrix(nrow = hd$nrel, ncol = hd$nint)
    trecov <- sum(nrecov.grp)
    rel.recov <- data.frame(grp = rep(0, trecov), len = rep(0, 
        trecov), fsh = rep(0, trecov), yr = rep(0, trecov), m = rep(0, 
        trecov), n = rep(0, trecov))
    rcount <- 1
    for (g in 1:hd$nrel) {
        st <- nd + 1
        nd <- st + 2
        rel[g, ] <- a[st:nd]
        st <- nd + 1
        nd <- st + hd$nint - 1
        rel.lens[g, ] <- a[st:nd]
        if (nrecov.grp[g] > 0) {
            for (r in 1:nrecov.grp[g]) {
                st <- nd + 1
                nd <- st + 4
                rel.recov[rcount, 1] <- g
                rel.recov[rcount, 2:6] <- a[st:nd]
                rcount <- rcount + 1
            }
        }
    }
    return(list(hd = hd, nrecov.grp = nrecov.grp, tagprog = tagprog, 
        rel = rel, rel.lens = rel.lens, rel.recov = rel.recov))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
