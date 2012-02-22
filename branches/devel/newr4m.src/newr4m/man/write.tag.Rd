\name{write.tag}
\alias{write.tag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
write.tag(tagfile, tag.obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tagfile}{
%%     ~~Describe \code{tagfile} here~~
}
  \item{tag.obj}{
%%     ~~Describe \code{tag.obj} here~~
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
function (tagfile, tag.obj) 
{
    x <- tag.obj
    a <- "# RELEASE GROUPS    STARTING LENGTH    NUMBER INTERVALS    INTERVAL LENGTH"
    a <- rbind(a, paste("       ", x$hd, collapse = "       "), 
        "#", "#")
    a <- rbind(a, "# TAG RECOVERIES", paste(c("# ", 1:x$hd$nrel), 
        collapse = "   "))
    x$nrecov.grp <- table(factor(x$rel.recov$grp, levels = 1:x$hd$nrel))
    a <- rbind(a, paste(c("  ", x$nrecov.grp), collapse = "   "))
    for (i in 1:x$hd$nrel) {
        a <- rbind(a, "#", "#", "#---------------------------------")
        a <- rbind(a, paste(c("#   ", i, " - RELEASE REGION    YEAR    MONTH   Tag_program ", 
            x$tagprog[i]), collapse = ""))
        a <- rbind(a, paste(c("    ", x$rel[i, ]), collapse = "         "))
        a <- rbind(a, paste(x$rel.lens[i, ], collapse = " "))
        if (x$nrecov.grp[i] > 0) {
            a <- rbind(a, "#", "#", "# LENGTH RELEASE    FISHERY    RECAP YEAR    RECAP MONTH    NUMBER")
            tags <- x$rel.recov[x$rel.recov[, 1] == i, 2:6]
            for (j in 1:dim(tags)[1]) {
                a <- rbind(a, paste(c("", tags[j, ]), collapse = "          "))
            }
        }
    }
    writeLines(a, tagfile)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
