\name{write.par}
\alias{write.par}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
write.par(par.file, par.obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par.file}{
%%     ~~Describe \code{par.file} here~~
}
  \item{par.obj}{
%%     ~~Describe \code{par.obj} here~~
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
function (par.file, par.obj) 
{
    names(par.obj)[grep("^pfl$", names(par.obj))] <- "# The parest_flags"
    names(par.obj)[grep("^nages$", names(par.obj))] <- "# The number of age classes"
    names(par.obj)[grep("^afl$", names(par.obj))] <- "# age flags"
    names(par.obj)[grep("^ffl$", names(par.obj))] <- "# fish flags"
    names(par.obj)[grep("^tfl$", names(par.obj))] <- "# tag flags"
    names(par.obj)[grep("^trpfl$", names(par.obj))] <- "# tag fish rep"
    names(par.obj)[grep("^trpgpfl$", names(par.obj))] <- "# tag fish rep group flags"
    names(par.obj)[grep("^trpacfl$", names(par.obj))] <- "# tag_fish_rep active flags"
    names(par.obj)[grep("^treptarg$", names(par.obj))] <- "# tag_fish_rep target"
    names(par.obj)[grep("^treppen$", names(par.obj))] <- "# treptarg"
    names(par.obj)[grep("^maturity$", names(par.obj))] <- "# percent maturity"
    names(par.obj)[grep("^totpop$", names(par.obj))] <- "# total populations scaling parameter"
    names(par.obj)[grep("^totpop_implicit$", names(par.obj))] <- "# implicit total populations scaling parameter"
    names(par.obj)[grep("^rec_init$", names(par.obj))] <- "# rec init pop level difference"
    names(par.obj)[grep("^rectimes$", names(par.obj))] <- "# recruitment times"
    names(par.obj)[grep("^rel_recruitment$", names(par.obj))] <- "# relative recruitment "
    names(par.obj)[grep("^selectivity$", names(par.obj))] <- "# fishery selectivity"
    names(par.obj)[grep("^Mbase$", names(par.obj))] <- "# natural mortality coefficient"
    names(par.obj)[grep("^effdevcoffs$", names(par.obj))] <- "# effort deviation coefficients"
    names(par.obj)[grep("^meanqs$", names(par.obj))] <- "# average catchability coefficients"
    names(par.obj)[grep("^obj$", names(par.obj))] <- "# Objective function value"
    names(par.obj)[grep("^npars$", names(par.obj))] <- "# The number of parameters"
    names(par.obj)[grep("^gradient$", names(par.obj))] <- "# Maximum magnitude gradient value"
    names(par.obj)[grep("^Richards$", names(par.obj))] <- "# extra par for Richards"
    ops <- options(digits = 12)
    on.exit(options(ops))
    fn <- file(par.file, "w")
    on.exit(close(fn), add = TRUE)
    for (i in 1:par.obj$num.elements) {
        write(names(par.obj)[i], fn, append = TRUE)
        if (is.list(par.obj[[i]])) {
            for (j in 1:length(par.obj[[i]])) {
                write(par.obj[[i]][[j]], fn, ncolumns = length(par.obj[[i]][[j]]), 
                  append = TRUE)
            }
        }
        else {
            dd <- dim(par.obj[[i]])
            if (length(dd) == 1) {
                if (dd > 0) 
                  write(par.obj[[i]], fn, ncolumns = dd, append = TRUE)
            }
            else {
                write.table(par.obj[[i]], fn, quote = FALSE, 
                  sep = " ", row.names = FALSE, col.names = FALSE, 
                  append = T)
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
